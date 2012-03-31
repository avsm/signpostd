(*
 * Copyright (c) 2012 Charalampos Rotsos <cr409@cl.cam.ac.uk>
 *
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)


open Lwt
open Lwt_unix
open Lwt_list
open Printf

module Manager = struct
  exception OpenVpnError of string
  exception MissingOpenVPNArgumentError

  type conn_type = {
    ip: string option;
    port: int;
    pid: int;
  }

  type conn_db_type = {
    conns : (int, conn_type) Hashtbl.t;
    mutable max_id : int;
    mutable can: unit Lwt.t option;
    mutable fd: file_descr option;
  }

  let conn_db = {conns=(Hashtbl.create 0); max_id=0; can=None;fd=None;}
(*******************************************************
 *             Testing code 
 *******************************************************)

(*
 * setup an echo udp listening socket. :w
 *
 * *)
  let run_server port =
    Printf.printf "[openvpn] Starting udp server\n%!";
    let buf = String.create 1500 in
    let sock =Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM
              (Unix.getprotobyname "udp").Unix.p_proto in
    let _ = 
      try
        (Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_any,
        port)))
      with Unix.Unix_error (e, _, _) ->
        Printf.printf "error: %s\n%!" (Unix.error_message e);
        raise (OpenVpnError("Couldn't be a udp server"))
    in
    (* save socket fd so that we can terminate it *)
    conn_db.fd <- Some(sock);

    (* start background echo udp server to test connectivity*)
    conn_db.can <- Some(while_lwt true do
        lwt (len, ip) = Lwt_unix.recvfrom sock buf 0 1500 [] in
        lwt _ = Lwt_unix.sendto sock (String.sub buf 0 len) 0 len [] ip in
            return (Printf.printf "received %s\n%!" 
                (String.sub buf 0 len) )
        done)

(*
 * a udp client to send data. 
 * *)
  let run_client port ips =
    let buf = String.create 1500 in
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM
              (Unix.getprotobyname "udp").Unix.p_proto in   
    let _ = try
      (* Make this a bit more random*)
        (Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_any,
        10000)))
  with 
      | exn -> Printf.eprintf "error: %s\n%!" (Printexc.to_string exn);
      raise (OpenVpnError("Couldn't be a udp server"))
    in
    let send_pkt_to port ip = 
      let ipaddr = (Unix.gethostbyname ip).Unix.h_addr_list.(0) in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
      let msg = ip in
      Lwt_unix.sendto sock msg 0 (String.length msg) [] portaddr;
      ()
    in
     let _ = List.iter (send_pkt_to port) ips in
     try 
       let _ = setsockopt_float sock SO_RCVTIMEO 2.0 in 
       lwt (len, _) = Lwt_unix.recvfrom sock buf 0 1500 [] in
       lwt _ = Lwt_unix.close sock in
         return ((String.sub buf 0 len))
     with err -> 
       eprintf "[openvpn] client test error: %s\n%!" 
         (Printexc.to_string err);
        raise (OpenVpnError(Printexc.to_string err))

  let test kind args =
    match kind with
    (* start udp server *)
    | "server_start" -> (
      Printf.printf "[openvpn] starting server...\n%!";
      let port = (int_of_string (List.hd args)) in 
      let _ = run_server port in
        return ("OK"))

    (* code to stop the udp echo server*)
    | "server_stop" -> (
      Printf.printf "[openvpn] stoping server...\n%!";
      match conn_db.can with
      | Some t ->
        cancel t;
        conn_db.can <- None;
        (match conn_db.fd with
        | Some(fd) -> (Lwt_unix.close fd; conn_db.fd <- None)
        | _ -> ());
        return ("OK")
      | _ -> return ("OK"))

    (* code to send udp packets to the destination*)
    | "client" -> (
      let port :: ips = args in 
      Printf.printf "[openvpn] starting client..\n";
      lwt ip = run_client (int_of_string port) ips in
      (Printf.printf "[openvpn] Received a reply from ip %s \n%!" ip);
        return (ip))

    | _ -> (
      Printf.printf "[openvpn] Action %s not supported in test" kind;
      return ("OK"))

(***************************************************************
 * Connection code 
 * ************************************************************)

  let setup_tmp_conf_dir node = 
    (* create a tmp file to store any configuration scripts *)
    let domain = node ^ (sprintf ".d%d.%s" 
      Config.signpost_number Config.domain) in  
    let conf_dir = Config.tmp_dir ^ "/" ^ domain in
    let _ = Lwt_unix.mkdir conf_dir in

    (* Create temporary vpn key for tunnel *)
    let _ = Key.create_rsa_key (conf_dir ^ "/vpn.pem") 2048 in 
      
    (* Create signpost self signed key for the device *)
    let convert_struct = 
      Key.({
        in_key=(Config.conf_dir ^  "/signpost.pem");
        in_issuer=(sprintf "C=UK,O=signpost,CN=%s.%s, " 
          (Nodes.get_local_name ()) domain);
        in_ca_priv=(Config.conf_dir ^  "/signpost.pem");
        in_type=PEM_PRIV;
        action=SIGN;
        cert_subj=(sprintf "C=UK,O=signpost,CN=%s.%s, " 
          (Nodes.get_local_name ()) domain);
        out_key= (conf_dir ^ "/tmp.crt");
        out_type=PEM_CERT; 
        duration=24*3600;
        ns_ip=Config.iodine_node_ip;
        ns_port=5354}) in
    lwt local_cert = Key.string_of_process convert_struct in 
    Printf.printf "certificate:%s" local_cert;
    return ()


  let connect kind args =
    let conn_id = conn_db.max_id + 1 in 
    conn_db.max_id <- conn_id;
    match kind with
    | "server" ->(
      try_lwt
        let port = List.nth args 0 in
        let node = List.nth args 1 in
        
        let cmd = Unix.getcwd () ^ "/client_tactics/openvpn/server" in
        lwt _ = setup_tmp_conf_dir node in      
        
        let _ = Unix.create_process cmd 
        [| cmd; port; (string_of_int conn_id);  |] 
            Unix.stdin Unix.stdout Unix.stderr in
(*             lwt _ = Lwt_unix.sleep 1.0 in *)
        Printf.printf "Server started ...\n%!";
        let buf = String.create 100 in
        let fd = Unix.openfile 
          ("./signpost_vpn_server_" ^ (string_of_int conn_id)) 
          [Unix.O_RDONLY]  0o640 in

        let len = Unix.read fd buf 0 100 in 
        Printf.printf "process created with pid %s...\n" (String.sub buf 0
        len);
        let pid = int_of_string (String.sub buf 0 (len-1)) in 
        Printf.printf "process created with pid %d...\n" pid;
        Hashtbl.add conn_db.conns pid {ip=None;port=(int_of_string port);pid;};
        lwt _ = Lwt_unix.sleep 1.0 in
        let ip = Nodes.discover_local_ips ~dev:("tun"^(string_of_int conn_id)) () in 
        return ((List.hd ip))
      with e -> 
        eprintf "[openvpn] server error: %s\n%!" (Printexc.to_string e); 
        raise (OpenVpnError((Printexc.to_string e)))
    )
    | "client" -> 
        let ip :: port :: args = args in
        let cmd = Unix.getcwd () ^ "/client_tactics/openvpn/client" in
        let _ = Unix.create_process cmd 
            [| ""; ip; port; (string_of_int conn_id) |] 
            Unix.stdin Unix.stdout Unix.stderr in
(*             lwt _ = Lwt_unix.sleep 1.0 in *)
        Printf.printf "Server started ...\n%!";
        let buf = String.create 100 in
        let fd = Unix.openfile ("./signpost_vpn_client_" ^ (string_of_int
        conn_id)) [Unix.O_RDONLY]  0o640 in
        let len = Unix.read fd buf 0 100 in 
        let pid = int_of_string (String.sub buf 0 (len-1)) in 
        Printf.printf "process created with pid %d...\n%!" pid;
        Hashtbl.add conn_db.conns pid {ip=Some(ip);port=(int_of_string port);pid;};
        lwt _ = Lwt_unix.sleep 3.0 in
          Printf.printf "device %s created\n%!" ("tun"^(string_of_int conn_id));
        let ip = Nodes.discover_local_ips ~dev:("tun"^(string_of_int conn_id)) () in
          Printf.printf "return ip addr %d\n%!" (List.length ip);
        return ((List.hd ip))        
    | _ -> raise(OpenVpnError(
        (Printf.sprintf "openvpn invalid invalid action %s" kind)))

  let teardown args =
    (* kill openvpn pid*)
    let id = int_of_string(List.hd args) in 
    (* Destroy state *)
    if (Hashtbl.mem conn_db.conns id) then
      Hashtbl.remove conn_db.conns id;
      true
end
