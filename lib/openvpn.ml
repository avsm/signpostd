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
    dev_id:int;
    mutable nodes: string list;
  }

  type conn_db_type = {
    (* connection details for a specific domain *)
    conns : (string, conn_type) Hashtbl.t;
    mutable max_id : int;
    mutable can: unit Lwt.t option;
    mutable fd: file_descr option;
  }

  let conn_db = {conns=(Hashtbl.create 0); max_id=0; can=None;fd=None;}
(*******************************************************
 *             Testing code 
 *******************************************************)

(*
 * setup an echo udp listening socket. 
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
    with exn -> Printf.eprintf "error: %s\n%!" (Printexc.to_string exn);
                raise (OpenVpnError("Couldn't be a udp server"))
    in
    let send_pkt_to port ip =
      let ipaddr = (Unix.gethostbyname ip).Unix.h_addr_list.(0) in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
        lwt _ = Lwt_unix.sendto sock ip 0 (String.length ip) [] portaddr in 
          return ()
    in
    lwt _ = Lwt_list.iter_p (send_pkt_to port) ips in
     try 
       let _ = setsockopt_float sock SO_RCVTIMEO 1.0 in
       let ret = ref "" in 
       let recv = 
         (lwt (len, _) = Lwt_unix.recvfrom sock buf 0 1500 [] in
        ret := (String.sub buf 0 len);
         return ()) in
         lwt _ = (Lwt_unix.sleep 4.0) <?> recv in 
          lwt _ = Lwt_unix.close sock in
          match (!ret) with
            | "" -> raise (OpenVpnError("Unreachable server"))
            | ip -> return (ip)
     with err -> 
       eprintf "[openvpn] client test error: %s\n%!" 
         (Printexc.to_string err);
        raise (OpenVpnError(Printexc.to_string err))

  let test kind args =
    match kind with
    (* start udp server *)
    | "server_start" -> (
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


(*
 * Old code trying to use the ocaml-crypto-keys library. 
 * TODO: some memory leak creates problems and I need to debug. 
 * *)
  let mkdir_internal dir = 
    try 
      Unix.mkdir dir 0o640
    with 
      | Unix.Unix_error(Unix.EEXIST, _, _) -> ()
      | ex -> failwith (Printexc.to_string ex)

  let setup_tmp_conf_dir node = 
    (* create a tmp file to store any configuration scripts *)
    let domain = node ^ (sprintf ".d%d.%s"
      Config.signpost_number Config.domain) in  
    let conf_dir = Config.tmp_dir ^ "/" ^ domain in

    let _ = mkdir_internal conf_dir in
    lwt _ = Lwt_unix.sleep 1.0 in 

    (* Create temporary vpn key for tunnel *)
    Printf.printf "test %s\n%!"  (conf_dir ^ "/vpn.pem");
    let _ = Key.create_rsa_key (conf_dir ^ "/vpn.pem") 1024 in 
      
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

  let rec connect_to_server domain typ tries =
    Printf.printf "[openvpn] trying to start %s with try %d\n%!" typ tries;
    let remove_file_if_exists file = 
      if (Sys.file_exists file) then 
        Unix.unlink file 
      else
        ()
    in
    let dst_dir = Config.tmp_dir ^ "/" ^ domain in 
    let pid_file = dst_dir ^ "/" ^typ ^ ".pid" in 
    let _ = remove_file_if_exists pid_file in 
    let _ = Unix.create_process "openvpn" 
              [|""; "--config"; 
                (Config.tmp_dir ^ "/" ^ domain ^"/" ^ typ ^ ".conf") |] 
              Unix.stdin Unix.stdout Unix.stderr in
      lwt _ = Lwt_unix.sleep 4.0 in 
    if (Sys.file_exists pid_file) then 
      return (true)
    else 
      (match tries with 
        | 0 -> return (false )
        | tries -> 
            connect_to_server domain typ (tries-1))


    let start_openvpn_server ip port node domain typ = 
      let conn_id = conn_db.max_id + 1 in 
      conn_db.max_id <- conn_id;
      (* /openvpn_tactic.sh 10000 1 d2.signpo.st debian haris 10.10.0.3 tmp/ conf/ *)
      (* Generate conf directories and keys *)
      let cmd = Config.dir ^ "/client_tactics/openvpn/openvpn_tactic.sh" in
      let exec_cmd = 
        if ((Nodes.get_local_name ()) = "unknown" ) then
           Printf.sprintf "%s %s %d %s d%d %s %s %s %s %s "
                   cmd port conn_id Config.domain
                   Config.signpost_number node ip domain Config.conf_dir 
                   Config.tmp_dir   
        else
          Printf.sprintf "%s %s %d %s %s.d%d %s %s %s %s %s "
                   cmd port conn_id Config.domain (Nodes.get_local_name ())
                   Config.signpost_number node ip domain Config.conf_dir 
                   Config.tmp_dir 
      in
      Printf.printf "[openvpn] executing %s\n%!" exec_cmd;
      lwt _ = Lwt_unix.system exec_cmd in 
(*     let _ = connect_to_server domain typ 3 in  *)
      let _ = Unix.create_process "openvpn" 
              [|""; "--config"; 
                (Config.tmp_dir ^ "/" ^ domain ^"/" ^ typ ^ ".conf") |] 
              Unix.stdin Unix.stdout Unix.stderr in
      lwt _ = Lwt_unix.sleep 4.0 in      
        return (conn_id)
      (* start server *)
  
    let server_append_dev node domain typ =
      let cmd = Config.dir ^ 
        "/client_tactics/openvpn/openvpn_append_device.sh" in
      let exec_cmd =  
        if ((Nodes.get_local_name ()) = "unknown" ) then
          Printf.sprintf "%s d%d %s %s %s %s %s"
                   cmd Config.signpost_number node Config.domain 
                   domain Config.conf_dir Config.tmp_dir  
        else
          Printf.sprintf "%s %s.d%d %s %s %s %s %s"
                   cmd  (Nodes.get_local_name ()) Config.signpost_number 
                   node Config.domain domain Config.conf_dir Config.tmp_dir  
      in
      Printf.printf "[openvpn] executing %s\n%!" exec_cmd;
      Lwt_unix.system exec_cmd  
        
    let read_pid_from_file filename = 
        let fd = open_in filename in
        let pid = int_of_string (input_line fd) in 
          close_in fd;
          Printf.printf "[openvpn] process created with pid %d...\n%!" pid;
          pid

  let connect kind args =
    match kind with
    | "server" ->(
      try_lwt
        let port = List.nth args 0 in
        let node = List.nth args 1 in
        let domain = List.nth args 2 in 

        lwt dev_id = 
            if Hashtbl.mem conn_db.conns domain then  (
                let conn = Hashtbl.find conn_db.conns domain in
                if (List.mem (node ^ "." ^ Config.domain) conn.nodes) then ( 
                    (* A connection already exists *)
                    Printf.printf "[openvpn] node %s is already added\n%!" node;
                    return (conn.dev_id)
                ) else (
                    (* Add a domain to the existing domain and restart service *)
                    Printf.printf "[openvpn] server already started. adding
                    device %s\n%!" node;
                    let _ = server_append_dev node domain "server" in
                    conn.nodes <- conn.nodes @ [(node ^ "." ^ Config.domain)];
                    (* restart server *)
                    Unix.kill conn.pid Sys.sigusr1;
                    lwt _ = Lwt_unix.sleep 4.0 in      
                    return (conn.dev_id)
                )
            ) else (
                (* if domain seen for the first time, setup conf dir and start 
                 * server *)
                Printf.printf "[openvpn] starting server and adding device %s\n%!" 
                         node;
                lwt dev_id = start_openvpn_server "0.0.0.0" port 
                       node domain "server" in 
                 let pid = read_pid_from_file (Config.tmp_dir ^ "/" ^ 
                             domain ^"/server.pid") in 
                 Hashtbl.add conn_db.conns (domain) 
                           {ip=None;port=(int_of_string port);pid;
                           dev_id;nodes=[node ^ "." ^ Config.domain]};
                           return(dev_id) ) 
        in
        let ip = Nodes.discover_local_ips  
          ~dev:("tap"^(string_of_int dev_id)) () in 
        return ((List.hd ip))
      with e -> 
        eprintf "[openvpn] server error: %s\n%!" (Printexc.to_string e); 
        raise (OpenVpnError((Printexc.to_string e)))
    )
    | "client" -> (
      try_lwt
        let ip :: port :: node :: domain :: args = args in
        lwt dev_id = start_openvpn_server ip port node domain 
                       "client" in  
        let pid = read_pid_from_file (Config.tmp_dir ^ "/" ^ 
                    domain ^  "/client.pid") in 
        Hashtbl.add conn_db.conns (domain) 
                     {ip=Some(ip); port=(int_of_string port);
                     pid;dev_id; nodes=[node^ "." ^ Config.domain];};
        let rec get_openvpn_ip = function
          | 0 -> raise( OpenVpnError("failed to start client"))
          | tries -> 
              let ip = Nodes.discover_local_ips 
                         ~dev:("tap"^(string_of_int dev_id)) () in
                if ((List.length ip) >= 1) then
                  return ((List.hd ip))
                else (
                  lwt _ = Lwt_unix.sleep 1.0 in 
                    get_openvpn_ip (tries - 1)
                )
        in
          get_openvpn_ip 10
      with ex ->
        raise(OpenVpnError(Printexc.to_string ex)))
    | _ -> raise(OpenVpnError(
        (Printf.sprintf "[openvpn] invalid invalid action %s" kind)))

  let teardown args =
    (* kill openvpn pid*)
    let domain = List.hd args in 
    (* Destroy state *)
    if (Hashtbl.mem conn_db.conns domain) then
      Hashtbl.remove conn_db.conns domain;
      true
end
