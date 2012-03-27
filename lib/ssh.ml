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

let ssh_port = 10000

module Manager = struct
  exception SshError of string
  exception MissingSshArgumentError

  type conn_type = {
    ip: string;
    key: string;
  }

  type conn_det = {
      domain: string;
      dev_id: int;
  }

  type server_det = {
      ip : string;
      port : int;
      key : string;
  }

  type conn_db_type = {
    conns_server: (string, server_det) Hashtbl.t;
    conns_client: (string, string) Hashtbl.t;
    mutable conns_cache: conn_det list;
    mutable max_dev_id : int;
(*     mutable can: unit Lwt.t option; *)
    mutable server_pid: int option;
  }

  let conn_db = {conns_server=(Hashtbl.create 32); 
  conns_client=(Hashtbl.create 32); conns_cache=[];
                 max_dev_id=0; server_pid=None;}


  (* start the sshd server *)
  let run_server () =
    (* TODO: Check if pid is still running *)
    match conn_db.server_pid with
      | None ->(
          try
            let cmd = Unix.getcwd () ^ "/client_tactics/ssh/server" in
            let _ = Unix.create_process cmd [| cmd; Config.conf_dir |] 
              Unix.stdin Unix.stdout Unix.stderr in
              let buf = String.create 100 in
              let fd = Unix.openfile "/tmp/signpost_sshd.pid" [Unix.O_RDONLY]  0o640 in
              let len = Unix.read fd buf 0 100 in 
                Printf.printf "process created with pid %s...\n" 
                  (String.sub buf 0 len);
                conn_db.server_pid <- Some(int_of_string (String.sub buf 0 (len-1)));
                Printf.printf "process created with pid %s...\n" (String.sub buf 0 (len-1));
                return("OK")
          with err ->
            Printf.eprintf "error : %s\n%!" (Printexc.to_string err);
            failwith  (Printexc.to_string err)
        )
      | Some(_) -> 
          Printf.printf "ssh server already started...\n%!";
          return("OK")

  let run_client port ips =
    let send_pkt_to port ip = 
      let buf = String.create 1500 in
      let sock = (Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM
                 ((Unix.getprotobyname "tcp").Unix.p_proto)) in        
      let ipaddr = (Unix.gethostbyname ip).Unix.h_addr_list.(0) in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
        try_lwt 
          lwt _ = Lwt_unix.connect sock portaddr in 
          Printf.printf "Connected to %s:%d\n%!" ip port;
          Lwt_unix.setsockopt sock Unix.TCP_NODELAY true;
          Lwt_unix.setsockopt_float sock Unix.SO_RCVTIMEO 1.0;
          lwt len = Lwt_unix.recv sock buf 0 1500 [] in  
          Printf.printf "Received (%s) from ipaddr %s\n%!" (String.sub buf 0 len);
          lwt _ = Lwt_unix.close sock in
          return true
        with ex ->
          Printf.eprintf "error in client test %s\n%!" (Printexc.to_string ex);
          return false
    in
    (* Need a parallel find *)
    Lwt_list.find_s (send_pkt_to port) ips 

  let update_authorized_keys () = 
      let file = open_out "/root/.ssh/signpost_tunnel" in 
      Hashtbl.iter (fun domain key -> 
          output_string file (key ^ "\n") 
      ) conn_db.conns_client; 
      close_out file

  let update_known_hosts () = 
      let file = open_out (Config.conf_dir ^ "/known_hosts") in 
      Hashtbl.iter (fun domain key ->
          Printf.sprintf "[%s]:%d %s\n" key.ip key.port key.key; 
          output_string file (Printf.sprintf "[%s]:%d %s\n" 
            key.ip key.port key.key ) 
      ) conn_db.conns_server; 
      close_out file

  let server_add_client domain = 
      Printf.printf "Adding new permitted key from domain %s\n%!" domain;
      lwt _ = 
          if(Hashtbl.mem conn_db.conns_client domain) then (
            return ()
          ) else (
              lwt key = Key.ssh_pub_key_of_domain ~server:(Config.iodine_node_ip) 
                          ~port:5354 domain in
              match key with 
              | Some(key) -> 
                Hashtbl.add conn_db.conns_client domain (List.hd key);
                return (update_authorized_keys ())
              | None ->
                return (Printf.printf "Couldn't find a valid dnskey record\n%!")
          )
      in
        return ("OK")

  let client_add_server domain ip port = 
      Printf.printf "Adding new server fingerprint from domain %s\n%!" domain;
      lwt _ = 
          if(Hashtbl.mem conn_db.conns_server domain) then (
            return ()
          ) else (
              lwt key = Key.ssh_pub_key_of_domain ~server:(Config.iodine_node_ip) 
                          ~port:5354 domain in
    Printf.eprintf "dns result returned\n%!";
              match key with 
              | Some(key) -> 
                Hashtbl.add conn_db.conns_server domain 
                {key=(List.hd key);port;ip=ip};
                return (update_known_hosts ())
              | None ->
                return (Printf.printf "Couldn't find a valid dnskey record\n%!")
          )
      in
      return ("OK")

  let client_connect server_ip server_port local_dev remote_dev subnet = 
      let cmd = Unix.getcwd () ^ "/client_tactics/ssh/client" in
      let pid = Unix.create_process cmd 
                [|cmd; Config.conf_dir; server_ip;
                (string_of_int server_port);local_dev;remote_dev; |] 
                Unix.stdin Unix.stdout Unix.stderr in
      (*             lwt _ = Lwt_unix.sleep 1.0 in *)
      return (Printf.sprintf "10.2.%s.2" remote_dev)
(*       Printf.printf "Server started ...\n%!"; *)

  let test kind args =
    match kind with
      (* start udp server *)
      | "server_start" -> (
          Printf.printf "starting server...\n%!";
          run_server ()) 

      (* code to send udp packets to the destination*)
      | "client" -> (
          let port :: ips = args in 
            Printf.printf "starting client.. %s\n" port;
            lwt ip = run_client ssh_port ips in
            (Printf.printf "Received a reply from ip %s \n%!" ip);
            return (ip))

      | _ -> (
        Printf.printf "Action %s not supported in test" kind;
        return ("OK"))

  let setup_dev dev_id ip =
    conn_db.max_dev_id <- conn_db.max_dev_id + 1;
    lwt _ = Lwt_unix.system 
              (Printf.sprintf "tunctl -t tap%d" dev_id) in
    lwt _ = Lwt_unix.system 
              (Printf.sprintf "ifconfig tap%d up" dev_id) in
    lwt _ = Lwt_unix.system 
              (Printf.sprintf 
                 "ifconfig tap%d %s netmask 255.255.255.0" 
                 dev_id ip) in
  return (ip)

  let connect kind args =
    match kind with
      | "server" ->
          Printf.printf "Setting up the ssh daemon...\n%!";
          server_add_client (List.hd args);
          conn_db.max_dev_id <- conn_db.max_dev_id + 1;
          let dev_id = conn_db.max_dev_id in 
          let ip = Printf.sprintf "10.2.%d.1" dev_id in 
          setup_dev dev_id ip
      | "client" ->
          Printf.printf "Setting up the ssh client..\n%!";
          let server_ip = List.nth args 0 in 
          let server_port = (int_of_string (List.nth args 1)) in 
          let domain = List.nth args 2 in 
          let subnet = List.nth args 3 in 
            lwt _ = client_add_server domain server_ip server_port in 
            conn_db.max_dev_id <- conn_db.max_dev_id + 1;
            let local_dev = conn_db.max_dev_id in
              (* Ip address is constructed using the dev number in the 3 
               * octet *)
              let remote_dev = List.nth 
                                 (Re_str.split (Re_str.regexp "\\.") subnet) 3 in 
                let ip = Printf.sprintf "10.2.%s.2" remote_dev in 
                lwt _ = setup_dev local_dev ip in                 
                client_connect server_ip server_port (string_of_int local_dev) 
                remote_dev subnet  
      | _ -> 
          Printf.eprintf "Invalid connect kind %s\n%!" kind;
          raise (SshError "Invalid connect kind")


  let teardown args =
    true

  let pkt_in_cb controlleri dpid evt = 
    return ()

end
