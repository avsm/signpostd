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

  type conn_db_type = {
    conns_server: (string, string) Hashtbl.t;
    conns_client: (string, string) Hashtbl.t;
    mutable max_dev_id : int;
(*     mutable can: unit Lwt.t option; *)
    mutable server_pid: int option;
  }

  let conn_db = {conns_server=(Hashtbl.create 32); conns_client=(Hashtbl.create
  32);
                 max_dev_id=0; server_pid=None;}


  (* start the sshd server *)
  let run_server () =
    Printf.printf "Starting ssh server\n%!";
    (* TODO: Check if pid is still running *)
    match conn_db.server_pid with
      | None ->
          let cmd = Unix.getcwd () ^ "/client_tactics/ssh/server" in
          let _ = Unix.create_process cmd [| |] 
            Unix.stdin Unix.stdout Unix.stderr in
(*             lwt _ = Lwt_unix.sleep 1.0 in *)
            Printf.printf "Server started ...\n%!";
            let buf = String.create 100 in
            let fd = Unix.openfile "/tmp/signpost_sshd.pid" [Unix.O_RDONLY]  0o640 in
            let len = Unix.read fd buf 0 100 in 
              Printf.printf "process created with pid %s...\n" (String.sub buf 0
                                                                  len);
              conn_db.server_pid <- Some(int_of_string (String.sub buf 0 (len-1)));
              Printf.printf "process created with pid %s...\n" (String.sub buf 0 (len-1));
              return("OK")
      | Some(_) -> 
          Printf.printf "ssh server already started...\n%!";
          return("OK")

  let run_client port ips =
    let send_pkt_to port ip = 
      let buf = String.create 20 in
      let sock = (Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM
                 ((Unix.getprotobyname "tcp").Unix.p_proto)) in        
      let ipaddr = (Unix.gethostbyname ip).Unix.h_addr_list.(0) in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
      lwt _ = Lwt_unix.connect sock (Unix.ADDR_INET (ipaddr, port)) in 
      Printf.printf "trying to connect to %s:%d\n%!" ip port;
(*
      lwt len = Lwt_unix.recv sock buf 0 20 [] in  
         Printf.printf "Received (%s) from ipaddr %s\n%!" (String.sub buf 0
         len);
 *)
        Lwt_unix.close sock;
        return true
    in
    (* Need a parallel find *)
    let ret = ref "" in 
    lwt _ = Lwt.choose (List.map (fun ip -> 
                          lwt _ = send_pkt_to port ip in
                          ret := ip;
                          return ()) ips) in 
    return (!ret)

  let update_authorized_keys () = 
      let file = open_out "/root/.ssh/signpost_tunnel" in 
      Hashtbl.iter (fun domain key -> 
          output_string file (key ^ "\n") 
      ) conn_db.conns_client; 
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
      (* Create tap device to connect to *)

    (* Setup an ip address for the new device and return the new ip address *)

      return ("OK")


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

  let setup_dev () =
    conn_db.max_dev_id <- conn_db.max_dev_id + 1;
    lwt _ = Lwt_unix.system 
              (Printf.sprintf "tunctl -t tap%d" conn_db.max_dev_id) in
    lwt _ = Lwt_unix.system 
              (Printf.sprintf "ifconfig tap%d up" conn_db.max_dev_id) in
    lwt _ = Lwt_unix.system 
              (Printf.sprintf 
                 "ifconfig tap%d 10.2.%d.1" conn_db.max_dev_id
              conn_db.max_dev_id) in
                return (Printf.sprintf "10.2.%d.1" conn_db.max_dev_id)


  let connect kind args =
      match kind with
      | "server" ->
        Printf.printf "Setting up the ssh daemon...\n%!";
        server_add_client (List.hd args);
        setup_dev ()
      | _ -> 
        Printf.eprintf "Invalid connect kind %s\n%!" kind;
        raise (SshError "Invalid connect kind")


  let teardown args =
    true

  let pkt_in_cb controlleri dpid evt = 
    return ()

end
