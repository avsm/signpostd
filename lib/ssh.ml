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
    conns_server: conn_type list;
    conns_client: conn_type list;
    mutable max_dev_id : int;
(*     mutable can: unit Lwt.t option; *)
    mutable server_pid: int option;
  }

  let conn_db = {conns=(Hashtbl.create 0); max_id=0; can=None;fd=None;}


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
              Printf.printf "process created with pid %d...\n" pid;
              return ("OK")
      | Some(_) -> 
          Printf.printf "ssh server already started...\n%!";
          return("OK")

  let run_client port ips =
    let send_pkt_to port ip = 
      let buf = String.create 1500 in
      let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM
                 (Unix.getprotobyname "tcp").Unix.p_proto in        
      let ipaddr = (Unix.gethostbyname ip).Unix.h_addr_list.(0) in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
      lwt _ = Lwt_unix.connect sock Unix.ADDR_INET (ipaddr, port) in 
      lwt len = Lwt_unix.recv socket buf 0 1500 [] in 
        Printf.printf "Received (%s) from ipaddr %s\n%!" (String.sub buf 0 len);
        lwt _ = Lwt_unix.close sock in
        return ip
    in
    (* Need a parallel find *)
    Lwt_list.find_s (send_pkt_to port) ips


  let test kind args =
    match kind with
      (* start udp server *)
      | "server_start" -> (
          Printf.printf "starting server...\n%!";
          run_server () 

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
  
  let connect kind args =
    return (["OK"])

  let teardown args =
    true

  let pkt_in_cb controlleri dpid evt = 
    return ()

end
