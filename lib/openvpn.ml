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

module Manager = struct

exception OpenVpnError of string

  type conn_type = {
    ip: string;
    port: int;
    pid: int;
  }

  type conn_db_type = {
    conns : (int, conn_type) Hashtbl.t;
    mutable max_id : int;
    mutable can: unit Lwt.t option;
  }

  let conn_db = {conns=(Hashtbl.create 0); max_id=0; can=None;}

  let run_server port = 
    let buf = String.create 1500 in
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM
              (Unix.getprotobyname "udp").Unix.p_proto in   
    try
        (Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_any,
        port)));
    with exn -> 
      Printf.printf
      raise (OpenVpnError("Couldn't be a udp server"));

    conn_db.can <- Some(while_lwt true do
        lwt (len, ip) = Lwt_unix.recvfrom sock buf 0 1500 [] in
            return (Printf.printf "received %s\n%!" 
                (String.sub buf 0 len) )
        done)

  let run_client ips port = 
    let buf = String.create 1500 in
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM
              (Unix.getprotobyname "udp").Unix.p_proto in   
    try
      (* Make this a bit more random*)
        (Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_any,
        10000)));
    with exn -> 
      Printf.printf
      raise (OpenVpnError("Couldn't be a udp server"));

    let send_pkt_to port ip = 
      let ipaddr = (Unix.gethostbyname ip).Unix.h_addr_list.(0)
      let portaddr = Unix.ADDR_INET (ipaddr, port)
      let msg = ip in
      Unix.sendto socket msg 0 (String.length msg) [] portaddr >>
      return ()
    in

    lwt _ = List_lwt.iter_p (send_pkt_to port) ips in
      lwt len = Unix.sendto socket msg 0 (String.length msg) [] portaddr in
      return ((String.sub msg 0 len))

  let test args =
   let typ = List.hd args in
    match typ with
    | "server_start" -> (
      (* start udp server *)
      let port = int_of_string (List.hd args) in 
      let _ = run_server port in
        return ("OK")
    )
    | "server_stop" -> (
      Printf.printf "stoping server ... %d\n%!" (List.length args);
      match conn_db.can with
      | Some t ->
        cancel t;
        conn_db.can <- None;
        return ("OK")
      | _ ->
          return ("OK")
          )
    | "client" -> (
      let port = int_of_string (List.hd args) in 
      lwt ip = send_pkt_to port args in
        (Print.printf "Received a reply from ip %s \n%!" ip)
        >> return (ip)
    )

  let connect args =
    let conn_id = conn_db.max_id + 1 in 
    conn_db.max_id <- conn_id;
    (true, conn_id, "success")

  let teardown args =
    (* kill openvpn pid*)
    let id = int_of_string(List.hd args) in 
    (* Destroy state *)
    if (Hashtbl.mem conn_db.conns id) then
      Hashtbl.remove conn_db.conns id;
      true
end
