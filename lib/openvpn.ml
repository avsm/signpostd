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
    mutable fd: file_descr option;
  }

  let conn_db = {conns=(Hashtbl.create 0); max_id=0; can=None;fd=None;}

  let run_server port =
    Printf.printf "Starting udp server\n%!";
    let buf = String.create 1500 in
    let sock =Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM
              (Unix.getprotobyname "udp").Unix.p_proto in
    let _ = try
        (Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_any,
        port)))
       with Unix.Unix_error (e, _, _) ->
      Printf.printf "error: %s\n%!" (Unix.error_message e);
      raise (OpenVpnError("Couldn't be a udp server"))
    in
    conn_db.fd <- Some(sock);

    conn_db.can <- Some(while_lwt true do
        lwt (len, ip) = Lwt_unix.recvfrom sock buf 0 1500 [] in
        lwt _ = Lwt_unix.sendto sock (String.sub buf 0 len) 0 len [] ip in
            return (Printf.printf "received %s\n%!" 
                (String.sub buf 0 len) )
        done)

  let run_client port ips =
    let buf = String.create 1500 in
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM
              (Unix.getprotobyname "udp").Unix.p_proto in   
    let _ = try
      (* Make this a bit more random*)
        (Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_any,
        10000)))
  with 
      | Unix.Unix_error (e, _, _) ->
        Printf.eprintf "error: %s\n%!" (Unix.error_message e);
      raise (OpenVpnError("Couldn't be a udp server"))
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
      lwt (len, _) = Lwt_unix.recvfrom sock buf 0 1500 [] in
      lwt _ = Lwt_unix.close sock in
      return ((String.sub buf 0 len))

  let test args =
   let typ::args = args in
    match typ with
    | "server_start" -> (
      (* start udp server *)
      Printf.printf "starting server...\n%!";
      let port = (int_of_string (List.hd args)) in 
      Printf.printf "starting server on port %d...\n%!" port;
      let _ = run_server port in
        return ("OK")
    )
    | "server_stop" -> (
      Printf.printf "stoping server ... %d\n%!" (List.length args);
      match conn_db.can with
      | Some t ->
        cancel t;
        conn_db.can <- None;
        (match conn_db.fd with
        | Some(fd) -> 
            (Lwt_unix.close fd; conn_db.fd <- None)
        | _ -> ());
        return ("OK")
      | _ -> return ("OK")
          )
    | "client" -> (
      let port :: ips = args in 
        Printf.printf "starting client.. %s\n" port;
      lwt ip = run_client (int_of_string port) ips in
        (Printf.printf "Received a reply from ip %s \n%!" ip);
        return (ip)
    )
    | _ ->  Printf.printf "Action %s not supported in test" typ;
    return ("OK")

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
