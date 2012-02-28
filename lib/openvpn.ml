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

module Manager = struct
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
    let s, w = Lwt.task () in 
    conn_db.can <- Some(s);
    while_lwt true do 
      return (Printf.printf "in server thread \n%!")
     done 

  let test args =
    let typ = List.hd args in
    match typ with
    | "server_start" -> (
      (* start udp server *)
      let port port = List.hd args in 
      let s, w = Lwt.task () in 
      conn_db.can <- Some(s);
      let _ = run_server port in
        return ("OK")
    )
    | "server_stop" -> (
      match conn_db.can with
      | Some t ->
        cancel t;
        conn_db.can <- None;
        return ("OK")
    | None ->
          return ("OK")
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
