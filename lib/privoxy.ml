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

  let test kind args =
    return ("OK")
  
  let connect kind args =
    return (["OK"])

  let teardown args =
    true

  let pkt_in_cb controller dpid evt = 
    return ()

end
