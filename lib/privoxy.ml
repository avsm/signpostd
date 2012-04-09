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

module OP = Ofpacket

module Manager = struct
  exception ProxyError of string
  exception MissingProxyArgumentError


  (********************************************************************
   *       Tactic state 
   ********************************************************************)
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

  (*********************************************************************
   * testing code
   * TODO: do we need any tests?
   *********************************************************************)
  let http_pkt_in_cb controller dpid evt = 
    Printf.printf "[proxy] http traffic found\n%!";
    return ()

  let https_pkt_in_cb controller dpid evt = 
    Printf.printf "[proxy] https traffic found\n%!";
    return ()

  let test kind args =
    match kind with 
      | "start" -> 
          return ("OK")
      | "forward" ->
          let flow_wild = OP.Wildcards.({
                in_port=true; dl_vlan=true;
                dl_src=true; dl_dst=true;
                dl_type=false; nw_proto=false;
                tp_src=false; tp_dst=true;
                nw_src=(char_of_int 32); nw_dst=(char_of_int 32);
                dl_vlan_pcp=true; nw_tos=true;
              }) in 
          let flow = OP.Match.create_flow_match flow_wild ~dl_type:(0x0800)
                       ~nw_proto:(char_of_int 6) ~tp_src:80 () in 
          Sp_controller.register_handler flow http_pkt_in_cb;
          return ("OK")
      | _ -> 
          Printf.eprintf "[proxy] Invalid connection kind %s \n%!" kind;
          raise (ProxyError "Invalid connection kind")
  
  (*********************************************************************
   * Connection code
   **********************************************************************)
  let connect kind args =
    return ("OK")

  (************************************************************************
   *         Tearing down connection code
   ************************************************************************)
  let teardown args =
    true


end
