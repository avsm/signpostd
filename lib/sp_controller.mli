(*
 * Copyright (c) 2005-2012 Anil Madhavapeddy <anil@recoil.org>, 
 *                         Charalampos Rotsos <cr409@cl.cam.ac.uk>
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

(* register a callback for a specific flow match *)

type pkt_in_cb_struct 

type switch_state = {
  mutable mac_cache: (Openflow.Packet.eaddr, Openflow.Packet.Port.t) Hashtbl.t; 
  mutable dpid: Openflow.Packet.datapath_id list;
  mutable of_ctrl: Openflow.Controller.state list;
  mutable pkt_in_cb_cache : pkt_in_cb_struct list;
  cb_register : (Openflow.Packet.Match.t, (Openflow.Controller.state ->Openflow.Packet.datapath_id -> 
                   Openflow.Controller.Event.e -> unit Lwt.t) ) Hashtbl.t;
} 

val switch_data : switch_state
(* setup a listening openflow controller *)
val listen : ?port:int -> unit -> unit Lwt.t

val register_handler : Openflow.Packet.Match.t -> 
  (Openflow.Controller.state -> Openflow.Packet.datapath_id -> 
     Openflow.Controller.Event.e -> unit Lwt.t) -> unit
val unregister_handler : Openflow.Packet.Match.t -> 
  (Openflow.Controller.state -> Openflow.Packet.datapath_id -> 
     Openflow.Controller.Event.e -> unit Lwt.t) -> unit

val add_dev : string -> string -> string -> unit Lwt.t
val del_dev : string -> string -> string -> unit Lwt.t
