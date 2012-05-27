(*
 * Copyright (c) 2005-2012 Charalampos Rotsos <cr490@cl.cam.ac.uk>
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

module Routing : sig
  type t

  val load_routing_table: unit -> unit Lwt.t
  val get_next_hop: int32 -> (int32 * int32 * string)
  val add_next_hop: int32 -> int32 -> int32 -> string -> unit
  val string_rev : string -> string 
end

(*
module Switching: sig
  type dev_typ = 
    | ETH
    | UNSPEC
  val load_arp : unit -> unit Lwt.t
  val string_of_dev_type : dev_typ -> string
  val string_of_mac : string -> string
  val mac_of_string : string -> string
  val add_entry : string -> int32 option -> string -> dev_typ -> unit
  val mac_of_ip : string ->  (int32 option * string * dev_typ) option
  val port_of_mac : string -> string option
  val ip_of_mac : int32 ->  (string * string * dev_typ) option
end
 *)

module Arp_cache : sig
  val string_of_mac : string -> string
  val mac_of_string : string -> string
  val add_mapping : string -> int32 -> unit
  val mac_of_ip : int32 -> string option
  val ip_of_mac : string -> int32 option
  val get_next_hop_mac: int32 -> string option
  val load_arp : unit -> unit Lwt.t
end

module Port_cache : sig
  val add_dev : string -> int -> unit
  val del_dev : string -> unit
  val dev_to_port_id : string -> int option
  val port_id_to_dev : int -> string option

  val add_mac: string -> int -> unit
  val del_mac: string -> unit
  val port_id_of_mac : string -> int option
  val mac_of_port_id : int -> string option
end
