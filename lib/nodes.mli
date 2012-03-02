(*
 * Copyright (c) 2012 Sebastian Probst Eide <sebastian.probst.eide@gmail.com>
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


(* API for sending rpc's to a node *)
val send : Sp.name -> Rpc.rpc -> unit Lwt.t
val send_to_server : Rpc.rpc -> unit Lwt.t
val send_blocking : Sp.name -> Rpc.rpc -> string Lwt.t

(* Let the SignalHandler wake up a pending sender *)
val wake_up_thread_with_reply : Rpc.id -> Rpc.rpc -> unit Lwt.t

(* API for updatating the node store *)
val set_signalling_channel : Sp.name -> Sp.ip -> Sp.port -> unit
val set_local_ips : Sp.name -> Sp.ip list -> unit
val discover_local_ips : ?dev:string -> unit -> Sp.ip list
val check_for_publicly_accessible_ips : Sp.name -> Sp.ip list -> Sp.ip list Lwt.t
val get_local_ips : Sp.name -> Sp.ip list
val get_node_ip : Sp.name -> int32
val convert_ip_string_to_int : Sp.ip -> int32
