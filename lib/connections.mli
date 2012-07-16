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


type link_state = 
  | SUCCESS_INACTIVE
  | SUCCESS_ACTIVE
  | IN_PROGRESS
  | FAILED

(* val lookup : Sp.name -> Sp.name -> Sp.addressable list *)

(** Stores the results of a tactc.
 *  It will replace earlier results from the same tactic
 *  if such already exist.
 *)
(*
val store_addresses : Sp.name -> Sp.name -> Rpc.tactic_name -> status -> (Sp.ip * Sp.ip) list -> unit
 *)
val store_tactic_state : Sp.name -> Sp.name -> 
      Rpc.tactic_name -> link_state -> int option -> unit

(** Stores all known public IPs for a named entity.
 *  It should not be used to store new public IPs that result
 *  from setting up channels using tactics
 *)
(* val set_public_ips : Sp.name -> Sp.ip list -> unit *)

(** Returns the status of a previously run tactic for
 *  a pair of nodes. Raises Not_found, if a tactic has
 *  not previously been run for the named pair 
 *)
val wait_for_link : Sp.name -> Sp.name -> link_state Lwt.t

val get_link_active_tactic : Sp.name -> Sp.name -> string option 

(** Returns the status of a previously run tactic for
 *  a pair of nodes. Raises Not_found, if a tactic has
 *  not previously been run for the named pair 
 *)
val get_link_status : Sp.name -> Sp.name -> link_state

(** Returns the status of a previously run tactic for
 *  a pair of nodes. Raises Not_found, if a tactic has
 *  not previously been run for the named pair 
 *)
val get_tactic_status : Sp.name -> Sp.name -> 
  Rpc.tactic_name -> link_state
