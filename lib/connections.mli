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


type status =
  | IN_PROGRESS
  | OK
  | FAILED


(** Find is the main entry point to the connections module.
 *  Given the name of two endpoints it will attempt to establish
 *  a link between them, and will immediately return with all
 *  known existing links
 *)
val find : Sp.name -> Sp.name -> Sp.addressable list

(** Stores the results of a tactc.
 *  It will replace earlier results from the same tactic
 *  if such already exist.
 *)
val store_addresses : Sp.name -> Sp.name -> 
    Rpc.tactic_name -> status -> (Sp.ip * Sp.ip) list -> unit

(** Stores all known public IPs for a named entity.
 *  It should not be used to store new public IPs that result
 *  from setting up channels using tactics
 *)
val set_public_ips : Sp.name -> Sp.ip list -> unit
