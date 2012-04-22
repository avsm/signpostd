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


open Lwt
open Printf


let tactics = [
(*  (module DirectConnection : Sp.TacticSig); *)
  (module OpenvpnConnection : Sp.TacticSig);
  (module PrivoxyConnection : Sp.TacticSig); 
  (module TorConnection : Sp.TacticSig); 
  (module SshConnection : Sp.TacticSig);
  ]

let tactics_not_attempted_or_failed_for a b =
  let open List in
  let tactic_status_fn = Connections.get_tactic_status_for a b in
  filter (fun t ->
    let module Tactic = (val t : Sp.TacticSig) in
    let name = Tactic.name () in
    try
      tactic_status_fn name;
      false
    with Not_found ->
      true
  ) tactics

let iter_over_tactics a b =
  let open List in
  Lwt_list.iter_p (fun t ->
    let module Tactic = (val t : Sp.TacticSig) in
    Tactic.connect a b
  ) (tactics_not_attempted_or_failed_for a b)

let connect a b =
  eprintf "Engine is trying to connect %s and %s\n" a b;
  iter_over_tactics a b

let connect_using_tactic tactic a b = 
  let open List in
  Printf.printf "using tactic %s to connect %s %s\n%!" tactic a b; 
  try_lwt
    lwt t = Lwt_list.find_s ( fun t -> 
    let module Tactic = (val t : Sp.TacticSig) in
      return ( (String.compare tactic (Tactic.name ())) == 0)
    ) tactics in
    let module Tactic = (val t : Sp.TacticSig) in
    Printf.eprintf "found tactic %s\n%!" (Tactic.name ()); 
    Tactic.connect a b
    
  with Not_found ->
    return (Printf.eprintf "cannot find tactic %s\n%!" tactic)

let tactic_by_name name =
  try 
    let tactic = List.find (fun t -> 
      let module Tactic = (val t : Sp.TacticSig) in
      Tactic.name () = name) tactics in
      Some(tactic)
    with Not_found ->
      None

let find a b =
  eprintf "Finding existing connections between %s and %s\n" a b;
  eprintf "Trying to establish new ones\n";
  connect a b;
  Connections.lookup a b
