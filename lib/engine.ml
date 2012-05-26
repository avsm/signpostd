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
  (module DirectConnection : Sp.TacticSig);
  (module OpenvpnConnection : Sp.TacticSig);
(*   (module PrivoxyConnection : Sp.TacticSig);  *)
(*   (module TorConnection : Sp.TacticSig);  *)
(*   (module SshConnection : Sp.TacticSig); *)
(*   (module AvahiConnection : Sp.TacticSig); *)
(*   (module NatpunchConnection : Sp.TacticSig); *)
  ]

let tactics_not_attempted_or_failed_for a b =
  let tactic_status_fn = Connections.get_tactic_status_for a b in
  List.filter (fun t ->
    let module Tactic = (val t : Sp.TacticSig) in
    let name = Tactic.name () in
    try
      tactic_status_fn name;
      false
    with Not_found ->
      true
  ) tactics

let iter_over_tactics wakener a b =
  Lwt_list.iter_p (fun t ->
    let module Tactic = (val t : Sp.TacticSig) in
    lwt res = Tactic.connect a b in
    match res with
      | false -> 
          Printf.printf "XXXXX tactic %s failed\n%!" (Tactic.name ());
          return ()
      | true -> return(Lwt.wakeup wakener "")
  ) (tactics_not_attempted_or_failed_for a b)

let connect wakener a b =
  eprintf "Engine is trying to connect %s and %s\n" a b;
  lwt ret = iter_over_tactics wakener a b in
  eprintf "XXXXXX got a first connection yeah!!!!\n%!";
    return () (* (Lwt.wakeup wakener ret) *)

let connect_using_tactic tactic a b = 
  Printf.printf "using tactic %s to connect %s %s\n%!" tactic a b; 
  try_lwt
    lwt t = Lwt_list.find_s ( fun t -> 
    let module Tactic = (val t : Sp.TacticSig) in
      return ( (String.compare tactic (Tactic.name ())) = 0)
    ) tactics in
    let module Tactic = (val t : Sp.TacticSig) in
    Printf.eprintf "found tactic %s\n%!" (Tactic.name ()); 
    let dst_ip = Tactic.connect a b in 
      return ()
    
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
(*   eprintf "Trying to establish new ones\n"; *)
  try_lwt
    let ret = Uri_IP.ipv4_to_string (Nodes.get_sp_ip b) in
    let waiter, wakener = Lwt.task () in 
    let _ = Lwt.ignore_result(connect wakener a b) in
    lwt _ =  waiter in 
      return (Sp.IPAddressInstance(ret))
  with exn ->
    Printf.printf "[Nodes] cannot find node %s \n%!" b;
    return(Sp.Unreachable)

