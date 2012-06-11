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
(*   (module DirectConnection : Sp.TacticSig); *)
   (module OpenvpnConnection : Sp.TacticSig);  
(*   (module PrivoxyConnection : Sp.TacticSig);  *)
(*   (module TorConnection : Sp.TacticSig);  *)
(*  (module SshConnection : Sp.TacticSig);   *)
(*   (module AvahiConnection : Sp.TacticSig); *)
(*    (module NatpunchConnection : Sp.TacticSig);  *)
  ]

let tactics_not_attempted_or_failed_for a b =
  let tactic_status_fn = 
    Connections.get_tactic_status a b in
  List.filter (fun t ->
    let module Tactic = (val t : Sp.TacticSig) in
    let name = Tactic.name () in
    try
      match (tactic_status_fn name) with
        | Connections.SUCCESS_INACTIVE
        | Connections.SUCCESS_ACTIVE -> false
        | _ -> true
    with Not_found ->
      true
  ) tactics

let iter_over_tactics wakener a b =
  lwt _ = Lwt_list.iter_p (fun t ->
    let module Tactic = (val t : Sp.TacticSig) in
    let _ = match (Connections.get_link_status a b) with 
      | Connections.FAILED ->
      Connections.store_tactic_state a b (Tactic.name ()) 
        Connections.IN_PROGRESS None 
      | _ -> () 
    in
    lwt (dir, ip) = Tactic.test a b in 
    lwt (res, dir, conn_id) = Tactic.connect a b (dir, ip) in 
(*     lwt _ = Lwt_unix.sleep 20.0 in  *)
(*     let res =true in  *)
    match res with
      | false -> 
          Printf.printf "XXXXX tactic %s failed\n%!" (Tactic.name ());
          return ()
      | true ->       
          Connections.store_tactic_state a b 
            (Tactic.name ()) Connections.SUCCESS_ACTIVE 
            None;
          lwt _ = Tactic.enable a b (dir, conn_id) in
          return(Lwt.wakeup wakener true)
  ) (tactics_not_attempted_or_failed_for a b) in
  match (Connections.get_link_status a b) with
    | Connections.IN_PROGRESS -> 
        Connections.store_tactic_state a b 
        ("direct") Connections.FAILED 
        None;
      return(Lwt.wakeup wakener false)
    | _ ->  return() 

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
    lwt (dir, ip) = Tactic.test a b in 
    lwt (ret, dir, conn) = Tactic.connect a b (dir,ip) in
    lwt _ = 
      if (ret) then 
        Tactic.enable a b (dir, conn) 
      else
        return (false)
    in
      return ret
  with Not_found ->
    Printf.eprintf "cannot find tactic %s\n%!" tactic;
    return false

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
    match (Connections.get_link_status a b) with 
      | Connections.FAILED -> (
          Printf.printf "[engine] failed tactic\n%!";
          let ret = Uri_IP.ipv4_to_string (Nodes.get_sp_ip b) in
          let waiter, wakener = Lwt.task () in 
          let _ = Lwt.ignore_result(connect wakener a b) in
          lwt res =  waiter in 
            (match res with
              | true -> return (Sp.IPAddressInstance(ret)) 
              | false -> return (Sp.Unreachable) ))
      | Connections.IN_PROGRESS ->
          Printf.printf "[engine] waiting for tactic\n%!";
          lwt res = Connections.wait_for_link a b in
            (match res with
              | Connections.SUCCESS_ACTIVE
              | Connections.SUCCESS_INACTIVE ->
            return(Sp.IPAddressInstance(
              (Uri_IP.ipv4_to_string (Nodes.get_sp_ip b))))
              | _ ->return (Sp.Unreachable) )
      | Connections.SUCCESS_ACTIVE
      | Connections.SUCCESS_INACTIVE-> 
          return(Sp.IPAddressInstance(
          (Uri_IP.ipv4_to_string (Nodes.get_sp_ip b))))
  with exn ->
    Printf.printf "[Nodes] cannot find node %s \n%!" b;
    return(Sp.Unreachable)

