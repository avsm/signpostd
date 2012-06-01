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
open List

type link_state = 
  | SUCCESS_INACTIVE
  | SUCCESS_ACTIVE
  | IN_PROGRESS
  | FAILED

type tunnel_state = {
  (* Current state ofthe tunnel *)
  mutable tactic_state : link_state;
  (*
   * Cache what was the result of the last try.
  * *)
  mutable last_res : link_state;
  (* A connection identification in order to be able to tear down 
   * connections.
  * *)
  mutable conn_id : int option;
  (*
   * Is there a local device we should store?
   * TODO: this is not accurate as for a tactic we might 
   * have multiple devices. Maybe make it a list and add
   * a node.
  * *)
  device : string option;
  (* For each tunel a string may have multiple ip addresses 
  * semantically miningful only for the tunnel *)
  address : (string, int32 list) Hashtbl.t;
}


type connection_state = {
  wait: unit Lwt_condition.t; 
  mutable link: link_state;
  tactic : (string, tunnel_state) Hashtbl.t;
}

(*
type _state = {
  connections : (handle * handle, tactic_result list) Hashtbl.t;
}
 *)

let connections = 
  (Hashtbl.create 0)

let in_order a b = a < b

let construct_key a b =
  match in_order a b with
    | true -> (a,b)
    | false -> (b,a)

(**********************************************************************
 * Public API *********************************************************)

let store_tactic_state a b tactic_name link_state conn_id = 
  let key = construct_key a b in
  let link = match (Hashtbl.mem connections key) with
    | true -> Hashtbl.find connections key
    | false -> 
        let link = {wait=(Lwt_condition.create ()); link=link_state;
        tactic=(Hashtbl.create 16);} in 
          Hashtbl.add connections key link;
          link
  in
  let _ = 
     match link_state with 
       | IN_PROGRESS -> ()
       | _ ->
           Lwt_condition.broadcast link.wait ()
   in 
    link.link <- link_state;
    let conn = 
      if (Hashtbl.mem link.tactic tactic_name) then
        Hashtbl.find link.tactic tactic_name
      else (
        let tactic = {tactic_state=link_state; last_res=link_state; 
                      conn_id; device=None;address=(Hashtbl.create 16);} in
          Hashtbl.add link.tactic tactic_name tactic;
          tactic
      )
    in 
      conn.conn_id <- conn_id;
      conn.tactic_state <- link_state;
      ()

let wait_for_link a b =
  let key = construct_key a b in
  try
    let conn = Hashtbl.find connections key in 
      match conn.link with
        | IN_PROGRESS ->
            lwt _ = Lwt_condition.wait conn.wait  in
              return(conn.link)
        | _ -> return(conn.link)
  with Not_found ->
    return(FAILED)

let get_link_status a b =
  let key = construct_key a b in
  try
    let conn = Hashtbl.find connections key in 
      conn.link
  with Not_found ->
    FAILED

let get_tactic_status a b tactic_name =
  let key = construct_key a b in
  try
    let conn = Hashtbl.find connections key in 
    let tactic = Hashtbl.find conn.tactic tactic_name in 
      tactic.tactic_state
  with Not_found ->
    FAILED

