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
open Int64

let name () = "Direct connection"

(* ******************************************
 * Try to establish if a direct connection between two hosts is possible
 * ******************************************)

let connect a b =
  (* Now, for fun, send a reply *)
  let send_hello () =
    try 
      let ip, port = Nodes.signalling_channel b in
      let sa = (Signal.Server.addr_from ip port) in
      let rpc = Rpc.create_request "get_local_ips" [] in
      Signal.Server.send_with_response rpc sa >>= fun resp ->
      eprintf "Got response 1: %s\n" resp;
      let rpc = Rpc.create_request "try_connecting_to" ["foo"] in
      Signal.Server.send_with_response rpc sa >>= fun resp ->
      eprintf "Got response 2: %s\n" resp;
      let notification = Rpc.create_notification "test" ["foo";"bar"] in
      Signal.Server.send notification sa
    with Not_found -> return () in
  send_hello () >>= (fun () ->
    eprintf "DirectConnection trying to establish connection between %s and %s\n" a b;
    return ())
