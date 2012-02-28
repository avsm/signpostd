(*
 * Copyright (c) 2012 Charalampos Rotsos <cr409@cl.cam.ac.uk>
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
open Lwt_unix
open Printf
open Int64
open Signal

let openvpn_port = 1194

let name () = "OpenVPN connection"

(* ******************************************
 * Try to establish if a direct connection between two hosts is possible
 * ******************************************)

let pairwise_connection_test a b =
  let (dst_ip, dst_port) = Nodes.signalling_channel a in
  let rpc = (Rpc.create_tactic_request "openvpnv" 
  Rpc.TEST ["server"; (string_of_int openvpn_port)]) in
  let res = (Signal.Server.send_with_response rpc
    (Lwt_unix.ADDR_INET((Unix.inet_addr_of_string dst_ip), 
        (Int64.to_int dst_port)))) in 
  (true, "127.0.0.2")

let pairwise_connection_establish a b =
  return ()

let addr_of (ip, port) =
  (Signal.Server.addr_from ip port)

let connect a b =
  let node_a = addr_of (Nodes.signalling_channel a) in
  let node_b = addr_of (Nodes.signalling_channel b) in
  eprintf "Requesting the nodes ip addresses\n";
  pairwise_connection_establish a b >> return ()
  (*
  (* Setup a server on b on openvpn port and ask a to connect to it *)
  let (result, ip) = pairwise_connection_test a b in 
  if result then
    (* setup connectivity *)
    pairwise_connection_establish a b
  else (
    let (result, ip) = pairwise_connection_test b a in
    if (result) then
      pairwise_connection_establish b a
    else
      (* Need to check connectivity through an intermediate step *)
      return ()
  )*)
