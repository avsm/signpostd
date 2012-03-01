(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

let node_name = ref "unknown"
let node_ip = ref "unknown"
let node_port = ref (of_int 0)
let local_ips = ref []

let usage () = eprintf "Usage: %s <node-name> <node-ip> <node-signalling-port>\n%!" Sys.argv.(0); exit 1

let update_server_if_state_has_changed () =
  let ips = Nodes.discover_local_ips () in
  match (ips <> !local_ips) with
  | true -> begin
      local_ips := ips;
      let hello_rpc = Rpc.Hello (!node_name, !node_ip, !node_port, ips) in
      Nodes.send_to_server hello_rpc
  end
  | false ->
      return ()

let client_t () =
  let xmit_t =
    while_lwt true do
      update_server_if_state_has_changed ();
      Lwt_unix.sleep 2.0
    done
  in
  xmit_t

module IncomingSignalling = SignalHandler.Make (ClientSignalling)

let signal_t ~port =
  IncomingSignalling.thread ~address:"0.0.0.0" ~port

let _ =
  (try node_name := Sys.argv.(1) with _ -> usage ());
  (try node_ip := Sys.argv.(2) with _ -> usage ());
  (try node_port := (of_int (int_of_string Sys.argv.(3))) with _ -> usage ());
  let daemon_t = join 
  [ 
    client_t (); 
    signal_t ~port:!node_port
  ] in
  Lwt_main.run daemon_t
