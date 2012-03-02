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


module IncomingSignalling = SignalHandler.Make (ClientSignalling)


let node_name = ref "unknown"
let node_ip = ref "unknown"
let node_port = ref (of_int 0)
let local_ips = ref []

let usage () = eprintf "Usage: %s <node-name> <node-ip> <node-signalling-port>\n%!" Sys.argv.(0); exit 1

let nxdomain =
  {Dns.Query.rcode=`NXDomain; aa=false; answer=[]; authority=[]; additional=[]}

  (* Figure out the response from a query packet and its question section *)
let get_response packet q =
  let open Dns.Packet in
  let module DQ = Dns.Query in
  (* Normalise the domain names to lower case *)
  let qnames = List.map String.lowercase q.q_name in
  eprintf "Q: %s\n%!" (String.concat " " qnames); 
  nxdomain 
(*     return nxdomain *)
(*
  let from_trie = Dns.(Query.answer_query q.q_name q.q_type Loader.(state.db.trie)) in
  match qnames with
    (* For this strawman, we assume a valid query has form
     * <dst node>.<src node>.<domain name>
     *)
  |dst::src::domain -> begin
     let domain'=String.concat "." domain in
     if domain' = our_domain then begin
       eprintf "src:%s dst:%s dom:%s\n%!" src dst domain';
       ip_resp ~dst ~src ~domain
     end else from_trie
  end
  |_ -> from_trie

*)

let dnsfn ~src ~dst packet =
  let open Dns.Packet in
  match packet.questions with
  |[] -> eprintf "bad dns query: no questions\n%!"; return None
  |[q] -> return (Some (get_response packet q))
  |_ -> eprintf "dns dns query: multiple questions\n%!"; return None

let dns_t () =
  lwt fd, src = Dns_server.bind_fd ~address:"127.0.0.1" ~port:5354 in
    Dns_server.listen ~fd ~src ~dnsfn 

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

let signal_t ~port =
  IncomingSignalling.thread ~address:"0.0.0.0" ~port

let _ =
  (try node_name := Sys.argv.(1) with _ -> usage ());
  (try node_ip := Sys.argv.(2) with _ -> usage ());
  (try node_port := (of_int (int_of_string Sys.argv.(3))) with _ -> usage ());
  let daemon_t = join 
  [ 
    client_t (); 
    signal_t ~port:!node_port;
    dns_t ();
  ] in
  Lwt_main.run daemon_t
