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

let list_of_ips_from_string ip_str =
  let open Re_str in
  let remove_character_return = regexp "\n" in
  let on_whitespace = regexp " " in
  let chomped_str = global_replace remove_character_return "" ip_str in
  split on_whitespace chomped_str

let connect a b =
  (* Returns the pairs of lists of ips of node a and b that seem *)
  (* to be in the same LAN *)
  let connectable_ips () =
    try 
      eprintf "Requesting the nodes ip addresses\n";
      let ips_a = Nodes.get_local_ips a in
      let ips_b = Nodes.get_local_ips b in
      eprintf "Have both nodes try to connect to all ips of the other\n";
      let node_a_listen = string_of_int(30000 + (Random.int 20000)) in
      let node_b_listen = string_of_int (30000 + (Random.int 20000)) in
      let token = "hello_there" in
      lwt [success_a; success_b] = Lwt_list.map_p (fun (node, listen_to, connect_to, ips) ->
        let args = listen_to :: connect_to :: token :: ips in
        let rpc = Rpc.create_request "try_connecting_to" args in
        Nodes.send_blocking node rpc >>= fun results ->
        return (list_of_ips_from_string results)
      ) [
          (a, node_a_listen, node_b_listen, ips_b); 
          (b, node_b_listen, node_a_listen, ips_a)
        ] in
      eprintf "A could connect to: %s\n%!" (String.concat ", " success_a);
      eprintf "B could connect to: %s\n%!" (String.concat ", " success_b);
      return []

    (* If we cannot find the signalling channel of one of the nodes, then we
     * cannot either find what IP ranges they have in common.
     * We therefore return that they share no ips *)
    with Not_found -> return [] in

    connectable_ips () >>= fun [] ->
    return ()
