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
exception Openvpn_error

(* ******************************************
 * Try to establish if a direct connection between two hosts is possible
 * ******************************************)

(*
 * TODO:
   * What garbage collection do I need to do in case something went wrong? 
   * How do I enforce the Node module to provide the new ip to the end node? 
   *
 * *)

let pairwise_connection_test a b =
  try 
  let (dst_ip, dst_port) = Nodes.signalling_channel a in
  let rpc = (Rpc.create_tactic_request "openvpn" 
  Rpc.TEST ["server_start"; (string_of_int openvpn_port)]) in
  lwt res = (Signal.Server.send_with_response rpc
    (Lwt_unix.ADDR_INET((Unix.inet_addr_of_string dst_ip), 
        (Int64.to_int dst_port)))) in
  Printf.printf "UDP server started at %s\n%!" a;

  let (dst_ip, dst_port) = Nodes.signalling_channel b in
  let ips = Nodes.get_local_ips a in 
  let rpc = (Rpc.create_tactic_request "openvpn" 
  Rpc.TEST (["client"; (string_of_int openvpn_port)] @ ips)) in
    lwt res = (Signal.Server.send_with_response rpc
        (Lwt_unix.ADDR_INET((Unix.inet_addr_of_string dst_ip), 
            (Int64.to_int dst_port)))) in 
  
  let (dst_ip, dst_port) = Nodes.signalling_channel a in
  let rpc = (Rpc.create_tactic_request "openvpn" 
  Rpc.TEST ["server_stop"; (string_of_int openvpn_port)]) in
  lwt res = (Signal.Server.send_with_response rpc
      (Lwt_unix.ADDR_INET((Unix.inet_addr_of_string dst_ip), 
          (Int64.to_int dst_port)))) in 
   return (true, res)
  with exn ->
    Printf.eprintf "Pairwise test %s->%s failed\n%!" a b;
    return (false, "")
(*    (true, "127.0.0.2") *)

let start_vpn_server node port =
  let (dst_ip, dst_port) = Nodes.signalling_channel node in 
  let rpc = (Rpc.create_tactic_request "openvpn" 
  Rpc.CONNECT ["server"; (string_of_int openvpn_port)]) in
  try
    lwt res = (Signal.Server.send_with_response rpc
        (Lwt_unix.ADDR_INET((Unix.inet_addr_of_string dst_ip), 
            (Int64.to_int dst_port)))) in 
        return (res)
  with exn -> 
    Printf.printf "Failed to start openvpn server on node %s\n%!" node;
    raise Openvpn_error


let start_vpn_client dst_ip dst_port node = 
  return ("")

let init_openvpn a b = 
  (* Init server on b *)
    lwt b_ip = start_vpn_server b openvpn_port in
  (*Init client on b and get ip *)
    lwt _ = start_vpn_client (Nodes.get_local_ips b) openvpn_port a in
  return (b_ip, "")

let start_local_server () =
  (* Maybe load a copy of the Openvpn module and let it 
   * do the magic? *)
  return ()

let connect a b =
  eprintf "Requesting the nodes ip addresses\n";
  (* Trying to see if connectivity is possible *)
    lwt (succ, ip) = pairwise_connection_test a b in
    if succ then
      lwt (a_ip, b_ip) = init_openvpn a b in 
        return ()
    else
      (* try the reverse direction *)
      lwt (succ, ip) = pairwise_connection_test b a  in
      if succ then
        lwt (b_ip, a_ip) = init_openvpn b a in
            return ()
      else
        lwt _ = start_local_server () in
        let ip = Config.external_ip in
        lwt [a_ip; b_ip] = (Lwt_list.map_p 
            (start_vpn_client ip openvpn_port) [a; b]) in
            return ()
