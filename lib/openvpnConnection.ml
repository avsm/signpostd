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
open Rpc

module OP =Ofpacket
module  OC = Controller

exception Openvpn_error


let openvpn_port = 1194

let name () = "openvpn"

(* ******************************************
 * Try to establish if a direct connection between two hosts is possible
 * ******************************************)

(*
 * TODO:
 * What garbage collection do I need to do in case something went wrong? 
 * How do I enforce the Node module to provide the new ip to the end node? 
 *
 *)

let pairwise_connection_test a b =
  try_lwt 
    Printf.printf "[openvpn] Trying to start ssh service...\n%!";
(*   let (dst_ip, dst_port) = Nodes.signalling_channel a in *)
    let rpc = (Rpc.create_tactic_request "openvpn" 
      Rpc.TEST "server_start" [(string_of_int openvpn_port)]) in
    lwt res = (Nodes.send_blocking a rpc) in
    Printf.printf "[openvpn] UDP server started at %s\n%!" a;

    let ips = Nodes.get_local_ips a in 
    let rpc = (Rpc.create_tactic_request "openvpn" 
      Rpc.TEST "client" ([(string_of_int openvpn_port)] @ ips)) in
    lwt res = (Nodes.send_blocking b rpc) in   
    let resp_ip = rpc_of_string res in 

    let rpc = (Rpc.create_tactic_request "openvpn" 
      Rpc.TEST "server_stop" [(string_of_int openvpn_port)]) in
    lwt _ = (Nodes.send_blocking a rpc) in 
    return (true, res)
  with exn ->
    let rpc = (Rpc.create_tactic_request "openvpn" 
      Rpc.TEST "server_stop" [(string_of_int openvpn_port)]) in
    lwt _ = (Nodes.send_blocking a rpc) in 
    Printf.eprintf "[openvpn] Pairwise test %s->%s failed:%s\n%!" a b
    (Printexc.to_string exn);
    return (false, "")
(*    (true, "127.0.0.2") *)

let start_vpn_server node port client domain =
  try_lwt
    let rpc = (Rpc.create_tactic_request "openvpn" 
      Rpc.CONNECT "server" [(string_of_int openvpn_port); 
                            client;domain;
                            (Uri_IP.ipv4_to_string (Nodes.get_sp_ip client))]) in
      Nodes.send_blocking node rpc
  with ex -> 
    Printf.printf "[openvpn ]Failed openvpn server %s:%s\n%!" node
      (Printexc.to_string ex);
    raise Openvpn_error

let start_vpn_client dst_ip host dst_port node domain local_ip
      remote_ip = 
  let rpc = (Rpc.create_tactic_request "openvpn" 
               Rpc.CONNECT "client" 
               [dst_ip; (string_of_int openvpn_port);node;domain;
                local_ip; remote_ip;
                (Uri_IP.ipv4_to_string 
                           (Nodes.get_sp_ip node))]) in
  try
    lwt res = (Nodes.send_blocking host rpc) in 
        return (res)
  with ex -> 
    Printf.printf "[openvpn]Failed openvpn client %s: %s\n%!" 
      node (Printexc.to_string ex);
    raise Openvpn_error

let init_openvpn ip a b = 
  (* Init server on b *)
  lwt b_ip = start_vpn_server a openvpn_port 
               (sprintf "%s.d%d" b Config.signpost_number) 
               (sprintf "%s.d%d.%s" b Config.signpost_number
                  Config.domain) in
   let dev_id = List.nth (Re_str.split 
                           (Re_str.regexp "\\.") remote_ip) 2 in 
   let local_ip = Printf.sprintf "10.3.%s.2" dev_id in 
     (*Init client on b and get ip *)
    lwt a_ip = start_vpn_client ip b openvpn_port
                 (sprintf "%s.d%d" a Config.signpost_number) 
                 (sprintf "%s.d%d.%s" a Config.signpost_number
                 Config.domain) local_ip remote_ip in
  return (a_ip, b_ip)

let start_local_server a b =
  (* Maybe load a copy of the Openvpn module and let it 
   * do the magic? *)
  lwt _ = Openvpn.Manager.connect "server" 
                   [(string_of_int openvpn_port); 
                    (sprintf "%s.d%d" a Config.signpost_number) ;
                    (sprintf "d%d.%s" Config.signpost_number
                       Config.domain);] in 
   lwt _ = Openvpn.Manager.connect "server" 
                   [(string_of_int openvpn_port); 
                    (sprintf "%s.d%d" b Config.signpost_number) ;
                    (sprintf "d%d.%s" Config.signpost_number
                       Config.domain);] in 
  return ()

let connect a b =
  try 
  (* Trying to see if connectivity is possible *)
    lwt (succ, ip) = pairwise_connection_test a b in
    if succ then
      lwt (a_ip, b_ip) = init_openvpn ip a b in 
        return true
    else
      (* try the reverse direction *)
      lwt (succ, ip) = pairwise_connection_test b a  in
      if succ then
        lwt (b_ip, a_ip) = init_openvpn ip b a in
            return true
      else
        lwt _ = start_local_server a b in
        let ip = Config.external_ip in
        lwt a_ip = start_vpn_client ip b openvpn_port 
                     (sprintf "d%d" Config.signpost_number) 
                     (sprintf "d%d.%s" Config.signpost_number 
                        Config.domain) in 
        lwt b_ip = start_vpn_client ip a openvpn_port
                     (sprintf "d%d" Config.signpost_number) 
                     (sprintf "d%d.%s" Config.signpost_number 
                        Config.domain) in 
          return true
  with exn ->
    Printf.eprintf "[openvpn] connect failed...\n%!";
    return false

(**********************************************************************
 * Handle tactic signature ********************************************)

let handle_request action method_name arg_list =
  let open Rpc in
    try_lwt 
      match action with
      | TEST ->
        lwt v = (Openvpn.Manager.test method_name arg_list) in
        return(Sp.ResponseValue v)
      | CONNECT ->
        lwt v = (Openvpn.Manager.connect method_name arg_list) in
        return(Sp.ResponseValue v)            
      | TEARDOWN ->
        eprintf "OpenVPN hasn't implemented the teardown action\n%!";
        return(Sp.ResponseError "OpenVPN doesn't support teardown")
    with ex -> 
      return(Sp.ResponseError (Printexc.to_string ex)) 

let handle_notification action method_name arg_list =
  eprintf "OpenVPN tactic doesn't handle notifications\n%!";
  return ()
