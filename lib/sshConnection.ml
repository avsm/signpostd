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


exception Ssh_error

let name () = "ssh"
let ssh_port = 10000


let pairwise_connection_test a b = 
  try
    Printf.printf "Trying to start ssh service...\n%!";
(*   let (dst_ip, dst_port) = Nodes.signalling_channel a in *)
  let rpc = (Rpc.create_tactic_request "ssh" Rpc.TEST "server_start" []) in
  lwt res = (Nodes.send_blocking a rpc) in
  Printf.printf "ssh server started at %s\n%!" b;

  let ips = Nodes.get_local_ips a in 
  let rpc = (Rpc.create_tactic_request "ssh" 
      Rpc.TEST "client" ([(string_of_int ssh_port)] @ ips)) in
  lwt res = (Nodes.send_blocking b rpc) in 
   
  return (true, res)
  with exn ->
    Printf.eprintf "Pairwise test %s->%s failed:%s\n%s\n%!" a b
    (Printexc.to_string exn) (Printexc.get_backtrace ());
    return (false, "")

 let start_ssh_server node port client_name = 
  let rpc = (Rpc.create_tactic_request "ssh" 
    Rpc.CONNECT "server" [client_name;]) in
  try
    lwt res = (Nodes.send_blocking node rpc) in 
    return (res)
  with exn -> 
    Printf.printf "Failed to start openvpn server on node %s\n%!" node;
    raise Ssh_error

let start_ssh_client dst_ip dst_port node = 
  let rpc = (Rpc.create_tactic_request "ssh" 
  Rpc.CONNECT "client" [dst_ip; (string_of_int ssh_port); node]) in
  try
    lwt res = (Nodes.send_blocking node rpc) in 
        return (res)
  with exn -> 
    Printf.printf "Failed to start openvpn server on node %s\n%!" node;
    raise Ssh_error

let init_ssh a b = 
  (* Init server on b *)
    lwt b_ip = start_ssh_server b ssh_port a in
  (*Init client on b and get ip *)
    lwt a_ip = start_ssh_client (List.hd (Nodes.get_local_ips b)) ssh_port b in
  return (a_ip, b_ip)

let start_local_server () =
  (* Maybe load a copy of the Openvpn module and let it 
   * do the magic? *)
  return ()

let connect a b =
  (* Trying to see if connectivity is possible *)
  eprintf "ssh connection %s -> %s...\n%!" a b;
    lwt (succ, ip) = pairwise_connection_test a b in
    if succ then
       lwt (a_ip, b_ip) = init_ssh a b in 
        return ()
    else
      (* try the reverse direction *)
      lwt (succ, ip) = pairwise_connection_test b a  in
      if succ then
         lwt (b_ip, a_ip) = init_ssh b a in 
            return ()
      else
(*         lwt _ = start_local_server () in *)
        let ip = Config.external_ip in
        lwt [a_ip; b_ip] = (Lwt_list.map_p 
            (start_ssh_client ip ssh_port ) [a; b]) in
            return ()
        
     
(* ******************************************
 * A tactic to forward all traffic to the privoxy proxy
 * ******************************************)

let handle_request action method_name arg_list =
  let open Rpc in
  match action with
  | TEST ->
      lwt ip = Ssh.Manager.test method_name arg_list in
        return(Sp.ResponseError ip)
  | CONNECT ->
      eprintf "Ssh doesn't support conect action\n%!";
      return(Sp.ResponseError "Ssh connect is not supported yet")            
  | TEARDOWN ->
      eprintf "Ssh doesn't support teardown action\n%!";
      return(Sp.ResponseError "Ssh teardown is not supported yet")

let handle_notification action method_name arg_list =
  eprintf "Ssh tactic doesn't handle notifications\n%!";
  return ()


(*
let connect a b =
  Printf.eprintf "[privoxy] connecting %s -> %s" a b; 
  return ()
*)