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

(*
 * A method to instruct client nodes to test if connectivity is possible
 * on tcp port 10000. The function test connectivity from host a to host b.
 * return the ip address the was accesible
 * *)
let pairwise_connection_test a b = 
  try_lwt
    Printf.printf "[ssh] Trying to start ssh service...\n%!";

    let rpc = (Rpc.create_tactic_request "ssh" Rpc.TEST "server_start" []) in
    lwt res = (Nodes.send_blocking a rpc) in
      Printf.printf "[ssh] ssh server started at %s\n%!" b;

      let dst_ips = Nodes.get_local_ips a in
      let not_ips =  Nodes.get_local_ips b in
      let ips = List.filter (fun a -> 
        not (List.mem a not_ips) ) dst_ips in  
      let rpc = (Rpc.create_tactic_request "ssh" 
                 Rpc.TEST "client" ([(string_of_int ssh_port)] @ ips)) in
        (*TODO: check if value is not an ip and return false. *)
       lwt res = (Nodes.send_blocking b rpc) in 
         return (true, res)
  with exn ->
    Printf.eprintf "[ssh] Pairwise test %s->%s failed:%s\n%s\n%!" a b
      (Printexc.to_string exn) (Printexc.get_backtrace ());
    return (false, "")

(* 
 * a function to start ssh server on node node and add 
 * client_name as a permitted connecting node.
 * *)
let start_ssh_server node port client_name =
  let remote_host = client_name^
                    (sprintf ".d%d.%s" Config.signpost_number 
                       Config.domain) in 
  let rpc = (Rpc.create_tactic_request "ssh" 
               Rpc.CONNECT "server" [remote_host;]) in
    try
      lwt res = (Nodes.send_blocking node rpc) in 
  return (res)
    with exn -> 
      Printf.printf "[ssh] server fail on node %s\n%!" node;
      raise Ssh_error

(*
 * a function to start an ssh client that connects to 
 * dst_ip:dst_port with host node and assigns an ip 
 * under the vpn_subnet subnet. 
 * *)
let start_ssh_client host dst_ip dst_port node vpn_subnet = 
  let remote_host = (node^ (sprintf ".d%d.%s" 
                             Config.signpost_number Config.domain)) in
  let rpc = (Rpc.create_tactic_request "ssh" 
               Rpc.CONNECT "client" [dst_ip; 
                                     (string_of_int ssh_port);
                                     remote_host; vpn_subnet;]) in
    try
      lwt res = (Nodes.send_blocking host rpc) in 
  return (res)
    with exn -> 
      Printf.printf "[ssh] client fail %s\n%!" node;
      raise Ssh_error

(*
 * setup an ssh tunnel between hosts a and b where a 
 * will connect to remote ip ip.
 * return ip pair of the ssh tunnel.
 * *)
let init_ssh a b ip = 
  (* Init server on b *)
  lwt a_ip = start_ssh_server a ssh_port b in
  (*Init client on b and get ip *)
  lwt b_ip = start_ssh_client b ip ssh_port a
               a_ip in
  return (a_ip, b_ip)

let start_local_server a b =
  (* Maybe load a copy of the Openvpn module and let it 
   * do the magic? *)
  printf "[ssh] Starting ssh server...\n%!";
  lwt _ = Ssh.Manager.run_server () in 
  let connect_client node = 
    let domain = (sprintf "d%d.%s" 
    Config.signpost_number Config.domain) in 
    let host =  (node^ "." ^ domain) in 
      printf "[ssh] connecting host %s\n%!" host;
      let dev_id = Tap.get_new_dev_ip () in 
        
        Ssh.Manager.server_add_client host dev_id;
        let ip = Printf.sprintf "10.2.%d.1" dev_id in 
        lwt _ = Tap.setup_dev dev_id ip in 
        let rpc = (Rpc.create_tactic_request "ssh" 
                 Rpc.CONNECT "client" [Config.external_ip; 
                                     (string_of_int ssh_port);
                                     domain; ip;]) in
        lwt res = (Nodes.send_blocking node rpc) in 
          return (res)
  in
  try 
    lwt [a_ip; b_ip ] = Lwt_list.map_s connect_client [a; b] in
      return [a_ip; b_ip]
  with ex ->
    Printf.printf "[ssh] client fail %s\n%!" (Printexc.to_string ex);
    failwith (Printexc.to_string ex)

(*
 * a function to setup an ssh tunnel between hosts 
 * a b.
 * *)
let connect a b =
  (* Trying to see if connectivity is possible *)
  eprintf "[ssh] establish tunnel between %s -> %s\n%!" a b;
  lwt (succ, ip) = pairwise_connection_test a b in
  if succ then (
    Printf.printf "[ssh] connect node %s using ip %s\n%!" a ip;
    lwt (a_ip, b_ip) = init_ssh a b ip in 
      return (true)
  ) else
    (* try the reverse direction *)
    lwt (succ, ip) = pairwise_connection_test b a  in
      if succ then (
        Printf.printf "[ssh] connect node %s using ip %s\n%!" a ip;
        lwt (b_ip, a_ip) = init_ssh b a ip in 
          return (true)
      ) else (
        Printf.printf "[ssh] Connecting through server\n%!";
        lwt _ = start_local_server a b  in
          return (true)
      )


(* ******************************************
 * A tactic to setup a layer 2 ssh tunnel
 * ******************************************)

let handle_request action method_name arg_list =
  let open Rpc in
    match action with
      | TEST ->
        (try_lwt 
          lwt ip = Ssh.Manager.test method_name arg_list in
            return(Sp.ResponseValue ip)
        with ex ->  
          return(Sp.ResponseError (Printexc.to_string ex)) )
      | CONNECT ->
          (try 
             lwt ip = Ssh.Manager.connect method_name arg_list in
               return(Sp.ResponseValue ip)            
           with e -> 
             return (Sp.ResponseError "ssh_connect"))
      | TEARDOWN ->
           eprintf "Ssh doesn't support teardown action\n%!";
             return(Sp.ResponseError "Ssh teardown is not supported yet")

let handle_notification action method_name arg_list =
  eprintf "Ssh tactic doesn't handle notifications\n%!";
  return ()
