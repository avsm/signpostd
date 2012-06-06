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
open Lwt_list
open Printf

module OP = Ofpacket
module OC = Controller
module OE = Controller.Event

exception Nat_error

let name () = "natpanch"

type natpanch_state_type = {
  public_ip : (string, int32) Hashtbl.t;
  node_name : (int32, string) Hashtbl.t;
}

let nat_socket = 11000

let natpanch_state = 
  {public_ip=(Hashtbl.create 1000); node_name=(Hashtbl.create 1000);}

(* A STUN like server to register the public ips of the nodes if they are behind
 * a NAT. 
 * TODO: need to check if src port translation is used and register that. *)
let netpanch_daemon = 
  printf "[netpanch] Starting intermediate socket..\n%!";
  let server_sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  (* so we can restart our server quickly *)
  Lwt_unix.setsockopt server_sock SO_REUSEADDR true ;

  (* build up my socket address *)
  bind server_sock (ADDR_INET (Unix.inet_addr_any, nat_socket)) ;

  (* Listen on the socket. Max of 10 incoming connections. *)
  listen server_sock 10 ;

  (* accept and process connections *)
  while_lwt true do
    try_lwt
      lwt (client_sock, client_addr) = accept server_sock in
      let  Unix.ADDR_INET(ip, port) = client_addr in 
      let ip = (Uri_IP.string_to_ipv4 (Unix.string_of_inet_addr ip)) in
      let rcv_buf = String.create 2048 in 
      lwt recvlen = Lwt_unix.recv client_sock rcv_buf 0 1048 [] in
      let buf = Bitstring.bitstring_of_string 
                  (String.sub rcv_buf 0 recvlen) in 
      bitmatch buf with 
        | {loc_ip:32; loc_port:16; 
           name_len:16; name:(name_len*8):string} ->
          (printf "received %s from %s:%d external %s:%d\n%!"
            name (Uri_IP.ipv4_to_string loc_ip) loc_port 
            (Uri_IP.ipv4_to_string ip) port;
          Hashtbl.add natpanch_state.public_ip name ip;
          Hashtbl.add natpanch_state.node_name ip name;
          return (Lwt_unix.shutdown client_sock SHUTDOWN_ALL))
  (*     let x = send client_sock str 0 len [] in *)
        | {_} ->
            printf "[natpanch] failed to parse packet\n%!";
            return (Lwt_unix.shutdown client_sock SHUTDOWN_ALL)
    with exn -> 
      Printf.eprintf "[natpanch]daemon error: %s\n%!" 
        (Printexc.to_string exn);
      return ()
  done

let test a b = 
  (* Fetch public ips to store them for mapping reasons *)
  let rpc = (Rpc.create_tactic_request "natpanch" Rpc.TEST "client_connect" 
               [Config.external_ip; (string_of_int nat_socket);]) in
  lwt _ = Lwt_list.map_p (fun n -> Nodes.send_blocking n rpc) [a;b;] in

  (* check if two nodes can connect *)
  let external_ip = 
          Uri_IP.ipv4_to_string (Hashtbl.find natpanch_state.public_ip b) in
  let rpc = (Rpc.create_tactic_request "natpanch" Rpc.TEST "client_test" 
               [external_ip; (string_of_int nat_socket); b; 
                (Uri_IP.ipv4_to_string (Nodes.get_sp_ip b));]) in
  lwt res = Nodes.send_blocking a rpc in
    return (bool_of_string res)

let connect a b =
  printf "[natpunch] Setting nat punch between host %s - %s\n%!" a b;
  lwt test_res = test a b in 
  match test_res with
    | false -> return false
    | true -> 
        (* register an openflow hook for tcp connections destined to specific port *)
        (let ips = Nodes.get_local_ips b in
        let external_ip = 
          Uri_IP.ipv4_to_string (Hashtbl.find natpanch_state.public_ip b) in
        let rpc = (Rpc.create_tactic_request "natpanch" 
                     Rpc.CONNECT "register_host" ([b;external_ip;]@ips)) in
        lwt _ = (Nodes.send_blocking a rpc) in
          return true)

let handle_notification _ method_name arg_list =
  match method_name with 
    | "server_connect" -> (
        try_lwt
          (* connection parameter *)
          let dst = List.nth arg_list 0 in 
          let src = List.nth arg_list 1 in 
          let _ = List.nth arg_list 2 in
          let tp_src = List.nth arg_list 3 in
          let tp_dst = List.nth arg_list 4 in
          let isn = List.nth arg_list 5 in
          (* TODO: High end NAT may use src port mapping which we could detect if we
           * tried to send a packet from the source port to the destination. *)
          let nw_dst = Hashtbl.find natpanch_state.public_ip src in
          let rpc = 
            (Rpc.create_tactic_request "natpanch" 
             Rpc.CONNECT "server_connect" 
             [(Uri_IP.ipv4_to_string nw_dst); 
              tp_src; tp_dst; isn;]) in
          lwt _ = Nodes.send_blocking dst rpc in 
            return () 
        with exn ->
          eprintf "[natpanch]notification error: %s\n%!" 
              (Printexc.to_string exn);
            return()
        )
    | _ -> 
        (eprintf "[natpanch] tactic doesn't handle notifications\n%!";
        return ())

(* ******************************************
 * A tactic to setup a Nat punch
 * ******************************************)
let handle_request action method_name arg_list =
  let open Rpc in
    match action with
      | TEST ->
        (try_lwt 
          lwt ip = Natpunch.Manager.test method_name arg_list in
            return(Sp.ResponseValue ip)
        with ex ->  
          return(Sp.ResponseError (Printexc.to_string ex)) )
      | CONNECT ->
          (try 
             lwt ip = Natpunch.Manager.connect method_name arg_list in
               return(Sp.ResponseValue ip)            
           with e -> 
             return (Sp.ResponseError (sprintf "ssh_connect %s" (Printexc.to_string e))))
      | TEARDOWN ->
           eprintf "Ssh doesn't support teardown action\n%!";
             return(Sp.ResponseError "Ssh teardown is not supported yet")


