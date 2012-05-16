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
    lwt (client_sock, client_addr) = accept server_sock in
    let  Unix.ADDR_INET(ip, port) = client_addr in 
    let ip = (Uri_IP.string_to_ipv4 (Unix.string_of_inet_addr ip)) in
    let rcv_buf = String.create 2048 in 
    lwt recvlen = Lwt_unix.recv client_sock 
      rcv_buf 0 1048 [] in
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
  done

(*
let test_state = 
  Lwt.choose [(netpanch_daemon ())]
*)

let connect_to_test_server ip port =
  let client_sock = socket PF_INET SOCK_STREAM 0 in
  let hentry = Unix.inet_addr_of_string (Uri_IP.ipv4_to_string ip) in
  lwt _ = Lwt_unix.connect client_sock (ADDR_INET (hentry, port)) in 
  let data = "Hello world!!!" in
  lwt _ = Lwt_unix.send client_sock data 0 (String.length data) [] in
    printf "client connected\n%!";
    return (Lwt_unix.shutdown client_sock SHUTDOWN_ALL)

let handle_incoming_synack_packet controller dpid evt =
  printf "[natpunch] Packet received\n%!";
  let (pkt, port, buffer_id) = match evt with 
    | Controller.Event.Packet_in(port, buffer_id, pkt, dpid) -> 
        (pkt,port,buffer_id)
    | _ -> eprintf "Unknown event";failwith "Invalid of action"
  in
  let m = OP.Match.parse_from_raw_packet port pkt in 
  let actions = [OP.Flow.Output((OP.Port.Local), 2000);] in
  let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD 
              ~buffer_id:(Int32.to_int buffer_id) actions () in 
  let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
  lwt _ = OC.send_of_data controller dpid bs in 
  
  let actions = [OP.Flow.Output(port, 2000);] in
   let m = OP.Match.({wildcards=(OP.Wildcards.exact_match);
                     in_port=OP.Port.Local; dl_src=m.OP.Match.dl_dst;
                     dl_dst=m.OP.Match.dl_src; dl_vlan=0xffff;
                     dl_vlan_pcp=(char_of_int 0); dl_type=0x0800;
                     nw_src=m.OP.Match.nw_dst; nw_dst=m.OP.Match.nw_src;
                     nw_tos=(char_of_int 0); nw_proto=(char_of_int 6);
                     tp_src=m.OP.Match.tp_dst; tp_dst=m.OP.Match.tp_src}) in 
  let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
  OC.send_of_data controller dpid bs
 

let http_pkt_in_cb controller dpid evt = 
  printf "[natpunch] Packet received\n%!";
  let (pkt, port, buffer_id) = match evt with 
    | Controller.Event.Packet_in(port, buffer_id, pkt, dpid) -> (pkt,port,buffer_id)
    | _ -> eprintf "Unknown event";failwith "Invalid of action"
  in
  let m = OP.Match.parse_from_raw_packet port pkt in 
  let isn = Tcp.get_tcp_sn pkt in  
  let rpc = 
    (Rpc.create_tactic_request "natpanch" 
       Rpc.TEST "server_connect" 
       [(Uri_IP.ipv4_to_string m.OP.Match.nw_src);
        (string_of_int m.OP.Match.tp_src); 
        (string_of_int m.OP.Match.tp_dst); 
        (Int32.to_string isn);]) in
  let a = Hashtbl.find natpanch_state.node_name m.OP.Match.nw_dst in 
  lwt res = (Nodes.send_blocking a rpc) in
  let flow_wild = OP.Wildcards.({
    in_port=true; dl_vlan=true; dl_src=true; dl_dst=true;
    dl_type=false; nw_proto=false; tp_dst=false; tp_src=true;
    nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
    dl_vlan_pcp=true; nw_tos=true;}) in 
  let flow = OP.Match.create_flow_match flow_wild ~dl_type:(0x0800)
               ~nw_proto:(char_of_int 6) 
               ~nw_dst:m.OP.Match.nw_src 
               ~tp_dst: m.OP.Match.tp_src () in 
  Sp_controller.register_handler flow handle_incoming_synack_packet;
  return () 

let connect a b =
  printf "[natpunch] Setting nat punch between host %s - %s\n%!" a b;
(*
  let rpc = (Rpc.create_tactic_request "natpanch" 
    Rpc.TEST "client_connect" [(string_of_int nat_socket)]) in
  lwt res = (Nodes.send_blocking a rpc) in
  lwt res = (Nodes.send_blocking b rpc) in
  let flow_wild = OP.Wildcards.({
    in_port=true; dl_vlan=true; dl_src=true; dl_dst=true;
    dl_type=false; nw_proto=false; tp_dst=false; tp_src=true;
    nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
    dl_vlan_pcp=true; nw_tos=true;}) in 
  let flow = OP.Match.create_flow_match flow_wild ~dl_type:(0x0800)
               ~nw_proto:(char_of_int 6) 
               ~nw_dst:(Hashtbl.find natpanch_state.public_ip b) 
               ~tp_dst:11001 () in 
  Sp_controller.register_handler flow http_pkt_in_cb;
  lwt _ = connect_to_test_server 
            (Hashtbl.find natpanch_state.public_ip b) 11001 in
 *)
(*
  let rpc = (Rpc.create_tactic_request "natpanch" 
    Rpc.TEST "server_listen" [(string_of_int nat_socket)]) in
*)
  let ips = Nodes.get_local_ips b in 
  let rpc = (Rpc.create_tactic_request "natpanch" 
    Rpc.CONNECT "register_host" ([b] @ ips)) in
  lwt res = (Nodes.send_blocking a rpc) in
     return ()

(* ******************************************
 * A tactic to setup a layer 2 ssh tunnel
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
             return (Sp.ResponseError "ssh_connect"))
      | TEARDOWN ->
           eprintf "Ssh doesn't support teardown action\n%!";
             return(Sp.ResponseError "Ssh teardown is not supported yet")

let handle_notification action method_name arg_list =
  match method_name with 
    | "server_connect" ->
        (let a = List.nth arg_list 0 in 
        let nw_src = List.nth arg_list 1 in
        let tp_src = List.nth arg_list 2 in
        let tp_dst = List.nth arg_list 3 in
        let isn = List.nth arg_list 4 in
        let nw_dst = Hashtbl.find natpanch_state.public_ip
                  a in         
        let rpc = 
          (Rpc.create_tactic_request "natpanch" 
             Rpc.TEST "server_connect" 
             [nw_src; tp_src; tp_dst; isn;]) in

(*         lwt res = (Nodes.send_blocking a rpc) in *)
        lwt _ = Nodes.send_blocking a rpc in 
          return () )
    | _ -> 
        (eprintf "[natpanch] tactic doesn't handle notifications\n%!";
        return ())
