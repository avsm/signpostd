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

type openvpn_client_state_type = {
  mutable name: string;
  mutable tactic_ip: int32;
  mutable extern_ip: int32 option;
(*   mutable conn_id: int32 option; *)
}

type openvpn_conn_state_type = {
  mutable nodes : openvpn_client_state_type list;
  mutable direction : int;
  mutable pid : int option;
  conn_id : int32;
}

type openvpn_state_type = {
  conns : ((string * string), 
           openvpn_conn_state_type) Hashtbl.t;
  mutable conn_counter : int32;
}

let get_external_ip state name =
  let rec inner_get_external_ip = function
    | [] -> None
    | peer :: rest -> 
        (if (peer.name = name) then
           peer.extern_ip
         else 
           inner_get_external_ip rest)
  in
    inner_get_external_ip state.nodes

let get_tactic_ip state name =
  let rec inner_get_tactic_ip = function
    | [] -> raise Not_found
    | peer :: rest -> 
        (if (peer.name = name) then
           peer.tactic_ip
         else
           inner_get_tactic_ip rest)
  in
    inner_get_tactic_ip state.nodes

let state = 
  {conns=Hashtbl.create 64; conn_counter=0l;}

let gen_key a b =
  if(a < b) then
    (a, b)
  else (b, a)

let get_state a b =
  let key = gen_key a b in 
  if (Hashtbl.mem state.conns key) then
    Hashtbl.find state.conns key 
  else (
    let ret = {nodes=[];direction=0;pid=None;conn_id=(state.conn_counter)} in 
      state.conn_counter <- (Int32.add state.conn_counter 1l);
      Hashtbl.add state.conns key ret;
      ret
  )

let openvpn_port = 1194

let name () = "openvpn"

(*
 * Testing methods 
 * *)
let test a b =
  (* Trying to see if connectivity is possible *)
  let (a, b) = gen_key a b in
  let conn = get_state a b in 
  let succ = ref false in 
  let dir = ref 3 in 
  let ip = ref Config.external_ip in

  let pairwise_connection_test a b direction =
    try_lwt 
      Printf.printf "[openvpn] Trying to start ssh service...\n%!";
  (*   let (dst_ip, dst_port) = Nodes.signalling_channel a in *)
      let rpc = (Rpc.create_tactic_request "openvpn" 
        Rpc.TEST "server_start" [(string_of_int openvpn_port)]) in
      lwt _ = (Nodes.send_blocking a rpc) in 
  
      let ips = Nodes.get_local_ips a in 
      let rpc = (Rpc.create_tactic_request "openvpn" 
        Rpc.TEST "client" ([(string_of_int openvpn_port)] @ ips)) in
      lwt res = (Nodes.send_blocking b rpc) in   
  (*     let resp_ip = rpc_of_string res in  *)
  
      let rpc = (Rpc.create_tactic_request "openvpn" 
        Rpc.TEST "server_stop" [(string_of_int openvpn_port)]) in
      lwt _ = (Nodes.send_blocking a rpc) in 
        dir := direction;
        succ := true;
        ip := res;
      return ()
    with exn ->
      let rpc = (Rpc.create_tactic_request "openvpn" 
        Rpc.TEST "server_stop" [(string_of_int openvpn_port)]) in
      lwt _ = (Nodes.send_blocking a rpc) in 
      Printf.eprintf "[openvpn] Pairwise test %s->%s failed:%s\n%!" a b
      (Printexc.to_string exn);
      return ()
  in

  lwt _ = (pairwise_connection_test a b 1) <?> 
             pairwise_connection_test b a 2 in
     match (!succ) with
      | true -> 
          let nodes = [ 
            {name=(sprintf "%s.d%d" a Config.signpost_number); 
             tactic_ip=(Int32.add 0x0a030001l (Int32.shift_left conn.conn_id 8)); 
             extern_ip=Some(Uri_IP.string_to_ipv4 !ip);};
            {name=(sprintf "%s.d%d" b Config.signpost_number);
             tactic_ip=(Int32.add 0x0a030002l (Int32.shift_left conn.conn_id 8)); 
             extern_ip=Some(Uri_IP.string_to_ipv4 !ip);} ] in 
          conn.nodes <- nodes;
          conn.direction <- !dir;
          conn.pid <- None;
          return true
      (* go through cloud then *)
      | false -> 
          let nodes = [ 
            {name=(sprintf "d%d" Config.signpost_number); 
             tactic_ip=(Int32.add 0x0a030001l (Int32.shift_left conn.conn_id 8)); 
             extern_ip=Some(Uri_IP.string_to_ipv4 Config.external_ip);};
            {name=(sprintf "%s.d%d" a Config.signpost_number); 
             tactic_ip=(Int32.add 0x0a030002l (Int32.shift_left conn.conn_id 8));
             extern_ip=None;};
            {name=(sprintf "%s.d%d" b Config.signpost_number); 
             tactic_ip=(Int32.add 0x0a030003l (Int32.shift_left conn.conn_id 8));
             extern_ip=None;}; ] in 
          conn.nodes <- nodes;
          conn.direction <- !dir;
          conn.pid <- None;
          return true

(*
 * Conection methods
 * *)
(*
 * TODO:
 * What garbage collection do I need to do in case something went wrong? 
 * How do I enforce the Node module to provide the new ip to the end node? 
 *
 *)
let start_vpn_server conn loc_node port rem_node domain =
  try_lwt
    let q_loc_node = Printf.sprintf "%s.d%d" loc_node Config.signpost_number in
    let rpc = 
      (Rpc.create_tactic_request "openvpn" Rpc.CONNECT "server" 
         [(string_of_int port); rem_node;domain;
          (Int32.to_string conn.conn_id); 
          (Uri_IP.ipv4_to_string (get_tactic_ip conn q_loc_node)); ]) in
      Nodes.send_blocking loc_node rpc
  with ex -> 
    Printf.printf "[openvpn ]Failed openvpn server %s:%s\n%!" loc_node
      (Printexc.to_string ex);
    raise Openvpn_error

let start_vpn_client conn loc_node port q_rem_node domain rem_node =
  let q_loc_node = Printf.sprintf "%s.d%d" loc_node Config.signpost_number in 
  let Some(ip) = get_external_ip conn q_rem_node in 
  let ip = Uri_IP.ipv4_to_string ip in 
  let rpc = (Rpc.create_tactic_request "openvpn" Rpc.CONNECT "client" 
               [ip; (string_of_int port);q_rem_node;domain;
                (Int32.to_string conn.conn_id); 
                (Uri_IP.ipv4_to_string (get_tactic_ip conn q_loc_node));]) in
  try
    lwt res = (Nodes.send_blocking loc_node rpc) in 
        return (res)
  with ex -> 
    Printf.printf "[openvpn]Failed openvpn client %s: %s\n%!" 
      loc_node (Printexc.to_string ex);
    raise Openvpn_error

let init_openvpn conn a b = 
  (* Init server on b *)
  lwt _ = start_vpn_server conn a openvpn_port 
               (sprintf "%s.d%d" b Config.signpost_number) 
               (sprintf "%s.d%d.%s" b Config.signpost_number
                  Config.domain) in
  (*Init client on b and get ip *)
  lwt _ = start_vpn_client conn b openvpn_port
               (sprintf "%s.d%d" a Config.signpost_number) 
               (sprintf "%s.d%d.%s" a Config.signpost_number
                  Config.domain) a in
    return ()

let start_local_server a b =
  (* Maybe load a copy of the Openvpn module and let it 
   * do the magic? *)
  lwt _ = Openvpn.Manager.connect "server" 
            [(string_of_int openvpn_port); 
             (sprintf "%s.d%d" a Config.signpost_number) ;
             (sprintf "d%d.%s" Config.signpost_number
                Config.domain);
             (Uri_IP.ipv4_to_string (Nodes.get_sp_ip a)); ] in 
  lwt ip = Openvpn.Manager.connect "server" 
             [(string_of_int openvpn_port); 
              (sprintf "%s.d%d" b Config.signpost_number) ;
              (sprintf "d%d.%s" Config.signpost_number
                 Config.domain);
              (Uri_IP.ipv4_to_string (Nodes.get_sp_ip b));] in 
    return (ip)

let connect a b =
  try_lwt
  (* Trying to see if connectivity is possible *)
    let (a, b) = gen_key a b in
    let conn = get_state a b in 
    match conn.direction with
      | 1 ->
          lwt _ = init_openvpn conn a b in 
            return true
      | 2 -> 
          lwt _ = init_openvpn conn b a in
            return true
      | 3 ->
          (lwt _ = start_local_server a b in
           lwt _ = start_vpn_client conn b openvpn_port 
                     (sprintf "d%d" Config.signpost_number) 
                     (sprintf "d%d.%s" Config.signpost_number 
                        Config.domain) a in 
           lwt _ = start_vpn_client conn a openvpn_port
                     (sprintf "d%d" Config.signpost_number) 
                     (sprintf "d%d.%s" Config.signpost_number 
                        Config.domain) b in 
             return true
        )
      | _ -> return false
  with exn ->
    Printf.eprintf "[openvpn] connect failed (%s)\n%!" 
      (Printexc.to_string exn);
    return false

(*
 * Enable functionality 
 * *)
let enable_openvpn conn a b = 
  (* Init server on b *)
  try_lwt
    let [q_a; q_b] = List.map (
      fun n -> Printf.sprintf "%s.d%d" n Config.signpost_number) [a; b] in 
    let rpc = 
      (Rpc.create_tactic_request "openvpn" Rpc.CONNECT "enable" 
         [(Int32.to_string conn.conn_id); (Nodes.get_node_mac b); 
          (Uri_IP.ipv4_to_string (get_tactic_ip conn q_a));
          (Uri_IP.ipv4_to_string (get_tactic_ip conn q_b));
          (Uri_IP.ipv4_to_string (Nodes.get_sp_ip a));
          (Uri_IP.ipv4_to_string (Nodes.get_sp_ip b))]) in
    lwt _ = Nodes.send_blocking a rpc in
    let rpc = 
      (Rpc.create_tactic_request "openvpn" Rpc.CONNECT "enable" 
         [(Int32.to_string conn.conn_id); (Nodes.get_node_mac a); 
          (Uri_IP.ipv4_to_string (get_tactic_ip conn q_b));
          (Uri_IP.ipv4_to_string (get_tactic_ip conn q_a));
          (Uri_IP.ipv4_to_string (Nodes.get_sp_ip b));
          (Uri_IP.ipv4_to_string (Nodes.get_sp_ip a))]) in
    lwt _ = Nodes.send_blocking b rpc in
      return ("true")
  with ex -> 
    Printf.printf "[openvpn ]Failed openvpn enabling %s->%s:%s\n%!" a b
      (Printexc.to_string ex);
    raise Openvpn_error

let enable a b =
  let (a, b) = gen_key a b in
  let conn = get_state a b in
  lwt ret = enable_openvpn conn a b in
    return (bool_of_string ret)

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

(* let handle_notification action method_name arg_list = *)
let handle_notification _ _ _ =
  eprintf "OpenVPN tactic doesn't handle notifications\n%!";
  return ()
