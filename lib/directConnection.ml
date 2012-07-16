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

exception Direct_error

let name () = "direct"
let direct_port = 10001

(* a struct to store details for each node participating in a 
 * tunnel formation*)
type direct_client_state_type = {

  mutable name: string;            (* node name *)
  mutable extern_ip: int32; (* public ip discovered through the test *)
}

type direct_conn_state_type = {
  (* a list of the nodes *)
  mutable nodes : direct_client_state_type list;
}

type direct_state_type = {
  (* a cache for connection informations *)
  conns : ((string * string), 
           direct_conn_state_type) Hashtbl.t;
  (* a monotonically increasing connection id generator *)
(*   mutable conn_counter : int32; *)
}

let state = {conns=Hashtbl.create 64;}

(*
 * Util functions to handle tactic state
 * *)
let get_external_ip state name =
  let ret = List.find (fun a -> (a.name = name)) state.nodes in
    ret.extern_ip

let gen_key a b =
  if(a < b) then (a, b) else (b, a)

let get_state a b =
  let key = gen_key a b in 
  if (Hashtbl.mem state.conns key) then
    Hashtbl.find state.conns key 
  else (
    let ret = {nodes=[];} in 
      Hashtbl.add state.conns key ret;
      ret
  )

(*
 * Testing methods 
 * *)
let test a b =
  (* Trying to see if connectivity is possible *)
  let (a, b) = gen_key a b in
  let conn = get_state a b in 

  (* Both servers must stop the same time *)
  lwt _ = Lwt_list.iter_p 
            (fun n ->
               let rpc = (Rpc.create_tactic_request "direct" 
                            Rpc.TEST "server_start" 
                            [(string_of_int direct_port)]) in
               lwt _ = (Nodes.send_blocking n rpc) in 
                  return()
            ) [a; b;] in 

  let pairwise_connection_test a b =
    try_lwt 
      let not_ips =  Nodes.get_local_ips b in
      let ips = List.filter (fun a -> not (List.mem a not_ips) ) 
                  (Nodes.get_local_ips a) in  

      lwt ret = 
        Nodes.send_blocking b (Rpc.create_tactic_request "direct" 
        Rpc.TEST "client" ([(string_of_int direct_port)] @ ips)) in   
  
      lwt _ = Nodes.send_blocking a (Rpc.create_tactic_request "direct" 
        Rpc.TEST "server_stop" [(string_of_int direct_port)]) in 
      return (Some(ret))
    with exn ->
      lwt _ = Nodes.send_blocking a (Rpc.create_tactic_request "direct" 
        Rpc.TEST "server_stop" [(string_of_int direct_port)]) in
        eprintf "[direct] Pairwise test %s->%s failed:%s\n%!" 
                a b (Printexc.to_string exn);
        return None
  in

  lwt [b_extern_ip;a_extern_ip;] = 
    Lwt_list.map_p ( fun (a,b) -> pairwise_connection_test a b) 
             [(a, b); (b,a)] in
     match (a_extern_ip, b_extern_ip) with
      | Some(a_extern_ip),Some(b_extern_ip) ->
          (* In case we have a direct tunnel then the nodes will receive an 
          * ip from the subnet 10.3.(conn_id).0/24 *)
          let nodes = [ 
            {name=(sprintf "%s.d%d" a Config.signpost_number); 
             extern_ip=(Uri_IP.string_to_ipv4 a_extern_ip);};
            {name=(sprintf "%s.d%d" b Config.signpost_number);
             extern_ip=(Uri_IP.string_to_ipv4 b_extern_ip);} ] in 
          conn.nodes <- nodes;
          return true
      (* go through cloud then *)
      | _,_ -> 
          return false

(* ******************************************
 * Try to establish if a direct connection between two hosts is possible
 * ******************************************)

let connect a b =
  (* Returns the pairs of lists of ips of node a and b that seem *)
  (* to be in the same LAN *)
  let key = gen_key a b in
    return (Hashtbl.mem state.conns key)

(*
 * Enable functionality 
 * *)
let enable_direct conn a b = 
  (* Init server on b *)
  try_lwt
    let [q_a; q_b] = List.map (
      fun n -> Printf.sprintf "%s.d%d" n Config.signpost_number) [a; b] in 
    let rpc = 
      (Rpc.create_tactic_request "direct" Rpc.ENABLE "enable" 
         [(Nodes.get_node_mac b); 
          (Uri_IP.ipv4_to_string (get_external_ip conn q_a));
          (Uri_IP.ipv4_to_string (get_external_ip conn q_b));
          (Uri_IP.ipv4_to_string (Nodes.get_sp_ip a));
          (Uri_IP.ipv4_to_string (Nodes.get_sp_ip b))]) in
    lwt _ = Nodes.send_blocking a rpc  in
      return ()
  with ex -> 
    Printf.printf "[direct]Failed direct enabling %s->%s:%s\n%!" a b
      (Printexc.to_string ex);
    raise Direct_error

let enable a b = 
  let key = gen_key a b in  
  match (Hashtbl.mem state.conns key) with
    | false -> return false
    | true -> 
        let conn = Hashtbl.find state.conns key in
        lwt _ = (enable_direct conn a b) <&>
                   (enable_direct conn b a) in
          return true
      
(*
 * disable code
 * *)
let disable_direct conn a b = 
  try_lwt
    let q_a = Printf.sprintf "%s.d%d" a Config.signpost_number in 
    let rpc_a = 
      (Rpc.create_tactic_request "ssh" Rpc.DISABLE "disable" 
         [(Uri_IP.ipv4_to_string (get_external_ip conn q_a));
          (Uri_IP.ipv4_to_string (Nodes.get_sp_ip b))]) in
    lwt _ = Nodes.send_blocking a rpc_a in
      return ()
  with ex -> 
    Printf.printf "[ssh]Failed ssh enabling :%s\n%!"
      (Printexc.to_string ex);
    raise Direct_error

let disable a b =
  let key = gen_key a b in
    match (Hashtbl.mem state.conns key) with 
      | false -> return true
      | true -> 
          let conn = Hashtbl.find state.conns key in
          lwt _ = (disable_direct conn a b) <&>
                     (disable_direct conn b a) in
            return true

(*
 * teardown code
 * *)
let teardown _ _ =
  return true


(**********************************************************************
 * Handle tactic signature ********************************************)

let handle_request action method_name arg_list =
  let open Rpc in
    try_lwt 
      match action with
      | TEST ->
          lwt v = (Direct.Manager.test method_name arg_list) in
            return(Sp.ResponseValue v)
      | CONNECT ->
        lwt v = (Direct.Manager.connect method_name arg_list) in
          return(Sp.ResponseValue v)
      | ENABLE ->
        lwt v = (Direct.Manager.enable method_name arg_list) in
          return(Sp.ResponseValue (string_of_bool v))
      | DISABLE ->
        lwt v = (Direct.Manager.disable method_name arg_list) in
          return(Sp.ResponseValue v)
    | TEARDOWN ->
      lwt v = (Direct.Manager.teardown method_name arg_list) in
          return(Sp.ResponseValue v)
    with ex ->
      return(Sp.ResponseError (Printexc.to_string ex))

let handle_notification _ _ _ =
  eprintf "Direct Connection tactic doesn't handle notifications\n%!";
  return ()
