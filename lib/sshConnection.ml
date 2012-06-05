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

module OP =Ofpacket
module  OC = Controller

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
               Rpc.CONNECT "server" 
               [remote_host; (Uri_IP.ipv4_to_string 
                                (Nodes.get_sp_ip client_name))]) in
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
let start_ssh_client host dst_ip dst_port node local_ip
      remote_ip = 
  let remote_host = (node^ (sprintf ".d%d.%s" 
                             Config.signpost_number Config.domain)) in
  let rpc = (Rpc.create_tactic_request "ssh" 
               Rpc.CONNECT "client" 
               [dst_ip; (string_of_int ssh_port); remote_host; 
                (* TODO check this param *)
                local_ip; remote_ip;
                (Uri_IP.ipv4_to_string (Nodes.get_sp_ip node));]) in
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
  lwt remote_ip = start_ssh_server a ssh_port b in
  (*Init client on b and get ip *)
  let dev_id = List.nth (Re_str.split 
                           (Re_str.regexp "\\.") remote_ip) 2 in 
  let local_ip = Printf.sprintf "10.2.%s.2" dev_id in
  lwt b_ip = start_ssh_client b ip ssh_port a
               local_ip remote_ip in
  return (remote_ip, local_ip)

let setup_cloud_flows a_dev b_dev = 
    let controller = (List.hd Sp_controller.
                      switch_data.Sp_controller.of_ctrl) in 
    let dpid = 
      (List.hd Sp_controller.switch_data.Sp_controller.dpid)  in
    let a_dev_str = Printf.sprintf "tap%d" a_dev in
    let b_dev_str = Printf.sprintf "tap%d" b_dev in
      Printf.printf "looking dev %s %s\n%!" a_dev_str b_dev_str;
    let Some(a_port) = Net_cache.Port_cache.dev_to_port_id a_dev_str in
    let Some(b_port) = Net_cache.Port_cache.dev_to_port_id b_dev_str in
    let a_ip =Uri_IP.string_to_ipv4 (sprintf "10.2.%d.2" a_dev) in 
    let b_ip =Uri_IP.string_to_ipv4 (sprintf "10.2.%d.2" b_dev) in 
    let flow_wild = OP.Wildcards.({
      in_port=false; dl_vlan=true; dl_src=true; dl_dst=true;
      dl_type=false; nw_proto=true; tp_dst=true; tp_src=true;
      nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
      dl_vlan_pcp=true; nw_tos=true;}) in
    let flow = OP.Match.create_flow_match flow_wild 
                ~in_port:a_port ~dl_type:(0x0800) 
                 ~nw_dst:b_ip () in
    let actions = [OP.Flow.Output((OP.Port.port_of_int b_port), 
                                   2000);] in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD 
                ~idle_timeout:0 ~buffer_id:(-1) actions () in 
    let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
    lwt _ = OC.send_of_data controller dpid bs in
    let flow = OP.Match.create_flow_match flow_wild 
                 ~in_port:b_port ~dl_type:(0x0800) 
                 ~nw_dst:a_ip () in
    let actions = [OP.Flow.Output((OP.Port.port_of_int a_port), 
                                   2000);] in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD 
                ~idle_timeout:0 ~buffer_id:(-1) actions () in 
    let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      OC.send_of_data controller dpid bs


let start_local_server a b =
  (* Maybe load a copy of the Openvpn module and let it 
   * do the magic? *)
  printf "[ssh] Starting ssh server...\n%!";
  lwt _ = Ssh.Manager.run_server () in

  let create_devices host = 
    let dev_id = Tap.get_new_dev_ip () in  
      Ssh.Manager.server_add_client host dev_id;
      dev_id
  in
  let connect_client node dst_node local_dev remote_dev =
    let domain = (sprintf "d%d.%s" 
    Config.signpost_number Config.domain) in 
    let host =  (node^ "." ^ domain) in 
      printf "[ssh] connecting host %s\n%!" host;

      let dev = Printf.sprintf "tap%d" local_dev in  
      let local_ip = Uri_IP.string_to_ipv4 
                       (Printf.sprintf "10.2.%d.1" local_dev) in  
      let ip = Printf.sprintf "10.2.%d.2" local_dev in 
      lwt _ = Tap.setup_dev local_dev ip in  
      let rem_ip = Printf.sprintf "10.2.%d.2" remote_dev in 
      let rpc = (Rpc.create_tactic_request "ssh" 
                   Rpc.CONNECT "client" 
                   [Config.external_ip; (string_of_int ssh_port);
                    domain; ip; rem_ip;
                    (Uri_IP.ipv4_to_string (Nodes.get_sp_ip dst_node));]) in
      lwt res = (Nodes.send_blocking node rpc) in 
      lwt _ = Lwt_unix.sleep 0.0 in  
        return (local_dev)
  in
  try_lwt
    let a_dev = create_devices a in
    let b_dev = create_devices b in
    lwt _ = Lwt_unix.sleep 1.0 in
    lwt a_dev = connect_client a b a_dev b_dev in 
    lwt b_dev = connect_client b a b_dev a_dev in
(*
    lwt [a_dev; b_dev ] = Lwt_list.map_p connect_client [a; b] in
 *)
    lwt _ = setup_cloud_flows a_dev b_dev in 
    let a_ip =Uri_IP.string_to_ipv4 (sprintf "10.2.%d.1" a_dev) in 
    let b_ip =Uri_IP.string_to_ipv4 (sprintf "10.2.%d.1" b_dev) in 
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
        lwt _ = start_local_server b a in
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
