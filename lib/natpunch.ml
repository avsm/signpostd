(*
 * Copyright (c) 2012 Charalampos Rotsos <cr409@cl.cam.ac.uk>
 *
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

module OP = Openflow.Packet
module OC = Openflow.Controller

let resolve t = Lwt.on_success t (fun _ -> ())
let pp = Printf.printf
let sp = Printf.sprintf
let ep = Printf.eprintf

module Manager = struct
  exception NatpunchError of string
  exception MissingNatpunchArgumentError

  (*********************************************************
   *                  Tactic state
   *********************************************************)
  type natpanch_state_type = {
    (* store local ip to signpost node names for 
     * the notification process *)
    map_ip_node : (int32, string) Hashtbl.t;
    (* translate signpost local to global ips
     * *)
    map_ip_ip : (int32, int32) Hashtbl.t;
    map_port_ip : (int, int32) Hashtbl.t;
  }

  let natpanch_state = {
    map_ip_node=(Hashtbl.create 1000);
    map_port_ip=(Hashtbl.create 1000);
    map_ip_ip=(Hashtbl.create 1000);
  }

  (**********************************************************
   *                  Init methods
   **********************************************************)

  let init_module () = 
    return ()

  let destroy_module () = 
    init_module ()

(*
 * Connection Methods
 * *)
  let filter_incoming_rst_packet controller dpid evt =
    try_lwt
      let (pkt, port, buffer_id) = match evt with 
        | OC.Event.Packet_in(port, buffer_id, pkt, dpid) ->
                (pkt,port,buffer_id)
        | _ -> ep "Unknown event";failwith "Invalid of action"
      in
      let m = OP.Match.parse_from_raw_packet port pkt in
        (* Ignore rst packets generated by nat if forwarding is not 
        * setup yet *)
      let flags = Tcp.get_tcp_flags pkt in
        match flags.Tcp.rst with 
          | true -> return ()
          | false -> (
              let nw_src = Hashtbl.find natpanch_state.map_port_ip 
                             m.OP.Match.tp_dst in
                 let actions = [
                   OP.Flow.Set_nw_src(nw_src);
                   OP.Flow.Output(OP.Port.Local, 2000);] in
                 let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD 
                             ~buffer_id:(Int32.to_int buffer_id) 
                             actions () in 
                 let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
                    OC.send_of_data controller dpid bs)
    with exn ->
      ep "[natpanch] Error: %s\n%!" (Printexc.to_string exn);
      return ()


  let handle_outgoing_syn_packet controller dpid evt =
    pp "[natpanch] received syn packet\n%!";
    try_lwt
      let (pkt, port, buffer_id) = match evt with 
        | OC.Event.Packet_in(port, buffer_id, pkt, dpid) ->
                (pkt,port,buffer_id)
        | _ -> ep "Unknown event";failwith "Invalid of action"
      in
      let flow = OP.Match.parse_from_raw_packet port pkt in
      let isn = Tcp.get_tcp_sn pkt in
      let node = Hashtbl.find natpanch_state.map_ip_node 
                    flow.OP.Match.nw_dst in  

      (* TODO: dest port should be discovered from state *)
      let Some(port) = Net_cache.Port_cache.dev_to_port_id 
                         Config.net_intf in 
      
      (* map to remote ip address. If no ip address was found simply
       * disregard the packet *)
      let nw_dst = Hashtbl.find natpanch_state.map_ip_ip 
                     flow.OP.Match.nw_dst in
      let m = OP.Match.(
        {wildcards=(OP.Wildcards.exact_match); 
         in_port=(OP.Port.port_of_int port); 
         dl_vlan=0xffff; dl_vlan_pcp=(char_of_int 0); 
         dl_type=0x0800; nw_tos=(char_of_int 0); 
         dl_src=flow.OP.Match.dl_dst; dl_dst=flow.OP.Match.dl_src;
         nw_src=nw_dst; nw_dst=flow.OP.Match.nw_src; 
         tp_src=flow.OP.Match.tp_dst;
         tp_dst=flow.OP.Match.tp_src; nw_proto=(char_of_int 6); }) in
        Sp_controller.register_handler m filter_incoming_rst_packet;     

       (* configure outgoing flow *)
        let actions = [
        OP.Flow.Set_nw_dst(nw_dst);
        OP.Flow.Output((OP.Port.port_of_int port), 2000);] in
      let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD 
                  ~buffer_id:(Int32.to_int buffer_id) actions () in 
      let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      lwt _ = OC.send_of_data controller dpid bs in 

      Hashtbl.replace natpanch_state.map_port_ip flow.OP.Match.tp_src 
        flow.OP.Match.nw_dst;

        let rpc =
          (Rpc.create_tactic_notification "natpanch" Rpc.CONNECT 
             "server_connect" 
             [node;(Nodes.get_local_name ()); 
              (Uri_IP.ipv4_to_string m.OP.Match.nw_src);
              (string_of_int m.OP.Match.tp_src); 
              (string_of_int m.OP.Match.tp_dst);
              (Int32.to_string isn);]) in
          Nodes.send_to_server rpc
    with exn ->
      ep "[natpanch] Error: %s\n%!" (Printexc.to_string exn);
      return ()

  let register_dst a external_ip ip =
    Hashtbl.replace natpanch_state.map_ip_node (Uri_IP.string_to_ipv4 ip) a;
    Hashtbl.replace natpanch_state.map_ip_ip (Uri_IP.string_to_ipv4 ip)
      (Uri_IP.string_to_ipv4 external_ip); 
    let flow_wild = OP.Wildcards.({
      in_port=true; dl_vlan=true; dl_src=true; dl_dst=true;
      dl_type=false; nw_proto=false; tp_dst=true; tp_src=true;
      nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
      dl_vlan_pcp=true; nw_tos=true;}) in
    let flow = OP.Match.create_flow_match flow_wild ~dl_type:(0x0800)
                 ~nw_proto:(char_of_int 6)  
                ~nw_dst:(Uri_IP.string_to_ipv4 ip) () in
      Sp_controller.register_handler flow handle_outgoing_syn_packet



  let l2_l3_addressing src_ip = 
    (* Address lookup *)
    let local_ips = Nodes.discover_local_ips () in  
    let (_, local_gw, dev) = Net_cache.Routing.get_next_hop src_ip in 
    let (mask, _, _) = Net_cache.Routing.get_next_hop local_gw in 
    let local_ip = 
      List.find 
        (fun ip ->
           let (test_mask, _, _) = 
             Net_cache.Routing.get_next_hop ip in
             test_mask = mask) 
        (List.map Uri_IP.string_to_ipv4 local_ips) in 
    let Some(src_mac) = (Net_cache.Arp_cache.get_next_hop_mac src_ip) in
    let Some(dst_mac) = (Net_cache.Arp_cache.get_next_hop_mac local_ip) in 
      (*TODO port should be discovered automatically *)
      let Some(port) = Net_cache.Port_cache.dev_to_port_id 
                         Config.net_intf in 
      let port = OP.Port.port_of_int port in      
        (src_mac, dst_mac, src_ip, local_ip, port)

  let connect kind args =
    match kind with
      | "register_host" ->
          (try_lwt
            let node::dst_ip::args = args in
              List.iter (register_dst node dst_ip) args; 
              return ("127.0.0.1")
          with  exn ->
            pp "[natpunch] error %s\n%!" (Printexc.to_string exn);
            return ("127.0.0.1"))
    | "server_connect" ->(
        try_lwt
          (* gathering all the important header fields *)
          let src_ip :: dst_port :: src_port :: isn :: _ = args in 
          let src_ip = Uri_IP.string_to_ipv4 src_ip in 
          let dst_port = int_of_string dst_port in 
          let src_port = int_of_string src_port in
          let isn = Int32.of_string isn in
          let controller = (List.hd 
                  Sp_controller.switch_data.Sp_controller.of_ctrl) in 
          let dpid = (List.hd 
                  Sp_controller.switch_data.Sp_controller.dpid)  in          
          let (src_mac, dst_mac, _, local_ip, port) = 
            l2_l3_addressing src_ip in
      
          (* create packet and send it over the openflow control
           * channel *)
          let pkt = Tcp.gen_tcp_syn isn src_mac dst_mac src_ip
                      local_ip src_port dst_port 0x3000 in 
          let bs = (OP.Packet_out.packet_out_to_bitstring 
                      (OP.Packet_out.create ~buffer_id:(-1l)
                      ~actions:[OP.(Flow.Output(OP.Port.Local , 2000))]
                      ~data:pkt ~in_port:(OP.Port.No_port) () )) in  
          lwt _ = OC.send_of_data controller dpid bs in

          let m = OP.Match.parse_from_raw_packet port pkt in
      
          (* Install appropriate flows to do processing in fast path*)
          let actions = [OP.Flow.Output((OP.Port.Local), 2000);] in
          let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD
                      ~buffer_id:(-1) actions () in
          let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
          lwt _ = OC.send_of_data controller dpid bs in

          let actions = [OP.Flow.Output(port, 2000);] in
          let m = OP.Match.(
            {wildcards=(OP.Wildcards.exact_match); in_port=OP.Port.Local;
             dl_src=m.OP.Match.dl_dst; dl_dst=m.OP.Match.dl_src;
             dl_vlan=0xffff;dl_vlan_pcp=(char_of_int 0);dl_type=0x0800; 
             nw_src=m.OP.Match.nw_dst; nw_dst=m.OP.Match.nw_src;
             nw_tos=(char_of_int 0); nw_proto=(char_of_int 6);
             tp_src=m.OP.Match.tp_dst; tp_dst=m.OP.Match.tp_src}) in
          let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
          lwt _ = OC.send_of_data controller dpid bs in
            return ("0.0.0.0")
        with exn ->
          let err = Printexc.to_string exn in
          pp "[natpunch] error %s\n%!" err;
          raise (NatpunchError(err)))
     | _ -> raise (NatpunchError((sprintf "unsupported %s" kind))) 



(*********************************************************
*       Testing methods
*********************************************************)
(* stun-like client *)
  let connect_client ip port =
    try_lwt 
      let client_sock = socket PF_INET SOCK_STREAM 0 in
      let hentry = Unix.inet_addr_of_string ip in
      lwt _ = 
         (Lwt_unix.sleep 4.0 >|= (fun _ -> failwith("Can't connect")) ) <?> 
              Lwt_unix.connect client_sock(ADDR_INET(hentry, port)) in 
      let ADDR_INET(loc_ip,loc_port) = Lwt_unix.getsockname client_sock in
      let pkt_bitstring = BITSTRING {
          (Uri_IP.string_to_ipv4 (Unix.string_of_inet_addr loc_ip)):32;
          loc_port:16; (String.length (Nodes.get_local_name ())):16;
          (Nodes.get_local_name ()):-1:string} in 
      let pkt = Bitstring.string_of_bitstring pkt_bitstring in 
      lwt _ = Lwt_unix.send client_sock pkt 0 (String.length pkt) [] in 
          Lwt_unix.shutdown client_sock SHUTDOWN_ALL; 
          return true
    with exn ->
      ep "[natpanch] tcp client error:%s\n%!" (Printexc.to_string exn);
      return false

  let unregister_dst a external_ip ip =
    let ip = Uri_IP.string_to_ipv4 ip in
      Hashtbl.remove natpanch_state.map_ip_node ip;
      Hashtbl.remove natpanch_state.map_ip_ip ip; 
      let flow_wild = OP.Wildcards.({
        in_port=true; dl_vlan=true; dl_src=true; dl_dst=true;
        dl_type=false; nw_proto=false; tp_dst=true; tp_src=true;
        nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
        dl_vlan_pcp=true; nw_tos=true;}) in
      let flow = OP.Match.create_flow_match flow_wild ~dl_type:(0x0800)
                   ~nw_proto:(char_of_int 6)  ~nw_dst:ip () in
        Sp_controller.unregister_handler flow handle_outgoing_syn_packet

  let test kind args =
    match kind with 
      | "client_connect" -> (
          try_lwt
            let ip :: port :: _ = args in 
            let port = int_of_string port in
            lwt ret = connect_client ip port in 
              return (string_of_bool ret)
          with exn -> 
            printf "[natpunch] error %s\n%!" (Printexc.to_string exn);
            raise (NatpunchError(Printexc.to_string exn))) 
      | "client_test" -> (
          let ip :: port :: node:: sp_ip :: _ = args in
          let port = int_of_string port in
          let _ = register_dst node ip ip in 
          lwt res = connect_client ip port in
          let _ = unregister_dst node ip ip in  
            return(string_of_bool res))
      | _ ->
          raise (NatpunchError((sprintf "[natpanch] invalid test action %s" kind)) )

  (*
   *             TEARDOWN methods of tactic
   ************************************************************************)

  let teardown _ =
    true

  let pkt_in_cb _ _ _ = 
    return ()

end
