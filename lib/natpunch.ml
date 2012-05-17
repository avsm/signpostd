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

module OP = Ofpacket
module OC = Controller

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
let natpanch_state = 
  {map_ip_node=(Hashtbl.create 1000);
  map_port_ip=(Hashtbl.create 1000);
  map_ip_ip=(Hashtbl.create 1000);}



  
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

  (* stun-like client *)
  let connect_client ip port =
      try_lwt 
        let client_sock = socket PF_INET SOCK_STREAM 0 in
        let hentry = Unix.inet_addr_of_string ip in
        lwt _ = Lwt_unix.connect client_sock (ADDR_INET (hentry, port)) in 
          let ADDR_INET(loc_ip,loc_port) = 
            Lwt_unix.getsockname client_sock in
              let pkt_bitstring = BITSTRING {
                (Uri_IP.string_to_ipv4 (Unix.string_of_inet_addr loc_ip)):32;
                loc_port:16;(String.length (Nodes.get_local_name ())):16;
                (Nodes.get_local_name ()):-1:string} in 
              let pkt = Bitstring.string_of_bitstring pkt_bitstring in 
              lwt _ = Lwt_unix.send client_sock pkt 0 (String.length pkt) [] in 
              return (Lwt_unix.shutdown client_sock SHUTDOWN_ALL)
      with exn ->
          Printf.eprintf "[natpanch] tcp client error:%s\n%!"
          (Printexc.to_string exn);
          return ()

  let filter_incoming_rst_packet controller dpid evt =
    try_lwt
      let (pkt, port, buffer_id) = match evt with 
        | Controller.Event.Packet_in(port, buffer_id, pkt, dpid) ->
                (pkt,port,buffer_id)
        | _ -> eprintf "Unknown event";failwith "Invalid of action"
      in
      let m = OP.Match.parse_from_raw_packet port pkt in
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
                             ~buffer_id:(Int32.to_int buffer_id) actions () in 
                 let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
                    OC.send_of_data controller dpid bs
            )

    with exn ->
        Printf.eprintf "[natpanch] Error: %s\n%!" (Printexc.to_string exn);
        return ()


  let handle_outgoing_syn_packet controller dpid evt =
    try_lwt
      let (pkt, port, buffer_id) = match evt with 
        | Controller.Event.Packet_in(port, buffer_id, pkt, dpid) ->
                (pkt,port,buffer_id)
        | _ -> eprintf "Unknown event";failwith "Invalid of action"
      in
      let m = OP.Match.parse_from_raw_packet port pkt in
      let isn = Tcp.get_tcp_sn pkt in
      let node = Hashtbl.find natpanch_state.map_ip_node 
                    m.OP.Match.nw_dst in  
      
      (* map to remote ip address. If no ip address was found simply
       * disregard the packet *)
      let nw_dst = Hashtbl.find natpanch_state.map_ip_ip 
                     m.OP.Match.nw_dst in
      
      (* configure outgoing flow *)
      (* TODO: dest port should be discovered from state *)
      let actions = [
        OP.Flow.Set_nw_dst(nw_dst);
        OP.Flow.Output((OP.Port.port_of_int 1), 2000);] in
      let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD 
                  ~buffer_id:(Int32.to_int buffer_id) actions () in 
      let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      lwt _ = OC.send_of_data controller dpid bs in 

      Hashtbl.replace natpanch_state.map_port_ip m.OP.Match.tp_src 
        m.OP.Match.nw_dst;
      let m = OP.Match.({wildcards=(OP.Wildcards.exact_match);
                         in_port=(OP.Port.port_of_int 1); 
                         dl_vlan=0xffff; dl_vlan_pcp=(char_of_int 0);
                         dl_type=0x0800; nw_tos=(char_of_int 0);
                         dl_src=m.OP.Match.dl_dst; dl_dst=m.OP.Match.dl_src;
                         nw_src=nw_dst; nw_dst=m.OP.Match.nw_src;
                         tp_src=m.OP.Match.tp_dst;tp_dst=m.OP.Match.tp_src;
                         nw_proto=(char_of_int 6); })
      in
        Sp_controller.register_handler m filter_incoming_rst_packet;

     (*  let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD 
        ~buffer_id:(-1) actions () in 
      let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      lwt _ = OC.send_of_data controller dpid bs in *)

      let rpc =
          (Rpc.create_tactic_notification "natpanch"
          Rpc.CONNECT "server_connect" 
          [node;(Nodes.get_local_name ()); 
          (Uri_IP.ipv4_to_string m.OP.Match.nw_src);
          (string_of_int m.OP.Match.tp_src); 
          (string_of_int m.OP.Match.tp_dst);
          (Int32.to_string isn);]) in
        Nodes.send_to_server rpc
    with exn ->
        Printf.eprintf "[natpanch] Error: %s\n%!" (Printexc.to_string exn);
        return ()

  let register_dst a external_ip ip =
      Hashtbl.replace natpanch_state.map_ip_node 
        (Uri_IP.string_to_ipv4 ip) a;
      Hashtbl.replace natpanch_state.map_ip_ip 
        (Uri_IP.string_to_ipv4 ip)
        (Uri_IP.string_to_ipv4 external_ip); 
      Printf.printf "Adding %s -> %s\n%!" external_ip ip;
       let flow_wild = OP.Wildcards.({
          in_port=true; dl_vlan=true; dl_src=true; dl_dst=true;
          dl_type=false; nw_proto=false; tp_dst=true; tp_src=true;
          nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
          dl_vlan_pcp=true; nw_tos=true;}) in
      let flow = OP.Match.create_flow_match flow_wild ~dl_type:(0x0800)
                    ~nw_proto:(char_of_int 6)  
                    ~nw_dst:(Uri_IP.string_to_ipv4 ip) () in
      Sp_controller.register_handler flow handle_outgoing_syn_packet

  let connect kind args =
    match kind with
    | "register_host" ->
      (try_lwt
        let node::dst_ip::args = args in
        List.iter (register_dst node dst_ip) args; 
          return ("127.0.0.1")
      with  exn ->
        printf "[natpunch] error %s\n%!" (Printexc.to_string exn);
        return ("127.0.0.1"))
    | "server_connect" ->
        (try_lwt
        (* gathering all the important header fields *)
          let src_ip = Uri_IP.string_to_ipv4 (List.nth args 0) in 
          let dst_port = int_of_string (List.nth args 1) in 
          let src_port = int_of_string (List.nth args 2) in
          let isn = Int32.of_string (List.nth args 3) in
          let local_ips = Nodes.discover_local_ips () in  
          let (_, local_gw, dev_id) = 
            Net_cache.Routing.get_next_hop src_ip in 
          let (mask, _, _) = 
            Net_cache.Routing.get_next_hop local_gw in 
          let local_ip = 
            (Uri_IP.string_to_ipv4 
               (List.find 
                  (fun ip ->
                     let ip = Uri_IP.string_to_ipv4 ip in 
                     let (test_mask, _, _) = 
                       (Net_cache.Routing.get_next_hop ip) in
                       test_mask = mask
                  ) 
                  local_ips)) in 
          let Some(src_mac) = 
            (Net_cache.Arp_cache.get_next_hop_mac src_ip) in
          let Some(dst_mac) = 
            (Net_cache.Arp_cache.get_next_hop_mac local_ip) in 
          let pkt = Tcp.gen_tcp_syn isn src_mac dst_mac src_ip
                      local_ip src_port dst_port 0x3000 in 
          
          (* create packet and send it over the openflow control
           * channel *)
          let bs = (OP.Packet_out.packet_out_to_bitstring 
                      (OP.Packet_out.create ~buffer_id:(-1l)
                      ~actions:[OP.(Flow.Output(OP.Port.Local , 2000))]
                      ~data:pkt ~in_port:(OP.Port.No_port) () )) in  
          let controller = (List.hd 
                  Sp_controller.switch_data.Sp_controller.of_ctrl) in 
          let dpid = (List.hd 
                  Sp_controller.switch_data.Sp_controller.dpid)  in
          lwt _ = OC.send_of_data controller dpid bs in
            printf "XXXXXXXXXX looking for port %s\n%!" dev_id;  
            (*          let Some(port) = Net_cache.Port_cache.dev_to_port_id "eth0" in  *)
            (*TODO port should be discovered automatically *)
          let port = OP.Port.port_of_int 1 in
          let m = OP.Match.parse_from_raw_packet port pkt in
          
          (* Install appropriate flows in order to do preocessing in fast path*)
          let actions = [OP.Flow.Output((OP.Port.Local), 2000);] in
          let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD
                      ~buffer_id:(-1) actions () in
          let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
          lwt _ = OC.send_of_data controller dpid bs in

          let actions = [OP.Flow.Output(port, 2000);] in
          let m = OP.Match.({wildcards=(OP.Wildcards.exact_match);
                             in_port=OP.Port.Local;
                             dl_src=m.OP.Match.dl_dst; dl_dst=m.OP.Match.dl_src;
                             dl_vlan=0xffff; dl_vlan_pcp=(char_of_int 0);
                             dl_type=0x0800; nw_src=m.OP.Match.nw_dst;
                             nw_dst=m.OP.Match.nw_src; nw_tos=(char_of_int 0);
                             nw_proto=(char_of_int 6); tp_src=m.OP.Match.tp_dst; 
                             tp_dst=m.OP.Match.tp_src}) in
          let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
          lwt _ = OC.send_of_data controller dpid bs in
            return ("0.0.0.0")
        with exn ->
                printf "[natpunch] error %s\n%!" 
                    (Printexc.to_string exn);
                raise (NatpunchError(Printexc.to_string exn)))
     | _ -> raise (NatpunchError((sprintf "unsupported %s" kind))) 



  (*********************************************************
   *       Testing methods
   *********************************************************)
  let test kind args =
    match kind with 
      | "client_connect" -> (
          try_lwt
            let port = int_of_string (List.hd args) in
            lwt _ = connect_client Config.external_ip port in 
              return ("0.0.0.0")
          with exn -> 
            printf "[natpunch] error %s\n%!" (Printexc.to_string exn);
            raise (NatpunchError(Printexc.to_string exn)))
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
