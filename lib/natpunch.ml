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
       public_ip : (int32, string) Hashtbl.t; 
(*       node_name : (int32, string) Hashtbl.t;  *)
  }
let natpanch_state = 
  {public_ip=(Hashtbl.create 1000);}



  
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
  let connect_client ip port =
      try_lwt 
        let client_sock = socket PF_INET SOCK_STREAM 0 in
        let hentry = Unix.inet_addr_of_string ip in
        lwt _ = Lwt_unix.connect client_sock (ADDR_INET (hentry, port)) in 
          printf "[natpanch] client connected\n%!";
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

      let handle_incoming_synack_packet controller dpid evt =
        printf "[natpunch] Packet received\n%!";
        try_lwt
          let (pkt, port, buffer_id) = match evt with 
            | Controller.Event.Packet_in(port, buffer_id, pkt, dpid) ->
                    (pkt,port,buffer_id)
            | _ -> eprintf "Unknown event";failwith "Invalid of action"
          in
          let m = OP.Match.parse_from_raw_packet port pkt in
          let isn = Tcp.get_tcp_sn pkt in
          let node = Hashtbl.find natpanch_state.public_ip 
                        m.OP.Match.nw_dst in  
     
      let m = OP.Match.parse_from_raw_packet port pkt in
      let actions = [
          OP.Flow.Set_nw_dst(0x561EF43Bl);
          OP.Flow.Output((OP.Port.port_of_int 1), 2000);] in
      let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD 
        ~buffer_id:(Int32.to_int buffer_id) actions () in 
      let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      lwt _ = OC.send_of_data controller dpid bs in 
  
       printf "[natpunch] send flow\n%!";

      let actions = [
          OP.Flow.Set_nw_src(m.OP.Match.nw_dst);
          OP.Flow.Output(OP.Port.Local, 2000);] in
      let m = OP.Match.({wildcards=(OP.Wildcards.exact_match);
                         in_port=OP.Port.Local; 
                         dl_src=m.OP.Match.dl_dst;
                         dl_dst=m.OP.Match.dl_src;
                         dl_vlan=0xffff; dl_vlan_pcp=(char_of_int 0);
                         dl_type=0x0800; 
(*                          nw_src=m.OP.Match.nw_dst; *)
                         nw_src=0x561EF43Bl;
                         nw_dst=m.OP.Match.nw_src;
                         nw_tos=(char_of_int 0);
                         nw_proto=(char_of_int 6);
                         tp_src=m.OP.Match.tp_dst;
                         tp_dst=m.OP.Match.tp_src})
      in 
      let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD 
        ~buffer_id:(-1) actions () in 
      let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      lwt _ = OC.send_of_data controller dpid bs in 

      lwt _ = connect_client Config.external_ip 11000 in 
      let rpc =
          (Rpc.create_tactic_notification "natpanch"
          Rpc.CONNECT "server_connect" 
          [node;(Nodes.get_local_name ()); 
          (Uri_IP.ipv4_to_string m.OP.Match.nw_src);
          (string_of_int m.OP.Match.tp_src); 
          (string_of_int m.OP.Match.tp_dst);
          (Int32.to_string isn);]) in
      lwt res = (Nodes.send_to_server rpc) in
      printf "[natpunch] notification send\n%!";
         return(printf "[natpunch] send flow 2\n%!")

    with exn ->
        Printf.eprintf "[natpanch] Error: %s" (Printexc.to_string exn);
        return ()

  let register_dst a ip =
      Hashtbl.replace natpanch_state.public_ip 
        (Uri_IP.string_to_ipv4 ip) a;
      let flow_wild = OP.Wildcards.({
          in_port=true; dl_vlan=true; dl_src=true; dl_dst=true;
          dl_type=false; nw_proto=false; tp_dst=true; tp_src=true;
          nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
          dl_vlan_pcp=true; nw_tos=true;}) in
      let flow = OP.Match.create_flow_match flow_wild ~dl_type:(0x0800)
                    ~nw_proto:(char_of_int 6)  
                    ~nw_dst:(Uri_IP.string_to_ipv4 ip) () in
      Sp_controller.register_handler flow handle_incoming_synack_packet

  let connect kind args =
    match kind with
    | "register_host" ->
      (try_lwt
        let node = List.nth args 0 in
        List.iter (register_dst node) (List.tl args); 
          return ("127.0.0.1")
      with  exn ->
        printf "[natpunch] error %s\n%!" (Printexc.to_string exn);
        return ("127.0.0.1"))
    | "server_connect" ->
        (try_lwt
          let src_ip = Uri_IP.string_to_ipv4 (List.nth args 0) in 
          let src_port = int_of_string (List.nth args 1) in 
          let dst_port = int_of_string (List.nth args 2) in
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
          let pkt = Tcp.gen_tcp_syn isn dst_mac src_mac src_ip
                      local_ip src_port dst_port 0x3000 in 
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
          let port = OP.Port.port_of_int 1 in
(*             port in *)
          let m = OP.Match.parse_from_raw_packet port pkt in
          let actions = [OP.Flow.Output((OP.Port.Local), 2000);] in
          let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD
                      ~buffer_id:(-1) actions () in
          let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
          lwt _ = OC.send_of_data controller dpid bs in

          let actions = [OP.Flow.Output(port, 2000);] in
          let m = OP.Match.({wildcards=(OP.Wildcards.exact_match);
                             in_port=OP.Port.Local;
                             dl_src=m.OP.Match.dl_dst;
                             dl_dst=m.OP.Match.dl_src;
                             dl_vlan=0xffff;
                             dl_vlan_pcp=(char_of_int 0);
                             dl_type=0x0800;
                             nw_src=m.OP.Match.nw_dst;
                             nw_dst=m.OP.Match.nw_src;
                             nw_tos=(char_of_int 0);
                             nw_proto=(char_of_int 6);
                             tp_src=m.OP.Match.tp_dst; 
                             tp_dst=m.OP.Match.tp_src}) in
          let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
          lwt _ = OC.send_of_data controller dpid bs in
            return ("0.0.0.0")
        with exn ->
                printf "[natpunch] error %s\n%!" 
                    (Printexc.to_string exn);
                raise (NatpunchError(Printexc.to_string exn)))

        (*         raise (NatpunchError ((Printexc.to_string exn)))  *)
     | _ -> raise (NatpunchError((sprintf "unsupported %s" kind))) 



  (*********************************************************
   *       Testing methods
   *********************************************************)
  let netpanch_daemon = 
    printf "[netpanch] Starting intermediate socket..\n%!";
    let server_sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
      (* so we can restart our server quickly *)
      Lwt_unix.setsockopt server_sock SO_REUSEADDR true ;
      (* build up my socket address *)
      bind server_sock (ADDR_INET (Unix.inet_addr_any, 11001)) ;
      (* Listen on the socket. Max of 10 incoming connections. *)
      listen server_sock 10 ;
      (* accept and process connections *)
      while_lwt true do
        lwt (client_sock, client_addr) = accept server_sock in
        let _ = 
        match client_addr with 
          | Unix.ADDR_INET(ip, port) -> 
              printf "[natpanch] client connected %s:%d\n%!"
              (Unix.string_of_inet_addr ip) port ;
          | _ -> 
              printf "[natpanch] invalid host"
        in
      (*     let x = send client_sock str 0 len [] in *)
        let rcv_buf = String.create 2048 in
        lwt recvlen = Lwt_unix.recv client_sock 
                        rcv_buf 0 1048 [] in
        lwt _ = Lwt_unix.send client_sock rcv_buf 0 
                  recvlen [] in 
        return (Lwt_unix.shutdown client_sock SHUTDOWN_ALL)
      done


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
    | "server_connect" ->
        (try_lwt
          let src_ip = Uri_IP.string_to_ipv4 (List.nth args 0) in 
          let src_port = int_of_string (List.nth args 1) in 
          let dst_port = int_of_string (List.nth args 2) in
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
          let pkt = Tcp.gen_tcp_syn isn dst_mac src_mac src_ip
                      local_ip src_port dst_port 0x3000 in 
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
(*          let Some(port) = Net_cache.Port_cache.dev_to_port_id "eth1" in  *)
          let port = OP.Port.port_of_int 1 in 
(*             port in *)
          let m = OP.Match.parse_from_raw_packet port pkt in
          let actions = [OP.Flow.Output((OP.Port.Local), 2000);] in
          let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD
                      ~buffer_id:(-1) actions () in
          let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
          lwt _ = OC.send_of_data controller dpid bs in

          let actions = [OP.Flow.Output(port, 2000);] in
          let m = OP.Match.({wildcards=(OP.Wildcards.exact_match);
                             in_port=OP.Port.Local;
                             dl_src=m.OP.Match.dl_dst;
                             dl_dst=m.OP.Match.dl_src;
                             dl_vlan=0xffff;
                             dl_vlan_pcp=(char_of_int 0);
                             dl_type=0x0800;
                             nw_src=m.OP.Match.nw_dst;
                             nw_dst=m.OP.Match.nw_src;
                             nw_tos=(char_of_int 0);
                             nw_proto=(char_of_int 6);
                             tp_src=m.OP.Match.tp_dst; 
                             tp_dst=m.OP.Match.tp_src}) in
          let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
          lwt _ = OC.send_of_data controller dpid bs in
            return ("0.0.0.0")
        with exn ->
                printf "[natpunch] error %s\n%!" 
                    (Printexc.to_string exn);
                raise (NatpunchError(Printexc.to_string exn)))
      | _ ->
          raise (NatpunchError((sprintf "Invalid match %s" kind)) )

  (*
   *             TEARDOWN methods of tactic
   ************************************************************************)

  let teardown _ =
    true

  let pkt_in_cb _ _ _ = 
    return ()

end
