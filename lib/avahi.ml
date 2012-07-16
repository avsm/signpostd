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

module DP = Dns.Packet

let resolve t = Lwt.on_success t (fun _ -> ())

module Manager = struct
  exception AvahiError of string
  exception MissingAvahiArgumentError

  (*********************************************************
   *                  Tactic state
   *********************************************************)
  type service_struct = {
    mutable port : int;
    mutable ttl : int32;
    mutable options : string;
  }
  type services_list_sruct = {
    services: (string, service_struct) Hashtbl.t;
  }
  
  let service_list =
    {services=(Hashtbl.create 64);}
  (**********************************************************
   *                  Init methods
   **********************************************************)

  let init_module () = 
    return ()

  let destroy_module () = 
    init_module ()


  let send_pkt controller dpid name data =
    let rpc = (Rpc.create_tactic_notification "avahi"
       Rpc.CONNECT "service" 
       [(Nodes.get_local_name ());name; 
        (string_of_int data.port); 
        (Int32.to_string data.ttl); data.options;]) in
      resolve (Nodes.send_to_server rpc)


  let send_avahi_notification controller dpid node_name
        name port ttl options =
    let details = DP.({qr=DP.(`Answer); opcode=DP.(`Query);
    aa=true; tc=false; rd=false; ra=false; rcode=DP.(`NoError);}) in 

      (*service PTR mapping*)
    let serv_type = Re_str.split (Re_str.regexp "\.") name in 
    let serv_name =  [(node_name);
                      ("d"^(string_of_int Config.signpost_number));
                      "signpo";"st"] in 

    let serv_ptr = DP.({rr_name=(List.tl serv_type); rr_class=DP.(`IN);
                    rr_ttl=ttl; rr_rdata=(DP.(`PTR(serv_type)));}) in
    let serv_srv = DP.({rr_name=serv_type; rr_class=DP.(`IN);
                    rr_ttl=ttl; rr_rdata=(DP.(`SRV(
                      (Dns.Wire.int16 0), 
                      (Dns.Wire.int16 0), 
                      (Dns.Wire.int16 port), serv_name)));}) in
    let serv_txt = DP.({rr_name=serv_type; rr_class=DP.(`IN);
                    rr_ttl=ttl; rr_rdata=(DP.(`TXT(
                      Re_str.split (Re_str.regexp "%") options)));}) in
      Printf.printf "serv_tp:%s rr: %s\n%!" (String.concat "." serv_type)
        (DP.rr_to_string serv_ptr);
    let pkt = DP.({id=(Dns.Wire.int16 0); detail=(DP.build_detail details);
                   questions=[]; 
                   answers=[serv_ptr; serv_srv;serv_txt];
                   authorities=[]; additionals=[]}) in
    let dns_data = DP.marshal_dns pkt in
    let pkt = Tcp.gen_udp_pkt "\x00\x22\x15\x7e\x93\x2a" 
                "\x01\x00\x5e\x00\x00\xfb" 0xc0a80004l
                0xe00000fbl 5353 5353 dns_data in 
    let bs = (OP.Packet_out.packet_out_to_bitstring 
                (OP.Packet_out.create ~buffer_id:(-1l)
                   ~actions:[OP.(Flow.Output(OP.Port.Flood,2000))]
                   ~data:pkt ~in_port:(OP.Port.No_port) () )) in  
      OC.send_of_data controller dpid bs
      
(*
 * Connection Methods
 * *)
  let handle_outgoing_syn_packet controller dpid evt =
    let (pkt, port, buffer_id) = match evt with 
      | Controller.Event.Packet_in(port, buffer_id, pkt, dpid) ->
          (pkt,port,buffer_id)
      | _ -> eprintf "Unknown event";failwith "Invalid of action"
    in
      try_lwt
        let names = Hashtbl.create 64 in
        let m = OP.Match.parse_from_raw_packet port pkt in
        let dns_data = bitmatch pkt with
          | {_:96:bitstring; 0x0800:16; 4:4; ihl:4; _:64:bitstring; 
             17:8; _:16; _:64:bitstring; _:(ihl-5)*32:bitstring; 
             _:64:bitstring; dns_data:-1:bitstring } -> 
              dns_data
        in
        let dns_pkt = DP.parse_dns names dns_data in 
        let rec print_dns_answers = function
          | [] -> ()
          | rr::rrs -> (
              let _ = match rr.DP.rr_rdata with
                | `SRV(prio, weight, port, target) ->
                    let name =  (String.concat "." rr.DP.rr_name) in
                    let service = 
                      match (Hashtbl.mem service_list.services name) with
                        | true -> Hashtbl.find service_list.services name
                        | false -> 
                            let t = {port=0; ttl=0l;options="";} in 
                              Hashtbl.add service_list.services 
                                name t;
                              t
                    in
                      service.ttl <- rr.DP.rr_ttl;
                      service.port <- (Dns.Wire.int16_to_int port)
                | `TXT fields -> 
                    let name = String.concat "." rr.DP.rr_name in
                    let opt = String.concat "%" fields in
                    let service = 
                      match (Hashtbl.mem service_list.services name) with
                        | true -> Hashtbl.find service_list.services name
                        | false -> 
                            let t = {port=0; ttl=0l;options="";} in 
                              Hashtbl.add service_list.services 
                                name t;
                              t
                     in
                      service.options <- opt
               | _ -> 
                    ()
              in
                print_dns_answers rrs)
        in 
          print_dns_answers dns_pkt.DP.answers;
          Hashtbl.iter (send_pkt controller dpid) 
            service_list.services;
          return ()
      with exn ->
        Printf.eprintf "[avahi] of error:%s\n%!" 
          (Printexc.to_string exn);
        return ()

  let connect kind args =
    match kind with
      | "service_discovery" ->(
          Printf.printf "[avahi] service discovery\n%!";
          let flow_wild = OP.Wildcards.({
            in_port=false; dl_vlan=true; dl_src=true; dl_dst=true;
            dl_type=false; nw_proto=false; tp_dst=false; tp_src=true;
            nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
          dl_vlan_pcp=true; nw_tos=true;}) in
      let flow = OP.Match.create_flow_match flow_wild
                   ~in_port:(OP.Port.int_of_port OP.Port.Local)
                   ~dl_type:(0x0800)
                   ~nw_proto:(char_of_int 17)  
                   ~nw_dst:(0xe00000fbl) ~tp_dst:5353 () in

      let _ = Sp_controller.register_handler 
                flow handle_outgoing_syn_packet in 
      
      (* Force switch to send full packets on controller *)
      let controller = (List.hd 
              Sp_controller.switch_data.Sp_controller.of_ctrl) in 
      let dpid = (List.hd 
              Sp_controller.switch_data.Sp_controller.dpid)  in
      let actions = [OP.Flow.Output((OP.Port.Flood), 0xfff); 
                     OP.Flow.Output((OP.Port.Controller), 0xfff);] in
      let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.DELETE
                  ~buffer_id:(-1) actions () in
      let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      lwt _ = OC.send_of_data controller dpid bs in      
      
      let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD
                   ~idle_timeout:(0xFFFF)
                  ~buffer_id:(-1) actions () in
      let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      lwt _ = OC.send_of_data controller dpid bs in      

        return () )
      | "service_advertise" -> 
          let node_name = List.nth args 0 in 
          let name = List.nth args 1 in 
          let port = int_of_string (List.nth args 2) in 
          let ttl = Int32.of_string (List.nth args 3) in 
          let options = List.nth args 4 in 
          let controller = 
            (List.hd Sp_controller.switch_data.Sp_controller.of_ctrl) in 
          let dpid = 
            (List.hd Sp_controller.switch_data.Sp_controller.dpid)  in
            send_avahi_notification controller dpid node_name
              name port ttl options
      | _ -> raise (AvahiError((sprintf "unsupported %s" kind))) 

  (*********************************************************
   *       Testing methods
   *********************************************************)
  let test kind args =
    match kind with 
      | _ ->
          raise (AvahiError(
            (sprintf "[natpanch] invalid test action %s" kind)) )

  (*
   *             TEARDOWN methods of tactic
   ********************************************************)

  let teardown _ =
    true

  let pkt_in_cb _ _ _ = 
    return ()

end
