(*
 * Copyright (c) 2005-2012 Anil Madhavapeddy <anil@recoil.org>
 *                         Charalampos Rotsos <cr409@cl.cam.ac.uk>
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


module OP = Ofpacket
module OC = Controller
module OE = OC.Event


(* TODO this the mapping is incorrect. the datapath must be moved to the key
 * of the hashtbl *)
type mac_switch = {
  addr: OP.eaddr; 
  switch: OP.datapath_id;
}

type pkt_in_cb_struct = {
  flow_match : OP.Match.t;
  cb: (state -> OP.datapath_id -> (OP.Port.t * int32 * Bitstring.t * OP.datapath_id) -> unit Lwt.t);
}

type switch_state = {
(*   mutable mac_cache: (mac_switch, OP.Port.t) Hashtbl.t; *)
  mutable mac_cache: (OP.eaddr, OP.Port.t) Hashtbl.t; 
  mutable dpid: OP.datapath_id list;
  mutable of_ctrl: OC.state list;
  mutable pkt_in_cb_cache : pkt_in_cb_struct list;
  cb_register : (OP.Match.t, (Controller.state -> OP.datapath_id -> 
                   OE.e -> unit Lwt.t) ) Hashtbl.t;
}

let resolve t = Lwt.on_success t (fun _ -> ())
let pp = Printf.printf
let sp = Printf.sprintf

let switch_data = { mac_cache = Hashtbl.create 0;
                    dpid = []; 
                    of_ctrl = [];
                    pkt_in_cb_cache = [];
                    cb_register = (Hashtbl.create 64);
                  } 

let datapath_join_cb controller dpid evt =
  let (ports, dp) = 
    match evt with
      | OE.Datapath_join (ports, c) -> (ports, c)
      | _ -> invalid_arg "bogus datapath_join event match!" 
  in
    Printf.printf "received %d ports\n%!" (List.length ports);
    List.iter ( fun port -> 
                    Net_cache.Dev_cache.add_dev port.OP.Port.name port.OP.Port.port_id
    ) ports;
  switch_data.dpid <- switch_data.dpid @ [dp];
  return (pp "+ datapath:0x%012Lx\n%!" dp)

let port_status_cb controller dpid evt =
  let _ = 
    match evt with
      | OE.Port_status (OP.Port.ADD, port, dpid) -> 
          pp "[openflow] device added %s %d\n%!" port.OP.Port.name port.OP.Port.port_id;
          Net_cache.Dev_cache.add_dev port.OP.Port.name port.OP.Port.port_id
      | OE.Port_status (OP.Port.DEL, port, dpid) -> 
          pp "[openflow] device removed %s %d\n%!" port.OP.Port.name port.OP.Port.port_id;
          Net_cache.Dev_cache.del_dev port.OP.Port.name
      | OE.Port_status (OP.Port.MOD, port, dpid) -> 
          pp "[openflow] device modilfied %s %d\n%!" port.OP.Port.name port.OP.Port.port_id
      | _ -> invalid_arg "bogus datapath_join event match!" 
  in
    return ()

let req_count = (ref 0)

let register_handler flow cb = 
  if (Hashtbl.mem switch_data.cb_register flow) then (
    Hashtbl.remove switch_data.cb_register flow;
    Hashtbl.add switch_data.cb_register flow cb
  ) else 
    Hashtbl.add switch_data.cb_register flow cb


let add_entry_in_hashtbl mac_cache ix in_port = 
  if not (Hashtbl.mem mac_cache ix ) then
      Hashtbl.add mac_cache ix in_port
  else  
      Hashtbl.replace mac_cache ix in_port 

let switch_packet_in_cb controller dpid buffer_id m data in_port =
  (* save src mac address *)
  let ix = m.OP.Match.dl_src in
  let dev = match (Net_cache.Dev_cache.port_id_to_dev 
                     (OP.Port.int_of_port m.OP.Match.in_port)) with
    | None -> "br0"
    | Some(dev) -> dev
  in 
    add_entry_in_hashtbl switch_data.mac_cache ix in_port;
    Net_cache.Switching.add_entry m.OP.Match.dl_src m.OP.Match.nw_src
      dev Net_cache.Switching.ETH;
 
  (* check if I know the output port in order to define what type of message
   * we need to send *)
    let ix = m.OP.Match.dl_dst in
      if ( (OP.eaddr_is_broadcast ix)
        || (not (Hashtbl.mem switch_data.mac_cache ix)) ) 
      then (
        let pkt = OP.Packet_out.create
                    ~buffer_id:buffer_id ~actions:[ OP.(Flow.Output(Port.All , 2000))] 
                    ~data:data ~in_port:in_port () 
        in
        let bs = OP.Packet_out.packet_out_to_bitstring pkt in 
          OC.send_of_data controller dpid bs
      (*     Printf.fprintf switch_data.log "%d %f\n" (!req_count) (((OS.Clock.time ()) -. ts)*.1000000.0) *)
      ) else (
        let out_port = (Hashtbl.find switch_data.mac_cache ix) in
        let actions = [OP.Flow.Output(out_port, 2000)] in
        let pkt = OP.Flow_mod.create m 0_L OP.Flow_mod.ADD 
                    ~buffer_id:(Int32.to_int buffer_id)
                    actions () in 
        let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
          OC.send_of_data controller dpid bs
      )

let lookup_flow of_match =
  (* Check the wilcard card table *)
  let ret_lst = ref [] in 
  let lookup_flow flow entry =
    if (OP.Match.flow_match_compare of_match flow
          flow.OP.Match.wildcards) then (
(*
            Printf.printf "[openflow] Found callback for %s \n%!"
              (OP.Match.match_to_string of_match);
 *)
            ret_lst := (!ret_lst) @ [entry]
          )
  in
    Hashtbl.iter lookup_flow switch_data.cb_register;
    if (List.length (!ret_lst) == 0) then 
      None
    else ( 
(*
      Printf.printf "[openflow] Found callback for %s\n%!"
        (OP.Match.match_to_string of_match);
 *)
      Some(List.hd (!ret_lst))
    )

let packet_in_cb controller dpid evt =
  incr req_count;
  let (in_port, buffer_id, data, dp) = 
    match evt with
      | OE.Packet_in (inp, buf, dat, dp) -> (inp, buf, dat, dp)
      | _ -> invalid_arg "bogus datapath_join event match!"
  in
  (* Parse Ethernet header *)
  let m = OP.Match.parse_from_raw_packet in_port data in 
    match (lookup_flow m) with
      | Some (cb) -> cb controller dpid evt
      | None -> switch_packet_in_cb controller dpid  buffer_id m data in_port

let init controller = 
  if (not (List.mem controller switch_data.of_ctrl)) then
    switch_data.of_ctrl <- (([controller] @ switch_data.of_ctrl));
  pp "test controller register datapath cb\n";
  OC.register_cb controller OE.DATAPATH_JOIN datapath_join_cb;
  pp "test controller register packet_in cb\n";
  OC.register_cb controller OE.PACKET_IN packet_in_cb;
  pp "test controller register port_stat cb\n";
  OC.register_cb controller OE.PORT_STATUS_CHANGE port_status_cb

let add_dev dev ip netmask =
  lwt _ = Lwt_unix.system ("ovs-vsctl --db=unix:/var/run/ovsdb-server "^
                           " add-port br0 " ^ dev) in 
  lwt _ = Lwt_unix.system (Printf.sprintf "ip addr add %s/%s dev br0" ip netmask) in
  return ()

let del_dev dev ip netmask =
  lwt _ = Lwt_unix.system ("ovs-vsctl --db=unix:/var/run/ovsdb-server "^
                           " del-port br0 " ^ dev) in 
  lwt _ = Lwt_unix.system (Printf.sprintf "ip addr del %s/%s dev br0" ip netmask) in
  return ()

let listen ?(port = 6633) () =
  try_lwt 
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    let _ = Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true in 
    lwt hostinfo = Lwt_unix.gethostbyname "localhost" in
    let _ = Printf.printf "[openflow] Starting switch...\n%!" in 
    let server_address = hostinfo.Lwt_unix.h_addr_list.(0) in
      Lwt_unix.bind sock (Lwt_unix.ADDR_INET (server_address, port)); 
      Lwt_unix.listen sock 10; 
      Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
      let _ = Printf.printf "[openflow] Waiting for controller...\n%!" in 
      while_lwt true do 
        lwt (fd, sockaddr) = Lwt_unix.accept sock in
          match sockaddr with
            | ADDR_INET (dst, port) ->
                let _ = Printf.printf "[openflow] Received a connection %s:%d\n%!"
                                      (Unix.string_of_inet_addr dst) port  in
                let ip = 
                  match (Nettypes.ipv4_addr_of_string 
                           (Unix.string_of_inet_addr dst)) with
                    | None -> invalid_arg "dest ip is Invalid"
                    | Some(ip) -> ip
                in
                  Lwt_unix.set_blocking fd true;
                  Controller.listen fd (ip, port) init
            | ADDR_UNIX(_) -> invalid_arg "invalid unix addr"

      done
    with
      | e ->
          return (Printf.eprintf "Unexpected exception : %s\n%!" (Printexc.to_string e))


