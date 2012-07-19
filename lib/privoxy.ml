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
open Lwt_list
open Printf

module OP = Openflow.Packet
module OC = Openflow.Controller
module OE = OC.Event

module Manager = struct
  exception ProxyError of string
  exception MissingProxyArgumentError


  (********************************************************************
   *       Tactic state 
   ********************************************************************)
  type proxy_flow_type = 
    | NON_SSL
    | SSL              
  let string_of_proxy_flow_type = function
    | NON_SSL -> "NON_SSL"
    | SSL -> "SSL"
  type ssl_state_type =
    | NO_SSL
    | SSL_SERVER_INIT
    | SSL_CLIENT_INIT
    | SSL_COMPLETE
  type conn_type = {
    src_mac: string;
    dst_mac: string;
    src_ip: int32;
    dst_ip : int32;
    dst_port : int; 
    mutable state : proxy_flow_type;
    mutable ssl_state : ssl_state_type;
    mutable src_isn : int32;
    mutable dst_isn : int32;
    data: string;
  }

  type conn_db_type = {
    http_conns : (int, conn_type) Hashtbl.t;
    mutable process_pid : int option;
  }

  let conn_db = {http_conns=(Hashtbl.create 0);process_pid=None;}


  let privoxy_port = 8118
  let connect_req="CONNECT %s:443 HTTP/1.1\n"^ 
                  "User-Agent: signpst\n"^ 
                  "Proxy-Connection: keep-alive\n"^ 
                  "Host: %s\n\n"

  (*********************************************************************
   * testing code
   * TODO: do we need any tests?
   *********************************************************************)
  let test _ _ =
    return ("OK")

  (*********************************************************************
   * Connection code
   **********************************************************************)
  let restart_privoxy () = 
    let cmd = "privoxy" in
    lwt _ = Lwt_unix.system ("killall " ^ cmd) in 
    let pid_file = (Printf.sprintf "%s/privoxy.pid" Config.tmp_dir) in 
    let conf_file = Printf.sprintf "%s/client_tactics/privoxy/config" Config.dir in
      Printf.printf "%s --no-daemon --pidfile %s %s\n%!" cmd pid_file conf_file;
    let _ = Unix.create_process cmd [| cmd; "--no-daemon"; "--pidfile"; pid_file; 
                                       conf_file; |] 
              Unix.stdin Unix.stdout Unix.stderr in
    lwt _ = Lwt_unix.sleep 2.0 in 
    let fd = open_in pid_file in
    let pid_buf = input_line fd in 
    let _ = close_in fd in 
    let pid = int_of_string pid_buf in 
      conn_db.process_pid <- Some(pid);
      Printf.printf "[privoxy] process created with pid %d...\n" pid;
      return(pid)
  
  let load_privoxy_sockets pid = 
    let fd_dir = (Printf.sprintf "/proc/%d/fd/" pid) in 
      let files = Sys.readdir fd_dir in 
      let sock_array = Array.map (fun dir -> 
                      let file_path = Printf.sprintf "%s/%s" fd_dir dir in
                      let file_stat = Unix.stat file_path in 
                        match file_stat.Unix.st_kind with
                          | Unix.S_SOCK -> file_stat.Unix.st_ino
                          | _ -> 0
        ) files in 
        List.filter (fun fd -> (fd <> 0)) (Array.to_list sock_array) 
          
   
  let is_privoxy_conn ip port = 
    lwt pid = match conn_db.process_pid with 
      | None -> restart_privoxy ()
      | Some(pid) -> return (pid)
    in

    let socket_ids = load_privoxy_sockets pid in 
(*     List.iter (fun fd -> Printf.printf "%d\n%!" fd ) socket_ids; *)
    let found = ref false in
    let lookup_string = Printf.sprintf "%s:%04X" (Net_cache.Routing.string_rev 
                                                       (Printf.sprintf "%08lX" ip)) 
                          port in 
    let tcp_conn = open_in "/proc/net/tcp" in 

      let rec parse_tcp_conn tcp_conn = 
        try 
          let conn = input_line tcp_conn in
          let det = Re_str.split (Re_str.regexp "[ ]+") conn in 
(*             Printf.printf "Looking sock %d\n%!" (int_of_string (List.nth det
 *             9)); *)
            if( (List.mem  (int_of_string (List.nth det 9)) socket_ids) &&
              ((List.nth det 1) = lookup_string) ) then (
              Printf.printf "%s %s\n%!" (List.nth det 1) lookup_string;
              found := true
            ) else (
              parse_tcp_conn tcp_conn
            )
        with End_of_file -> () 
      in
      let _ = input_line tcp_conn in 
        let _ = parse_tcp_conn tcp_conn in  
          close_in tcp_conn;
          return (!found)
  
  let ssl_send_conect_req controller dpid conn m dst_port = 
    let (_, gw, _) = Net_cache.Routing.get_next_hop conn.dst_ip in
    
(*
    (* ack the the SYNACK from privoxy *)
    let pkt = Tcp.gen_server_ack 
                (Int32.sub conn.src_isn
                   (Int32.of_int ((String.length conn.data) - 1) ) )
                (Int32.add conn.dst_isn 1l) 
                conn.src_mac conn.dst_mac
                gw conn.src_ip
                m.OP.Match.tp_src dst_port 0xffff in 
    let bs = (OP.Packet_out.packet_out_to_bitstring 
                (OP.Packet_out.create ~buffer_id:(-1l)
                   ~actions:[OP.(Flow.Output(OP.Port.Local , 2000))]
                   ~data:pkt ~in_port:(OP.Port.No_port) () )) in  
    lwt _ = OC.send_of_data controller dpid bs in

 *)
  (* SYNACK the client in order to establish the
     * connection *)
    let pkt = Tcp.gen_server_synack 
                (Int32.add conn.dst_isn 68l ) (* 68 bytes for http reply *)
                (Int32.add conn.src_isn 1l)
                conn.src_mac conn.dst_mac 
                conn.dst_ip conn.src_ip
                dst_port conn.dst_port in 
    let bs = (OP.Packet_out.packet_out_to_bitstring 
                (OP.Packet_out.create ~buffer_id:(-1l)
                   ~actions:[OP.(Flow.Output(OP.Port.Local , 2000))] 
                   ~data:pkt ~in_port:(OP.Port.No_port) () )) in  
    lwt _ = OC.send_of_data controller dpid bs in 

  (* Send an http request to setup the persistent connection to the ssl server *)
  let sock_req = Bitstring.bitstring_of_string conn.data in 
  let pkt = (Tcp.gen_tcp_data_pkt 
               (Int32.sub conn.src_isn
                  (Int32.of_int ((String.length conn.data) - 1)) )
               (Int32.add conn.dst_isn 1l)
               conn.src_mac conn.dst_mac
               gw conn.src_ip
               m.OP.Match.tp_src dst_port sock_req) in 
  let bs = (OP.Packet_out.packet_out_to_bitstring 
              (OP.Packet_out.create ~buffer_id:(-1l)
                 ~actions:[OP.(Flow.Output(OP.Port.Local , 2000))] 
                ~data:pkt ~in_port:(OP.Port.No_port) () )) in  
    OC.send_of_data controller dpid bs

  let ssl_complete_flow controller dpid conn m dst_port = 
    let (_, gw, _) = Net_cache.Routing.get_next_hop conn.dst_ip in
    
    (* ack the privoxy connect http reply *)
    let pkt = Tcp.gen_server_ack 
                (Int32.add conn.src_isn 1l)
                (Int32.add conn.dst_isn 69l) 
                conn.src_mac conn.dst_mac
                gw conn.src_ip
                m.OP.Match.tp_src dst_port 0xffff in 
    let bs = (OP.Packet_out.packet_out_to_bitstring 
                (OP.Packet_out.create ~buffer_id:(-1l)
                   ~actions:[OP.(Flow.Output(OP.Port.Local , 2000))]
                   ~data:pkt ~in_port:(OP.Port.No_port) () )) in  
    lwt _ = OC.send_of_data controller dpid bs in

    let pkt = Tcp.gen_server_ack 
                (Int32.add conn.dst_isn 68l ) (* 68 bytes for http reply *)
                (Int32.add conn.src_isn 1l)
                conn.src_mac conn.dst_mac 
                conn.dst_ip conn.src_ip
                dst_port conn.dst_port 0xffff in 
    let bs = (OP.Packet_out.packet_out_to_bitstring 
                (OP.Packet_out.create ~buffer_id:(-1l)
                   ~actions:[OP.(Flow.Output(OP.Port.Local , 2000))]
                   ~data:pkt ~in_port:(OP.Port.No_port) () )) in  
    lwt _ = OC.send_of_data controller dpid bs in

    
  (* Setup the appropriate flows in the openflow flow table *)
  let actions = [
    OP.Flow.Set_dl_src(conn.dst_mac);
    OP.Flow.Set_dl_dst(conn.src_mac);
    OP.Flow.Set_nw_src(conn.dst_ip);
    OP.Flow.Set_nw_dst(conn.src_ip);
    OP.Flow.Set_tp_src(443);
    OP.Flow.Output((OP.Port.Local), 2000);] in
  let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD 
              ~buffer_id:(-1) actions () in 
  let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
  lwt _ = OC.send_of_data controller dpid bs in 

  let m = OP.Match.({wildcards=(OP.Wildcards.exact_match);
                     in_port=OP.Port.Local; dl_src=conn.src_mac;
                     dl_dst=conn.dst_mac; dl_vlan=0xffff;
                     dl_vlan_pcp=(char_of_int 0); dl_type=0x0800;
                     nw_src=conn.src_ip; nw_dst=conn.dst_ip;
                     nw_tos=(char_of_int 0); nw_proto=(char_of_int 6);
                     tp_src=dst_port; tp_dst=443}) in 
  let actions = [
    OP.Flow.Set_dl_src(conn.dst_mac);
    OP.Flow.Set_dl_dst(conn.src_mac);
    OP.Flow.Set_nw_src(gw);
    OP.Flow.Set_nw_dst(conn.src_ip);
    OP.Flow.Set_tp_dst(privoxy_port);
    OP.Flow.Output((OP.Port.Local), 2000);] in
  let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD 
              ~buffer_id:(-1) actions () in 
  let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
    OC.send_of_data controller dpid bs 
    
  
  let http_pkt_in_cb controller dpid evt = 
    let (in_port, buffer_id, data, _) = 
      match evt with
        | OE.Packet_in (inp, buf, dat, dp) -> (inp, buf, dat, dp)
        | _ -> invalid_arg "bogus datapath_join event match!"
    in
    let m = OP.Match.parse_from_raw_packet in_port data in
    let _ = 
      match (m.OP.Match.tp_src, m.OP.Match.tp_dst) with
        | (src_port, 80) -> (
          try_lwt
            Printf.printf "[proxy] http traffic found\n%!";
            lwt is_privoxy = is_privoxy_conn m.OP.Match.nw_src src_port in
            match is_privoxy with
              | true -> (
                  Printf.printf "[privoxy] a privoxy connection on port %d\n%!" 
                    src_port;
                  Printf.printf "Looking up mac %s\n%!" 
                    (Net_cache.Arp_cache.string_of_mac m.OP.Match.dl_src);
                  let port_id = match (Net_cache.Port_cache.port_id_of_mac 
                                         m.OP.Match.dl_dst) with
                    | Some(port_id) -> OP.Port.port_of_int port_id
                    | None -> OP.Port.All
                  in
                  let pkt = OP.Flow_mod.create m 0_L OP.Flow_mod.ADD 
                              ~buffer_id:(Int32.to_int buffer_id)
                              [OP.Flow.Output(port_id, 2000);] 
                              () in 
                  let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
                    OC.send_of_data controller dpid bs
                )
              | false -> (
                  let mapping = {src_mac=m.OP.Match.dl_src; dst_mac=m.OP.Match.dl_dst; 
                                 src_ip=m.OP.Match.nw_src; dst_ip = m.OP.Match.nw_dst;
                                 dst_port=80; state=NON_SSL; ssl_state=NO_SSL;
                                 src_isn=0l;dst_isn=0l;data="";} in
                    Hashtbl.add conn_db.http_conns src_port mapping;
                    let (_, gw, _) = Net_cache.Routing.get_next_hop m.OP.Match.nw_dst in
                      Printf.printf "looking up for %s\n%!" (Uri_IP.ipv4_to_string gw);
                      let Some(dst_mac) = 
                        match gw with 
                          | 0l -> Net_cache.Arp_cache.mac_of_ip m.OP.Match.nw_dst
                          | ip -> Net_cache.Arp_cache.mac_of_ip ip 
                      in
(*                   let Some(dst_mac,_, _ ) = Net_cache.Switching.ip_of_mac gw in
 *                   *)
                      let actions = [
                        OP.Flow.Set_dl_src(dst_mac);
                        OP.Flow.Set_dl_dst(m.OP.Match.dl_src);
                        OP.Flow.Set_nw_dst(m.OP.Match.nw_src);
                        OP.Flow.Set_nw_src(gw);
                        OP.Flow.Set_tp_dst(privoxy_port);
                        OP.Flow.Output((OP.Port.Local), 2000);] in
                      let pkt = OP.Flow_mod.create m 0_L OP.Flow_mod.ADD 
                                  ~buffer_id:(Int32.to_int buffer_id)
                                  actions () in 
                      let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
                        OC.send_of_data controller dpid bs)
            with ex ->
              return (Printf.printf "[privoxy] error: %s\n%!" (Printexc.to_string ex))

        )
        | (src_port, 443) -> (
            lwt is_privoxy = is_privoxy_conn m.OP.Match.nw_src src_port in 
            let state_found = (Hashtbl.mem conn_db.http_conns src_port) in 
              match (is_privoxy, state_found) with
                | (true, _) ->
                    (Printf.printf "[privoxy] a privoxy connection on port %d\n%!" 
                      src_port;
                    try_lwt
                      Printf.printf "Looking up mac %s\n%!" 
                      (Net_cache.Arp_cache.string_of_mac m.OP.Match.dl_src);
                      let port_id = match (Net_cache.Port_cache.port_id_of_mac 
                               m.OP.Match.dl_dst) with
                        | Some(port_id) -> OP.Port.port_of_int port_id
                        | None -> OP.Port.All
                      in
                      let pkt = OP.Flow_mod.create m 0_L OP.Flow_mod.ADD 
                                  ~buffer_id:(Int32.to_int buffer_id)
                                  [OP.Flow.Output(port_id, 2000);] 
                                  () in 
                      let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
                        OC.send_of_data controller dpid bs

                    with ex -> 
                      return (Printf.printf "[privoxy] error: %s\n%!" 
                                (Printexc.to_string ex)) )
                | (false, true) -> ( 
                    Printf.printf "[privoxy] non-privoxy established connection %d\n%!" 
                      src_port;
                    return () )
                | (_, false) -> (
                     let (_, gw, _) = Net_cache.Routing.get_next_hop
                                        m.OP.Match.nw_dst in 
                    let _ = Printf.printf 
                              "[privoxy] non-privoxy coonection on port %d\n%!" 
                              src_port in
(*                     let Some(dst_mac,_, _ ) = Net_cache.Switching.ip_of_mac
 *                     gw in  *)
                    let isn = Tcp.get_tcp_sn data in
                    let req = Printf.sprintf "CONNECT %s:443 HTTP/1.1\nUser-Agent: signpst\nProxy-Connection: keep-alive\nHost: %s\n\n"
                                (Uri_IP.ipv4_to_string m.OP.Match.nw_dst)
                                (Uri_IP.ipv4_to_string m.OP.Match.nw_dst) in 
                    let mapping = {src_mac=m.OP.Match.dl_src; dst_mac=m.OP.Match.dl_dst; 
                                   src_ip=m.OP.Match.nw_src; dst_ip = m.OP.Match.nw_dst;
                                   dst_port=443; state=SSL; ssl_state=SSL_SERVER_INIT;
                                   src_isn=isn;dst_isn=0l; data=req;} in
                      Hashtbl.add conn_db.http_conns src_port mapping;
                      (* establishing connection with privoxy socket *)
                      let pkt = Tcp.gen_server_syn data
                                  (Int32.sub isn
                                     ((Int32.of_int ((String.length mapping.data)))) )
                                  mapping.src_mac mapping.dst_mac
                                  mapping.src_ip gw privoxy_port in 
                      let bs = (OP.Packet_out.packet_out_to_bitstring 
                                  (OP.Packet_out.create ~buffer_id:(-1l)
                                     ~actions:[OP.(Flow.Output(OP.Port.Local , 2000))] 
                                     ~data:pkt ~in_port:(OP.Port.No_port) () )) in  
                        OC.send_of_data controller dpid bs 
            ) 
          )
        | (8118, dst_port) ->
            (try 
               let conn = Hashtbl.find conn_db.http_conns dst_port in
                 match conn.state with 
                   | NON_SSL -> 
                       let actions = [
                         OP.Flow.Set_dl_src(conn.dst_mac);
                         OP.Flow.Set_dl_dst(conn.src_mac);
                         OP.Flow.Set_nw_src(conn.dst_ip);
                         OP.Flow.Set_nw_dst(conn.src_ip);
                         OP.Flow.Set_tp_src(80);
                         OP.Flow.Output((OP.Port.Local), 2000);] in
                       let pkt = OP.Flow_mod.create m 0L OP.Flow_mod.ADD 
                                   ~buffer_id:(Int32.to_int buffer_id)
                                   actions () in 
                       let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
                         OC.send_of_data controller dpid bs
                   | SSL ->
                       (
                         match conn.ssl_state with
                           | SSL_SERVER_INIT -> ( 
                               let isn = Tcp.get_tcp_sn data in 
                                 conn.dst_isn <- isn;
                                 conn.ssl_state <- SSL_CLIENT_INIT;
                                 ssl_send_conect_req controller dpid conn m dst_port )
                           | SSL_CLIENT_INIT -> (
                               let payload_len = (Bitstring.bitstring_length
                                                    (Tcp.get_tcp_packet_payload data)) in
                                 if (payload_len > 0) then (
(*
                                   let isn = Tcp.get_tcp_sn data in 
                                     conn.dst_isn <- (Int32.add isn 
                                                        (Int32.of_int payload_len));
 *)
                                     conn.ssl_state <- SSL_COMPLETE;
                                     ssl_complete_flow controller dpid conn m dst_port 
                             ) else (
                               return (Printf.printf "[privoxy] Ignoring ACK packet\n%!")
                             ))
                           | SSL_COMPLETE -> (
                               return (Printf.printf "[privoxy] Connection has been completed, ignoring flow \n%!");
                             )
                       )
                   | _ ->
                       let _ = Printf.printf "state not implemented\n%!" in 
                         return ()
             with Not_found ->
               return(eprintf "[openflow] dropping incoming packet. No state found for port\n%!") )
        | (_, _) ->
            return (eprintf "[openflow] ERROR: ")

    in
      return ()

  let connect kind _ =
    match kind with 
      | "start" -> 
          (lwt _ = match conn_db.process_pid with
            | None -> restart_privoxy () 
            | Some(pid) -> return (pid)
          in
          return ("OK"))
      | "forward" ->
          Printf.printf "[privoxy] forwarding started\n%!";
          let flow_wild = OP.Wildcards.({
                in_port=true; dl_vlan=true;
                dl_src=true; dl_dst=true;
                dl_type=false; nw_proto=false;
                tp_dst=false; tp_src=true;
                nw_src=(char_of_int 32); nw_dst=(char_of_int 32);
                dl_vlan_pcp=true; nw_tos=true;}) in 
          let flow = OP.Match.create_flow_match flow_wild ~dl_type:(0x0800)
                       ~nw_proto:(char_of_int 6) ~tp_dst:80 () in 
          Sp_controller.register_handler flow http_pkt_in_cb;
          let flow_wild = OP.Wildcards.({
                in_port=true; dl_vlan=true;
                dl_src=true; dl_dst=true;
                dl_type=false; nw_proto=false;
                tp_dst=false; tp_src=true;
                nw_src=(char_of_int 32); nw_dst=(char_of_int 32);
                dl_vlan_pcp=true; nw_tos=true;}) in 
          let flow = OP.Match.create_flow_match flow_wild ~dl_type:(0x0800)
                       ~nw_proto:(char_of_int 6) ~tp_dst:443 () in 
          Sp_controller.register_handler flow http_pkt_in_cb;          
          let flow_wild = OP.Wildcards.({
                in_port=true; dl_vlan=true;
                dl_src=true; dl_dst=true;
                dl_type=false; nw_proto=false;
                tp_dst=true; tp_src=false;
                nw_src=(char_of_int 32); nw_dst=(char_of_int 32);
                dl_vlan_pcp=true; nw_tos=true;}) in 
          let flow = OP.Match.create_flow_match flow_wild ~dl_type:(0x0800)
                       ~nw_proto:(char_of_int 6) ~tp_src:8118 () in 
          Sp_controller.register_handler flow http_pkt_in_cb;
          return ("OK")
      | _ -> 
          Printf.eprintf "[proxy] Invalid connection kind %s \n%!" kind;
          raise (ProxyError "Invalid connection kind")
  

  (************************************************************************
   *         Tearing down connection code
   ************************************************************************)
  let teardown _ =
    true


end
