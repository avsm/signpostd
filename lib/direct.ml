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

module Manager = struct
  exception DirectError of string
  exception MissingDirectArgumentError

  let tactic_priority = 6
  
  
  (* local state of the tactic*)
  type conn_type = {
    ip: string;    (* tunnel node ip address *)
    (* list of nodes participating in the tunnel *)
    mutable nodes: string list;
  }

  type conn_db_type = {
    (* connection details for a specific domain *)
    conns : (string, conn_type) Hashtbl.t;
    (* an lwt thread with the udp server *)
    mutable can: unit Lwt.t option;
    (* a file descriptor for the udp server *)
    mutable fd: file_descr option;
  }

  let conn_db = {conns=(Hashtbl.create 0); can=None;fd=None;}
(*******************************************************
 *             Testing code 
 *******************************************************)

(*
 * setup an echo udp listening socket. 
 *
 * *)
  let run_server port =
    Printf.printf "[direct] Starting udp server\n%!";
    let buf = String.create 1500 in
    let sock =Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM
              (Unix.getprotobyname "udp").Unix.p_proto in
    let _ = 
      try
        (Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_any,
        port)))
      with Unix.Unix_error (e, _, _) ->
        printf "[direct] error: %s\n%!" (Unix.error_message e);
        raise (DirectError("Couldn't be a udp server"))
    in
    (* save socket fd so that we can terminate it *)
    conn_db.fd <- Some(sock);

    (* start background echo udp server to test connectivity*)
    conn_db.can <- Some(while_lwt true do
        lwt (len, ip) = Lwt_unix.recvfrom sock buf 0 1500 [] in
        lwt _ = Lwt_unix.sendto sock 
                  (String.sub buf 0 len) 0 len [] ip in
            return ( )
        done)

(*
 * a udp client to send data. 
 * *)
  let run_client port ips =
    let buf = String.create 1500 in
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM
              (Unix.getprotobyname "udp").Unix.p_proto in   
    let send_pkt_to port ip =
      let ipaddr = (Unix.gethostbyname ip).Unix.h_addr_list.(0) in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
        lwt _ = Lwt_unix.sendto sock ip 0 
                  (String.length ip) [] portaddr in 
          return ()
    in
    lwt _ = Lwt_list.iter_p (send_pkt_to port) ips in
     try_lwt 
       let ret = ref None in 
       let recv = 
         (lwt (len, _) = Lwt_unix.recvfrom sock buf 0 1500 [] in
         ret := Some(String.sub buf 0 len);
         return ()) in
       lwt _ = (Lwt_unix.sleep 1.0) <?> recv in 
       lwt _ = Lwt_unix.close sock in
         match (!ret) with
           | None -> raise (DirectError("Unreachable server"))
           | Some(ip) -> return (ip)
     with err -> 
       eprintf "[direct] client test error: %s\n%!" 
         (Printexc.to_string err);
        raise (DirectError(Printexc.to_string err))

  let test kind args =
    match kind with
      (* start udp server *)
      | "server_start" -> (
          let port = (int_of_string (List.hd args)) in 
          let _ = run_server port in
            return ("OK"))
      (* code to stop the udp echo server*)
      | "server_stop" -> (
        match conn_db.can with
          | Some t ->
              cancel t;
              conn_db.can <- None;
              (match conn_db.fd with
                 | Some(fd) -> (Lwt_unix.close fd; conn_db.fd <- None; ())
                 | _ -> ());
              return ("OK")
          | _ -> return ("OK"))
      (* code to send udp packets to the destination*)
      | "client" -> (
          let port :: ips = args in 
            lwt ip = run_client (int_of_string port) ips in
              (printf "[direct] Received a reply from ip %s \n%!" ip);
              return (ip))
      | _ -> (
          printf "[direct] Action %s not supported in test" kind;
          return ("OK"))

  (***************************************************************
   * Connection code 
   * ************************************************************)
  
  let setup_flows dev mac_addr local_ip rem_ip local_sp_ip 
        remote_sp_ip = 
    let controller = 
      (List.hd Sp_controller.switch_data.Sp_controller.of_ctrl) in 
    let dpid = 
      (List.hd Sp_controller.switch_data.Sp_controller.dpid)  in

    let Some(port) = Net_cache.Port_cache.dev_to_port_id dev in
    (* outgoing flow configuration *)

    let flow_wild = OP.Wildcards.({
      in_port=true; dl_vlan=true; dl_src=true; dl_dst=true;
      dl_type=false; nw_proto=true; tp_dst=true; tp_src=true;
      nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
      dl_vlan_pcp=true; nw_tos=true;}) in
    let flow = OP.Match.create_flow_match flow_wild 
                 ~dl_type:(0x0800) ~nw_dst:remote_sp_ip () in
    let actions = [ OP.Flow.Set_nw_src(local_ip);
                    OP.Flow.Set_nw_dst(rem_ip);
                    OP.Flow.Set_dl_dst(
                      (Net_cache.Arp_cache.mac_of_string mac_addr));
                    OP.Flow.Output((OP.Port.port_of_int port), 
                                   2000);] in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD 
                ~priority:tactic_priority ~idle_timeout:0 
                ~buffer_id:(-1) actions () in 
    let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
    lwt _ = OC.send_of_data controller dpid bs in
      
    (* get local mac address *)
    let ip_stream = 
      (Unix.open_process_in
         (Config.dir^"/client_tactics/get_local_device br0")) in
    let ips = Re_str.split (Re_str.regexp " ") 
                (input_line ip_stream) in 
    let _::mac::_ = ips in
    let mac = Net_cache.Arp_cache.mac_of_string mac in 
    
    (* Setup incoming flow *)
    let flow_wild = OP.Wildcards.({
      in_port=false; dl_vlan=true; dl_src=true; dl_dst=true;
      dl_type=false; nw_proto=true; tp_dst=true; tp_src=true;
      nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
      dl_vlan_pcp=true; nw_tos=true;}) in
    let flow = OP.Match.create_flow_match flow_wild 
                 ~in_port:port ~dl_type:(0x0800) 
                 ~nw_dst:local_ip () in
    let actions = [ OP.Flow.Set_nw_dst(local_sp_ip);
                     OP.Flow.Set_nw_src(remote_sp_ip); 
                    OP.Flow.Set_dl_dst(mac);
                    OP.Flow.Output(OP.Port.Local, 2000);] in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD 
                ~priority:tactic_priority ~idle_timeout:0  
                ~buffer_id:(-1) actions () in 
    let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      OC.send_of_data controller dpid bs

       
  let connect kind args =
    raise(DirectError(
        (Printf.sprintf "[direct] invalid connect action %s" kind)))

  let enable kind args =
    match kind with
    | "enable" ->(
      try_lwt
        let mac_addr::local_ip::remote_ip::
            local_sp_ip::remote_sp_ip::_ = args in
        let [local_ip; remote_ip; local_sp_ip; remote_sp_ip;] = 
          List.map Uri_IP.string_to_ipv4 
            [local_ip; remote_ip; local_sp_ip; remote_sp_ip;] in 
        lwt _ = setup_flows Config.net_intf mac_addr 
                  local_ip remote_ip local_sp_ip remote_sp_ip in
          return true
      with e -> 
        eprintf "[direct] enable error: %s\n%!" (Printexc.to_string e); 
        raise (DirectError((Printexc.to_string e)))
    )    
    | _ -> raise(DirectError(
        (Printf.sprintf "[direct] invalid invalid action %s" kind)))

  (* tearing down the flow that push traffic over the tunnel 
   * *)
  let unset_flows dev local_tun_ip remote_sp_ip = 
    let controller = 
      (List.hd Sp_controller.switch_data.Sp_controller.of_ctrl) in 
    let dpid = 
      (List.hd Sp_controller.switch_data.Sp_controller.dpid)  in
    let Some(port) = Net_cache.Port_cache.dev_to_port_id dev in

    (* outgoing flow removal *)
    let flow_wild = OP.Wildcards.({
      in_port=true; dl_vlan=true; dl_src=true; dl_dst=true;
      dl_type=false; nw_proto=true; tp_dst=true; tp_src=true;
      nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
      dl_vlan_pcp=true; nw_tos=true;}) in
    let flow = OP.Match.create_flow_match flow_wild 
                 ~dl_type:(0x0800) ~nw_dst:remote_sp_ip () in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.DELETE_STRICT 
                ~priority:tactic_priority ~idle_timeout:0 
                ~buffer_id:(-1) [] () in 
    lwt _ = OC.send_of_data controller dpid
      ( OP.Flow_mod.flow_mod_to_bitstring pkt  ) in
      
    (* Setup incoming flow *)
    let Some(port) = Net_cache.Port_cache.dev_to_port_id dev in
    let flow_wild = OP.Wildcards.({
      in_port=false; dl_vlan=true; dl_src=true; dl_dst=true;
      dl_type=false; nw_proto=true; tp_dst=true; tp_src=true;
      nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
      dl_vlan_pcp=true; nw_tos=true;}) in
    let flow = OP.Match.create_flow_match flow_wild 
                 ~in_port:port ~dl_type:(0x0800) 
                 ~nw_dst:local_tun_ip () in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.DELETE_STRICT
                ~priority:tactic_priority ~idle_timeout:0  
                ~buffer_id:(-1) [] () in 
      OC.send_of_data controller dpid 
        (OP.Flow_mod.flow_mod_to_bitstring pkt)

  let disable kind  args =
    match kind with 
      | "disable" ->
          let local_tun_ip::remote_sp_ip::_ = args in
          let [local_tun_ip; remote_sp_ip;] = 
            List.map Uri_IP.string_to_ipv4 
              [local_tun_ip; remote_sp_ip;] in 

          (* disable required openflow flows *)
          lwt _ = unset_flows Config.net_intf local_tun_ip 
                    remote_sp_ip in
            return ("true")
      | _ -> (
          printf "[direct] teardown action %s not supported in test" kind;
          return ("false"))
  
  let teardown kind args = 
    printf "[direct] teardown action %s not supported" kind;
    return ("false")
end
