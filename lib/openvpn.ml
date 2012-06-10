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
  exception OpenVpnError of string
  exception MissingOpenVPNArgumentError

  type conn_type = {
    ip: string;
    port: int;
    pid: int;
    dev_id:int;
    mutable nodes: string list;
  }

  type conn_db_type = {
    (* connection details for a specific domain *)
    conns : (string, conn_type) Hashtbl.t;
    mutable max_id : int;
    mutable can: unit Lwt.t option;
    mutable fd: file_descr option;
  }

  let conn_db = {conns=(Hashtbl.create 0); max_id=0; can=None;fd=None;}
(*******************************************************
 *             Testing code 
 *******************************************************)

(*
 * setup an echo udp listening socket. 
 *
 * *)
  let run_server port =
    Printf.printf "[openvpn] Starting udp server\n%!";
    let buf = String.create 1500 in
    let sock =Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM
              (Unix.getprotobyname "udp").Unix.p_proto in
    let _ = 
      try
        (Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_any,
        port)))
      with Unix.Unix_error (e, _, _) ->
        Printf.printf "[openvpn] error: %s\n%!" (Unix.error_message e);
        raise (OpenVpnError("Couldn't be a udp server"))
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
    let _ = try
      (* Make this a bit more random*)
        (Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_any,
        10000)))
    with exn -> Printf.eprintf "[openvpn] error:%s\n%!" 
                  (Printexc.to_string exn);
                raise (OpenVpnError("Couldn't be a udp server"))
    in
    let send_pkt_to port ip =
      let ipaddr = (Unix.gethostbyname ip).Unix.h_addr_list.(0) in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
        lwt _ = Lwt_unix.sendto sock ip 0 
                  (String.length ip) [] portaddr in 
          return ()
    in
    lwt _ = Lwt_list.iter_p (send_pkt_to port) ips in
     try_lwt 
       let ret = ref "" in 
       let recv = 
         (lwt (len, _) = Lwt_unix.recvfrom sock buf 0 1500 [] in
        ret := (String.sub buf 0 len);
         return ()) in
         lwt _ = (Lwt_unix.sleep 1.0) <?> recv in 
         lwt _ = Lwt_unix.close sock in
         match (!ret) with
           | "" -> raise (OpenVpnError("Unreachable server"))
           | ip -> return (ip)
     with err -> 
       eprintf "[openvpn] client test error: %s\n%!" 
         (Printexc.to_string err);
        raise (OpenVpnError(Printexc.to_string err))

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
      (Printf.printf "[openvpn] Received a reply from ip %s \n%!" ip);
        return (ip))
    | _ -> (
      Printf.printf "[openvpn] Action %s not supported in test" kind;
      return ("OK"))

(***************************************************************
 * Connection code 
 * ************************************************************)
  let intercept_arp controller dpid ip in_port out_port = 
    let flow_wild = OP.Wildcards.({
      in_port=false; dl_vlan=true; dl_src=true; dl_dst=true;
      dl_type=false; nw_proto=true; tp_dst=true; tp_src=true;
      nw_dst=(char_of_int 8); nw_src=(char_of_int 32);
      dl_vlan_pcp=true; nw_tos=true;}) in
    let flow = OP.Match.create_flow_match flow_wild 
                 ~in_port:(OP.Port.int_of_port in_port) 
                 ~dl_type:(0x0806) ~nw_dst:ip () in
    let actions = [ OP.Flow.Output(out_port, 2000);] in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD 
                ~idle_timeout:0 ~buffer_id:(-1) actions () in 
    let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      OC.send_of_data controller dpid bs


  let setup_flows dev local_ip rem_ip sp_ip = 
    let controller = (List.hd Sp_controller.
                      switch_data.Sp_controller.of_ctrl) in 
    let dpid = 
      (List.hd Sp_controller.switch_data.Sp_controller.dpid)  in

    (* configure arp interception *)
    let Some(port) = Net_cache.Port_cache.dev_to_port_id dev in
    lwt _ = intercept_arp controller dpid local_ip 
              OP.Port.Local (OP.Port.port_of_int port) in
    lwt _ = intercept_arp controller dpid local_ip 
              (OP.Port.port_of_int port) OP.Port.Local in 

    (* outgoing flow configuration *)
    let flow_wild = OP.Wildcards.({
      in_port=true; dl_vlan=true; dl_src=true; dl_dst=true;
      dl_type=false; nw_proto=true; tp_dst=true; tp_src=true;
      nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
      dl_vlan_pcp=true; nw_tos=true;}) in
    let flow = OP.Match.create_flow_match flow_wild 
                 ~dl_type:(0x0800) ~nw_dst:sp_ip () in
    let Some(port) = Net_cache.Port_cache.dev_to_port_id dev in
    let actions = [ OP.Flow.Set_nw_src(local_ip);
                    OP.Flow.Set_nw_dst(rem_ip);
                    OP.Flow.Output((OP.Port.port_of_int port), 
                                   2000);] in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD 
                ~idle_timeout:0 ~buffer_id:(-1) actions () in 
    let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
    lwt _ = OC.send_of_data controller dpid bs in
      
    (* get local mac address *)
    let ip_stream = (Unix.open_process_in
                       (Config.dir ^ 
                        "/client_tactics/get_local_device br0")) in
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
    let actions = [ OP.Flow.Set_nw_dst(local_ip);
                     OP.Flow.Set_nw_src(sp_ip); 
                    OP.Flow.Set_dl_dst(mac);
                    OP.Flow.Output(OP.Port.Local, 2000);] in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD 
                ~idle_timeout:0  ~buffer_id:(-1) actions () in 
    let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      OC.send_of_data controller dpid bs

      
    let start_openvpn_daemon server_ip port node domain typ conn_id = 
      (* /openvpn_tactic.sh 10000 1 d2.signpo.st debian haris 10.10.0.3 tmp/ conf/ *)
      (* Generate conf directories and keys *)
      let cmd = Config.dir ^ 
                "/client_tactics/openvpn/openvpn_tactic.sh" in
      let exec_cmd = 
        if ((Nodes.get_local_name ()) = "unknown" ) then
           Printf.sprintf "%s %s %d %s d%d %s %s %s %s %s "
             cmd port conn_id Config.domain
             Config.signpost_number node server_ip domain 
             Config.conf_dir Config.tmp_dir   
        else
          Printf.sprintf "%s %s %d %s %s.d%d %s %s %s %s %s "
            cmd port conn_id Config.domain (Nodes.get_local_name ())
            Config.signpost_number node server_ip domain 
            Config.conf_dir Config.tmp_dir in
      Printf.printf "[openvpn] executing %s\n%!" exec_cmd;
      lwt _ = Lwt_unix.system exec_cmd in 
      let _ = Unix.create_process "openvpn" 
              [|""; "--config"; 
                (Config.tmp_dir^"/"^domain^"/" ^ typ ^ ".conf") |] 
              Unix.stdin Unix.stdout Unix.stderr in
      lwt _ = Lwt_unix.sleep 1.0 in      
        return (conn_id)
      (* start server *)
  
    let server_append_dev node domain =
      let cmd = Config.dir ^ 
                "/client_tactics/openvpn/openvpn_append_device.sh" in
      let exec_cmd =  
        if ((Nodes.get_local_name ()) = "unknown" ) then
          Printf.sprintf "%s d%d %s %s %s %s %s"
            cmd Config.signpost_number node Config.domain 
            domain Config.conf_dir Config.tmp_dir  
        else
          Printf.sprintf "%s %s.d%d %s %s %s %s %s"
            cmd  (Nodes.get_local_name ()) Config.signpost_number 
            node Config.domain domain Config.conf_dir 
            Config.tmp_dir in
      Printf.printf "[openvpn] executing %s\n%!" exec_cmd;
      Lwt_unix.system exec_cmd  
        
    let read_pid_from_file filename = 
        let fd = open_in filename in
        let pid = int_of_string (input_line fd) in 
          close_in fd;
          Printf.printf "[openvpn] process (pid %d) ...\n%!" pid;
          pid

    let get_domain_dev_id node domain port = 
      if Hashtbl.mem conn_db.conns domain then  (
        let conn = Hashtbl.find conn_db.conns domain in
          if (List.mem (node^"."^Config.domain) conn.nodes) then (
            (* A connection already exists *)
            Printf.printf 
              "[openvpn] node %s is already added\n%!" node;
            return (conn.dev_id)
          ) else (
            (* Add domain to server and restart service *)
            Printf.printf "[openvpn] adding device %s\n%!" node;
            let _ = server_append_dev node domain in
              conn.nodes <- conn.nodes@[(node^"."^Config.domain)];
              (* restart server *)
              Unix.kill conn.pid Sys.sigusr1;
              lwt _ = Lwt_unix.sleep 4.0 in      
                return (conn.dev_id))
      ) else (
        (* if domain seen for the first time, setup conf dir 
         * and start server *)
        let _ = Printf.printf "[openvpn] start serv add device %s\n%!" 
          node in
        let dev_id = Tap.get_new_dev_ip () in 
        let ip = Printf.sprintf "10.3.%d.1" dev_id in
        lwt _ = Tap.setup_dev dev_id ip in
        lwt dev_id = start_openvpn_daemon "0.0.0.0" port 
                       node domain "server" dev_id in 
        let pid = read_pid_from_file (Config.tmp_dir ^ "/" ^ 
                                      domain ^"/server.pid") in 
          Hashtbl.add conn_db.conns (domain) 
            {ip=ip;port=(int_of_string port);pid;
             dev_id;nodes=[node ^ "." ^ Config.domain]};
          return(dev_id) ) 
        
  let connect kind args =
    match kind with
    | "server" ->(
      try_lwt
        let port::node::domain::sp_ip::_ = args in
        lwt dev_id = get_domain_dev_id node domain port in
        let dev = Printf.sprintf "tap%d" dev_id in
        let local_ip = Uri_IP.string_to_ipv4 
                         (Printf.sprintf "10.3.%d.1" dev_id) in  
        let rem_ip = Uri_IP.string_to_ipv4 
                       (Printf.sprintf "10.3.%d.2" dev_id) in 
(*
        lwt _ = Lwt_unix.sleep 1.0 in
        let cmd = (Printf.sprintf "route add %s gw %s" sp_ip 
                     (Uri_IP.ipv4_to_string rem_ip)) in
        lwt _ = Lwt_unix.system cmd in
        lwt _ = setup_flows dev local_ip rem_ip 
                  (Uri_IP.string_to_ipv4 sp_ip) in
 *)
          return (Uri_IP.ipv4_to_string local_ip)
      with e -> 
        eprintf "[openvpn] server error: %s\n%!" (Printexc.to_string e); 
        raise (OpenVpnError((Printexc.to_string e)))
    )
    | "server_enable" ->(
      try_lwt
        let dev_id::sp_ip::_ = args in
        let dev = Printf.sprintf "tap%s" dev_id in
        let local_ip = Uri_IP.string_to_ipv4 
                         (Printf.sprintf "10.3.%s.1" dev_id) in  
        let rem_ip = Uri_IP.string_to_ipv4 
                       (Printf.sprintf "10.3.%s.2" dev_id) in 
        lwt _ = setup_flows dev local_ip rem_ip 
                  (Uri_IP.string_to_ipv4 sp_ip) in
          return (Uri_IP.ipv4_to_string local_ip)
      with e -> 
        eprintf "[openvpn] server error: %s\n%!" (Printexc.to_string e); 
        raise (OpenVpnError((Printexc.to_string e)))
    )    
    | "client" -> (
      try_lwt
        let ip :: port :: node :: domain :: 
            local_ip :: rem_ip :: sp_ip :: _ = args in
        let sp_ip = Uri_IP.string_to_ipv4 sp_ip in 
        let dev_id = Tap.get_new_dev_ip () in
        let net_dev = Printf.sprintf "tap%d" dev_id in
        Printf.printf "settind dev %s ip %s\n%!" net_dev local_ip;
        lwt _ = Tap.setup_dev dev_id local_ip in
        lwt _ = start_openvpn_daemon ip port node domain 
                  "client" dev_id in  
        lwt _ = setup_flows net_dev (Uri_IP.string_to_ipv4 local_ip) 
                  (Uri_IP.string_to_ipv4 rem_ip) sp_ip in
(*
        lwt _ = Lwt_unix.system (Printf.sprintf "route add %s gw %s"
                     (Uri_IP.ipv4_to_string sp_ip) rem_ip) in
          Printf.printf "route add %s gw %s"
            (Uri_IP.ipv4_to_string sp_ip) rem_ip; 
 *)
        let pid = read_pid_from_file (Config.tmp_dir ^ "/" ^ 
                                      domain ^  "/client.pid") in 
          Hashtbl.add conn_db.conns (domain) 
            {ip=local_ip; port=(int_of_string port);
             pid;dev_id; nodes=[node^ "." ^ Config.domain];};
          return (local_ip)
      with ex ->
        raise(OpenVpnError(Printexc.to_string ex)))
    | _ -> raise(OpenVpnError(
        (Printf.sprintf "[openvpn] invalid invalid action %s" kind)))

  let teardown args =
    (* kill openvpn pid*)
    let domain = List.hd args in 
    (* Destroy state *)
    if (Hashtbl.mem conn_db.conns domain) then
      Hashtbl.remove conn_db.conns domain;
      true
end
