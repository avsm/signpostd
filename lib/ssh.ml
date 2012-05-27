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

let ssh_port = 10000

module OP = Ofpacket
module OC = Controller

module Manager = struct
  exception SshError of string
  exception MissingSshArgumentError

  (**************************************************
   *         Tactic state
   **************************************************)
  (* storing all required informations to rebuild 
   * known_hosts file and destroy the connection *)
  type server_det = {
    ip : string;
    port : int;
    dev_id : int;
    mutable pid : int;
    cl_key : string;
  }

  (* storing all required informations to rebuild 
   * authorized_keys file and destroy the connection *)
  type client_det = {
    s_key: string;
    local_dev: int;
  }

  type conn_db_type = {
    conns_server: (string, server_det) Hashtbl.t;
    conns_client: (string, client_det) Hashtbl.t;
    mutable server_pid: int option;
  }

  let conn_db = {conns_server=(Hashtbl.create 32); 
                 conns_client=(Hashtbl.create 32); 
                 server_pid=None;}

(**********************************************************
 *  Init methods
 * *)

  let init_module () = 
   (* TODO: 
    * - Remove all tap* devices.
    * - kill previous sshd server. 
    * *)
    return ()
  let destroy_module () = 
  init_module ()

  (*********************************************************
   *       Testing methods
   *********************************************************)

  (* start the sshd server *)
  let run_server () =
    (* TODO: Check if pid is still running *)
    match conn_db.server_pid with
    | None ->(
      try
        let cmd = Config.dir ^ "/client_tactics/ssh/server" in
        printf "%s %s\n%!" cmd Config.conf_dir;
        let _ = Unix.create_process cmd [| cmd; Config.conf_dir |] 
        Unix.stdin Unix.stdout Unix.stderr in
        lwt _ = Lwt_unix.sleep 2.0 in 
        let buf = String.create 100 in
        let fd = Unix.openfile "/tmp/signpost_sshd.pid" [Unix.O_RDONLY]  0o640 in
        let len = Unix.read fd buf 0 100 in 
        conn_db.server_pid <- Some(int_of_string (String.sub buf 0 (len-1)));
        Printf.printf "[ssh] process created with pid %s...\n" (String.sub buf 0 (len-1));
        return("OK")
      with err ->
        Printf.eprintf "[ssh] error : %s\n%!" (Printexc.to_string err);
        failwith  (Printexc.to_string err)
        )
      | Some(_) -> 
          Printf.printf "[ssh] ssh server already started...\n%!";
          return("OK")

  (*TODO:
   * - timeout connect 
   * - if all tests fail how do I notify the server? 
   * - remove ips that match local ips *)
  let run_client port ips =
    let ret = ref None in 
    (* check if I can connect to ssh port on a remote ip *)
    let send_pkt_to wakener port ip = 
      let buf = String.create 1500 in
      let sock = (Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM
                    ((Unix.getprotobyname "tcp").Unix.p_proto)) in        
      let ipaddr = (Unix.gethostbyname ip).Unix.h_addr_list.(0) in
      let portaddr = Unix.ADDR_INET (ipaddr, port) in
        try_lwt 
          (* TODO: Need to get a better timeout mechanism in connect phase 
           * otherwise we will wiat for 2 min *)
          lwt _ = Lwt_unix.connect sock portaddr in 
          (* If no data received in 2 seconds, fail the thread. *)
          let _ = setsockopt_float sock SO_RCVTIMEO 2.0 in 
          lwt len = Lwt_unix.recv sock buf 0 1500 [] in  
            Printf.printf "[ssh] Received (%s) from ipaddr %s\n%!" 
               (String.sub buf 0 len) ip;
             lwt _ = Lwt_unix.close sock in
             ret := Some(ip);
             let _ = Lwt.wakeup wakener () in 
            return () 
        with ex ->
          Printf.eprintf "[ssh] error in client test %s\n%!" (Printexc.to_string ex);
          return ()
  in
    (* Run client test for all remote ips and return the ip that reasponded
    * first *)
    let _, wakener = Lwt.task () in 
(*     lwt _ = Lwt.choose [(Lwt_list.iter_p (send_pkt_to wakener port) ips);
 *     sleeper] in  *)
     lwt _ = (Lwt_list.iter_p (send_pkt_to wakener port) ips) in
       printf "list iter failed\n%!";
       match (!ret) with
         | None -> raise (SshError("Error"))
         | Some(ip) -> return (ip)

  let test kind args =
    match kind with
      (* start ssh server *)
      | "server_start" -> run_server ()

      (* test tcp connectivity *)
      | "client" -> (
          try_lwt
            let _ :: ips = args in 

            lwt ip = ((lwt _ = Lwt_unix.sleep 2.0 in 
                         failwith("client can't connect") ) 
                        <?> run_client ssh_port ips) in
              return (ip)
          with ex ->
            Printf.printf "[ssh] failed to start client: %s\n%!" 
              (Printexc.to_string ex);
            raise(SshError(Printexc.to_string ex)) )
      | _ -> (
          Printf.printf "Action %s not supported in test" kind;
          return ("OK"))

  (*******************************************************************
   *    connection functions     
   *******************************************************************)
  let setup_flows dev local_ip rem_ip = 

    let controller = (List.hd Sp_controller.
                      switch_data.Sp_controller.of_ctrl) in 
    let dpid = 
      (List.hd Sp_controller.switch_data.Sp_controller.dpid)  in
    let flow_wild = OP.Wildcards.({
      in_port=true; dl_vlan=true; dl_src=true; dl_dst=true;
      dl_type=false; nw_proto=true; tp_dst=true; tp_src=true;
      nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
      dl_vlan_pcp=true; nw_tos=true;}) in
    let flow = OP.Match.create_flow_match flow_wild 
                 ~dl_type:(0x0800) ~nw_dst:rem_ip () in
    let Some(port) = Net_cache.Port_cache.dev_to_port_id dev in
    let actions = [ OP.Flow.Set_nw_dst(rem_ip);
                    OP.Flow.Output((OP.Port.port_of_int port), 
                                   2000);] in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD 
                ~idle_timeout:0  ~buffer_id:(-1) actions () in 
    let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
    lwt _ = OC.send_of_data controller dpid bs in
      
    let ip_stream = (Unix.open_process_in
                       (Config.dir ^ 
                        "/client_tactics/get_local_device br0")) in
    let ips = Re_str.split (Re_str.regexp " ") (input_line ip_stream) in 
    let _::mac::_ = ips in
    Printf.printf "XXXXXXXX mac %s\n%!" mac;
    let mac = Net_cache.Arp_cache.mac_of_string mac in 
    let flow_wild = OP.Wildcards.({
      in_port=false; dl_vlan=true; dl_src=true; dl_dst=true;
      dl_type=false; nw_proto=true; tp_dst=true; tp_src=true;
      nw_dst=(char_of_int 0); nw_src=(char_of_int 32);
      dl_vlan_pcp=true; nw_tos=true;}) in
    let flow = OP.Match.create_flow_match flow_wild 
                 ~in_port:port ~dl_type:(0x0800) 
                 ~nw_dst:local_ip () in
    let actions = [ OP.Flow.Set_nw_dst(local_ip);
                    OP.Flow.Set_dl_dst(mac);
                    OP.Flow.Output(OP.Port.Local, 2000);] in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD 
                ~idle_timeout:0  ~buffer_id:(-1) actions () in 
    let bs = OP.Flow_mod.flow_mod_to_bitstring pkt in
      OC.send_of_data controller dpid bs

  let server_add_client domain dev_id = 
    Printf.printf "[ssh] Adding new permitted key from domain %s\n%!" domain;
    lwt _ = run_server () in 
    (* Dump keys in authorized_key file *)
    let update_authorized_keys () = 
      let file = open_out "/root/.ssh/signpost_tunnel" in 
        Hashtbl.iter (fun _ client -> 
                        output_string file (client.s_key ^ "\n") 
        ) conn_db.conns_client; 
        close_out file
    in
      (* if the domain is not in the cache, add it and update the authorized
       * key file *)
      lwt _ = 
        if(Hashtbl.mem conn_db.conns_client domain) then (
          Printf.eprintf "[ssh] A connection the specific domain already exists\n%!";
          return ()
        ) else (
          lwt key = Key.ssh_pub_key_of_domain ~server:(Config.iodine_node_ip) 
                      ~port:5354 domain in
    match key with 
      | Some(key) -> 
          Hashtbl.add conn_db.conns_client domain 
            {s_key=(List.hd key);local_dev=dev_id;};
          return (update_authorized_keys ())
      | None ->
          return (Printf.printf "[ssh] Couldn't find a valid dnskey record\n%!")
        )
                 in
                   return ("OK")

  let client_add_server domain ip port local_dev = 
    Printf.printf "[ssh] Adding new know_host from domain %s\n%!" domain;
    (* Dump keys in authorized_key file *)
    let update_known_hosts () = 
      let file = open_out (Config.conf_dir ^ "/known_hosts") in 
        Hashtbl.iter (fun _ server ->
                      output_string file (
                        Printf.sprintf "[%s]:%d %s\n" 
                          server.ip server.port                    
                          server.cl_key)) conn_db.conns_server; 
        close_out file
    in
      (* if the domain is not in the cache, add it and update the authorized
       * key file *)    
      lwt _ = 
        if(Hashtbl.mem conn_db.conns_server domain) then (
          Printf.eprintf "[ssh] A connection the specific domain already exists\n%!";
          return ()
        ) else (
          try_lwt 
            lwt key = Key.ssh_pub_key_of_domain 
                      ~server:(Config.iodine_node_ip) ~port:5354 domain in
            match key with 
              | Some(key) -> 
                Hashtbl.add conn_db.conns_server domain 
                  {cl_key=(List.hd key);port;ip; dev_id=local_dev;pid=0;};
                return (update_known_hosts ())
              | None ->
                return (Printf.printf "[ssh] no valid dnskey record\n%!")
                with ex ->
                  Printf.printf "[ssh] client fail %s\n%!" 
                    (Printexc.to_string ex);
                  raise (SshError(Printexc.to_string ex)))
      in
        return ("OK")

  let client_connect server_ip server_port local_dev remote_dev _ = 
    let cmd = Unix.getcwd () ^ "/client_tactics/ssh/client" in
    (* TODO: add pid in client state. *)
    let _ = Unix.create_process cmd [|cmd; Config.conf_dir; server_ip;
                                        (string_of_int server_port);
                                        local_dev;remote_dev; |] 
              Unix.stdin Unix.stdout Unix.stderr in
      return (Printf.sprintf "10.2.%s.2" remote_dev)

  let connect kind args =
    try_lwt
    match kind with
      | "server" -> (
          let dev_id = Tap.get_new_dev_ip () in 
            server_add_client (List.hd args) dev_id;
            let dev = Printf.sprintf "tap%d" dev_id in
            let ip = Printf.sprintf "10.2.%d.1" dev_id in 
            let local_ip = Uri_IP.string_to_ipv4 
                           (Printf.sprintf "10.2.%d.1" dev_id) in  
            let rem_ip = Uri_IP.string_to_ipv4 
                           (Printf.sprintf "10.2.%d.2" dev_id) in 
            lwt _ = Tap.setup_dev dev_id ip in 
            lwt _ = Lwt_unix.sleep 0.0 in
            lwt _ = setup_flows dev local_ip rem_ip in
              return(ip))

      | "client" ->
          let server_ip = List.nth args 0 in 
          let server_port = (int_of_string (List.nth args 1)) in 
          let domain = List.nth args 2 in 
          let subnet = List.nth args 3 in 
          let local_dev = Tap.get_new_dev_ip () in        
          lwt _ = client_add_server domain server_ip 
                    server_port local_dev in 
  (* Ip address is constructed using the dev number in the 3 
   * octet *)
          let remote_dev = 
            List.nth (Re_str.split (Re_str.regexp "\\.") subnet) 2 in 
          let ip = Printf.sprintf "10.2.%s.2" remote_dev in 
          let gw_ip = Printf.sprintf "10.2.%s.1" remote_dev in 
          lwt _ = Tap.setup_dev local_dev ip in                 
          lwt _ = Lwt_unix.sleep 0.0 in
          let dev = Printf.sprintf "tap%d" local_dev in
          let local_ip = Uri_IP.string_to_ipv4 
                           (Printf.sprintf "10.2.%d.2" local_dev) in  
          let rem_ip = Uri_IP.string_to_ipv4 
                         (Printf.sprintf "10.2.%d.1" local_dev) in 
          lwt _ = setup_flows dev local_ip rem_ip in
           
            (* TODO: temporary hack to allow 2 nodes to talk when connected 
            * over the server. I need to set 2 different subnets. With the 
            * usage of openflow, this can be corrected. *) 
          lwt _ = Lwt_unix.system 
                    (Printf.sprintf "route add -net 10.2.0.0/16 gw %s" 
                       gw_ip) in              
            client_connect server_ip server_port 
              (string_of_int local_dev) remote_dev subnet  
      | _ -> 
          Printf.eprintf "[ssh] Invalid connect kind %s\n%!" kind;
          raise (SshError "Invalid connect kind")
      with exn ->
        Printf.eprintf "[ssh]Error:%s\n%!" (Printexc.to_string exn);
        raise (SshError(Printexc.to_string exn))

  (*************************************************************************
   *             TEARDOWN methods of tactic
   * ***********************************************************************)

  let teardown _ =
    true

  let pkt_in_cb _ _ _ = 
    return ()

end
