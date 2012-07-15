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
let tactic_priority = 5 

module OP = Ofpacket
module OC = Controller

module Manager = struct
  exception SshError of string
  exception MissingSshArgumentError

  (**************************************************
   *         Tactic state
   **************************************************)

  type conn_type =
    | SSH_SERVER
    | SSH_CLIENT

 (* storing all required informations to rebuild 
   * authorized_keys file and destroy the connection *)
  type client_det = {
    key: string;
    ip: int32;
    port : int;
    conn_id: int32;
    dev_id : int;
    mutable pid : int;
    conn_tp : conn_type;
  }

  type conn_db_type = {
    conns: (string, client_det) Hashtbl.t;
    mutable server_pid: int option;
  }

  let conn_db = {conns=(Hashtbl.create 32); 
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
      try_lwt 
        let buf = String.create 1500 in
        let sock = (Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM
                      ((Unix.getprotobyname "tcp").Unix.p_proto)) in        
        let ipaddr = (Unix.gethostbyname ip).Unix.h_addr_list.(0) in
        let portaddr = Unix.ADDR_INET (ipaddr, port) in
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
      with
          ex ->
            Printf.eprintf "[ssh] error in client test %s\n%!" (Printexc.to_string ex);
            return ()
    in
    (* Run client test for all remote ips and return the ip that reasponded
    * first *)
    let listener, wakener = Lwt.task () in 
(*     lwt _ = Lwt.choose [(Lwt_list.iter_p (send_pkt_to wakener port) ips);
 *     sleeper] in  *)
     Lwt.ignore_result(Lwt_list.iter_p (send_pkt_to wakener port) ips);
     lwt _ = (Lwt_unix.sleep 2.0) <?> listener in 
       match (!ret) with
         | None -> raise (SshError("Error"))
         | Some(ip) -> return (ip)

  let test kind args =
    match kind with
      (* start ssh server *)
      | "server_start" -> 
          run_server ()
      (* test tcp connectivity *)
      | "client" -> (
          try_lwt
            let ssh_port :: ips = args in 
            let ssh_port = int_of_string ssh_port in
            lwt ip = ((lwt _ = Lwt_unix.sleep 2.0 in 
                         failwith("client can't connect") ) 
                        <?> (run_client ssh_port ips)) in
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

  let server_add_client conn_id domain rem_extern_ip loc_dev_id = 
    Printf.printf "[ssh] Adding new key from domain %s\n%!" domain;
    lwt _ = run_server () in 
    (* Dump keys in authorized_key file *)
    let update_authorized_keys () = 
      let file = open_out "/root/.ssh/signpost_tunnel" in 
        Hashtbl.iter (fun _ client ->
                        if (client.conn_tp = SSH_CLIENT) then
                             output_string file (client.key ^ "\n") 
        ) conn_db.conns; 
        close_out file
    in
      (* if the domain is not in the cache, add it and update 
       * the authorized key file *)
    lwt _ = 
      if(Hashtbl.mem conn_db.conns domain) then (
        eprintf "[ssh] connection already exists\n%!";
        return ()
      ) else (
        lwt key = Key.ssh_pub_key_of_domain 
                    ~server:(Config.iodine_node_ip) 
                    ~port:5354 domain in
          match key with 
            | Some(key) -> 
                Hashtbl.add conn_db.conns domain 
                  {key=(List.hd key);ip=rem_extern_ip;
                   port=0; conn_id; dev_id=(loc_dev_id);
                   pid=0;  conn_tp=SSH_CLIENT; };
                return (update_authorized_keys ())
            | None ->
                return (Printf.printf 
                          "[ssh] Couldn't find dnskey record\n%!")
        )
      in
        return ("OK")

  let client_add_server conn_id node ip port dev_id = 
    Printf.printf "[ssh] Adding know_host domain %s\n%!" node;
    let domain = sprintf "%s.%s" node Config.domain in
    (* Dump keys in authorized_key file *)
    let update_known_hosts () = 
      let file = open_out (Config.conf_dir ^ "/known_hosts") in 
      let _ = 
        Hashtbl.iter 
          (fun _ server ->
             if (server.conn_tp = SSH_SERVER) then
                 output_string file (sprintf "[%s]:%d %s\n" 
                                       (Uri_IP.ipv4_to_string server.ip)
                                       server.port server.key)) 
          conn_db.conns 
      in
        close_out file
    in
      (* if the domain is not in the cache, add it and update the authorized
       * key file *)    
    lwt _ = 
      if(Hashtbl.mem conn_db.conns domain) then (
        Printf.eprintf "[ssh] A connection already exists\n%!";
        return ()
      ) else (
        lwt key = Key.ssh_pub_key_of_domain 
                    ~server:(Config.iodine_node_ip) ~port:5354 domain in
          match key with 
            | Some(key) -> 
                Hashtbl.add conn_db.conns domain 
                  {key=(List.hd key);port;ip; conn_id;dev_id;pid=0;
                  conn_tp=SSH_SERVER; };
                return (update_known_hosts ())
            | None ->
                return (Printf.printf "[ssh] no valid dnskey record\n%!")
      )
    in
      return ("OK")

  let client_connect server_ip server_port local_dev remote_dev = 
    let cmd = Unix.getcwd () ^ "/client_tactics/ssh/client" in
    (* TODO: add pid in client state. *)
    let _ = Unix.create_process cmd [|cmd; Config.conf_dir; server_ip;
                                        (string_of_int server_port);
                                        (string_of_int local_dev);
                                        (string_of_int remote_dev); |] 
              Unix.stdin Unix.stdout Unix.stderr in
      return ()

  let setup_flows dev mac_addr local_ip rem_ip local_sp_ip 
        remote_sp_ip = 

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
                 ~dl_type:(0x0800) ~nw_dst:remote_sp_ip () in
    let Some(port) = Net_cache.Port_cache.dev_to_port_id dev in
    let actions = [ OP.Flow.Set_nw_src(local_ip);
                    OP.Flow.Set_nw_dst(rem_ip);
                    OP.Flow.Set_dl_dst(
                      (Net_cache.Arp_cache.mac_of_string mac_addr));                    
                    OP.Flow.Output((OP.Port.port_of_int port), 
                                   2000);] in
    let pkt = OP.Flow_mod.create flow 0L OP.Flow_mod.ADD 
                ~priority:tactic_priority 
                ~idle_timeout:0 ~buffer_id:(-1) actions () in 
    lwt _ = OC.send_of_data controller dpid 
              (OP.Flow_mod.flow_mod_to_bitstring pkt) in
    
    (* get local mac address *)
    let ip_stream = (Unix.open_process_in
                       (Config.dir ^ 
                        "/client_tactics/get_local_device br0")) in
    let ips = Re_str.split (Re_str.regexp " ") (input_line ip_stream) in 
    let _::mac::_ = ips in
    let mac = Net_cache.Arp_cache.mac_of_string mac in

    (* setup incoming flow *)
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
      OC.send_of_data controller dpid 
        (OP.Flow_mod.flow_mod_to_bitstring pkt)

  let connect kind args =
    try_lwt
      match kind with
          | "server" -> (
          let rem_node::conn_id::rem_sp_ip::loc_tun_ip::_ = args in 
          let conn_id = Int32.of_string conn_id in
          let rem_sp_ip = Uri_IP.string_to_ipv4 rem_sp_ip in
  
          (* Setup tunel tun tap device *)
          let dev_id = Tap.get_new_dev_ip () in
          lwt _ = Tap.setup_dev dev_id loc_tun_ip in 

          (* Adding remote node public key in authorized keys file *)
          let q_rem_node = sprintf "%s.%s" rem_node Config.domain in
          let _ = server_add_client conn_id q_rem_node rem_sp_ip dev_id in
  
           return(string_of_int dev_id))
        | "client" ->
          let server_ip::ssh_port::rem_node::conn_id::loc_tun_ip::
              rem_dev:: _ = args in
          let conn_id = Int32.of_string conn_id in 
          let ssh_port = int_of_string ssh_port in 
          let rem_dev = int_of_string rem_dev in 
          let loc_dev = Tap.get_new_dev_ip () in 
          lwt _ = client_add_server conn_id rem_node
                    (Uri_IP.string_to_ipv4 server_ip)
                    ssh_port loc_dev in
          lwt _ = Tap.setup_dev loc_dev loc_tun_ip in
          lwt _ = client_connect server_ip ssh_port loc_dev rem_dev in
           return (string_of_int loc_dev)
        | "enable" -> begin
          let conn_id::mac_addr::local_ip::remote_ip::
              local_sp_ip::remote_sp_ip::_ = args in
          let conn_id = Int32.of_string conn_id in
          let [local_ip; remote_ip; local_sp_ip; remote_sp_ip;] = 
            List.map Uri_IP.string_to_ipv4 
              [local_ip; remote_ip; local_sp_ip; remote_sp_ip;] in 
          let dev_id = ref None in 
          let _ = 
            Hashtbl.iter 
              (fun _ conn -> 
                 if (conn.conn_id = conn_id) then
                   dev_id := Some(conn.dev_id)) conn_db.conns
          in 
            match (!dev_id) with
              | None -> 
                  raise (SshError(("openvpn enable invalid conn_id")))
              | Some (dev) ->
                  (lwt _ = setup_flows (sprintf "tap%d" dev) mac_addr 
                            local_ip remote_ip local_sp_ip remote_sp_ip in
                    return ("true"))
          end
        | _ -> 
            Printf.eprintf "[ssh] Invalid connect kind %s\n%!" kind;
            raise (SshError("Invalid connect kind"))
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
