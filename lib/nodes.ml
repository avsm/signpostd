(*
 * Copyright (c) 2012 Sebastian Probst Eide <sebastian.probst.eide@gmail.com>
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
open Printf
open Int64
open Unix
open Re_str


(* ---------------------------------------------------------------------- *)

exception InconsistentState of string

let sp_ip_network = "172.31.0.0"
let sp_ip_netmask = 16



(* Nodes have a lot of associated information.
 * It is all part of the node store.
 *)
type node = {
  signalling_channel: Sp.signalling_channel;
  name: Sp.name;
  local_ips: Sp.ip list;
  public_ips : Sp.ip list;
  sp_ip : int32;
}

type nodes_state = {
  nodes: (string, node) Hashtbl.t;
}

let local_name = ref "unknown"
let server_fd = ref None

(* node name -> Sp.node *)
let node_db = {nodes=(Hashtbl.create 0);}

let rec find_free_ip () =  
    (* 172.31.0.0 is the network, 172.31.0.1 is the cloud,
     *  172.31.255.255 is the broadcast *)
    let node_ip = Int32.add (Uri_IP.string_to_ipv4 sp_ip_network)  
                  (Int32.add (Random.int32 0xFFFCl)  2l) in
    let found = ref false in 
      Hashtbl.iter (fun _ a -> 
      found := ((!found) || (node_ip = a.sp_ip)) ) node_db.nodes;
      match (!found) with
        | false -> node_ip
        | true -> find_free_ip ()

let new_node_with_name name ?(ips=[]) ?(public_ips=[]) () = {
  name = name;
  signalling_channel = Sp.NoSignallingChannel;
  local_ips = ips;
  public_ips = []; 
  sp_ip = (find_free_ip ());
}

let update name node =
  Hashtbl.replace node_db.nodes name node

let get name = 
(*   Hashtbl.find node_db.nodes name *)
  try (Hashtbl.find node_db.nodes name)
  with Not_found -> (new_node_with_name name () ) 

let get_ip name =
  let node = get name in
  match node.signalling_channel with
    | Sp.NoSignallingChannel -> raise Not_found
    | Sp.SignallingChannel(fd) -> fd

let get_local_ips name =
  let node = get name in
  node.local_ips

let get_sp_ip name = 
  let node = get name in
    node.sp_ip

let get_local_name () = 
  (!local_name)

let set_local_name name =
  local_name := name

let get_nodes () = 
  let ret = ref [] in 
  Hashtbl.iter (fun a b -> ret := [a] @ (!ret)) node_db.nodes;
  !ret

(* ---------------------------------------------------------------------- *)

(* Sending RPC's to nodes, are defined as part of the Nodes module
 * functionality, in order to break som nasty circular dependencies.
 * Not ideal from a design perspective, but not catastrophic either.
 *
 * Sending RPCs: 
 *)
(* id -> handler *)

let signalling_channel name =
  let node = get name in
  match node.signalling_channel with
  | Sp.NoSignallingChannel -> raise Not_found
  | Sp.SignallingChannel(fd) -> fd

let set_server_signalling_channel fd = 
  server_fd := Some(fd)
let server_signalling_channel () =
  match (!server_fd) with 
    | Some(fd) -> fd
    | None -> raise Not_found

let pending_responses = Hashtbl.create 1

let addr_from ip port = 
  Unix.(ADDR_INET (inet_addr_of_string ip, (to_int port)))

let send_fd = Lwt_unix.(socket PF_INET SOCK_DGRAM 0)

let register_sender id wakeup_cbk = 
  Hashtbl.replace pending_responses id wakeup_cbk

let register_thread_timer id sleeper = 
  Lwt_unix.sleep (float_of_int Config.rpc_timeout) >>= fun _ ->
  match (Lwt.state sleeper) with
  | Sleep -> begin
      Hashtbl.remove pending_responses id;
      return (Lwt.cancel sleeper)
  end
  | _ -> return ()
let send_datagram text dst =
   Lwt_unix.sendto send_fd text 0 (String.length text) [] dst 

let send_tcp_pkt text fd =
  Lwt_unix.send fd text 0 (String.length text) []

let send_to_addr fd rpc = 
  let buf = Rpc.rpc_to_string rpc in
  lwt len' = send_tcp_pkt buf fd in
   return (eprintf "sent [%d]: %s\n%!" len' buf) 
(*     return () *)

let send name rpc =
  let fd = signalling_channel name in
(*   let dst = addr_from ip port in *)
  send_to_addr fd rpc

let send_to_server rpc =
(*  let ip = Config.iodine_node_ip in
  let port = of_int Config.signal_port in *)
  let fd = server_signalling_channel () in 
(*   eprintf "Sending to %s:%Li\n%!" ip port; *)
(*   let server = addr_from ip port in *)
  send_to_addr fd rpc

let send_blocking name rpc =
  let open Rpc in
  let sleeper, wakener = Lwt.task () in
  let id = match rpc with
  | Request(_, _, id) -> id 
  | _ -> raise (Sp.Client_error "Invalid rpc send ")
  in
  register_sender id wakener;
  register_thread_timer id sleeper;
  send name rpc;
  sleeper >>= fun result ->
  match (Lwt.state sleeper) with
  | Lwt.Fail(Lwt.Canceled) -> begin
      (* the RPC timed out, so we return None, 
       * to the caller *)
      raise Rpc.Timeout
  end
  | Lwt.Fail error -> Lwt.fail error
  | Lwt.Return result -> begin
      match result with
      | Response(Result r, _) -> return r
      | Response(Error e, _) -> raise (Sp.Client_error e)
      | _ -> 
          raise (Sp.Client_error "Blocking send received unknsupported response")
  end
  | Lwt.Sleep -> begin
      (* The thread should not reach this case,
       * as it only executes after the thread has
       * returned or been canceled. If we get here
       * we should raise an exception! *)
      raise (InconsistentState "RPC sleeping, but executed?")
  end

let wake_up_thread_with_reply id data =
  try 
    let wakener = Hashtbl.find pending_responses id in
    Hashtbl.remove pending_responses id;
    return (Lwt.wakeup wakener data)
  with Not_found -> 
    (* the RPC must have timed out. A timeout would
     * have caused the wakener to be removed from
     * the pending responses table *)
    return ()

(* ---------------------------------------------------------------------- *)

(* Public API *)

(*let add_new_node name = 
  if (Hashtbl.mem )*)

let set_signalling_channel name fd =
  let node = get name in
  let sch = Sp.SignallingChannel(fd) in
    update name {node with signalling_channel = sch}

let set_local_ips name local_ips =
  let node = get name in
    update name {node with local_ips = local_ips}

let discover_local_ips ?(dev="") () =
  let ip_stream = (Unix.open_process_in
  (Config.dir ^ "/client_tactics/get_local_ips " ^ dev)) in
  let buf = String.make 1500 ' ' in
  let len = input ip_stream buf 0 1500 in
  let ips = Re_str.split (Re_str.regexp " ") (String.sub buf 0 (len-1)) in
  let rec print_ips = function
    | ip :: ips ->
(*         Printf.printf "ip: %s\n%!" ip; *)
        print_ips ips
    | [] -> ()
  in
  ips 

(* in int32 format for dns. default to 0.0.0.0 *)
let convert_ip_string_to_int ip_string =
  let ipv4_addr_of_tuple (a,b,c,d) =
    let (+) = Int32.add in
    (Int32.shift_left a 24) +
    (Int32.shift_left b 16) +
    (Int32.shift_left c 8) + d
  in
  (* Read an IPv4 address dot-separated string *)
  let ipv4_addr_of_string x =
    let ip = ref 0l in
    (try Scanf.sscanf x "%ld.%ld.%ld.%ld"
      (fun a b c d -> ip := ipv4_addr_of_tuple (a,b,c,d));
    with _ -> ());
    !ip
  in
  ipv4_addr_of_string ip_string

(*
let get_node_ip name =
  try 
    let ip_string = (get_ip name) in
    convert_ip_string_to_int ip_string
  with Not_found -> 0l
 *)

let check_for_publicly_accessible_ips name ips =
  let token = "hi_from_server" in
  let listen_port = 30000 + (Random.int 20000) in
  let list_of_ips_from_string ip_str =
    let open Re_str in
    let remove_character_return = regexp "\n" in
    let on_whitespace = regexp " " in
    let chomped_str = global_replace remove_character_return "" ip_str in
    split on_whitespace chomped_str 
  in
  let listen_for_datagrams () =
    let args = (string_of_int listen_port) :: token :: [] in
    let rpc = Rpc.create_request "listen_for_datagrams" args in
(*     eprintf "About to send RPC and wait for IP results...\n"; *)
    try 
      send_blocking name rpc >>= fun results ->
(*       eprintf "Got a list of ips back from the server....\n"; *)
      let public_ips = list_of_ips_from_string results in
      let node = get name in
      update name {node with public_ips = public_ips};
      let ip_list = list_of_ips_from_string results in
(*       List.iter (fun ip -> eprintf "Received for %s\n%!" ip) ip_list; *)
(*       eprintf "About to return...\n"; *)
      return ip_list
    with Rpc.Timeout ->
      return []
  in
  let send_datagrams () =
    (* Wait for a while to allow the RPC to reach the client first. *)
    Lwt_unix.sleep 1.0 >>
    Lwt_list.iter_p (fun ip ->
      let target = addr_from ip (of_int listen_port) in
      let msg = sprintf "%s-%s" token ip in
      lwt _ = send_datagram msg target in
       eprintf "Sent datagram %s\n%!" msg; 
      return ()
    ) ips >>
    return []
  in
  let fn_list = [listen_for_datagrams; send_datagrams] in
  lwt ip_list :: _ = Lwt_list.map_p (fun f -> f ()) fn_list in
  return ip_list

(* ---------------------------------------------------------------------- *)
