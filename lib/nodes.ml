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

exception Client_error


(* Nodes have a lot of associated information.
 * It is all part of the node store.
 *)
type node = {
  signalling_channel: Sp.signalling_channel;
  name: Sp.name;
  local_ips: Sp.ip list;
  public_ips : Sp.ip list
}

type nodes_state = {
  nodes: (string, node) Hashtbl.t;
}


(* node name -> Sp.node *)
let node_db = {nodes=(Hashtbl.create 0);}

let new_node_with_name name ?(ips=[]) ?(public_ips=[]) () = {
  name = name;
  signalling_channel = Sp.NoSignallingChannel;
  local_ips = ips;
  public_ips = [];
}

let update name node =
  Hashtbl.replace node_db.nodes name node

let get name = 
  try (Hashtbl.find node_db.nodes name)
  with Not_found -> (new_node_with_name name () )

let get_ip name =
  let node = get name in
  match node.signalling_channel with
    | Sp.NoSignallingChannel -> raise Not_found
    | Sp.SignallingChannel(ip, _port) -> ip

let get_local_ips name =
  let node = get name in
  node.local_ips


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
  | Sp.SignallingChannel(ip, port) -> (ip, port)

let pending_responses = Hashtbl.create 1

let addr_from ip port = 
  Unix.(ADDR_INET (inet_addr_of_string ip, (to_int port)))

let send_fd = Lwt_unix.(socket PF_INET SOCK_DGRAM 0)

let register_sender id wakeup_cbk = 
  Hashtbl.replace pending_responses id wakeup_cbk

let send_datagram text dst =
  Lwt_unix.sendto send_fd text 0 (String.length text) [] dst

let send_to_addr addr rpc = 
  let buf = Rpc.rpc_to_string rpc in
  lwt len' = send_datagram buf addr in
  return (eprintf "sent [%d]: %s\n%!" len' buf)

let send name rpc =
  let ip, port = signalling_channel name in
  let dst = addr_from ip port in
  send_to_addr dst rpc

let send_to_server rpc =
  let ip = Config.iodine_node_ip in
  let port = of_int Config.signal_port in
  eprintf "Sending to %s:%Li\n%!" ip port;
  let server = addr_from ip port in
  send_to_addr server rpc

let send_blocking name rpc =
  let open Rpc in
  let sleeper, wakener = Lwt.wait () in
  
  let id = match rpc with
  | Request(_, _, id) -> id 
  | Tactic_request (_, _, _, id) -> id 
  | _ -> raise Client_error("Invalid rpc send ") 
  in
  register_sender id wakener;
  send name rpc;
  sleeper >>= fun result ->
  match result with
  | Tactic_response(_, Result r, _,_) -> return r
  | Tactic_response(_, _, Error e, _) -> raise Client_error e
  | Response(Result r, _, _) -> return r
  | Response(_, Error e, _) -> raise Sp.Client_error e

let wake_up_thread_with_reply id data =
  try 
    let wakener = Hashtbl.find pending_responses id in
    Hashtbl.remove pending_responses id;
    return (Lwt.wakeup wakener data)
  with Not_found -> return ()

(* ---------------------------------------------------------------------- *)

(* Public API *)
let set_signalling_channel name channel_ip port =
  let node = get name in
  let sch = Sp.SignallingChannel(channel_ip, port) in
  update name {node with signalling_channel = sch}

let set_local_ips name local_ips =
  let node = get name in
  update name {node with local_ips = local_ips}

let discover_local_ips ?(dev="") () =
  let ip_stream = (Unix.open_process_in
  (Unix.getcwd () ^ "/client_tactics/get_local_ips " ^ dev)) in
  let buf = String.make 1500 ' ' in
  let len = input ip_stream buf 0 1500 in
  let ips = Re_str.split (Re_str.regexp " ") (String.sub buf 0 (len-1)) in
  let rec print_ips = function
    | ip :: ips ->
        Printf.printf "ip: %s\n%!" ip;
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

let get_node_ip name =
  try 
    let ip_string = (get_ip name) in
    convert_ip_string_to_int ip_string
  with Not_found -> 0l

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
    eprintf "About to send RPC and wait for IP results...\n";
    send_blocking name rpc >>= fun results ->
    eprintf "Got a list of ips back from the server....\n";
    let public_ips = list_of_ips_from_string results in
    let node = get name in
    update name {node with public_ips = public_ips};
    let ip_list = list_of_ips_from_string results in
    List.iter (fun ip -> eprintf "Received for %s\n%!" ip) ip_list;
    eprintf "About to return...\n";
    return ip_list
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
