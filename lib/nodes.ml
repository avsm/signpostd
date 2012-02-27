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

open Printf
open Int64
open Unix
open Re_str

type node = {
  signalling_channel: Sp.signalling_channel;
  name: Sp.name;
  local_ips: string list;
}

type nodes_state = {
    nodes: (string, node) Hashtbl.t;
}

(* node name -> Sp.node *)
let node_db = {nodes=(Hashtbl.create 0);}

let new_node_with_name name ?(ips=[]) () = {
  name = name;
  signalling_channel = Sp.NoSignallingChannel;
  local_ips = ips;
}

let update name node =
  Hashtbl.replace node_db.nodes name node

let get name = 
  try (Hashtbl.find node_db.nodes name)
  with Not_found -> (new_node_with_name name () )

let update_sig_channel name channel_ip port local_ips =
  let node = get name in
  let sch = Sp.SignallingChannel(channel_ip, port) in
  (update name {name;signalling_channel=sch;local_ips;})

let get_ip name =
  let node = get name in
  match node.signalling_channel with
    | Sp.NoSignallingChannel -> raise Not_found
    | Sp.SignallingChannel(ip, _port) -> ip

let get_local_ip () =
    let ip_stream = (Unix.open_process_in
    (Unix.getcwd () ^ "/client_tactics/get_local_ips")) in
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
let get_node_ip name =
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
  let ip =
    try ipv4_addr_of_string (get_ip name)
    with Not_found -> 0l
  in
  ip

let signalling_channel name =
  let node = get name in
  match node.signalling_channel with
  | Sp.NoSignallingChannel -> raise Not_found
  | Sp.SignallingChannel(ip, port) -> (ip, port)
  
let get_local_ip () =
  Printf.printf "fetching local ip information \n%!"; 
  let ip_stream = (Unix.open_process_in 
    "ip addr show | grep \"inet \" | awk '{print $2}' | cut -d \\/ -f 1") in 
  let buf = String.make 1500 ' ' in
  let ips = Re_str.split (Re_str.regexp "\n") buf in 
  let rec print_ips = function
    | ip :: ips -> 
        Printf.printf "ip: %s\n%!" ip; 
        print_ips ips
    | [] -> () 
  in
    print_ips ips;
    let _ = input ip_stream buf 0 1500 in
      Printf.printf "%s\n%!" buf


(* It seems this is needed in order to have the compiler understand
 * the type of the hash table... nasty stuff. *)
let testing = 
  let name = "me" in
  let me = {
    name = name;
    signalling_channel = Sp.SignallingChannel("127.0.0.1", (of_int 4444));
    local_ips=(get_local_ip ())
  } in
  update name me
