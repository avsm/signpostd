(*
 * Copyright (c) 2005-2012 Charalampos Rotsos <cr490@cl.cam.ac.uk>
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

module OP = Openflow.Packet

module Routing = struct

  (* Iface   Destination     Gateway         Flags   RefCnt  Use     Metric
  * Mask            MTU     Window  IRTT *)
  let route_regexp = "\\([a-zA-Z0-9]*\\)\t\\([0-9A-Z]*\\)\t\\([0-9A-Z]*\\)\t" ^
                     "\\([0-9A-Z]*\\)\t\\([0-9]*\\)\t\\([0-9]*\\)\t\\([0-9]*\\)" ^
                     "\t\\([0-9A-Z]*\\)"
  type t = {
    ip: int32;
    mask : int32;
    gw : int32;
    (* this is a bit useless, as the device will usually be br0 *)
    dev_id : string;
  }

  type routing_tbl_typ = {
    mutable tbl : t list;
  }

  let routing_tbl = {tbl=[];}

  let string_rev str =
    let len = String.length str in
    let ret = String.create len in 
    let rec string_rev src dst n i =
      match i with 
        | -1 ->  dst.[n] <- str.[0]; dst
        | -2 -> dst
        | i -> 
            dst.[n - i - 1] <- str.[i]; 
            dst.[n - i ] <- str.[i + 1]; 
            string_rev src dst n (i-2)
    in
      string_rev str ret (len-1) (len-2)

  let load_routing_table () =
    let pat = Re_str.regexp route_regexp in 
    let route = open_in "/proc/net/route" in
    let rec load_routing route = 
      try 
        let entry = input_line route in
        let _ =
          if (Re_str.string_match pat entry 0 ) then (
            let dev_id = Re_str.matched_group 1 entry in 
            let ip = Int32.of_string ("0x"^(String.lowercase 
                    (string_rev (Re_str.matched_group 2 entry)))) in 
            let gw = Int32.of_string ("0x"^( String.lowercase 
                    (string_rev (Re_str.matched_group 3 entry)))) in 
            let mask = Int32.of_string ("0x"^( String.lowercase 
                        (string_rev (Re_str.matched_group 8 entry)))) in 
            let fib = {ip;mask;gw;dev_id;} in
              routing_tbl.tbl <- routing_tbl.tbl @ [fib]
(*               Printf.printf "reading dev:%s net:%lx gw:%lx mask:%lx\n%!"
 *               dev_id ip gw mask *)
          ) else (
            Printf.printf "Failed to match entry\n%!"
          )
        in
          load_routing route
      with End_of_file -> ()
    in 
    (* SKip first line as this is the header *)
    let _ = input_line route in 
    let _ = load_routing route in 
    let _ = close_in_noerr route in 
    return ()

  let get_next_hop dst =
    let ip = ref 0l in 
    let mask = ref 0l in
    let gw = ref 0l in 
    let dev = ref "lo" in
      (* TODO need to consider weights in the routing table? *)
    let match_ip fib = 
      if ( ( ( (Int32.logor fib.mask !mask) <> !mask) || (!mask = Int32.zero)) &&
          ((Int32.logand fib.ip fib.mask) = (Int32.logand dst fib.mask))) then (
            ip := fib.ip;
            mask := fib.mask;
            gw := fib.gw;
            dev := fib.dev_id
          ) else (())
    in
      List.iter match_ip routing_tbl.tbl;
      (!ip, !gw, !dev)

  let add_next_hop ip mask gw dev_id = 
    let entry = {ip; mask; gw; dev_id;} in 
    let _ = 
      if (List.mem entry routing_tbl.tbl) then
        ()
      else
        routing_tbl.tbl <- routing_tbl.tbl @ [entry;]
    in
      ()

  let del_next_hop ip mask gw dev_id = 
    let entry = {ip; mask; gw; dev_id;} in 
    let _ = 
      if (List.mem entry routing_tbl.tbl) then 
        routing_tbl.tbl <- 
          (List.filter (fun a -> entry <> a) routing_tbl.tbl)
    in      
      ()
end


module Port_cache = struct
  let dev_cache = Hashtbl.create 64 

  (* Maybe I need here an additional field to define a datapath id*)
  let mac_cache = Hashtbl.create 64

  let string_of_mac = function
    | "" -> ""
    | mac ->
        let ret = ref "" in 
        String.iter (fun a -> ret := Printf.sprintf 
                          "%s%02x:" !ret (int_of_char a)) mac; 
    String.sub (!ret) 0 ((String.length !ret) - 1)

  let add_dev dev port_id =
    let dev = Re_str.global_replace (Re_str.regexp "\x00") "" dev in 
      Hashtbl.replace dev_cache dev port_id

  let del_dev dev =
    Hashtbl.remove dev_cache dev

  let dev_to_port_id dev =
    if (Hashtbl.mem dev_cache dev) then
      Some(Hashtbl.find dev_cache dev )
    else 
      None

  let port_id_to_dev port_id = 
    let ret = ref None in 
      Hashtbl.iter (fun a b -> 
                      if (b = port_id) then 
                        ret := Some(a)
      ) dev_cache;
      (!ret)

  let add_mac mac port_id = 
(*     Printf.printf "[dev_cahce] adding mac %s on port %d\n%!"  *)
(*       (string_of_mac mac) port_id; *)
    if (Hashtbl.mem mac_cache mac) then
      Hashtbl.replace mac_cache mac port_id
    else
      Hashtbl.add mac_cache mac port_id


  let port_id_of_mac mac =
    Printf.printf "port_id_of_mac %s\n%!" (string_of_mac mac);
    if(Hashtbl.mem mac_cache mac ) then
      Some(Hashtbl.find mac_cache mac )
    else 
      None

  let mac_of_port_id port_id =
    Printf.printf "port_id_of_mac %d\n%!" port_id;
    let ret = ref None in
    Hashtbl.iter (fun a b ->
                    if(b = port_id) then
                      ret := Some(a)
    ) mac_cache;
    !ret
end
module Arp_cache = struct 
  let cache = Hashtbl.create 64

  let add_mapping mac ip = 
    (* Check if ip addr is local *)
    let (_,gw,_) = Routing.get_next_hop ip in 
      match gw with 
        | 0l -> (
            if (Hashtbl.mem cache ip) then 
              Hashtbl.replace cache ip mac
            else
              Hashtbl.add cache ip mac
          ) 
        | _ -> Printf.printf "[net_cache] ip %s is not local. ignoring.\n%!"
                 (Uri_IP.ipv4_to_string ip)

  let del_mapping ip = 
    (* Check if ip addr is local *)
    let (_,gw,_) = Routing.get_next_hop ip in 
      match gw with 
        | 0l -> (
            if (Hashtbl.mem cache ip) then 
              Hashtbl.remove cache ip
          ) 
        | _ -> Printf.printf "[net_cache] ip %s is not local. ignoring.\n%!"
                 (Uri_IP.ipv4_to_string ip)

  let string_of_mac = function
    | "" -> ""
    | mac ->
        let ret = ref "" in 
        String.iter (fun a -> ret := Printf.sprintf 
                          "%s%02x:" !ret (int_of_char a)) mac; 
    String.sub (!ret) 0 17


  let mac_of_string mac = 
    let entries = Re_str.split (Re_str.regexp ":") mac in 
      let rec parse_mac = function
        | [] -> ""
        | [b] -> (String.make 1 (char_of_int (int_of_string ("0x"^b))))
        | b :: r -> ((String.make 1 (char_of_int (int_of_string ("0x"^b)))) ^ 
                    (parse_mac r))
      in 
        parse_mac entries

  let load_arp () =
    let mac_pat = "[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]:" ^
                  "[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]" in 
    let mac_regexp = Re_str.regexp mac_pat in 
    
      (* reading ip dev mappings *)
    let ip_stream = (Unix.open_process_in
                       (Config.dir ^ "/client_tactics/get_local_dev_ip ")) in
    let rec read_ip ip_stream = 
      try 
        let ips = Re_str.split (Re_str.regexp " ") (input_line ip_stream) in 
        let dev::ip::mac::_ = ips in
          Printf.printf "%s %s %s\n%!" dev ip mac; 
          add_mapping (mac_of_string mac) (Uri_IP.string_to_ipv4 ip);
          Port_cache.add_mac (mac_of_string mac) 
            (OP.Port.int_of_port OP.Port.Local);
          read_ip ip_stream
      with End_of_file -> ()
    in 

    (* reading arp cache *)
    let route = open_in "/proc/net/arp" in
    let rec read_arp_cache ip_stream = 
      try 
        let ips = Re_str.split (Re_str.regexp "[ ]+") (input_line ip_stream) in
        let ip = Uri_IP.string_to_ipv4 (List.nth ips 0) in
        let mac = mac_of_string (List.nth ips 3) in
           Printf.printf "%s %s %s \n%!" (string_of_mac mac)  
             (Uri_IP.ipv4_to_string ip); 
           add_mapping mac ip;
           read_arp_cache ip_stream
      with 
          End_of_file -> ()
    in 
    let _ = input_line route in 
    let _ = read_arp_cache route in 
    let _ = close_in_noerr route in 
      return (read_ip ip_stream)

  let mac_of_ip ip = 
    match (Hashtbl.mem cache ip) with
      | true -> Some(Hashtbl.find cache ip)
      | false -> None

  let ip_of_mac mac = 
    let ret = ref None in 
      Hashtbl.iter (fun ip dev -> 
                      if (dev = mac) then 
                        ret := Some(ip)
      ) cache;
      (!ret)
    
  let get_next_hop_mac dst = 
    let (_, gw, _) = Routing.get_next_hop dst in
      Printf.printf "looking up for %s\n%!" (Uri_IP.ipv4_to_string gw);
      match gw with 
        | 0l -> mac_of_ip dst
        | ip -> mac_of_ip ip    
end

