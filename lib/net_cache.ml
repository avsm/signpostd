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

module OP = Ofpacket

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
(*     Printf.printf "[dev_cahce] adding device %s as port %d\n%!" dev port_id;
 *     *)
    if (Hashtbl.mem dev_cache dev) then (
      Hashtbl.remove dev_cache dev;
      Hashtbl.add dev_cache dev port_id
    ) else
      Hashtbl.add dev_cache dev port_id

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
(*
    Printf.printf "[dev_cahce] adding device %s on port %d\n%!" 
      (string_of_mac mac) port_id;
*)
    if (Hashtbl.mem mac_cache mac) then (
      Hashtbl.remove mac_cache mac;
      Hashtbl.add mac_cache mac port_id
    ) else
      Hashtbl.add mac_cache mac port_id

  let del_mac mac = 
(*
    Printf.printf "[dev_cahce] removing device %s\n%!" 
      (Arp_cache.string_of_mac mac);
 *)
    if (Hashtbl.mem mac_cache mac) then 
      Hashtbl.remove mac_cache mac

  let port_id_of_mac mac =
    Printf.printf "port_id_of_mac %s\n%!" (string_of_mac mac);
    if(Hashtbl.mem mac_cache mac ) then
      Some(Hashtbl.find mac_cache mac )
    else 
      None
end
module Arp_cache = struct 
  let cache = Hashtbl.create 64

  let add_mapping mac ip = 
    (* Check if ip addr is local *)
    let (_,gw,_) = Routing.get_next_hop ip in 
      match gw with 
        | 0l -> (
            if (Hashtbl.mem cache ip) then (
              Hashtbl.remove cache ip;
              Hashtbl.add cache ip mac
            ) else
              Hashtbl.add cache ip mac
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

(*
module Switching = struct
  type dev_typ = 
    | ETH
    | UNSPEC
  let dev_type_of_string = function
    | "Ethernet" -> ETH
    | "UNSPEC"   -> UNSPEC
    | a -> failwith ("dev_type_of_string: Unk " ^ a)
  let string_of_dev_type = function
    | ETH    -> "Ethernet" 
    | UNSPEC -> "UNSPEC"   

  type t = {
    mac : string;
    (*TODO: the IP is only interesting for us when it 
    * is a local ip. Is it possible to have multihoming for the 
    * same mac address *)
    ip : int32 option;
    time : float;
    dev_id : string;
    typ : dev_typ;
  }

  type switch_tbl_typ = {
    mutable ip_tbl : (int32, t ref) Hashtbl.t;
    mutable mac_tbl : (string, t ref) Hashtbl.t;
  }

  let switch_tbl = {ip_tbl=(Hashtbl.create 64); mac_tbl=(Hashtbl.create 64);}

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
        | [b] -> (String.make 1 (char_of_int (int_of_string ("0x"^b))))
        | b :: r -> ((String.make 1 (char_of_int (int_of_string ("0x"^b)))) ^ 
                    (parse_mac r))
      in 
        parse_mac entries

  let add_entry mac ip dev_id typ =
    let entry=ref {mac;ip;time=(Sys.time ()); dev_id; typ} in
    let _ = 
    match ip with 
      | None -> 
          Printf.printf "[net_cache-switching] adding entry %s None %s %s \n%!" 
            (string_of_mac mac) dev_id (string_of_dev_type typ)
      | Some(ip) -> 
          (Printf.printf "[net_cache-switching] adding entry %s %s %s %s \n%!" 
            (string_of_mac mac) (Uri_IP.ipv4_to_string ip)
            dev_id (string_of_dev_type typ);
          if (Hashtbl.mem  switch_tbl.ip_tbl ip) then (
            Hashtbl.remove switch_tbl.ip_tbl ip;
            Hashtbl.add switch_tbl.ip_tbl ip entry
          ) else (
            Hashtbl.add switch_tbl.ip_tbl ip entry
          ))
    in 
    let _ = 
      if (Hashtbl.mem switch_tbl.mac_tbl mac) then (
        Hashtbl.remove switch_tbl.mac_tbl mac;
        Hashtbl.add switch_tbl.mac_tbl mac entry
      ) else (
        Hashtbl.add switch_tbl.mac_tbl mac entry
      )
    in
      ()

  let load_arp () =
    let mac_pat = "[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]:" ^
                  "[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]:[0-9a-f][0-9a-f]" in 
    let mac_regexp = Re_str.regexp mac_pat in 
    
    (* Load dev to mac mapping *)
    let mac_dev = Hashtbl.create 64 in 
    let ip_stream = (Unix.open_process_in
                       (Config.dir ^ "/client_tactics/get_local_device ")) in
    let rec read_mac ip_stream = 
      try
        let record = input_line ip_stream in
        let ips = Re_str.split (Re_str.regexp " ") record in
        let dev = List.nth ips 0 in 
        let mac = List.nth ips 1 in
        let typ = dev_type_of_string (List.nth ips 2) in 
        let mac = 
          if (Re_str.string_match mac_regexp mac 0) then
            mac_of_string mac 
          else 
            ""
        in
(*          Printf.printf "Reading line %s %s %s\n%!" dev (string_of_mac mac) 
            (string_of_dev_type typ); *)
          Hashtbl.add mac_dev dev (mac, typ);
          read_mac ip_stream
      with End_of_file -> ()
    in
    let _ = read_mac ip_stream in

      (* reading ip dev mappings *)
    let ip_stream = (Unix.open_process_in
                       (Config.dir ^ "/client_tactics/get_local_dev_ip ")) in
    let rec read_ip ip_stream = 
      try 
        let ips = Re_str.split (Re_str.regexp " ") (input_line ip_stream) in 
        let dev::ip::_ = ips in
(*           Printf.printf "%s %s \n%!" dev ip; *)
          let _ =
            if (Hashtbl.mem mac_dev dev) then 
              let (mac, typ) = Hashtbl.find mac_dev dev in 
              let ip = Uri_IP.string_to_ipv4 ip in 
                add_entry mac (Some(ip)) dev typ
            else 
                  Printf.printf "Failed to find dev %s\n%!" dev
          in 
            read_ip ip_stream
      with End_of_file -> ()
    in 

    (* reading arp cache *)
    let route = open_in "/proc/net/arp" in
    let rec read_arp_cache ip_stream = 
      try 
        let ips = Re_str.split (Re_str.regexp "[ ]+") (input_line ip_stream) in
        let dev =List.nth ips 5 in 
        let ip = Uri_IP.string_to_ipv4 (List.nth ips 0) in
        let mac = mac_of_string (List.nth ips 3) in
           Printf.printf "%s %s %s \n%!" (string_of_mac mac) dev 
             (Uri_IP.ipv4_to_string ip); 
           add_entry mac (Some(ip)) dev ETH;
           read_arp_cache ip_stream
      with 
          End_of_file -> ()
    in 
    let _ = input_line route in 
    let _ = read_arp_cache route in 
    let _ = close_in_noerr route in 
      return (read_ip ip_stream)


  let mac_of_ip mac =
    if (Hashtbl.mem switch_tbl.mac_tbl mac) then 
      let entry = Hashtbl.find switch_tbl.mac_tbl mac in 
        Some(((!entry).ip, (!entry).dev_id, (!entry).typ))
        else
          None

  let ip_of_mac ip = 
    if (Hashtbl.mem switch_tbl.ip_tbl ip) then 
      let entry = Hashtbl.find switch_tbl.ip_tbl ip in 
        Some(((!entry).mac, (!entry).dev_id, (!entry).typ))
        else
          None

  let port_of_mac mac = 
    if (Hashtbl.mem switch_tbl.mac_tbl mac) then 
      let entry = Hashtbl.find switch_tbl.mac_tbl mac in 
        Some((!entry).dev_id)
        else
          None
end
 *)

