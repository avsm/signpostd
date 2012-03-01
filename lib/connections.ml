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


type handle = 
  | Name of Sp.name
  | Wildcard

type address =
  | Address of Sp.addressable
  | NoAddress

type connection_state = {
  connections : (handle * handle, (address * address) list) Hashtbl.t;
}


let connection_db = {connections = (Hashtbl.create 0);}

let in_order a b = a < b

let get pair =
  try (Hashtbl.find connection_db.connections pair)
  with Not_found -> []

let set key value =
  Hashtbl.replace connection_db.connections key value

let flip_addresses addrs =
  List.map (fun (a,b) -> (b,a)) addrs

let name_to_db_name (a,b) =
  (Name(a), Name(b))

let ip_list_to_address_list ips =
  List.map (fun (a,b) -> 
    (Address(Sp.IPAddressInstance(a)), Address(Sp.IPAddressInstance(b)))
  ) ips

let store_addresses a b addr_list =
  match in_order a b with
  | true -> set (name_to_db_name (a,b)) (ip_list_to_address_list addr_list)
  | false -> set (name_to_db_name (b,a)) (ip_list_to_address_list (flip_addresses addr_list))

let set_public_ips a ips =
  eprintf "Storing ips for %s\n" a;
  List.iter (fun s -> eprintf "storing ip: %s\n%!" s) ips;
  let key = (Wildcard, Name(a)) in
  let addrs = List.map (fun ip -> (NoAddress, Address(Sp.IPAddressInstance(ip)))) ips in
  set key addrs

let lookup a_name b_name =
  eprintf "Looking up connections from %s to %s\n%!" a_name b_name;
  let a = Name a_name in
  let b = Name b_name in
  let tunnels = match in_order a b with
  | true -> begin
      let addresses = get (a,b) in
      List.map (fun (_, addr) -> addr) addresses
  end
  | false -> begin
      let addresses = get (b,a) in
      List.map (fun (addr, _) -> addr) addresses
  end in
  let direct_connection = 
    let addresses = get (Wildcard, b) in
    List.map (fun (_, addr) -> addr) addresses in
  let connections = direct_connection @ tunnels in
  let filtered = List.filter (function
    | NoAddress -> false
    | _ -> true) connections in
  List.map (fun (Address(a)) -> a) filtered

let find a b =
  eprintf "Finding existing connections between %s and %s\n" a b;
  eprintf "Trying to establish new ones\n";
  Engine.connect a b;
  lookup a b
