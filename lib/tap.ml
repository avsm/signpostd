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

type tap_det = {
    mutable dev_id: int;
}

let tap_state = {dev_id=0;}

let get_new_dev_ip () =
  let dev_id = tap_state.dev_id in 
    tap_state.dev_id <- tap_state.dev_id + 1;
    dev_id

let setup_dev dev_id ip =
  let dev = Printf.sprintf "tap%d" dev_id in 
  lwt _ = Lwt_unix.system 
            (Printf.sprintf "tunctl -t %s" dev) in
  lwt _ = Lwt_unix.system 
            (Printf.sprintf "ifconfig tap%d up" dev_id) in
  lwt _ = Sp_controller.add_dev dev ip "24" in
  let _ = Net_cache.Routing.add_next_hop (Uri_IP.string_to_ipv4 ip) 
            0xffffff00l 0l dev in 
  let ip_stream = 
    (Unix.open_process_in 
       (Config.dir ^ "/client_tactics/get_local_device " ^ dev)) in
  let record = input_line ip_stream in 
  let ips = Re_str.split (Re_str.regexp " ") record in
  let dev::mac::_ = ips in 
    Net_cache.Arp_cache.add_mapping 
      (Net_cache.Arp_cache.mac_of_string mac) (Uri_IP.string_to_ipv4 ip);
    Net_cache.Port_cache.add_mac (Net_cache.Arp_cache.mac_of_string mac)
      (OP.Port.int_of_port OP.Port.Local );
    return (ip)

