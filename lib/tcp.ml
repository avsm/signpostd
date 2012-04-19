(* 
 * Copyright (c) 2012 Charalampos Rotsos <cr409@cl.cam.ac.uk>
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

open Bitstring
open Printf

module Checksum = struct 

  let ones_complement data = 
    let rec add count data =
      bitmatch data with
        | {value:16:littleendian; data:-1:bitstring} ->
            (value + (add (count + 1) data)) 
        | { value:8 } -> 
            (value)
        | { _ } -> 0
    in 
    let res = add 1 data in 
      if (res > 0xffff) then (
        ((lnot ((res land 0xffff) + (res lsr 16))) land 0xffff)
      ) else (
        ((lnot res) land 0xffff)
      )
end

let get_tcp_packet_payload data = 
  bitmatch data with 
    | {_:116:bitstring; ihl:4; _:((ihl*32)-8):bitstring;
         _:96:bitstring; tcp_len:4;
         _:((tcp_len*32) - 100):bitstring; tcp_body:-1:bitstring } ->
(*
        (Printf.printf "get_tcp_packet_payload matched packet\n%!");
        (Bitstring.hexdump_bitstring Pervasives.stdout tcp_body);
 *)
        tcp_body
    | { _ } -> 
        (Bitstring.hexdump_bitstring Pervasives.stdout data;
        Printf.printf "get_tcp_packet_payload failed to parse\n%!";
         Bitstring.empty_bitstring)

let get_tcp_sn data = 
  bitmatch data with 
    | {_:96:bitstring; 0x0800:16; 4:4; ihl:4; _:64:bitstring; 6:8; _:16; 
       _:64:bitstring; _:(ihl-5)*32:bitstring; _:32; isn:32;
       _:-1:bitstring } ->
        isn
    | { _ } -> invalid_arg("get_tcp_sn packet is not TCP")

(*
 * generate a tcp syn packet
* *)
let gen_server_syn data new_isn local_mac gw_mac 
      local_ip gw_ip new_dst_port = 
    bitmatch data with 
      | {_:48:bitstring; _:48:bitstring; header:20:bitstring; 
         ihl:4;  tos:8; tlen:16; ipid:16; flags:3; fragoff:13;
         ttl:8; proto:8; _:16; nw_src:32; nw_dst:32;
         header2:(ihl-5)*32:bitstring;src_port:16; dst_port:16; isn:32; 
         header3:64:bitstring; checksum:16; tcp_body:-1:bitstring } ->
            let ip_chk = 
              Checksum.ones_complement 
                (BITSTRING {4:4; ihl:4; tos:8; tlen:16; ipid:16; 
                            flags:3; fragoff:13; ttl:8; proto:8; 0:16; 
                            gw_ip:32; local_ip:32}) in 
            let tcp_chk = 
                (Checksum.ones_complement 
                   (BITSTRING{gw_ip:32; local_ip:32; 0:8; 6:8; 
                      (((Bitstring.bitstring_length tcp_body)/8) + 18):16; 
                       src_port:16; new_dst_port:16; new_isn:32; 
                       header3:64:bitstring; 0:16;                     
                       tcp_body:(Bitstring.bitstring_length tcp_body):bitstring})) in
              BITSTRING{local_mac:48:string; gw_mac:48:string;
                        header:20:bitstring; ihl:4; tos:8; tlen:16; 
                        ipid:16; flags:3; fragoff:13; ttl:8; proto:8; 
                        ip_chk:16:littleendian; gw_ip:32; local_ip:32; 
                        header2:(ihl-5)*32:bitstring; src_port:16; new_dst_port:16; 
                        new_isn:32; header3:64:bitstring;  tcp_chk:16:littleendian;
                        tcp_body:(Bitstring.bitstring_length tcp_body):bitstring}
      | { _ } -> invalid_arg("gen_server_syn input packet is not TCP") 


(* 
 * generate an ack packet with any data
* *)
let gen_server_ack isn ack local_mac gw_mac gw_ip local_ip 
      dst_port src_port win =
  let eth_hdr = 
      BITSTRING{local_mac:48:string; gw_mac:48:string; 
                0x0800:16} in 
  let ip_chk = Checksum.ones_complement 
                 (BITSTRING{ 4:4; 5:4; 0:8; 40:16; 0:16; 0:3; 
                             0:13; 64:8; 6:8; 0:16; gw_ip:32; 
                             local_ip:32}) in
  let ipv4_hdr = 
    BITSTRING { 4:4; 5:4; 0:8; 40:16; 0:16; 0:3; 0:13; 64:8; 
                6:8; ip_chk:16:littleendian; gw_ip:32; local_ip:32} in
  let tcp_chk = 
    (Checksum.ones_complement (
      BITSTRING{gw_ip:32; local_ip:32; 0:8; 6:8; 20:16; src_port:16; 
                dst_port:16; isn:32;  ack:32; 5:4; 0:6; false:1; 
                true:1; false:1; false:1; false:1; false:1; win:16;
                0:16; 0:16})) in 
  let tcp_hdr = 
    BITSTRING {src_port:16; dst_port:16; isn:32; ack:32; 5:4;
               0:6; false:1; true:1; false:1; false:1; false:1; 
               false:1; win:16; tcp_chk:16:littleendian;0:16} in  
    Bitstring.concat [eth_hdr; ipv4_hdr; tcp_hdr;] 


(* 
* Generate a syn+ack packet 
* *)
let gen_server_synack isn ack local_mac gw_mac src_ip local_ip 
      dst_port src_port =
  let window = 0xffff in 
  let eth_hdr = BITSTRING{local_mac:48:string; gw_mac:48:string; 0x0800:16} in 
  let ip_chk = Checksum.ones_complement (BITSTRING { 4:4; 5:4; 0:8; 40:16; 0:16; 0:3; 
        0:13; 64:8; 6:8; 0:16; src_ip:32; local_ip:32}) in
  let ipv4_hdr = BITSTRING { 4:4; 5:4; 0:8; 40:16; 0:16; 0:3; 0:13; 64:8; 6:8; 
                             ip_chk:16:littleendian; src_ip:32; local_ip:32} in
  let tcp_chk = 
    (Checksum.ones_complement (BITSTRING{src_ip:32; local_ip:32; 0:8; 6:8; 
        20:16; src_port:16; dst_port:16; isn:32;  ack:32; 5:4;
        0:6; false:1; true:1; false:1; false:1; true:1; false:1; window:16;0:16;
        0:16})) in 
  let tcp_hdr = BITSTRING {src_port:16; dst_port:16; isn:32; ack:32; 5:4;
        0:6; false:1; true:1; false:1; false:1; true:1; false:1; 
        window:16; tcp_chk:16:littleendian; 0:16 } in  
    Bitstring.concat [eth_hdr; ipv4_hdr; tcp_hdr;] 

(* 
* Generate a tcp packet with data 
* *)
let gen_tcp_data_pkt isn ack local_mac gw_mac gw_ip local_ip 
      dst_port src_port data =
  let eth_hdr = BITSTRING{local_mac:48:string; gw_mac:48:string; 0x0800:16} in 
  let ip_chk = Checksum.ones_complement (BITSTRING { 4:4; 5:4; 0:8; 
        (40 + ((Bitstring.bitstring_length data)/8)):16; 0:16; 0:3; 
        0:13; 64:8; 6:8; 0:16; gw_ip:32; local_ip:32}) in
  let ipv4_hdr = BITSTRING { 4:4; 5:4; 0:8; 
    (40 + ((Bitstring.bitstring_length data)/8)):16; 0:16; 0:3; 0:13; 
    64:8; 6:8; ip_chk:16:littleendian; gw_ip:32; local_ip:32} in
  let tcp_chk = 
    (Checksum.ones_complement (BITSTRING{gw_ip:32; local_ip:32; 0:8; 6:8; 
        (20+((Bitstring.bitstring_length data)/8)):16; 
        src_port:16; dst_port:16; isn:32;  ack:32; 5:4; 0:6; 
        false:1; true:1; false:1; false:1; false:1; false:1; 0xffff:16;0:16; 0:16; 
        data:(Bitstring.bitstring_length data):bitstring})) in 
  let tcp_hdr = BITSTRING {src_port:16; dst_port:16; isn:32; ack:32; 5:4;
        0:6; false:1; true:1; false:1; false:1; false:1; false:1; 
        0xffff:16; tcp_chk:16:littleendian; 0:16;
        data:(Bitstring.bitstring_length data):bitstring} in  
    Bitstring.concat [eth_hdr; ipv4_hdr; tcp_hdr;] 
