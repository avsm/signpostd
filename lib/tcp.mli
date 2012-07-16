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

type tcp_flags_struct = {
  urg:bool; ack: bool; 
  psh:bool; rst:bool; 
  syn:bool; fin:bool;}

val get_tcp_flags : Bitstring.t -> tcp_flags_struct
val get_tcp_sn : Bitstring.t -> int32
val get_tcp_packet_payload : Bitstring.t -> Bitstring.t
val gen_server_syn : Bitstring.t -> int32 -> string -> 
  string -> int32 -> int32 -> int -> Bitstring.t  
val gen_tcp_syn : int32 -> string -> 
  string -> int32 -> int32 -> int -> int ->  int -> Bitstring.t
val gen_server_ack : int32 -> int32 -> string -> 
  string -> int32 -> int32 -> int -> int ->  int -> Bitstring.t
val gen_server_synack : int32 -> int32 -> string -> 
  string -> int32 -> int32 -> int -> int -> Bitstring.t

val gen_tcp_data_pkt : int32 -> int32 -> string -> 
  string -> int32 -> int32 -> int -> int -> Bitstring.t -> 
  Bitstring.t
val gen_udp_pkt : string -> string -> int32 -> int32 -> 
int -> int -> Bitstring.t -> Bitstring.t
