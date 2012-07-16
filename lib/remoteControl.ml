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


open Lwt
open Printf
open Int64
open Re_str

let bind_fd ~port =
  let src = Unix.ADDR_INET (Unix.inet_addr_any, (to_int port)) in 
  let fd = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
  let () = Lwt_unix.bind fd src in
  return fd

let usage () = 
  return (Printf.printf "Invalid arg\n%!")

lwt _ =
  try 
    let server_ip = Sys.argv.(1) in 
    let remote_port = (int_of_string Sys.argv.(2)) in 
    let command = Sys.argv.(3) in 
    let fd = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
    let ipaddr = (Unix.gethostbyname server_ip).Unix.h_addr_list.(0) in
    let portaddr = Unix.ADDR_INET (ipaddr, remote_port) in
    lwt _ = Lwt_unix.sendto fd command 0 (String.length command) [] portaddr in
    return ()
  with _ -> 
    usage ()
