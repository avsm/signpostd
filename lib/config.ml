(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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


let user = "sebastian"
let signpost_number = 2
let domain = "signpo.st"
let ip_slash_24 = "172.16.11."
(* This is the ip of the local test machine *)
let external_ip = "192.168.56.101"
let external_dns = "ec2-107-20-47-111.compute-1.amazonaws.com"
let dir = "/home/cr409/scratch/code/signpostd/"
let conf_dir = "/root/signpostd/conf/"

let iodine_node_ip = "172.16.11.1"
let ns_server="8.8.8.8"
(* for testing *)

let signal_port = 3456

(* RPCs timeout after 5 minutes *)
let rpc_timeout = 5 * 6
