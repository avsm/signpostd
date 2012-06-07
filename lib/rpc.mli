(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012 Sebastian Probst Eide <sebastian.probst.eide@gmail.com>
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


exception Timeout
exception BadRpc of string


type tactic_name = string

type method_name = string

type action =
  | TEST
  | CONNECT
  | TEARDOWN

type command =
  | TacticCommand of tactic_name * action * method_name
  | Command of string

type arg = string
type id = int64

type result =
  | Result of string
  | Error of string

type t =
  | Request of command * arg list * id
  | Response of result * id
  | Notification of command * arg list


(* For encoding and decoding RPCs to JSON *)
val rpc_of_string : string -> (t option * int)
val rpc_to_string : t -> string

(* Convenience method for creating valid RPCs *)
val create_request : method_name -> arg list -> t
val create_tactic_request : tactic_name -> action -> method_name -> arg list -> t
val create_notification : method_name -> arg list -> t
val create_tactic_notification : tactic_name -> action -> method_name -> arg list -> t
val create_response_ok : string -> id -> t
val create_response_error : string -> id -> t
