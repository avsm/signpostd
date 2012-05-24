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


exception Client_error of string
exception InvalidState of string


type name = string
type ip = string
type port = int64
type srv = SRV of ip * port

type addressable =
  | IPAddressInstance of ip
  | SRVInstance of srv
  | Unreachable

type signalling_channel =
  | SignallingChannel of ip * port
  | NoSignallingChannel

type request_response =
  | ResponseValue of string
  | ResponseError of string
  | NoResponse

module type TacticSig = sig
  val name : unit -> Rpc.tactic_name
  (* val provides : unit -> channel_property list *)
  val connect : name -> name -> unit Lwt.t
  val handle_request : Rpc.action -> Rpc.method_name -> Rpc.arg list ->
    request_response Lwt.t
  val handle_notification : Rpc.action -> Rpc.method_name -> Rpc.arg list ->
    unit Lwt.t
end


val iprecord : ip -> addressable
val srvrecord : ip -> port -> addressable
