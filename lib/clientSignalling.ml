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
open Int64

let execute_tactic cmd arg_list =
  let open Lwt_process in
  let command = Unix.getcwd () ^ "/client_tactics/" ^ cmd in
  let args = String.concat " " arg_list in
  let full_command = command ^ " " ^ args in
  eprintf "Executing RPC '%s'.\n%!" full_command;
  let cmd = shell full_command in
  pread ~timeout:10.0 cmd >>= fun value ->
  return value

let sa = (Config.iodine_node_ip, (of_int Config.signal_port))

let handle_rpc rpc =
  eprintf "ERROR: Client doesn't handle arbitrary RPCs\n%!";
  return ()

let handle_request command arg_list =
  let args = String.concat ", " arg_list in
  eprintf "REQUEST: %s with args %s\n%!" command args;
  execute_tactic command arg_list >>= fun value ->
  return (Sp.ResponseValue value)

let handle_notification command arg_list =
  let args = String.concat ", " arg_list in
  eprintf "NOTIFICATION: %s with args %s\n%!" command args;
  execute_tactic command arg_list >>= fun _ ->
  return ()


exception Tactic_error of string

let handle_tactic_request tactic act args =
    try 
    match tactic with
    | "openvpn" ->(
        match act with 
            | Rpc.TEST -> 
                    Printf.printf "Openvpn run action found\n%!";
                    lwt v = (Openvpn.Manager.test args) in
                        return(Sp.ResponseValue v)
            | _ -> raise (Tactic_error((Printf.sprintf "not implemented %s %s"
            tactic (Rpc.string_of_action act))))
            )
    | _ -> raise (Tactic_error((Printf.sprintf "not implemented %s %s" tactic
        (Rpc.string_of_action act))))
    with exn ->
        Printf.printf "Exception captured in client handler %s\n%!"
         (Printexc.to_string exn) ;
        return(Sp.ResponseError("Exception captured in client handler"))
