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
open Rpc


exception Tactic_error of string


let execute_tactic cmd arg_list =
  let open Lwt_process in
  let command = Unix.getcwd () ^ "/client_tactics/" ^ cmd in
  let args = String.concat " " arg_list in
  let full_command = command ^ " " ^ args in
  eprintf "Executing RPC '%s'.\n%!" full_command;
  let cmd = shell full_command in
  pread ~timeout:120.0 cmd >>= fun value ->
  return (Sp.ResponseValue value)

let handle_request command arg_list =
  match command with
  | Command(command_name) -> 
      eprintf "REQUEST %s with args %s\n%!" 
          command_name (String.concat ", " arg_list);
      execute_tactic command_name arg_list
  | TacticCommand(tactic_name, action, method_name) ->
      match Engine.tactic_by_name tactic_name with
      | Some(t) ->
          eprintf "REQUEST for %s with args %s\n%!" 
              tactic_name (String.concat ", " arg_list);
          let module Tactic = (val t : Sp.TacticSig) in
          Tactic.handle_request action method_name arg_list
      | None ->
          eprintf "Client doesn't know how to handle requests for %s\n%!"
              tactic_name;
          return Sp.NoResponse

let handle_notification command arg_list =
  match command with
  | Command(command_name) -> 
      eprintf "NOTIFICATION: %s with args %s\n%!" 
          command_name (String.concat ", " arg_list);
      lwt _ = execute_tactic command_name arg_list in
      return ()
  | TacticCommand(tactic_name, action, method_name) ->
      match Engine.tactic_by_name tactic_name with
      | Some(t) ->
          eprintf "NOTIFICATION for %s with args %s\n%!" 
              tactic_name (String.concat ", " arg_list);
          let module Tactic = (val t : Sp.TacticSig) in
          Tactic.handle_notification action method_name arg_list
      | None ->
          eprintf "Client doesn't know how to handle requests for %s\n%!"
              tactic_name;
          return ()
