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

let config_json =
  let open Json in
  Object [
    ("user", String Config.user);
    ("signpost_number", Int (Int64.of_int Config.signpost_number));
    ("domain", String Config.domain);
    ("external_ip", String Config.external_ip);
    ("external_dns", String Config.external_dns)
  ]

let config_datagram =
  let open Json in
  to_string(Object [
    "response", (Object [
      ("result", config_json);
      ("error", Null);
      ("id", Int (Int64.of_int 1))
  ])
    ])

let handle_config_discovery = function
  | ip :: port_str :: [] -> 
    let port = Int64.of_int (int_of_string port_str) in
    Printf.printf "Sending reply to %s:%Li\n%!" ip port;
    let addr = Nodes.addr_from ip port in
    Nodes.send_datagram config_datagram addr >>= fun len ->
      return ()
  | _ -> 
    eprintf "Handle_config_discovery failed because of invalid RPC\n%!";
    return ()

let handle_hello fd src_ip args =
  let node :: ip :: str_port :: local_ips = args in
  let port = Int64.of_int (int_of_string str_port) in
  eprintf "rpc: hello %s -> %s:%Li\n%!" node ip port;
  Nodes.set_signalling_channel node fd;
  Nodes.set_local_ips node ([(Uri_IP.ipv4_to_string src_ip)] @ local_ips);
(*   eprintf "About to check for publicly accesible ips\n%!"; *)
  let rpc = Rpc.create_notification "setup_sp_ip" 
              [(Uri_IP.ipv4_to_string (Nodes.get_sp_ip node))] in 
  lwt _ = Nodes.send node rpc in 
  Nodes.check_for_publicly_accessible_ips node local_ips >>= fun public_ips -> 
(*     eprintf "Got public ips... store them\n%!"; *)
(*     Connections.set_public_ips node public_ips; *)
    return ()

let handle_request fd src_ip command arg_list =
  match command with
  | Command("config_discovery") ->
    handle_config_discovery arg_list >> 
    return Sp.NoResponse
  | Command(command_name) -> 
    eprintf "ERROR: Received a REQUEST RPC that the server can't handle
    (%s)\n%!" command_name;
    return Sp.NoResponse
  | TacticCommand(tactic_name, action, method_name) ->
    match Engine.tactic_by_name tactic_name with
    | Some(t) ->
      eprintf "REQUEST for %s with args %s\n%!" 
      tactic_name (String.concat ", " arg_list);
      let module Tactic = (val t : Sp.TacticSig) in
      Tactic.handle_request action method_name arg_list
    | None ->
      eprintf "Server doesn't know how to handle requests for %s\n%!"
      tactic_name;
      return Sp.NoResponse

let handle_notification fd ip command arg_list =
  match command with
  | Command("hello") -> 
    eprintf "HELLO with args %s\n%!" 
    (String.concat ", " arg_list);
    handle_hello fd ip arg_list
  | Command("exec_tactic") -> 
    begin 
      try
        let tactic = List.nth arg_list 0 in
        let src = List.nth arg_list 1 in 
        let dst = List.nth arg_list 2 in 
        Printf.eprintf "Execute tactic %s %s->%s\n%!" tactic src dst;
        lwt _ = Engine.connect_using_tactic tactic src dst in 
          return ()
    with Failure( str) ->
      Printf.eprintf "Insufficient number of arguments to execute tactic:
        %s\n%!" str;
        return ()
    end
  | Command(value)  ->
    Printf.eprintf "Invalid command %s\n%!" value;
    return ()
  | TacticCommand(tactic_name, action, method_name) ->
    match Engine.tactic_by_name tactic_name with
    | Some(t) ->
      eprintf "NOTIFICATION for %s with args %s\n%!" 
      tactic_name (String.concat ", " arg_list);
      let module Tactic = (val t : Sp.TacticSig) in
      Tactic.handle_notification action method_name arg_list
    | None ->
      eprintf "Server doesn't know how to handle requests for %s\n%!"
      tactic_name;
      return ()
