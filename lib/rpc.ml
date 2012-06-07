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


(* This module constructs and destructs RPCs following the JSON-RPC
 * specification (http://json-rpc.org/wiki/specification).
 * Tactic specific RPCs are layered on top of JSON-RPC through
 * making the method name follow this pattern:
 *
 *    Tactic#Action#Method
 *
 * Where:
 * - tactic: is the name of the tactic being invoked
 * - action: is one of test, connect or teardown
 * - method: is the piece of functionality being invoked
 *)


open Int64
open Printf


exception Timeout
exception BadRpc of string


type tactic_name = string

type method_name = string

type action =
  | TEST
  | CONNECT
  | TEARDOWN

type command =
  | TacticCommand of tactic_name * action * string
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


(**********************************************************************
 * Utils **************************************************************)

let id_count = ref 0L

let rpc_id () =
	id_count := Int64.add 1L !id_count;
  !id_count

let string_of_action = function
  | TEST -> "test"
  | CONNECT -> "connect"
  | TEARDOWN -> "teardown"

let action_of_string = function
  | "test" -> TEST
  | "connect" -> CONNECT
  | "teardown" -> TEARDOWN
  | act ->  begin
      let msg = Printf.sprintf "Invalid action %s" act in 
      raise (BadRpc msg)
  end

(**********************************************************************
 * Decode JSON-RPCs ***************************************************)

let get_entry_of_name entries name =
  let find_fun = function
    | (n, entry) -> n=name
    | _ -> false in
  try 
    let (_, entry) = List.find find_fun entries in
    Some(entry)
  with Not_found -> 
    None

let construct_command command =
  let open Re_str in
  let rxp = regexp "#" in
  match split rxp command with
  | c :: [] -> Command c
  | tactic :: action :: c :: [] -> TacticCommand(tactic, (action_of_string action), c)
  | _ -> raise (BadRpc "Invalid method structure")

let construct_args string_args =
  let map_fn = function
    | Json.String s -> s
    | _ -> raise (BadRpc "Invalid non-string argument") in
  List.map map_fn string_args

let construct_request_rpc command args id =
  let c = construct_command command in
  let a = construct_args args in
  Request(c, a, id)

let construct_response_rpc result id =
  Response(result, id)

let construct_notification_rpc command args =
  let c = construct_command command in
  let a = construct_args args in
  Notification(c, a)

let check_for_valid_request id command entries =
  match get_entry_of_name entries "params" with
  | Some(Json.Array args) -> construct_request_rpc command args id
  | Some(_thing_else) -> 
      raise (BadRpc "Failed parsing request. Received malformed parameters")
  | None -> 
      raise (BadRpc "Failed parsing as request. Did not contain any parameters")

let check_for_valid_response id entries =
  let open Json in
  let result = get_entry_of_name entries "result" in
  let error = get_entry_of_name entries "error" in
  match (result, error) with
  | (Some(String result), Some(Null)) -> (construct_response_rpc (Result result) id)
  | (Some(Null), Some(String error)) -> (construct_response_rpc (Error error) id)
  | (Some(_thing_else), _) -> 
      raise (BadRpc "Failed parsing response. Malformed result")
  | (_, Some(_thing_else)) -> 
      raise (BadRpc "Failed parsing response. Malformed error")
  | (None, None) -> 
      raise (BadRpc "Failed parsing as response. Neither the result or error fields are present.")

let check_for_valid_notification command entries =
  let args = get_entry_of_name entries "params" in
  match args with
  | Some(Json.Array args) -> construct_notification_rpc command args
  | Some(_thing_else) -> 
      raise (BadRpc "Failed at parsing as notification. Malformed arguments")
  | None -> raise (BadRpc "Failed parsing as notification. Missing arguments")

let rpc_classifier entries =
  let open Json in
  let param_id = get_entry_of_name entries "id" in
  let param_method = get_entry_of_name entries "method" in
  match (param_id, param_method) with 
  | (Some(Int id), Some(String command)) -> check_for_valid_request id command entries
  | (Some(Int id), None) -> check_for_valid_response id entries
  | (Some(Null), Some(String command)) -> check_for_valid_notification command entries
  | _ -> raise (BadRpc "Failed highlevel JSON-RPC parsing test")

let rpc_of_json = 
  function
  | Json.Object entries ->
      try (Some(rpc_classifier entries))
      with (BadRpc error) -> begin
        eprintf "Rpc parsing failed with error %s\n%!" error;
        None
      end
  | _ -> begin
    eprintf "Rpc parsing failed. The message did not contain a top level object\n%!";
    None
  end

let rpc_of_string s =
  let json = try Some (Json.of_string s) with _ -> None in
  match json with
  | None -> (None, 0)
  | Some (x, i)-> (rpc_of_json x, i)

(**********************************************************************
 * Encode JSON-RPCs ***************************************************)

let args_to_json args =
  Json.Array (List.map (fun a -> Json.String a) args)

let command_to_json =
  let open Json in
  function
  | TacticCommand(tactic_name, action, command) -> 
        String (sprintf "%s#%s#%s" tactic_name (string_of_action action) command)
  | Command c -> String c

let result_to_json =
  let open Json in
  function
  | Result r -> [("result", String r);("error", Null)]
  | Error e -> [("result", Null);("error", String e)]

let rpc_to_json rpc =
  let open Json in
  let object_data = match rpc with
    | Request (c, args, id) -> 
       [("method", command_to_json c);
        ("params", args_to_json args);
        ("id", Int id)]
    | Notification (c, args) -> 
       [("method", command_to_json c);
        ("params", args_to_json args);
        ("id", Null)]
    | Response (r, id) -> 
        ("id", Int id) :: (result_to_json r) in
  Object object_data

let rpc_to_string rpc =
  Json.to_string (rpc_to_json rpc)

(**********************************************************************
 * Helper methods to create valid JSON-RPCs ***************************)

(* REQUESTS *)
let create_request method_name args =
  let id = rpc_id () in
  Request(Command(method_name), args, id)

let create_tactic_request tactic action method_name args =
  let id = rpc_id () in
  Request(TacticCommand(tactic, action, method_name), args, id)

(* NOTIFICATION *)
let create_notification method_name args =
  Notification(Command(method_name), args)

let create_tactic_notification tactic action method_name args =
  Notification(TacticCommand(tactic, action, method_name), args)

(* RESPONSE *)
let create_response_ok result id =
  Response (Result result, id)

let create_response_error error id =
  Response (Error error, id)
