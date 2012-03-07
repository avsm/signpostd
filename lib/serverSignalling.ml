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

let handle_config_discovery ip port_str =
  let port = Int64.of_int (int_of_string port_str) in
  Printf.printf "Sending reply to %s:%Li\n%!" ip port;
  let addr = Nodes.addr_from ip port in
  Nodes.send_datagram config_datagram addr >>= fun len ->
  return ()

let handle_rpc =
  let open Rpc in function
  | None ->
      eprintf "warning: bad rpc\n%!";
      return ()
  | Some(Hello(node, ip, port, local_ips)) -> begin
      eprintf "rpc: hello %s -> %s:%Li\n%!" node ip port;
      Nodes.set_signalling_channel node ip port;
      Nodes.set_local_ips node local_ips;
      eprintf "About to check for publicly accesible ips\n%!";
      Nodes.check_for_publicly_accessible_ips node local_ips >>= fun public_ips -> 
      eprintf "Got public ips... store them\n%!";
      Connections.set_public_ips node public_ips;
      return ()
  end
  | _ -> 
      eprintf "ERROR: Received an RPC that the server can't handle\n%!";
      return ()

let handle_request command args = match command with
  | "config_discovery" -> begin
    let ip :: port :: _ = args in
    (handle_config_discovery ip port) >> 
    return Sp.NoResponse
  end
  | _ -> begin
    eprintf "The server received a REQUEST RPC, but doesn't handle those.\n%!";
    return Sp.NoResponse
  end

let handle_tactic_request tactic action args =
  eprintf "The server received a REQUEST RPC, but doesn't handle those.\n%!";
  return Sp.NoResponse

let handle_notification command args =
  eprintf "The server received a NOTIFCATION RPC, but doesn't handle those.\n%!";
  return ()
