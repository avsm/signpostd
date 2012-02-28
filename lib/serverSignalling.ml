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

let handle_rpc =
  let open Rpc in function
  | None ->
      eprintf "warning: bad rpc\n%!";
      return ()
  | Some(Hello(node, ip, port, local_ips)) -> begin
      eprintf "rpc: hello %s -> %s:%Li\n%!" node ip port;
      Nodes.set_signalling_channel node ip port;
      Nodes.set_local_ips node local_ips;
      Nodes.check_if_the_ips_are_publicly_accessible node local_ips;
      return ()
  end
  | _ -> 
      eprintf "ERROR: Received an RPC that the server can't handle\n%!";
      return ()

let handle_request command args =
  eprintf "The server received a REQUEST RPC, but doesn't handle those.\n%!";
  return Sp.NoResponse

let handle_notification command args =
  eprintf "The server received a NOTIFCATION RPC, but doesn't handle those.\n%!";
  return ()
