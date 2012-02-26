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
  | Some data -> begin
      match data with
      | Hello (node,ip, port) ->
          eprintf "rpc: hello %s -> %s:%Li\n%!" node ip port;
          Nodes.update_sig_channel node ip port;
          return ()
      | Response(Result r, _, id) -> begin
          eprintf "Response OK [%Li]: %s\n%!" id r;
          return ()
      end
      | Response(_, Error e, id) -> begin
          eprintf "Response ERROR [%Li]: %s\n%!" id e;
          return ()
      end
      | _ -> begin
          eprintf "ERROR: Received an RPC that clients don't handle\n%!";
          return ()
      end
  end
