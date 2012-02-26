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

let handle_rpc =
  let open Rpc in begin function
  | None ->
      eprintf "warning: bad rpc\n%!";
      return ()
  | Some data ->
      match data with
      | Request(command, arg_list, id) -> begin
          let args = String.concat ", " arg_list in
          eprintf "REQUEST: %s with args %s (ID: %Li)\n%!" command args id;
          return ()
      end
      | Notification(command, arg_list) -> begin
          let args = String.concat ", " arg_list in
          eprintf "NOTIFICATION: %s with args %s\n%!" command args;
          return ()
      end
      | _ -> begin
          eprintf "ERROR: Received an RPC that clients don't handle\n%!";
          return ()
      end
  end
