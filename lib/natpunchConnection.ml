(*
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

open Lwt
open Lwt_unix
open Lwt_list
open Printf

exception Nat_error

let name () = "nat"

let connect a b =
  return ()

(* ******************************************
 * A tactic to setup a layer 2 ssh tunnel
 * ******************************************)

let handle_request action method_name arg_list =
  let open Rpc in
    match action with
      | TEST ->
        (try_lwt 
          lwt ip = Natpunch.Manager.test method_name arg_list in
            return(Sp.ResponseValue ip)
        with ex ->  
          return(Sp.ResponseError (Printexc.to_string ex)) )
      | CONNECT ->
          (try 
             lwt ip = Natpunch.Manager.connect method_name arg_list in
               return(Sp.ResponseValue ip)            
           with e -> 
             return (Sp.ResponseError "ssh_connect"))
      | TEARDOWN ->
           eprintf "Ssh doesn't support teardown action\n%!";
             return(Sp.ResponseError "Ssh teardown is not supported yet")

let handle_notification action method_name arg_list =
  eprintf "Ssh tactic doesn't handle notifications\n%!";
  return ()
