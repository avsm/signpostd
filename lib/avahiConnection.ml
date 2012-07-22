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

module OP = Openflow.Packet
module OC = Openflow.Controller
module OE = OC.Event

exception Avahi_error

let name () = "avahi"

let connect a b =
  printf "[Avahi] Setting nat punch between host %s - %s\n%!" a b;

  (* register an opernflow hook for tcp connections destined to specific port *)
  try_lwt
    let rpc = (Rpc.create_tactic_request "avahi" 
      Rpc.CONNECT "service_discovery" []) in
    lwt res = (Nodes.send_blocking a rpc) in
    lwt res = (Nodes.send_blocking b rpc) in
       return (false)
  with exn -> 
    Printf.printf "[avahi] failed to enable tactic\n%!";
    return (false)

let test a b =
  return true

let enable a b =
  return false 

let handle_notification action method_name arg_list =
  match method_name with
    | "service" -> 
      (let node = List.nth arg_list 0 in 
       let nodes = Nodes.get_nodes ()  in
       let rpc = (Rpc.create_tactic_request "avahi" 
                    Rpc.CONNECT "service_advertise" arg_list) in
         List.iter (
           fun a -> 
             if (node <> a) then
               ignore_result((Nodes.send_blocking a rpc))
         ) nodes;
         return ()
      )
    | _ -> 
        (eprintf "[avahi] tactic doesn't handle notifications\n%!";
        return ())

(* ******************************************
 * A tactic to intercept and advertise avani messages
 * ******************************************)
let handle_request action method_name arg_list =
  let open Rpc in
    match action with
      | TEST ->
        (try_lwt 
          lwt ip = Avahi.Manager.test method_name arg_list in
            return(Sp.ResponseValue ip)
        with ex ->  
          return(Sp.ResponseError (Printexc.to_string ex)) )
      | CONNECT ->
          (try 
             lwt _ = Avahi.Manager.connect method_name arg_list in
               return(Sp.ResponseValue "")
           with e -> 
             return (Sp.ResponseError "ssh_connect"))
      | TEARDOWN ->
           eprintf "Avahi doesn't support teardown action\n%!";
             return(Sp.ResponseError "Avahi teardown is not supported yet")


