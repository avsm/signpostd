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

let name () = "natpanch"

type natpanch_state_type = {
  listener : unit t;
}

let nat_socket = 11000

let netpanch_daemon = 
  printf "[netpanch] Starting intermediate socket..\n%!";
  let server_sock = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
  (* so we can restart our server quickly *)
  Lwt_unix.setsockopt server_sock SO_REUSEADDR true ;

  (* build up my socket address *)
  bind server_sock (ADDR_INET (Unix.inet_addr_any, nat_socket)) ;

  (* Listen on the socket. Max of 10 incoming connections. *)
  listen server_sock 10 ;

  (* accept and process connections *)
  while_lwt true do
    lwt (client_sock, client_addr) = accept server_sock in
    let _ = 
      match client_addr with 
      | Unix.ADDR_INET(ip, port) -> 
          printf "[natpanch] client connected %s:%d\n%!"
          (Unix.string_of_inet_addr ip) port ;
      | _ -> 
          printf "[natpanch] invalid host"
    in
(*     let x = send client_sock str 0 len [] in *)
    return (Lwt_unix.shutdown client_sock SHUTDOWN_ALL)
  done

(*
let test_state = 
  Lwt.choose [(netpanch_daemon ())]
*)

let connect a b =
  printf "[natpunch] Setting nat punch between host %s - %s\n%!" a b;
  let rpc = (Rpc.create_tactic_request "natpanch" 
    Rpc.TEST "client_connect" [(string_of_int nat_socket)]) in
  lwt res = (Nodes.send_blocking a rpc) in
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
