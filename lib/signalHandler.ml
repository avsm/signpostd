(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
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

(* Signalling UDP server that runs over Iodine *)
open Lwt
open Printf
open Int64


module type HandlerSig = sig
  val handle_request : Rpc.command -> Rpc.arg list -> Sp.request_response Lwt.t
  val handle_notification : Rpc.command -> Rpc.arg list -> unit Lwt.t
  val handle_rpc : Rpc.rpc option -> unit Lwt.t
end

module type Functor = sig
  val thread : address:Sp.ip -> port:Sp.port -> unit Lwt.t
end


module Make (Handler : HandlerSig) = struct
  let dispatch_rpc rpc = 
    let open Rpc in
    match rpc with 
    | Some(Response(r, e, id)) ->
        Nodes.wake_up_thread_with_reply id (Response(r, e, id))
    (* Only clients deal with Requests... so this code makes no sense from a
     * server perspective... refactor? *)
    | Some(Request(c, args, id)) -> begin
        lwt response = (Handler.handle_request c args) in
        match response with
        | Sp.ResponseValue v -> begin
            let resp = (Rpc.create_response_ok v id) in
            Nodes.send_to_server resp 
        end
        | Sp.ResponseError e -> begin
            let error = Rpc.create_response_error e id in
            Nodes.send_to_server error 
        end
        | Sp.NoResponse -> begin
            return ()
        end
    end
    | Some(Notification(c, args)) ->
        Handler.handle_notification c args
    | data -> Handler.handle_rpc data

  (* Listens on port Config.signal_port *)
  let bind_fd ~address ~port =
    lwt src = try_lwt
      let hent = Unix.gethostbyname address in
      return (Unix.ADDR_INET (hent.Unix.h_addr_list.(0), (to_int port)))
    with _ ->
      raise_lwt (Failure ("cannot resolve " ^ address))
    in
    let fd = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
    let () = Lwt_unix.bind fd src in
    return fd

  let sockaddr_to_string =
    function
    | Unix.ADDR_UNIX x -> sprintf "UNIX %s" x
    | Unix.ADDR_INET (a,p) -> sprintf "%s:%d" (Unix.string_of_inet_addr a) p

  let thread ~address ~port =
    (* Listen for UDP packets *)
    lwt fd = bind_fd ~address ~port in
    while_lwt true do
      let buf = String.create 4096 in
      lwt len, dst = Lwt_unix.recvfrom fd buf 0 (String.length buf) [] in
      let subbuf = String.sub buf 0 len in
      eprintf "udp recvfrom %s : %s\n%!" (sockaddr_to_string dst) subbuf;
      let rpc = Rpc.rpc_of_string subbuf in
      dispatch_rpc rpc
    done
end

module Client = Make (ClientSignalling)
