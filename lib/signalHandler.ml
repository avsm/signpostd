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

type sp_msg = {
  src_ip : int32;
  src_port : int;
  cmd : Rpc.t option;
}

module type HandlerSig = sig
  val handle_request : Lwt_unix.file_descr -> int32 -> Rpc.command -> 
    Rpc.arg list -> Sp.request_response Lwt.t
  val handle_notification : Lwt_unix.file_descr -> int32 -> Rpc.command -> 
    Rpc.arg list -> unit Lwt.t
end

module type Functor = sig
  val thread_client : unit Lwt.u -> unit Lwt.u -> address:Sp.ip -> 
    port:Sp.port -> unit Lwt.t
  val thread_server : address:Sp.ip -> port:Sp.port -> unit Lwt.t
end

module Make (Handler : HandlerSig) = struct
  let classify fd msg =
    let open Rpc in
    match msg.cmd with
    | Some (Request(c, args, id)) -> begin
        lwt response = (Handler.handle_request fd msg.src_ip c args) in
        match response with
        | Sp.ResponseValue v -> begin
            let resp = (create_response_ok v id) in
            Nodes.send_to_server resp 
        end
        | Sp.ResponseError e -> begin
            let error = Rpc.create_response_error e id in
            Nodes.send_to_server error 
        end
        | Sp.NoResponse -> return ()
    end
    | Some (Response(r, id)) ->
        Nodes.wake_up_thread_with_reply id (Response(r, id))
    | Some (Notification(c, args)) ->
        Handler.handle_notification fd msg.src_ip c args
    | _ -> Printf.eprintf "[signalHandler] failed to process req\n%!";
           return ()

  let dispatch_rpc fd msg = 
    match msg.cmd with 
      | Some _ -> classify fd msg
      | None -> 
          eprintf "signal handler cannot dispatch a 'None'-RPC\n%!";
          return ()

  (* Listens on port Config.signal_port *)
  let create_fd ~address ~port =
    let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
      (* so we can restart our server quickly *)
    return fd

  let bind_fd ~address ~port =
(*
    lwt src = try_lwt
      let hent = Unix.gethostbyname address in
      return (Unix.ADDR_INET (hent.Unix.h_addr_list.(0), (to_int port)))
    with _ ->
      raise_lwt (Failure ("cannot resolve " ^ address))
    in
    let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
      (* so we can restart our server quickly *)
      Lwt_unix.setsockopt fd Unix.SO_REUSEADDR true ;
 *)
    lwt fd = create_fd address port in 
    lwt src = try_lwt
      let hent = Unix.gethostbyname address in
      return (Unix.ADDR_INET (hent.Unix.h_addr_list.(0), (to_int port)))
    with _ ->
      raise_lwt (Failure ("cannot resolve " ^ address))
    in
    Lwt_unix.setsockopt fd Unix.SO_REUSEADDR true ;
    let () = Lwt_unix.bind fd src in
    let _ = Lwt_unix.listen fd 10 in 
      return fd


  let sockaddr_to_string =
    function
    | Unix.ADDR_UNIX x -> sprintf "UNIX %s" x
    | Unix.ADDR_INET (a,p) -> sprintf "%s:%d" (Unix.string_of_inet_addr a) p

  let process_channel sock dst =
    let data = ref "" in 
      while_lwt true do
        let buf = String.create 4096 in
        lwt len = Lwt_unix.recv sock buf 0 (String.length buf) [] in
        let subbuf = String.sub buf 0 len in
          data := !data ^ subbuf;
         eprintf "udp recvfrom %s : %s\n%!" (sockaddr_to_string dst) subbuf; 
        let (rpc, len) = Rpc.rpc_of_string !data in
          data := String.sub !data len ((String.length !data) - len);
        let msg = 
          match dst with 
            |  Unix.ADDR_UNIX _ -> {src_ip=0l;src_port=0; cmd=rpc;}
            | Unix.ADDR_INET (a,_) -> {
                src_ip=(Uri_IP.string_to_ipv4 (Unix.string_of_inet_addr a)); 
                src_port=0; cmd=rpc;}
        in 
        dispatch_rpc sock msg;
        return ()
      done

  let thread_server ~address ~port =
    (* Listen for UDP packets *)
    lwt fd = bind_fd ~address ~port in
    while_lwt true do 
      lwt (sock, dst) = Lwt_unix.accept fd in
      let _ = Lwt.ignore_result (process_channel sock dst) in 
        return ()
    done 

let thread_client wakener_connect wakener_end ~address ~port =
    (* Listen for UDP packets *)
Printf.printf "XXXXXXXXXXXXXXXX client tcp thread\n%!";
  lwt fd = create_fd ~address ~port in
    lwt src = try_lwt
      let hent = Unix.gethostbyname address in
      return (Unix.ADDR_INET (hent.Unix.h_addr_list.(0), (to_int port)))
    with _ ->
      raise_lwt (Failure ("cannot resolve " ^ address))
    in
    Printf.printf "XXXXXXXXXXXXX Connecting to %s:%Ld\n%!" address port;
      lwt _ = Lwt_unix.connect fd src in
      let _ = Nodes.set_server_signalling_channel fd in
      Lwt.wakeup wakener_connect ();
         Printf.printf "XXXXXXXXXXXXXXXX client tcp success\n%!";
      lwt _ = process_channel fd src in
         return (Lwt.wakeup wakener_end ())
end
