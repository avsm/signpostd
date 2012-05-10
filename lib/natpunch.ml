(*
 * Copyright (c) 2012 Charalampos Rotsos <cr409@cl.cam.ac.uk>
 *
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

module Manager = struct
  exception NatpunchError of string
  exception MissingNatpunchArgumentError

  (*********************************************************
   *                  Tactic state
   *********************************************************)
  
  
  (**********************************************************
   *                  Init methods
   **********************************************************)

  let init_module () = 
    return ()

  let destroy_module () = 
    init_module ()

(*
 * Connection Methods
 * *)
  let connect kind args =
    match kind with
    | "client" -> 
      try_lwt
        let ips = List.nth args 0 in 
          return ("127.0.0.1")
      with  exn ->
        printf "[natpunch] error %s\n%!" (Printexc.to_string exn);
        return ("127.0.0.1")
        (*         raise (NatpunchError ((Printexc.to_string exn)))  *)
(*     | _ -> raise (NatpunchError((sprintf "unsupported %s" kind))) *)

  (*********************************************************
   *       Testing methods
   *********************************************************)
  let connect_client ip port = 
    let client_sock = socket PF_INET SOCK_STREAM 0 in
    let hentry = Unix.inet_addr_of_string ip in
    lwt _ = Lwt_unix.connect client_sock (ADDR_INET (hentry, port)) in 
      printf "[natpanch] client connected\n%!";
      let ADDR_INET(loc_ip,loc_port) = 
        Lwt_unix.getsockname client_sock in
      let pkt_bitstring = BITSTRING {
        (Uri_IP.string_to_ipv4 (Unix.string_of_inet_addr loc_ip)):32;
        loc_port:16;(String.length (Nodes.get_local_name ())):16;
        (Nodes.get_local_name ()):-1:string} in 
      let pkt = Bitstring.string_of_bitstring pkt_bitstring in 
      lwt _ = Lwt_unix.send client_sock pkt 0 (String.length pkt) [] in 
      return (Lwt_unix.shutdown client_sock SHUTDOWN_ALL)


  let test kind args =
    match kind with 
    | "client_connect" -> (
      try_lwt
        let port = int_of_string (List.hd args) in
        lwt _ = connect_client Config.external_ip port in 
          return ("0.0.0.0")
      with exn -> 
        printf "[natpunch] error %s\n%!" (Printexc.to_string exn);
        raise (NatpunchError(Printexc.to_string exn)))
    | "server_connect" -> 
      return ("0.0.0.0")
    | _ ->
      raise (NatpunchError((sprintf "Invalid match %s" kind)) )

  (*************************************************************************
   *             TEARDOWN methods of tactic
   * ***********************************************************************)

  let teardown _ =
    true

  let pkt_in_cb _ _ _ = 
    return ()

end
