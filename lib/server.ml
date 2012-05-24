(*
 * Copyright (c) 2005-2012 Anil Madhavapeddy <anil@recoil.org>
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

open Dns
open Lwt 
open Printf
open Int64
open Sp_controller
open Key

(* The domain we are authoritative for *)
let our_domain =
  sprintf "d%d.%s" Config.signpost_number Config.domain

let our_domain_l =
  let d = "d" ^ (string_of_int Config.signpost_number) in
  [ d; Config.domain ]

(* Respond with an NXDomain if record doesnt exist *)
let nxdomain =
  return (Some { Dns.Query.rcode=`NXDomain; aa=false;
    answer=[]; authority=[]; additional=[] })

(* Ip address response for a node *)
let ip_resp ~dst ~src ~domain =
  let open Dns.Packet in
  lwt ip = Engine.find src dst in
    match ip with
      | (Sp.IPAddressInstance(ip)) -> (
          let answers = { rr_name=dst::src::domain;
                          rr_class=`IN; rr_ttl=0l;
                          rr_rdata=`A (Uri_IP.string_to_ipv4 ip);} in
            return ({ Dns.Query.rcode=`NoError; aa=true; answer=[answers]; 
              authority=[]; additional=[]; }) )
      | _ -> return ( { Dns.Query.rcode=`NXDomain; aa=false;
               answer=[]; authority=[]; additional=[] })

(* Figure out the response from a query packet and its question section *)
let get_response packet q =
  let open Dns.Packet in
  let module DQ = Dns.Query in
  (* Normalise the domain names to lower case *)
  let qnames = List.map String.lowercase q.q_name in
  eprintf "Q: %s\n%!" (String.concat " " qnames);
  let from_trie = Dns.(Query.answer_query q.q_name q.q_type Loader.(state.db.trie)) in
  match qnames with
    (* For this strawman, we assume a valid query has form
     * <dst node>.<src node>.<domain name>
     *)
  |dst::src::domain -> begin
     let domain'=String.concat "." domain in
     if domain' = our_domain then begin
       eprintf "src:%s dst:%s dom:%s\n%!" src dst domain';
       ip_resp ~dst ~src ~domain
     end else return(from_trie)
  end
  |_ -> return (from_trie)

let dnsfn ~src ~dst packet =
  let open Dns.Packet in
  match packet.questions with
  |[] -> eprintf "bad dns query: no questions\n%!"; return None
  |[q] -> lwt resp = get_response packet q in
    return (Some (resp))
  |_ -> eprintf "dns dns query: multiple questions\n%!"; return None

let load_dnskey_rr () = 
  let ret = ref "" in 
  let dir = (Unix.opendir (Config.conf_dir ^ "/authorized_keys/")) in
  let rec read_pub_key dir =  
  try 
    let file = Unix.readdir dir in
    if ( Re_str.string_match (Re_str.regexp ".*\\.pub") file 0) then (
      lwt dnskey_rr = dnskey_of_pem_pub_file 
      (Config.conf_dir ^ "/authorized_keys/" ^ file) in
      let hostname = (List.nth (Re_str.split (Re_str.regexp "\\.") file) 0) in 
      match dnskey_rr with
      | Some(value) -> 
        Printf.printf "file : %s \n %s \n%!" hostname (List.hd value);
        ret := (!ret) ^ "\n" ^ 
        (Printf.sprintf "%s IN %s\n" hostname (List.hd value));
        read_pub_key dir
      | None -> read_pub_key dir
     ) else (
       read_pub_key dir
     )
  with End_of_file -> 
    Unix.closedir dir;
    return (!ret)
  in 
  read_pub_key dir

let dns_t () =
  lwt fd, src = Dns_server.bind_fd ~address:"0.0.0.0" ~port:5354 in
  lwt dns_keys = load_dnskey_rr () in 
  lwt zone_keys = (dnskey_of_pem_priv_file 
    (Config.conf_dir ^ "/signpost.pem"))  in 
  let zsk = 
    match zone_keys with
    | None -> failwith "Cannot open signpost.pem private key"
    | Some(keys) -> List.hd keys
  in
  let zonebuf = sprintf "
$ORIGIN %s. ;
$TTL 0

@ IN SOA %s. hostmaster.%s. (
  2012011206      ; serial number YYMMDDNN
  28800           ; Refresh
  7200            ; Retry
  864000          ; Expire
  86400           ; Min TTL
)

@ A %s
i NS %s.
@ %s
%s" our_domain Config.external_ip our_domain Config.external_ip 
   Config.external_dns zsk dns_keys in
  eprintf "%s\n%!" zonebuf;
  Dns.Zone.load_zone [] zonebuf;
  Dns_server.listen ~fd ~src ~dnsfn


module IncomingSignalling = SignalHandler.Make (ServerSignalling)

let signal_t () =
  IncomingSignalling.thread ~address:"0.0.0.0" ~port:(of_int Config.signal_port)

let _ =
  let daemon_t = join [ dns_t (); signal_t ();
                        Sp_controller.listen () ] in
  Lwt_main.run daemon_t
