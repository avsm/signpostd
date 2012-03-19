(* register a callback for a specific flow match *)
val register_pkt_in_cb : Ofpacket.Match.t -> (Controller.state -> Ofpacket.datapath_id -> (Ofpacket.Port.t * int32 *
Bitstring.t * Ofpacket.datapath_id) -> unit Lwt.t) -> unit

(* setup a listening openflow controller *)
val listen : ?port:int -> unit -> unit Lwt.t
