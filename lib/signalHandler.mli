module type HandlerSig = 
  sig
    val handle_request : Rpc.command -> Rpc.arg list -> Sp.request_response Lwt.t
    val handle_notification : Rpc.command -> Rpc.arg list -> unit Lwt.t
    val handle_rpc : Rpc.rpc option -> unit Lwt.t
  end

module type Functor = sig
  val thread : address:Sp.ip -> port:Sp.port -> unit Lwt.t
end

module Make (Handler : HandlerSig) : Functor 
