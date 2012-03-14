val connect : Sp.name -> Sp.name -> unit Lwt.t
val tactic_by_name : Rpc.tactic_name -> (module Sp.TacticSig) option
