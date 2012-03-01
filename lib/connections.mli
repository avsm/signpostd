val store_addresses : Sp.name -> Sp.name -> (Sp.ip * Sp.ip) list -> unit
val set_public_ips : Sp.name -> Sp.ip list -> unit
val lookup : Sp.name -> Sp.name -> Sp.addressable list

val find : Sp.name -> Sp.name -> Sp.addressable list
