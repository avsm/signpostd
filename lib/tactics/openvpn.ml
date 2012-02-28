
module Manager = struct
  type conn = {
    ip: string;
    port: int;
    pid: int;

  }

  type conn_db = {
    conns : (int, conn) Hashtbl.t;
    mutable max_id : int;
  }

  let test node ip port =
    true

  let connect node ip port =
    let conn_id = conn_db.max_id + 1 in 
    conn_db.max_id <- conn_id;
    (true, conn_id, "success")

  let teardown node id =
    (* kill openvpn pid*)

    (* Destroy state *)
    if (Hashtbl.mem conn_db.conn id) then
      Hashtbl.remove conn_db.conn id
      true
end
