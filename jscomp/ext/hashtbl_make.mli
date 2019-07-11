module Make (Key : Hashtbl.HashedType) : Hashtbl_gen.S with type key = Key.t
