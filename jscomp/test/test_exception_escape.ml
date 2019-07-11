module N : sig
  val f : int
end = struct
  exception A of int

  let f = try raise (A 3) with _ -> 3
end
