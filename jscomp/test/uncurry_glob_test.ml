

let v = Caml_utils.repeat (100, "x") [@uncurry]

module M ( U : sig val f : int * string -> string [@uncurry] end ) = struct
  let v = U.f (100, "x") [@uncurry]
end
