module M () : sig
  type t

  val v : t
end = struct
  type t = int

  let v = 3
end

module V = M ()
