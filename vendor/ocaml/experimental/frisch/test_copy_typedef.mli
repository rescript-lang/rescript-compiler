module type S = sig
  type t
  val x: int
end

module type T = sig
  type t

  module type M = sig
    type t = A | B of t
  end
end

module M : sig
  type t =
    | A
    | B of string
end

type t = int list
