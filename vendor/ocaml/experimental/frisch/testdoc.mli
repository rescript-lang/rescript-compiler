[@@doc section "First section"]

module M : sig
  [@@doc section "Public definitions"]

  type t =
    | A
    | B

  [@@doc section "Internal definitions"]

  val zero: int
      [@@doc "A very important integer."]
end
  [@@doc "This is an internal module."]

val incr: int -> int
  [@@doc "This function returns the next integer."]

[@@doc section "Second section"]

val decr: int -> int
  [@@doc "This function returns the previous integer."]

val is_a: M.t -> bool
  [@@doc "This function checks whether its argument is the A constructor."]

module X: Hashtbl.HashedType
  [@@doc "An internal module"]
