(** Testing display of types.

   @test_types_display
 *)

let x = 1


module M = struct
  let y = 2

end

module type MT = sig
  type t = string -> int -> string -> (string * string * string) ->
    (string * string * string) ->
      (string * string * string) -> unit
  val y : int

  type obj_type =
     < foo : int ; bar : float -> string ; gee : int -> (int * string) >
end
