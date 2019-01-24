(* Adapted from: An Expressive Language of Signatures
   by Norman Ramsey, Kathleen Fisher and Paul Govereau *)

module type VALUE = sig
  type value (* a Lua value *)
  type state (* the state of a Lua interpreter *)
  type usert (* a user-defined value *)
end;;

module type CORE0 = sig
  module V : VALUE
  val setglobal : V.state -> string -> V.value -> unit
  (* five more functions common to core and evaluator *)
end;;

module type CORE = sig
  include CORE0
  val apply : V.value -> V.state -> V.value list -> V.value
  (* apply function f in state s to list of args *)
end;;

module type AST = sig
  module Value : VALUE
  type chunk
  type program
  val get_value : chunk -> Value.value
end;;

module type EVALUATOR = sig
  module Value : VALUE
  module Ast : (AST with module Value := Value)
  type state = Value.state
  type value = Value.value
  exception Error of string
  val compile : Ast.program -> string
  include CORE0 with module V := Value
end;;

module type PARSER = sig
  type chunk
  val parse : string -> chunk
end;;

module type INTERP = sig
  include EVALUATOR
  module Parser : PARSER with type chunk = Ast.chunk
  val dostring : state -> string -> value list
  val mk : unit -> state
end;;

module type USERTYPE = sig
  type t
  val eq : t -> t -> bool
  val to_string : t -> string
end;;

module type TYPEVIEW = sig
  type combined
  type t
  val map : (combined -> t) * (t -> combined)
end;;

module type COMBINED_COMMON = sig
  module T : sig type t end
  module TV1 : TYPEVIEW with type combined := T.t
  module TV2 : TYPEVIEW with type combined := T.t
end;;

module type COMBINED_TYPE = sig
  module T : USERTYPE
  include COMBINED_COMMON with module T := T
end;;

module type BARECODE = sig
  type state
  val init : state -> unit
end;;

module USERCODE(X : TYPEVIEW) = struct
  module type F =
      functor (C : CORE with type V.usert = X.combined) ->
        BARECODE with type state := C.V.state
end;;

module Weapon = struct type t end;;

module type WEAPON_LIB = sig
  type t = Weapon.t
  module T : USERTYPE with type t = t
  module Make :
    functor (TV : TYPEVIEW with type t = t) -> USERCODE(TV).F
end;;

module type X = functor (X: CORE) -> BARECODE;;
module type X = functor (_: CORE) -> BARECODE;;
