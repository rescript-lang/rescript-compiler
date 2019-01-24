(** Testing display of extensible variant types and exceptions.

   @test_types_display
 *)

(** Also check reference for {!M.A}, {!M.B}, {!M.C} and {!E} *)

(** Extensible type *)
type e = ..

module M = struct
  type e +=
  | A (** A doc *)
  | B (** B doc *)
  | C (** C doc *)
end

module type MT = sig
  type e +=
  | A (** A doc *)
  | B (** B doc *)
  | C (** C doc *)
end

exception E
