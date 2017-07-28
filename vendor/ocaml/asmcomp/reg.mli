(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Pseudo-registers *)

module Raw_name : sig
  type t
  val create_from_ident : Ident.t -> t
end

type t =
  { mutable raw_name: Raw_name.t;       (* Name *)
    stamp: int;                         (* Unique stamp *)
    typ: Cmm.machtype_component;        (* Type of contents *)
    mutable loc: location;              (* Actual location *)
    mutable spill: bool;                (* "true" to force stack allocation  *)
    mutable part: int option;           (* Zero-based index of part of value *)
    mutable interf: t list;             (* Other regs live simultaneously *)
    mutable prefer: (t * int) list;     (* Preferences for other regs *)
    mutable degree: int;                (* Number of other regs live sim. *)
    mutable spill_cost: int;            (* Estimate of spilling cost *)
    mutable visited: bool }             (* For graph walks *)

and location =
    Unknown
  | Reg of int
  | Stack of stack_location

and stack_location =
    Local of int
  | Incoming of int
  | Outgoing of int

val dummy: t
val create: Cmm.machtype_component -> t
val createv: Cmm.machtype -> t array
val createv_like: t array -> t array
val clone: t -> t
val at_location: Cmm.machtype_component -> location -> t

val anonymous : t -> bool

(* Name for printing *)
val name : t -> string

module Set: Set.S with type elt = t
module Map: Map.S with type key = t

val add_set_array: Set.t -> t array -> Set.t
val diff_set_array: Set.t -> t array -> Set.t
val inter_set_array: Set.t -> t array -> Set.t
val disjoint_set_array: Set.t -> t array -> bool
val set_of_array: t array -> Set.t

val reset: unit -> unit
val all_registers: unit -> t list
val num_registers: unit -> int
val reinit: unit -> unit
