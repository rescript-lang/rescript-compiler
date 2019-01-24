(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

type environment

val env_add : Ident.t -> Reg.t array -> environment -> environment

val env_find : Ident.t -> environment -> Reg.t array

val size_expr : environment -> Cmm.expression -> int

module Effect : sig
  type t =
    | None
    | Raise
    | Arbitrary
end

module Coeffect : sig
  type t =
    | None
    | Read_mutable
    | Arbitrary
end

module Effect_and_coeffect : sig
  type t

  val none : t
  val arbitrary : t

  val effect : t -> Effect.t
  val coeffect : t -> Coeffect.t

  val effect_only : Effect.t -> t
  val coeffect_only : Coeffect.t -> t

  val join : t -> t -> t
  val join_list_map : 'a list -> ('a -> t) -> t
end

class virtual selector_generic : object
  (* The following methods must or can be overridden by the processor
     description *)
  method virtual is_immediate : int -> bool
    (* Must be defined to indicate whether a constant is a suitable
       immediate operand to arithmetic instructions *)
  method virtual select_addressing :
    Cmm.memory_chunk -> Cmm.expression -> Arch.addressing_mode * Cmm.expression
    (* Must be defined to select addressing modes *)
  method is_simple_expr: Cmm.expression -> bool
  method effects_of : Cmm.expression -> Effect_and_coeffect.t
    (* Can be overridden to reflect special extcalls known to be pure *)
  method select_operation :
    Cmm.operation ->
    Cmm.expression list ->
    Debuginfo.t ->
    Mach.operation * Cmm.expression list
    (* Can be overridden to deal with special arithmetic instructions *)
  method select_condition : Cmm.expression -> Mach.test * Cmm.expression
    (* Can be overridden to deal with special test instructions *)
  method select_store :
    bool -> Arch.addressing_mode -> Cmm.expression ->
                                         Mach.operation * Cmm.expression
    (* Can be overridden to deal with special store constant instructions *)
  method regs_for : Cmm.machtype -> Reg.t array
    (* Return an array of fresh registers of the given type.
       Default implementation is like Reg.createv.
       Can be overridden if float values are stored as pairs of
       integer registers. *)
  method insert_op :
    Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array
    (* Can be overridden to deal with 2-address instructions
       or instructions with hardwired input/output registers *)
  method insert_op_debug :
    Mach.operation -> Debuginfo.t -> Reg.t array -> Reg.t array -> Reg.t array
    (* Can be overridden to deal with 2-address instructions
       or instructions with hardwired input/output registers *)
  method emit_extcall_args :
    environment -> Cmm.expression list -> Reg.t array * int
    (* Can be overridden to deal with stack-based calling conventions *)
  method emit_stores :
    environment -> Cmm.expression list -> Reg.t array -> unit
    (* Fill a freshly allocated block.  Can be overridden for architectures
       that do not provide Arch.offset_addressing. *)

  method mark_call : unit
  (* informs the code emitter that the current function is non-leaf:
     it may perform a (non-tail) call; by default, sets
     [Proc.contains_calls := true] *)

  method mark_tailcall : unit
  (* informs the code emitter that the current function may end with
     a tail-call; by default, does nothing *)

  method mark_c_tailcall : unit
  (* informs the code emitter that the current function may call
     a C function that never returns; by default, does nothing.

     It is unnecessary to save the stack pointer in this situation
     (which is the main purpose of tracking leaf functions) but some
     architectures still need to ensure that the stack is properly
     aligned when the C function is called. This is achieved by
     overloading this method to set [Proc.contains_calls := true] *)

  method mark_instr : Mach.instruction_desc -> unit
  (* dispatches on instructions to call one of the marking function
     above; overloading this is useful if Ispecific instructions need
     marking *)

  (* The following method is the entry point and should not be overridden
     (except by [Spacetime_profiling]). *)
  method emit_fundecl : Cmm.fundecl -> Mach.fundecl

  (* The following methods should not be overridden.  They cannot be
     declared "private" in the current implementation because they
     are not always applied to "self", but ideally they should be private. *)
  method extract : Mach.instruction
  method extract_core : end_instr:Mach.instruction -> Mach.instruction
  method insert : Mach.instruction_desc -> Reg.t array -> Reg.t array -> unit
  method insert_debug : Mach.instruction_desc -> Debuginfo.t ->
                                        Reg.t array -> Reg.t array -> unit
  method insert_move : Reg.t -> Reg.t -> unit
  method insert_move_args : Reg.t array -> Reg.t array -> int -> unit
  method insert_move_results : Reg.t array -> Reg.t array -> int -> unit
  method insert_moves : Reg.t array -> Reg.t array -> unit
  method adjust_type : Reg.t -> Reg.t -> unit
  method adjust_types : Reg.t array -> Reg.t array -> unit
  method emit_expr :
    environment -> Cmm.expression -> Reg.t array option
  method emit_tail : environment -> Cmm.expression -> unit

  (* Only for the use of [Spacetime_profiling]. *)
  method select_allocation : int -> Mach.operation
  method select_allocation_args : environment -> Reg.t array
  method select_checkbound : unit -> Mach.integer_operation
  method select_checkbound_extra_args : unit -> Cmm.expression list
  method emit_blockheader
     : environment
    -> nativeint
    -> Debuginfo.t
    -> Reg.t array option
  method about_to_emit_call
     : environment
    -> Mach.instruction_desc
    -> Reg.t array
    -> Reg.t array option
  method initial_env : unit -> environment
  method insert_prologue
     : Cmm.fundecl
    -> loc_arg:Reg.t array
    -> rarg:Reg.t array
    -> spacetime_node_hole:(Ident.t * Reg.t array) option
    -> env:environment
    -> Mach.spacetime_shape option

  val mutable instr_seq : Mach.instruction

end

val reset : unit -> unit
