(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                   Mark Shinwell, Jane Street Europe                 *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

module type S = sig
  (* The distance between two instructions, in arbitrary units (typically
     the natural word size of instructions). *)
  type distance = int

  module Cond_branch : sig
    (* The various types of conditional branches for a given target that
       may require relaxation. *)
    type t

    (* All values of type [t] that the emitter may produce. *)
    val all : t list

    (* If [max_displacement branch] is [n] then [branch] is assumed to
       reach any address in the range [pc - n, pc + n] (inclusive), after
       the [pc] of the branch has been adjusted by [offset_pc_at_branch]
       (see below). *)
    val max_displacement : t -> distance

    (* Which variety of conditional branch may be produced by the emitter for a
       given instruction description.  For the moment we assume that only one
       such variety per instruction description is needed.

       N.B. The only instructions supported are the following:
                - Lop (Ialloc _)
                - Lop (Iintop Icheckbound)
                - Lop (Iintop_imm (Icheckbound, _))
                - Lop (Ispecific _)
                - Lcondbranch (_, _)
                - Lcondbranch3 (_, _, _)
       [classify_instr] is expected to return [None] when called on any
       instruction not in this list. *)
    val classify_instr : Linearize.instruction_desc -> t option
  end

  (* The value to be added to the program counter (in [distance] units)
     when it is at a branch instruction, prior to calculating the distance
     to a branch target. *)
  val offset_pc_at_branch : distance

  (* The maximum size of a given instruction. *)
  val instr_size : Linearize.instruction_desc -> distance

  (* Insertion of target-specific code to relax operations that cannot be
     relaxed generically.  It is assumed that these rewrites do not change
     the size of out-of-line code (cf. branch_relaxation.mli). *)
  val relax_allocation : num_words:int -> Linearize.instruction_desc
  val relax_intop_checkbound : unit -> Linearize.instruction_desc
  val relax_intop_imm_checkbound : bound:int -> Linearize.instruction_desc
  val relax_specific_op : Arch.specific_operation -> Linearize.instruction_desc
end
