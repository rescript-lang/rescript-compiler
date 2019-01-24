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

(* Representation of machine code by sequences of pseudoinstructions *)

(** N.B. Backends vary in their treatment of call gc and checkbound
    points.  If the positioning of any labels associated with these is
    important for some new feature in the compiler, the relevant backends'
    behaviour should be checked. *)
type label = Cmm.label

type integer_comparison =
    Isigned of Cmm.comparison
  | Iunsigned of Cmm.comparison

type integer_operation =
    Iadd | Isub | Imul | Imulh | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Icomp of integer_comparison
  | Icheckbound of { label_after_error : label option;
        spacetime_index : int; }
    (** For Spacetime only, [Icheckbound] operations take two arguments, the
        second being the pointer to the trie node for the current function
        (and the first being as per non-Spacetime mode). *)

type test =
    Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of Cmm.comparison * bool
  | Ioddtest
  | Ieventest

type operation =
    Imove
  | Ispill
  | Ireload
  | Iconst_int of nativeint
  | Iconst_float of int64
  | Iconst_symbol of string
  | Icall_ind of { label_after : label; }
  | Icall_imm of { func : string; label_after : label; }
  | Itailcall_ind of { label_after : label; }
  | Itailcall_imm of { func : string; label_after : label; }
  | Iextcall of { func : string; alloc : bool; label_after : label; }
  | Istackoffset of int
  | Iload of Cmm.memory_chunk * Arch.addressing_mode
  | Istore of Cmm.memory_chunk * Arch.addressing_mode * bool
                                 (* false = initialization, true = assignment *)
  | Ialloc of { words : int; label_after_call_gc : label option;
      spacetime_index : int; }
    (** For Spacetime only, Ialloc instructions take one argument, being the
        pointer to the trie node for the current function. *)
  | Iintop of integer_operation
  | Iintop_imm of integer_operation * int
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat
  | Ispecific of Arch.specific_operation
  | Iname_for_debugger of { ident : Ident.t; which_parameter : int option;
      provenance : unit option; is_assignment : bool; }
    (** [Iname_for_debugger] has the following semantics:
        (a) The argument register(s) is/are deemed to contain the value of the
            given identifier.
        (b) If [is_assignment] is [true], any information about other [Reg.t]s
            that have been previously deemed to hold the value of that
            identifier is forgotten. *)

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    mutable live: Reg.Set.t;
    mutable available_before: Reg_availability_set.t;
    mutable available_across: Reg_availability_set.t option;
  }

and instruction_desc =
    Iend
  | Iop of operation
  | Ireturn
  | Iifthenelse of test * instruction * instruction
  | Iswitch of int array * instruction array
  | Iloop of instruction
  | Icatch of Cmm.rec_flag * (int * instruction) list * instruction
  | Iexit of int
  | Itrywith of instruction * instruction
  | Iraise of Cmm.raise_kind

type spacetime_part_of_shape =
  | Direct_call_point of { callee : string; (* the symbol *) }
  | Indirect_call_point
  | Allocation_point

(** A description of the layout of a Spacetime profiling node associated with
    a given function.  Each call and allocation point instrumented within
    the function is marked with a label in the code and assigned a place
    within the node.  This information is stored within the executable and
    extracted when the user saves a profile.  The aim is to minimise runtime
    memory usage within the nodes and increase performance. *)
type spacetime_shape = (spacetime_part_of_shape * Cmm.label) list

type fundecl =
  { fun_name: string;
    fun_args: Reg.t array;
    fun_body: instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t;
    fun_spacetime_shape : spacetime_shape option;
  }

val dummy_instr: instruction
val end_instr: unit -> instruction
val instr_cons:
      instruction_desc -> Reg.t array -> Reg.t array -> instruction ->
        instruction
val instr_cons_debug:
      instruction_desc -> Reg.t array -> Reg.t array -> Debuginfo.t ->
        instruction -> instruction
val instr_iter: (instruction -> unit) -> instruction -> unit

val spacetime_node_hole_pointer_is_live_before : instruction -> bool

val operation_can_raise : operation -> bool
