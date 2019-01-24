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
  | Ialloc of { words : int; label_after_call_gc : label option;
        spacetime_index : int; }
  | Iintop of integer_operation
  | Iintop_imm of integer_operation * int
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat
  | Ispecific of Arch.specific_operation
  | Iname_for_debugger of { ident : Ident.t; which_parameter : int option;
      provenance : unit option; is_assignment : bool; }

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
  | Direct_call_point of { callee : string; }
  | Indirect_call_point
  | Allocation_point

type spacetime_shape = (spacetime_part_of_shape * Cmm.label) list

type fundecl =
  { fun_name: string;
    fun_args: Reg.t array;
    fun_body: instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t;
    fun_spacetime_shape : spacetime_shape option;
  }

let rec dummy_instr =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let end_instr () =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r;
    dbg = Debuginfo.none; live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let instr_cons_debug d a r dbg n =
  { desc = d; next = n; arg = a; res = r; dbg = dbg; live = Reg.Set.empty;
    available_before = Reg_availability_set.Ok Reg_with_debug_info.Set.empty;
    available_across = None;
  }

let rec instr_iter f i =
  match i.desc with
    Iend -> ()
  | _ ->
      f i;
      match i.desc with
        Iend -> ()
      | Ireturn | Iop(Itailcall_ind _) | Iop(Itailcall_imm _) -> ()
      | Iifthenelse(_tst, ifso, ifnot) ->
          instr_iter f ifso; instr_iter f ifnot; instr_iter f i.next
      | Iswitch(_index, cases) ->
          for i = 0 to Array.length cases - 1 do
            instr_iter f cases.(i)
          done;
          instr_iter f i.next
      | Iloop(body) ->
          instr_iter f body; instr_iter f i.next
      | Icatch(_, handlers, body) ->
          instr_iter f body;
          List.iter (fun (_n, handler) -> instr_iter f handler) handlers;
          instr_iter f i.next
      | Iexit _ -> ()
      | Itrywith(body, handler) ->
          instr_iter f body; instr_iter f handler; instr_iter f i.next
      | Iraise _ -> ()
      | _ ->
          instr_iter f i.next

let spacetime_node_hole_pointer_is_live_before insn =
  match insn.desc with
  | Iop op ->
    begin match op with
    | Icall_ind _ | Icall_imm _ | Itailcall_ind _ | Itailcall_imm _ -> true
    | Iextcall { alloc; } -> alloc
    | Ialloc _ ->
      (* Allocations are special: the call to [caml_call_gc] requires some
         instrumentation code immediately prior, but this is not inserted until
         the emitter (since the call is not visible prior to that in any IR).
         As such, none of the Mach / Linearize analyses will ever see that
         we use the node hole pointer for these, and we do not need to say
         that it is live at such points. *)
      false
    | Iintop op | Iintop_imm (op, _) ->
      begin match op with
      | Icheckbound _
        (* [Icheckbound] doesn't need to return [true] for the same reason as
           [Ialloc]. *)
      | Iadd | Isub | Imul | Imulh | Idiv | Imod
      | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
      | Icomp _ -> false
      end
    | Ispecific specific_op ->
      Arch.spacetime_node_hole_pointer_is_live_before specific_op
    | Imove | Ispill | Ireload | Iconst_int _ | Iconst_float _
    | Iconst_symbol _ | Istackoffset _ | Iload _ | Istore _
    | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
    | Ifloatofint | Iintoffloat
    | Iname_for_debugger _ -> false
    end
  | Iend | Ireturn | Iifthenelse _ | Iswitch _ | Iloop _ | Icatch _
  | Iexit _ | Itrywith _ | Iraise _ -> false

let operation_can_raise op =
  match op with
  | Icall_ind _ | Icall_imm _ | Iextcall _
  | Iintop (Icheckbound _) | Iintop_imm (Icheckbound _, _)
  | Ialloc _ -> true
  | _ -> false
