(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the Intel x86 *)

open Misc
open Arch
open Proc
open Cmm
open Mach

(* Auxiliary for recognizing addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression
  | Ascale of expression * int
  | Ascaledadd of expression * expression * int

let rec select_addr exp =
  match exp with
    Cconst_symbol s ->
      (Asymbol s, 0)
  | Cop((Caddi | Caddv | Cadda), [arg; Cconst_int m], _) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop(Csubi, [arg; Cconst_int m], _) ->
      let (a, n) = select_addr arg in (a, n - m)
  | Cop((Caddi | Caddv | Cadda), [Cconst_int m; arg], _) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop(Clsl, [arg; Cconst_int(1|2|3 as shift)], _) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, 1 lsl shift), n lsl shift)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [arg; Cconst_int(2|4|8 as mult)], _) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [Cconst_int(2|4|8 as mult); arg], _) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop((Caddi | Cadda | Caddv), [arg1; arg2], _) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | ((Alinear e1, n1), (Ascale(e2, scale), n2)) ->
              (Ascaledadd(e1, e2, scale), n1 + n2)
        | ((Ascale(e1, scale), n1), (Alinear e2, n2)) ->
              (Ascaledadd(e2, e1, scale), n1 + n2)
        | (_, (Ascale(e2, scale), n2)) ->
              (Ascaledadd(arg1, e2, scale), n2)
        | ((Ascale(e1, scale), n1), _) ->
              (Ascaledadd(arg2, e1, scale), n1)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | arg ->
      (Alinear arg, 0)

(* C functions to be turned into Ifloatspecial instructions if -ffast-math.
   If you update this list, you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
let inline_float_ops =
  ["atan"; "atan2"; "cos"; "log"; "log10"; "sin"; "sqrt"; "tan"]

(* Estimate number of float temporaries needed to evaluate expression
   (Ershov's algorithm) *)

let rec float_needs = function
    Cop((Cnegf | Cabsf), [arg], _) ->
      float_needs arg
  | Cop((Caddf | Csubf | Cmulf | Cdivf), [arg1; arg2], _) ->
      let n1 = float_needs arg1 in
      let n2 = float_needs arg2 in
      if n1 = n2 then 1 + n1 else if n1 > n2 then n1 else n2
  | Cop(Cextcall(fn, _ty_res, _alloc, _label), args, _dbg)
    when !fast_math && List.mem fn inline_float_ops ->
      begin match args with
        [arg] -> float_needs arg
      | [arg1; arg2] -> max (float_needs arg2 + 1) (float_needs arg1)
      | _ -> assert false
      end
  | _ ->
      1

(* Special constraints on operand and result registers *)

exception Use_default

let eax = phys_reg 0
let ecx = phys_reg 2
let edx = phys_reg 3
let tos = phys_reg 100

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations *)
    Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor) ->
      ([|res.(0); arg.(1)|], res, false)
  (* Two-address unary operations *)
  | Iintop_imm((Iadd|Isub|Imul|Iand|Ior|Ixor|Ilsl|Ilsr|Iasr), _) ->
      (res, res, false)
  (* For imull, first arg must be in eax, eax is clobbered, and result is in
     edx. *)
  | Iintop(Imulh) ->
      ([| eax; arg.(1) |], [| edx |], true)
  (* For shifts with variable shift count, second arg must be in ecx *)
  | Iintop(Ilsl|Ilsr|Iasr) ->
      ([|res.(0); ecx|], res, false)
  (* For div and mod, first arg must be in eax, edx is clobbered,
     and result is in eax or edx respectively.
     Keep it simple, just force second argument in ecx. *)
  | Iintop(Idiv) ->
      ([| eax; ecx |], [| eax |], true)
  | Iintop(Imod) ->
      ([| eax; ecx |], [| edx |], true)
  (* For floating-point operations and floating-point loads,
     the result is always left at the top of the floating-point stack *)
  | Iconst_float _ | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iload((Single | Double | Double_u), _)
  | Ispecific(Isubfrev | Idivfrev | Ifloatarithmem _ | Ifloatspecial _) ->
      (arg, [| tos |], false)           (* don't move it immediately *)
  (* For storing a byte, the argument must be in eax...edx.
     (But for a short, any reg will do!)
     Keep it simple, just force the argument to be in edx. *)
  | Istore((Byte_unsigned | Byte_signed), _, _) ->
      let newarg = Array.copy arg in
      newarg.(0) <- edx;
      (newarg, res, false)
  (* Other instructions are regular *)
  | _ -> raise Use_default

let chunk_double = function
    Single -> false
  | Double -> true
  | Double_u -> true
  | _ -> assert false

(* The selector class *)

class selector = object (self)

inherit Selectgen.selector_generic as super

method is_immediate (_n : int) = true

method! is_simple_expr e =
  match e with
  | Cop(Cextcall(fn, _, _alloc, _), args, _)
    when !fast_math && List.mem fn inline_float_ops ->
      (* inlined float ops are simple if their arguments are *)
      List.for_all self#is_simple_expr args
  | _ ->
      super#is_simple_expr e

method! effects_of e =
  match e with
  | Cop(Cextcall(fn, _, _, _), args, _)
    when !fast_math && List.mem fn inline_float_ops ->
      Selectgen.Effect_and_coeffect.join_list_map args self#effects_of
  | _ ->
      super#effects_of e

method select_addressing _chunk exp =
  match select_addr exp with
    (Asymbol s, d) ->
      (Ibased(s, d), Ctuple [])
  | (Alinear e, d) ->
      (Iindexed d, e)
  | (Aadd(e1, e2), d) ->
      (Iindexed2 d, Ctuple[e1; e2])
  | (Ascale(e, scale), d) ->
      (Iscaled(scale, d), e)
  | (Ascaledadd(e1, e2, scale), d) ->
      (Iindexed2scaled(scale, d), Ctuple[e1; e2])

method! select_store is_assign addr exp =
  match exp with
    Cconst_int n ->
      (Ispecific(Istore_int(Nativeint.of_int n, addr, is_assign)), Ctuple [])
  | (Cconst_natint n | Cblockheader (n, _)) ->
      (Ispecific(Istore_int(n, addr, is_assign)), Ctuple [])
  | Cconst_pointer n ->
      (Ispecific(Istore_int(Nativeint.of_int n, addr, is_assign)), Ctuple [])
  | Cconst_natpointer n ->
      (Ispecific(Istore_int(n, addr, is_assign)), Ctuple [])
  | Cconst_symbol s ->
      (Ispecific(Istore_symbol(s, addr, is_assign)), Ctuple [])
  | _ ->
      super#select_store is_assign addr exp

method! select_operation op args dbg =
  match op with
  (* Recognize the LEA instruction *)
    Caddi | Caddv | Cadda | Csubi ->
      begin match self#select_addressing Word_int (Cop(op, args, dbg)) with
        (Iindexed _, _)
      | (Iindexed2 0, _) -> super#select_operation op args dbg
      | (addr, arg) -> (Ispecific(Ilea addr), [arg])
      end
  (* Recognize float arithmetic with memory.
     In passing, apply Ershov's algorithm to reduce stack usage *)
  | Caddf ->
      self#select_floatarith Iaddf Iaddf Ifloatadd Ifloatadd args
  | Csubf ->
      self#select_floatarith Isubf (Ispecific Isubfrev) Ifloatsub Ifloatsubrev
                             args
  | Cmulf ->
      self#select_floatarith Imulf Imulf Ifloatmul Ifloatmul args
  | Cdivf ->
      self#select_floatarith Idivf (Ispecific Idivfrev) Ifloatdiv Ifloatdivrev
                             args
  (* Recognize store instructions *)
  | Cstore ((Word_int | Word_val) as chunk, _) ->
      begin match args with
        [loc; Cop(Caddi, [Cop(Cload _, [loc'], _); Cconst_int n], _)]
        when loc = loc' ->
          let (addr, arg) = self#select_addressing chunk loc in
          (Ispecific(Ioffset_loc(n, addr)), [arg])
      | _ ->
          super#select_operation op args dbg
      end
  (* Recognize inlined floating point operations *)
  | Cextcall(fn, _ty_res, false, _label)
    when !fast_math && List.mem fn inline_float_ops ->
      (Ispecific(Ifloatspecial fn), args)
  (* i386 does not support immediate operands for multiply high signed *)
  | Cmulhi ->
      (Iintop Imulh, args)
  (* Default *)
  | _ -> super#select_operation op args dbg

(* Recognize float arithmetic with mem *)

method select_floatarith regular_op reversed_op mem_op mem_rev_op args =
  match args with
    [arg1; Cop(Cload (chunk, _), [loc2], _)] ->
      let (addr, arg2) = self#select_addressing chunk loc2 in
      (Ispecific(Ifloatarithmem(chunk_double chunk, mem_op, addr)),
                 [arg1; arg2])
  | [Cop(Cload (chunk, _), [loc1], _); arg2] ->
      let (addr, arg1) = self#select_addressing chunk loc1 in
      (Ispecific(Ifloatarithmem(chunk_double chunk, mem_rev_op, addr)),
                 [arg2; arg1])
  | [arg1; arg2] ->
      (* Evaluate bigger subexpression first to minimize stack usage.
         Because of right-to-left evaluation, rightmost arg is evaluated
         first *)
      if float_needs arg1 <= float_needs arg2
      then (regular_op, [arg1; arg2])
      else (reversed_op, [arg2; arg1])
  | _ ->
      fatal_error "Proc_i386: select_floatarith"

(* Deal with register constraints *)

method! insert_op_debug op dbg rs rd =
  try
    let (rsrc, rdst, move_res) = pseudoregs_for_operation op rs rd in
    self#insert_moves rs rsrc;
    self#insert_debug (Iop op) dbg rsrc rdst;
    if move_res then begin
      self#insert_moves rdst rd;
      rd
    end else
      rdst
  with Use_default ->
    super#insert_op_debug op dbg rs rd

(* Selection of push instructions for external calls *)

method select_push exp =
  match exp with
    Cconst_int n -> (Ispecific(Ipush_int(Nativeint.of_int n)), Ctuple [])
  | Cconst_natint n -> (Ispecific(Ipush_int n), Ctuple [])
  | Cconst_pointer n -> (Ispecific(Ipush_int(Nativeint.of_int n)), Ctuple [])
  | Cconst_natpointer n -> (Ispecific(Ipush_int n), Ctuple [])
  | Cconst_symbol s -> (Ispecific(Ipush_symbol s), Ctuple [])
  | Cop(Cload ((Word_int | Word_val as chunk), _), [loc], _) ->
      let (addr, arg) = self#select_addressing chunk loc in
      (Ispecific(Ipush_load addr), arg)
  | Cop(Cload (Double_u, _), [loc], _) ->
      let (addr, arg) = self#select_addressing Double_u loc in
      (Ispecific(Ipush_load_float addr), arg)
  | _ -> (Ispecific(Ipush), exp)

method! mark_c_tailcall =
  Proc.contains_calls := true

method! emit_extcall_args env args =
  let rec size_pushes = function
  | [] -> 0
  | e :: el -> Selectgen.size_expr env e + size_pushes el in
  let sz1 = size_pushes args in
  let sz2 = Misc.align sz1 stack_alignment in
  let rec emit_pushes = function
  | [] ->
      if sz2 > sz1 then
        self#insert (Iop (Istackoffset (sz2 - sz1))) [||] [||]
  | e :: el ->
      emit_pushes el;
      let (op, arg) = self#select_push e in
      match self#emit_expr env arg with
      | None -> ()
      | Some r -> self#insert (Iop op) r [||] in
  emit_pushes args;
  ([||], sz2)

end

let fundecl f = (new selector)#emit_fundecl f
