(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Instruction selection for the AMD64 *)

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
    Cconst_symbol s when not !Clflags.dlcode ->
      (Asymbol s, 0)
  | Cop((Caddi | Cadda), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Csubi | Csuba), [arg; Cconst_int m]) ->
      let (a, n) = select_addr arg in (a, n - m)
  | Cop((Caddi | Cadda), [Cconst_int m; arg]) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop(Clsl, [arg; Cconst_int(1|2|3 as shift)]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, 1 lsl shift), n lsl shift)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [arg; Cconst_int(2|4|8 as mult)]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop(Cmuli, [Cconst_int(2|4|8 as mult); arg]) ->
      begin match select_addr arg with
        (Alinear e, n) -> (Ascale(e, mult), n * mult)
      | _ -> (Alinear exp, 0)
      end
  | Cop((Caddi | Cadda), [arg1; arg2]) ->
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

(* Special constraints on operand and result registers *)

exception Use_default

let rax = phys_reg 0
let rcx = phys_reg 5
let rdx = phys_reg 4

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations: arg.(0) and res.(0) must be the same *)
    Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor) | Iaddf|Isubf|Imulf|Idivf ->
      ([|res.(0); arg.(1)|], res)
  (* One-address unary operations: arg.(0) and res.(0) must be the same *)
  | Iintop_imm((Iadd|Isub|Imul|Iand|Ior|Ixor|Ilsl|Ilsr|Iasr), _)
  | Iabsf | Inegf
  | Ispecific(Ibswap (32|64)) ->
      (res, res)
  (* For xchg, args must be a register allowing access to high 8 bit register
     (rax, rbx, rcx or rdx). Keep it simple, just force the argument in rax. *)
  | Ispecific(Ibswap 16) ->
      ([| rax |], [| rax |])
  (* For imulq, first arg must be in rax, rax is clobbered, and result is in
     rdx. *)
  | Iintop(Imulh) ->
      ([| rax; arg.(1) |], [| rdx |])
  | Ispecific(Ifloatarithmem(_,_)) ->
      let arg' = Array.copy arg in
      arg'.(0) <- res.(0);
      (arg', res)
  (* For shifts with variable shift count, second arg must be in rcx *)
  | Iintop(Ilsl|Ilsr|Iasr) ->
      ([|res.(0); rcx|], res)
  (* For div and mod, first arg must be in rax, rdx is clobbered,
     and result is in rax or rdx respectively.
     Keep it simple, just force second argument in rcx. *)
  | Iintop(Idiv) ->
      ([| rax; rcx |], [| rax |])
  | Iintop(Imod) ->
      ([| rax; rcx |], [| rdx |])
  (* Other instructions are regular *)
  | _ -> raise Use_default

let inline_ops =
  [ "sqrt"; "caml_bswap16_direct"; "caml_int32_direct_bswap";
    "caml_int64_direct_bswap"; "caml_nativeint_direct_bswap" ]

(* The selector class *)

class selector = object (self)

inherit Selectgen.selector_generic as super

method is_immediate n = n <= 0x7FFFFFFF && n >= -0x80000000

method is_immediate_natint n = n <= 0x7FFFFFFFn && n >= -0x80000000n

method! is_simple_expr e =
  match e with
  | Cop(Cextcall(fn, _, _, _), args)
    when List.mem fn inline_ops ->
      (* inlined ops are simple if their arguments are *)
      List.for_all self#is_simple_expr args
  | _ ->
      super#is_simple_expr e

method select_addressing chunk exp =
  let (a, d) = select_addr exp in
  (* PR#4625: displacement must be a signed 32-bit immediate *)
  if d < -0x8000_0000 || d > 0x7FFF_FFFF
  then (Iindexed 0, exp)
  else match a with
    | Asymbol s ->
        (Ibased(s, d), Ctuple [])
    | Alinear e ->
        (Iindexed d, e)
    | Aadd(e1, e2) ->
        (Iindexed2 d, Ctuple[e1; e2])
    | Ascale(e, scale) ->
        (Iscaled(scale, d), e)
    | Ascaledadd(e1, e2, scale) ->
        (Iindexed2scaled(scale, d), Ctuple[e1; e2])

method! select_store is_assign addr exp =
  match exp with
    Cconst_int n when self#is_immediate n ->
      (Ispecific(Istore_int(Nativeint.of_int n, addr, is_assign)), Ctuple [])
  | (Cconst_natint n | Cconst_blockheader n) when self#is_immediate_natint n ->
      (Ispecific(Istore_int(n, addr, is_assign)), Ctuple [])
  | Cconst_pointer n when self#is_immediate n ->
      (Ispecific(Istore_int(Nativeint.of_int n, addr, is_assign)), Ctuple [])
  | Cconst_natpointer n when self#is_immediate_natint n ->
      (Ispecific(Istore_int(n, addr, is_assign)), Ctuple [])
  | Cconst_symbol s when not (!pic_code || !Clflags.dlcode) ->
      (Ispecific(Istore_symbol(s, addr, is_assign)), Ctuple [])
  | _ ->
      super#select_store is_assign addr exp

method! select_operation op args =
  match op with
  (* Recognize the LEA instruction *)
    Caddi | Cadda | Csubi | Csuba ->
      begin match self#select_addressing Word (Cop(op, args)) with
        (Iindexed d, _) -> super#select_operation op args
      | (Iindexed2 0, _) -> super#select_operation op args
      | (addr, arg) -> (Ispecific(Ilea addr), [arg])
      end
  (* Recognize float arithmetic with memory. *)
  | Caddf ->
      self#select_floatarith true Iaddf Ifloatadd args
  | Csubf ->
      self#select_floatarith false Isubf Ifloatsub args
  | Cmulf ->
      self#select_floatarith true Imulf Ifloatmul args
  | Cdivf ->
      self#select_floatarith false Idivf Ifloatdiv args
  | Cextcall("sqrt", _, false, _) ->
     begin match args with
       [Cop(Cload (Double|Double_u as chunk), [loc])] ->
         let (addr, arg) = self#select_addressing chunk loc in
         (Ispecific(Ifloatsqrtf addr), [arg])
     | [arg] ->
         (Ispecific Isqrtf, [arg])
     | _ ->
         assert false
     end
  (* Recognize store instructions *)
  | Cstore Word ->
      begin match args with
        [loc; Cop(Caddi, [Cop(Cload _, [loc']); Cconst_int n])]
        when loc = loc' && self#is_immediate n ->
          let (addr, arg) = self#select_addressing Word loc in
          (Ispecific(Ioffset_loc(n, addr)), [arg])
      | _ ->
          super#select_operation op args
      end
  | Cextcall("caml_bswap16_direct", _, _, _) ->
      (Ispecific (Ibswap 16), args)
  | Cextcall("caml_int32_direct_bswap", _, _, _) ->
      (Ispecific (Ibswap 32), args)
  | Cextcall("caml_int64_direct_bswap", _, _, _)
  | Cextcall("caml_nativeint_direct_bswap", _, _, _) ->
      (Ispecific (Ibswap 64), args)
  (* AMD64 does not support immediate operands for multiply high signed *)
  | Cmulhi ->
      (Iintop Imulh, args)
  | _ -> super#select_operation op args

(* Recognize float arithmetic with mem *)

method select_floatarith commutative regular_op mem_op args =
  match args with
    [arg1; Cop(Cload (Double|Double_u as chunk), [loc2])] ->
      let (addr, arg2) = self#select_addressing chunk loc2 in
      (Ispecific(Ifloatarithmem(mem_op, addr)),
                 [arg1; arg2])
  | [Cop(Cload (Double|Double_u as chunk), [loc1]); arg2] when commutative ->
      let (addr, arg1) = self#select_addressing chunk loc1 in
      (Ispecific(Ifloatarithmem(mem_op, addr)),
                 [arg2; arg1])
  | [arg1; arg2] ->
      (regular_op, [arg1; arg2])
  | _ ->
      assert false

method! mark_c_tailcall =
  Proc.contains_calls := true

(* Deal with register constraints *)

method! insert_op_debug op dbg rs rd =
  try
    let (rsrc, rdst) = pseudoregs_for_operation op rs rd in
    self#insert_moves rs rsrc;
    self#insert_debug (Iop op) dbg rsrc rdst;
    self#insert_moves rdst rd;
    rd
  with Use_default ->
    super#insert_op_debug op dbg rs rd

end

let fundecl f = (new selector)#emit_fundecl f
