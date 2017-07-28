(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Benedikt Meurer, University of Siegen              *)
(*                                                                     *)
(*    Copyright 1998 Institut National de Recherche en Informatique    *)
(*    et en Automatique. Copyright 2012 Benedikt Meurer. All rights    *)
(*    reserved.  This file is distributed  under the terms of the Q    *)
(*    Public License version 1.0.                                      *)
(*                                                                     *)
(***********************************************************************)

(* Instruction selection for the ARM processor *)

open Arch
open Proc
open Cmm
open Mach

let is_offset chunk n =
  match chunk with
  (* VFPv{2,3} load/store have -1020 to 1020 *)
    Single | Double | Double_u
    when !fpu >= VFPv2 ->
      n >= -1020 && n <= 1020
  (* ARM load/store byte/word have -4095 to 4095 *)
  | Byte_unsigned | Byte_signed
  | Thirtytwo_unsigned | Thirtytwo_signed
  | Word | Single
    when not !thumb ->
      n >= -4095 && n <= 4095
  (* Thumb-2 load/store have -255 to 4095 *)
  | _ when !arch > ARMv6 && !thumb ->
      n >= -255 && n <= 4095
  (* Everything else has -255 to 255 *)
  | _ ->
      n >= -255 && n <= 255

let select_shiftop = function
    Clsl -> Ishiftlogicalleft
  | Clsr -> Ishiftlogicalright
  | Casr -> Ishiftarithmeticright
  | __-> assert false

(* Special constraints on operand and result registers *)

exception Use_default

let r1 = phys_reg 1
let r6 = phys_reg 6
let r7 = phys_reg 7
let r12 = phys_reg 8

let pseudoregs_for_operation op arg res =
  match op with
  (* For mul rd,rm,rs and mla rd,rm,rs,ra (pre-ARMv6) the registers rm
     and rd must be different. We deal with this by pretending that rm
     is also a result of the mul / mla operation. *)
    Iintop Imul | Ispecific Imuladd when !arch < ARMv6 ->
      (arg, [| res.(0); arg.(0) |])
  (* For smull rdlo,rdhi,rn,rm (pre-ARMv6) the registers rdlo, rdhi and rn
     must be different.  Also, rdlo (whose contents we discard) is always
     forced to be r12 in proc.ml, which means that neither rdhi and rn can
     be r12.  To keep things simple, we force both of those two to specific
     hard regs: rdhi in r6 and rn in r7. *)
  | Iintop Imulh when !arch < ARMv6 ->
      ([| r7; arg.(1) |], [| r6 |])
  (* Soft-float Iabsf and Inegf: arg.(0) and res.(0) must be the same *)
  | Iabsf | Inegf when !fpu = Soft ->
      ([|res.(0); arg.(1)|], res)
  (* VFPv{2,3} Imuladdf...Inegmulsubf: arg.(0) and res.(0) must be the same *)
  | Ispecific(Imuladdf | Inegmuladdf | Imulsubf | Inegmulsubf) ->
      let arg' = Array.copy arg in
      arg'.(0) <- res.(0);
      (arg', res)
  (* We use __aeabi_idivmod for Cmodi only, and hence we care only
     for the remainder in r1, so fix up the destination register. *)
  | Iextcall("__aeabi_idivmod", false) ->
      (arg, [|r1|])
  (* Other instructions are regular *)
  | _ -> raise Use_default

(* Instruction selection *)
class selector = object(self)

inherit Selectgen.selector_generic as super

method! regs_for tyv =
  Reg.createv (if !fpu = Soft then begin
                 (* Expand floats into pairs of integer registers *)
                 let rec expand = function
                   [] -> []
                 | Float :: tyl -> Int :: Int :: expand tyl
                 | ty :: tyl -> ty :: expand tyl in
                 Array.of_list (expand (Array.to_list tyv))
               end else begin
                 tyv
               end)

method is_immediate n =
  is_immediate (Int32.of_int n)

method! is_simple_expr = function
  (* inlined floating-point ops are simple if their arguments are *)
  | Cop(Cextcall("sqrt", _, _, _), args) when !fpu >= VFPv2 ->
      List.for_all self#is_simple_expr args
  (* inlined byte-swap ops are simple if their arguments are *)
  | Cop(Cextcall("caml_bswap16_direct", _, _, _), args) when !arch >= ARMv6T2 ->
      List.for_all self#is_simple_expr args
  | Cop(Cextcall("caml_int32_direct_bswap", _,_,_), args) when !arch >= ARMv6 ->
      List.for_all self#is_simple_expr args
  | e -> super#is_simple_expr e

method select_addressing chunk = function
  | Cop(Cadda, [arg; Cconst_int n])
    when is_offset chunk n ->
      (Iindexed n, arg)
  | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int n])])
    when is_offset chunk n ->
      (Iindexed n, Cop(Cadda, [arg1; arg2]))
  | arg ->
      (Iindexed 0, arg)

method select_shift_arith op arithop arithrevop args =
  match args with
    [arg1; Cop(Clsl | Clsr | Casr as op, [arg2; Cconst_int n])]
    when n > 0 && n < 32 ->
      (Ispecific(Ishiftarith(arithop, select_shiftop op, n)), [arg1; arg2])
  | [Cop(Clsl | Clsr | Casr as op, [arg1; Cconst_int n]); arg2]
    when n > 0 && n < 32 ->
      (Ispecific(Ishiftarith(arithrevop, select_shiftop op, n)), [arg2; arg1])
  | args ->
      begin match super#select_operation op args with
      (* Recognize multiply high and add *)
        (Iintop Iadd, [Cop(Cmulhi, args); arg3])
      | (Iintop Iadd, [arg3; Cop(Cmulhi, args)]) as op_args
        when !arch >= ARMv6 ->
          begin match self#select_operation Cmulhi args with
            (Iintop Imulh, [arg1; arg2]) ->
              (Ispecific Imulhadd, [arg1; arg2; arg3])
          | _ -> op_args
          end
      (* Recognize multiply and add *)
      | (Iintop Iadd, [Cop(Cmuli, args); arg3])
      | (Iintop Iadd, [arg3; Cop(Cmuli, args)]) as op_args ->
          begin match self#select_operation Cmuli args with
            (Iintop Imul, [arg1; arg2]) ->
              (Ispecific Imuladd, [arg1; arg2; arg3])
          | _ -> op_args
          end
      (* Recognize multiply and subtract *)
      | (Iintop Isub, [arg3; Cop(Cmuli, args)]) as op_args
        when !arch > ARMv6 ->
          begin match self#select_operation Cmuli args with
            (Iintop Imul, [arg1; arg2]) ->
              (Ispecific Imulsub, [arg1; arg2; arg3])
          | _ -> op_args
          end
      | op_args -> op_args
      end

method! select_operation op args =
  match (op, args) with
  (* Recognize special shift arithmetic *)
    ((Cadda | Caddi), [arg; Cconst_int n])
    when n < 0 && self#is_immediate (-n) ->
      (Iintop_imm(Isub, -n), [arg])
  | ((Cadda | Caddi as op), args) ->
      self#select_shift_arith op Ishiftadd Ishiftadd args
  | ((Csuba | Csubi), [arg; Cconst_int n])
    when n < 0 && self#is_immediate (-n) ->
      (Iintop_imm(Iadd, -n), [arg])
  | ((Csuba | Csubi), [Cconst_int n; arg])
    when self#is_immediate n ->
      (Ispecific(Irevsubimm n), [arg])
  | ((Csuba | Csubi as op), args) ->
      self#select_shift_arith op Ishiftsub Ishiftsubrev args
  | (Cand as op, args) ->
      self#select_shift_arith op Ishiftand Ishiftand args
  | (Cor as op, args) ->
      self#select_shift_arith op Ishiftor Ishiftor args
  | (Cxor as op, args) ->
      self#select_shift_arith op Ishiftxor Ishiftxor args
  | (Ccheckbound _, [Cop(Clsl | Clsr | Casr as op, [arg1; Cconst_int n]); arg2])
    when n > 0 && n < 32 ->
      (Ispecific(Ishiftcheckbound(select_shiftop op, n)), [arg1; arg2])
  (* ARM does not support immediate operands for multiplication *)
  | (Cmuli, args) ->
      (Iintop Imul, args)
  | (Cmulhi, args) ->
      (Iintop Imulh, args)
  (* Turn integer division/modulus into runtime ABI calls *)
  | (Cdivi, args) ->
      (Iextcall("__aeabi_idiv", false), args)
  | (Cmodi, args) ->
      (* See above for fix up of return register *)
      (Iextcall("__aeabi_idivmod", false), args)
  (* Recognize 16-bit bswap instruction (ARMv6T2 because we need movt) *)
  | (Cextcall("caml_bswap16_direct", _, _, _), args) when !arch >= ARMv6T2 ->
      (Ispecific(Ibswap 16), args)
  (* Recognize 32-bit bswap instructions (ARMv6 and above) *)
  | (Cextcall("caml_int32_direct_bswap", _, _, _), args) when !arch >= ARMv6 ->
      (Ispecific(Ibswap 32), args)
  (* Turn floating-point operations into runtime ABI calls for softfp *)
  | (op, args) when !fpu = Soft -> self#select_operation_softfp op args
  (* Select operations for VFPv{2,3} *)
  | (op, args) -> self#select_operation_vfpv3 op args

method private select_operation_softfp op args =
  match (op, args) with
  (* Turn floating-point operations into runtime ABI calls *)
  | (Caddf, args) -> (Iextcall("__aeabi_dadd", false), args)
  | (Csubf, args) -> (Iextcall("__aeabi_dsub", false), args)
  | (Cmulf, args) -> (Iextcall("__aeabi_dmul", false), args)
  | (Cdivf, args) -> (Iextcall("__aeabi_ddiv", false), args)
  | (Cfloatofint, args) -> (Iextcall("__aeabi_i2d", false), args)
  | (Cintoffloat, args) -> (Iextcall("__aeabi_d2iz", false), args)
  | (Ccmpf comp, args) ->
      let func = (match comp with
                    Cne    (* there's no __aeabi_dcmpne *)
                  | Ceq -> "__aeabi_dcmpeq"
                  | Clt -> "__aeabi_dcmplt"
                  | Cle -> "__aeabi_dcmple"
                  | Cgt -> "__aeabi_dcmpgt"
                  | Cge -> "__aeabi_dcmpge") in
      let comp = (match comp with
                    Cne -> Ceq (* eq 0 => false *)
                  | _   -> Cne (* ne 0 => true *)) in
      (Iintop_imm(Icomp(Iunsigned comp), 0),
       [Cop(Cextcall(func, typ_int, false, Debuginfo.none), args)])
  (* Add coercions around loads and stores of 32-bit floats *)
  | (Cload Single, args) ->
      (Iextcall("__aeabi_f2d", false), [Cop(Cload Word, args)])
  | (Cstore Single, [arg1; arg2]) ->
      let arg2' =
        Cop(Cextcall("__aeabi_d2f", typ_int, false, Debuginfo.none),
            [arg2]) in
      self#select_operation (Cstore Word) [arg1; arg2']
  (* Other operations are regular *)
  | (op, args) -> super#select_operation op args

method private select_operation_vfpv3 op args =
  match (op, args) with
  (* Recognize floating-point negate and multiply *)
    (Cnegf, [Cop(Cmulf, args)]) ->
      (Ispecific Inegmulf, args)
  (* Recognize floating-point multiply and add *)
  | (Caddf, [arg; Cop(Cmulf, args)])
  | (Caddf, [Cop(Cmulf, args); arg]) ->
      (Ispecific Imuladdf, arg :: args)
  (* Recognize floating-point negate, multiply and subtract *)
  | (Csubf, [Cop(Cnegf, [arg]); Cop(Cmulf, args)])
  | (Csubf, [Cop(Cnegf, [Cop(Cmulf, args)]); arg]) ->
      (Ispecific Inegmulsubf, arg :: args)
  (* Recognize floating-point negate, multiply and add *)
  | (Csubf, [arg; Cop(Cmulf, args)]) ->
      (Ispecific Inegmuladdf, arg :: args)
  (* Recognize multiply and subtract *)
  | (Csubf, [Cop(Cmulf, args); arg]) ->
      (Ispecific Imulsubf, arg :: args)
  (* Recognize floating-point square root *)
  | (Cextcall("sqrt", _, false, _), args) ->
      (Ispecific Isqrtf, args)
  (* Other operations are regular *)
  | (op, args) -> super#select_operation op args

method! select_condition = function
  (* Turn floating-point comparisons into runtime ABI calls *)
    Cop(Ccmpf _ as op, args) when !fpu = Soft ->
      begin match self#select_operation_softfp op args with
        (Iintop_imm(Icomp(Iunsigned Ceq), 0), [arg]) -> (Ifalsetest, arg)
      | (Iintop_imm(Icomp(Iunsigned Cne), 0), [arg]) -> (Itruetest, arg)
      | _ -> assert false
      end
  | expr ->
      super#select_condition expr

(* Deal with some register constraints *)

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
