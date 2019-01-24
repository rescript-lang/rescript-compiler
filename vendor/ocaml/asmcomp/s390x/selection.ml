(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt            *)
(*                          Bill O'Farrell, IBM                           *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2015 IBM (Bill O'Farrell with help from Tristan Amini).    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the Z processor *)

open Cmm
open Arch
open Mach

(* Recognition of addressing modes *)

exception Use_default

type addressing_expr =
  | Alinear of expression
  | Aadd of expression * expression

let rec select_addr = function
  | Cop((Caddi | Cadda | Caddv), [arg; Cconst_int m], _) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Caddi | Cadda | Caddv), [Cconst_int m; arg], _) ->
      let (a, n) = select_addr arg in (a, n + m)
  | Cop((Caddi | Cadda | Caddv), [arg1; arg2], _) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1), (Alinear e2, n2)) ->
              (Aadd(e1, e2), n1 + n2)
        | _ ->
              (Aadd(arg1, arg2), 0)
      end
  | exp ->
      (Alinear exp, 0)

(* Instruction selection *)

let pseudoregs_for_operation op arg res =
  match op with
  (* Two-address binary operations: arg.(0) and res.(0) must be the same *)
  | Iintop(Iadd|Isub|Imul|Iand|Ior|Ixor)  | Iaddf|Isubf|Imulf|Idivf ->
      ([|res.(0); arg.(1)|], res)
  | Ispecific _ ->
    ( [| arg.(0); arg.(1); res.(0) |], [| res.(0) |])
  (* One-address unary operations: arg.(0) and res.(0) must be the same *)
  |  Iintop_imm((Imul|Iand|Ior|Ixor), _) -> (res, res)
  (* Other instructions are regular *)
  | _ -> raise Use_default

class selector = object (self)

inherit Selectgen.selector_generic as super

method is_immediate n = n <= 0x7FFF_FFFF && n >= (-1-0x7FFF_FFFF)
  (* -1-.... : hack so that this can be compiled on 32-bit
     (cf 'make check_all_arches') *)

method select_addressing _chunk exp =
  let (a, d) = select_addr exp in
  (* 20-bit signed displacement *)
  if d < 0x80000 && d >= -0x80000 then begin
    match a with
    | Alinear e -> (Iindexed d, e)
    | Aadd(e1, e2) -> (Iindexed2 d, Ctuple [e1; e2])
  end else
    (Iindexed 0, exp)

method! select_operation op args dbg =
  match (op, args) with
  (* Z does not support immediate operands for multiply high *)
    (Cmulhi, _) -> (Iintop Imulh, args)
  (* The and, or and xor instructions have a different range of immediate
     operands than the other instructions *)
  | (Cand, _) ->
      self#select_logical Iand (-1 lsl 32 (*0x1_0000_0000*)) (-1) args
  | (Cor, _) -> self#select_logical Ior 0 (1 lsl 32 - 1 (*0xFFFF_FFFF*)) args
  | (Cxor, _) -> self#select_logical Ixor  0 (1 lsl 32 - 1 (*0xFFFF_FFFF*)) args
  (* Recognize mult-add and mult-sub instructions *)
  | (Caddf, [Cop(Cmulf, [arg1; arg2], _); arg3]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Caddf, [arg3; Cop(Cmulf, [arg1; arg2], _)]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Csubf, [Cop(Cmulf, [arg1; arg2], _); arg3]) ->
      (Ispecific Imultsubf, [arg1; arg2; arg3])
  | _ ->
      super#select_operation op args dbg

method select_logical op lo hi = function
    [arg; Cconst_int n] when n >= lo && n <= hi ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int n; arg] when n >= lo && n <= hi ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)


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
