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

(* Instruction selection for the Power PC processor *)

open Cmm
open Arch
open Mach

(* Recognition of addressing modes *)

type addressing_expr =
    Asymbol of string
  | Alinear of expression
  | Aadd of expression * expression

let rec select_addr = function
    Cconst_symbol s ->
      (Asymbol s, 0, Debuginfo.none)
  | Cop((Caddi | Caddv | Cadda), [arg; Cconst_int m], dbg) ->
      let (a, n, _) = select_addr arg in (a, n + m, dbg)
  | Cop((Caddi | Caddv | Cadda), [Cconst_int m; arg], dbg) ->
      let (a, n, _) = select_addr arg in (a, n + m, dbg)
  | Cop((Caddi | Caddv | Cadda), [arg1; arg2], dbg) ->
      begin match (select_addr arg1, select_addr arg2) with
          ((Alinear e1, n1, _), (Alinear e2, n2, _)) ->
              (Aadd(e1, e2), n1 + n2, dbg)
        | _ ->
              (Aadd(arg1, arg2), 0, dbg)
      end
  | exp ->
      (Alinear exp, 0, Debuginfo.none)

(* Instruction selection *)

class selector = object (self)

inherit Selectgen.selector_generic as super

method is_immediate n = (n <= 32767) && (n >= -32768)

method select_addressing _chunk exp =
  match select_addr exp with
    (Asymbol s, d, _dbg) ->
      (Ibased(s, d), Ctuple [])
  | (Alinear e, d, _dbg) ->
      (Iindexed d, e)
  | (Aadd(e1, e2), d, dbg) ->
      if d = 0
      then (Iindexed2, Ctuple[e1; e2])
      else (Iindexed d, Cop(Cadda, [e1; e2], dbg))

method! select_operation op args dbg =
  match (op, args) with
  (* PowerPC does not support immediate operands for multiply high *)
    (Cmulhi, _) -> (Iintop Imulh, args)
  (* The and, or and xor instructions have a different range of immediate
     operands than the other instructions *)
  | (Cand, _) -> self#select_logical Iand args
  | (Cor, _) -> self#select_logical Ior args
  | (Cxor, _) -> self#select_logical Ixor args
  (* Recognize mult-add and mult-sub instructions *)
  | (Caddf, [Cop(Cmulf, [arg1; arg2], _); arg3]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Caddf, [arg3; Cop(Cmulf, [arg1; arg2], _)]) ->
      (Ispecific Imultaddf, [arg1; arg2; arg3])
  | (Csubf, [Cop(Cmulf, [arg1; arg2], _); arg3]) ->
      (Ispecific Imultsubf, [arg1; arg2; arg3])
  | _ ->
      super#select_operation op args dbg

method select_logical op = function
    [arg; Cconst_int n] when n >= 0 && n <= 0xFFFF ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int n; arg] when n >= 0 && n <= 0xFFFF ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

end

let fundecl f = (new selector)#emit_fundecl f
