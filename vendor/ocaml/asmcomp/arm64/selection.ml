(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the ARM processor *)

open Arch
open Cmm
open Mach

let is_offset chunk n =
   (n >= -256 && n <= 255)               (* 9 bits signed unscaled *)
|| (n >= 0 &&
    match chunk with     (* 12 bits unsigned, scaled by chunk size *)
    | Byte_unsigned | Byte_signed ->
        n < 0x1000
    | Sixteen_unsigned | Sixteen_signed ->
        n land 1 = 0 && n lsr 1 < 0x1000
    | Thirtytwo_unsigned | Thirtytwo_signed | Single ->
        n land 3 = 0 && n lsr 2 < 0x1000
    | Word_int | Word_val | Double | Double_u ->
        n land 7 = 0 && n lsr 3 < 0x1000)

(* An automaton to recognize ( 0+1+0* | 1+0+1* )

               0          1          0
              / \        / \        / \
              \ /        \ /        \ /
        -0--> [1] --1--> [2] --0--> [3]
       /
     [0]
       \
        -1--> [4] --0--> [5] --1--> [6]
              / \        / \        / \
              \ /        \ /        \ /
               1          0          1

The accepting states are 2, 3, 5 and 6. *)

let auto_table = [|   (* accepting?, next on 0, next on 1 *)
  (* state 0 *) (false, 1, 4);
  (* state 1 *) (false, 1, 2);
  (* state 2 *) (true,  3, 2);
  (* state 3 *) (true,  3, 7);
  (* state 4 *) (false, 5, 4);
  (* state 5 *) (true,  5, 6);
  (* state 6 *) (true,  7, 6);
  (* state 7 *) (false, 7, 7)   (* error state *)
|]

let rec run_automata nbits state input =
  let (acc, next0, next1) = auto_table.(state) in
  if nbits <= 0
  then acc
  else run_automata (nbits - 1)
                    (if input land 1 = 0 then next0 else next1)
                    (input asr 1)

(* We are very conservative wrt what ARM64 supports: we don't support
   repetitions of a 000111000 or 1110000111 pattern, just a single
   pattern of this kind. *)

let is_logical_immediate n =
  n <> 0 && n <> -1 && run_automata 64 0 n

(* If you update [inline_ops], you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
let inline_ops =
  [ "sqrt"; "caml_bswap16_direct"; "caml_int32_direct_bswap";
    "caml_int64_direct_bswap"; "caml_nativeint_direct_bswap" ]

let use_direct_addressing _symb =
  not !Clflags.dlcode

(* Instruction selection *)

class selector = object(self)

inherit Selectgen.selector_generic as super

method is_immediate n =
  let mn = -n in
  n land 0xFFF = n || n land 0xFFF_000 = n
  || mn land 0xFFF = mn || mn land 0xFFF_000 = mn

method! is_simple_expr = function
  (* inlined floating-point ops are simple if their arguments are *)
  | Cop(Cextcall (fn, _, _, _), args, _) when List.mem fn inline_ops ->
      List.for_all self#is_simple_expr args
  | e -> super#is_simple_expr e

method! effects_of e =
  match e with
  | Cop(Cextcall (fn, _, _, _), args, _) when List.mem fn inline_ops ->
      Selectgen.Effect_and_coeffect.join_list_map args self#effects_of
  | e -> super#effects_of e

method select_addressing chunk = function
  | Cop((Caddv | Cadda), [Cconst_symbol s; Cconst_int n], _)
    when use_direct_addressing s ->
      (Ibased(s, n), Ctuple [])
  | Cop((Caddv | Cadda), [arg; Cconst_int n], _)
    when is_offset chunk n ->
      (Iindexed n, arg)
  | Cop((Caddv | Cadda as op), [arg1; Cop(Caddi, [arg2; Cconst_int n], _)], dbg)
    when is_offset chunk n ->
      (Iindexed n, Cop(op, [arg1; arg2], dbg))
  | Cconst_symbol s
    when use_direct_addressing s ->
      (Ibased(s, 0), Ctuple [])
  | arg ->
      (Iindexed 0, arg)

method! select_operation op args dbg =
  match op with
  (* Integer addition *)
  | Caddi | Caddv | Cadda ->
      begin match args with
      (* Add immediate *)
      | [arg; Cconst_int n] when self#is_immediate n ->
          ((if n >= 0 then Iintop_imm(Iadd, n) else Iintop_imm(Isub, -n)),
           [arg])
      | [Cconst_int n; arg] when self#is_immediate n ->
          ((if n >= 0 then Iintop_imm(Iadd, n) else Iintop_imm(Isub, -n)),
           [arg])
      (* Shift-add *)
      | [arg1; Cop(Clsl, [arg2; Cconst_int n], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, n)), [arg1; arg2])
      | [arg1; Cop(Casr, [arg2; Cconst_int n], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, -n)), [arg1; arg2])
      | [Cop(Clsl, [arg1; Cconst_int n], _); arg2] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, n)), [arg2; arg1])
      | [Cop(Casr, [arg1; Cconst_int n], _); arg2] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftadd, -n)), [arg2; arg1])
      (* Multiply-add *)
      | [arg1; Cop(Cmuli, args2, dbg)] | [Cop(Cmuli, args2, dbg); arg1] ->
          begin match self#select_operation Cmuli args2 dbg with
          | (Iintop_imm(Ilsl, l), [arg3]) ->
              (Ispecific(Ishiftarith(Ishiftadd, l)), [arg1; arg3])
          | (Iintop Imul, [arg3; arg4]) ->
              (Ispecific Imuladd, [arg3; arg4; arg1])
          | _ ->
              super#select_operation op args dbg
          end
      | _ ->
          super#select_operation op args dbg
      end
  (* Integer subtraction *)
  | Csubi ->
      begin match args with
      (* Sub immediate *)
      | [arg; Cconst_int n] when self#is_immediate n ->
          ((if n >= 0 then Iintop_imm(Isub, n) else Iintop_imm(Iadd, -n)),
           [arg])
      (* Shift-sub *)
      | [arg1; Cop(Clsl, [arg2; Cconst_int n], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftsub, n)), [arg1; arg2])
      | [arg1; Cop(Casr, [arg2; Cconst_int n], _)] when n > 0 && n < 64 ->
          (Ispecific(Ishiftarith(Ishiftsub, -n)), [arg1; arg2])
      (* Multiply-sub *)
      | [arg1; Cop(Cmuli, args2, dbg)] ->
          begin match self#select_operation Cmuli args2 dbg with
          | (Iintop_imm(Ilsl, l), [arg3]) ->
              (Ispecific(Ishiftarith(Ishiftsub, l)), [arg1; arg3])
          | (Iintop Imul, [arg3; arg4]) ->
              (Ispecific Imulsub, [arg3; arg4; arg1])
          | _ ->
              super#select_operation op args dbg
          end
      | _ ->
          super#select_operation op args dbg
      end
  (* Checkbounds *)
  | Ccheckbound ->
      begin match args with
      | [Cop(Clsr, [arg1; Cconst_int n], _); arg2] when n > 0 && n < 64 ->
          (Ispecific(Ishiftcheckbound { shift = n; label_after_error = None; }),
            [arg1; arg2])
      | _ ->
          super#select_operation op args dbg
      end
  (* Integer multiplication *)
  (* ARM does not support immediate operands for multiplication *)
  | Cmuli ->
      (Iintop Imul, args)
  | Cmulhi ->
      (Iintop Imulh, args)
  (* Bitwise logical operations have a different range of immediate
     operands than the other instructions *)
  | Cand -> self#select_logical Iand args
  | Cor -> self#select_logical Ior args
  | Cxor -> self#select_logical Ixor args
  (* Recognize floating-point negate and multiply *)
  | Cnegf ->
      begin match args with
      | [Cop(Cmulf, args, _)] -> (Ispecific Inegmulf, args)
      | _ -> super#select_operation op args dbg
      end
  (* Recognize floating-point multiply and add/sub *)
  | Caddf ->
      begin match args with
      | [arg; Cop(Cmulf, args, _)] | [Cop(Cmulf, args, _); arg] ->
          (Ispecific Imuladdf, arg :: args)
      | _ ->
          super#select_operation op args dbg
      end
  | Csubf ->
      begin match args with
      | [arg; Cop(Cmulf, args, _)] ->
          (Ispecific Imulsubf, arg :: args)
      | [Cop(Cmulf, args, _); arg] ->
          (Ispecific Inegmulsubf, arg :: args)
      | _ ->
          super#select_operation op args dbg
      end
  (* Recognize floating-point square root *)
  | Cextcall("sqrt", _, _, _) ->
      (Ispecific Isqrtf, args)
  (* Recognize bswap instructions *)
  | Cextcall("caml_bswap16_direct", _, _, _) ->
      (Ispecific(Ibswap 16), args)
  | Cextcall("caml_int32_direct_bswap", _, _, _) ->
      (Ispecific(Ibswap 32), args)
  | Cextcall(("caml_int64_direct_bswap"|"caml_nativeint_direct_bswap"),
              _, _, _) ->
      (Ispecific (Ibswap 64), args)
  (* Other operations are regular *)
  | _ ->
      super#select_operation op args dbg

method select_logical op = function
  | [arg; Cconst_int n] when is_logical_immediate n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int n; arg] when is_logical_immediate n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

end

let fundecl f = (new selector)#emit_fundecl f
