(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module A = Simple_value_approx
module C = Inlining_cost
module I = Simplify_boxed_integer_ops
module S = Simplify_common

let phys_equal (approxs:A.t list) =
  match approxs with
  | [] | [_] | _ :: _ :: _ :: _ ->
      Misc.fatal_error "wrong number of arguments for equality"
  | [a1; a2] ->
    (* N.B. The following would be incorrect if the variables are not
       bound in the environment:
       match a1.var, a2.var with
       | Some v1, Some v2 when Variable.equal v1 v2 -> true
       | _ -> ...
    *)
    match a1.symbol, a2.symbol with
    | Some (s1, None), Some (s2, None) -> Symbol.equal s1 s2
    | Some (s1, Some f1), Some (s2, Some f2) -> Symbol.equal s1 s2 && f1 = f2
    | _ -> false

let is_known_to_be_some_kind_of_int (arg:A.descr) =
  match arg with
  | Value_int _ | Value_char _ | Value_constptr _ -> true
  | Value_block (_, _) | Value_float _ | Value_set_of_closures _
  | Value_closure _ | Value_string _ | Value_float_array _
  | A.Value_boxed_int _ | Value_unknown _ | Value_extern _
  | Value_symbol _ | Value_unresolved _ | Value_bottom -> false

let is_known_to_be_some_kind_of_block (arg:A.descr) =
  match arg with
  | Value_block _ | Value_float _ | Value_float_array _ | A.Value_boxed_int _
  | Value_closure _ | Value_string _ -> true
  | Value_set_of_closures _ | Value_int _ | Value_char _ | Value_constptr _
  | Value_unknown _ | Value_extern _ | Value_symbol _
  | Value_unresolved _ | Value_bottom -> false

let rec structurally_different (arg1:A.t) (arg2:A.t) =
  match arg1.descr, arg2.descr with
  | (Value_int n1 | Value_constptr n1), (Value_int n2 | Value_constptr n2)
    when n1 <> n2 ->
    true
  | Value_block (tag1, fields1), Value_block (tag2, fields2) ->
    not (Tag.equal tag1 tag2)
    || (Array.length fields1 <> Array.length fields2)
    || Misc.Stdlib.Array.exists2 structurally_different fields1 fields2
  | descr1, descr2 ->
    (* This is not very precise as this won't allow to distinguish
       blocks from strings for instance. This can be improved if it
       is deemed valuable. *)
    (is_known_to_be_some_kind_of_int descr1
     && is_known_to_be_some_kind_of_block descr2)
    || (is_known_to_be_some_kind_of_block descr1
        && is_known_to_be_some_kind_of_int descr2)

let phys_different (approxs:A.t list) =
  match approxs with
  | [] | [_] | _ :: _ :: _ :: _ ->
    Misc.fatal_error "wrong number of arguments for equality"
  | [a1; a2] ->
    structurally_different a1 a2

let primitive (p : Lambda.primitive) (args, approxs) expr dbg ~size_int
      ~big_endian : Flambda.named * A.t * Inlining_cost.Benefit.t =
  let fpc = !Clflags.float_const_prop in
  match p with
  | Pmakeblock(tag_int, tag_info, Asttypes.Immutable, shape) ->
    let tag = Tag.create_exn tag_int in
    let shape = match shape with
      | None -> List.map (fun _ -> Lambda.Pgenval) args
      | Some shape -> shape
    in
    let approxs = List.map2 A.augment_with_kind approxs shape in
    let shape = List.map2 A.augment_kind_with_approx approxs shape in
    Prim (Pmakeblock(tag_int, tag_info, Asttypes.Immutable, Some shape), args, dbg),
    A.value_block tag (Array.of_list approxs), C.Benefit.zero
  | Praise _ ->
    expr, A.value_bottom, C.Benefit.zero
  | Pignore -> begin
      match args, A.descrs approxs with
      | [arg], [(Value_int 0 | Value_constptr 0)] ->
        S.const_ptr_expr (Flambda.Expr (Var arg)) 0
      | _ -> S.const_ptr_expr expr 0
    end
  | Pmakearray(_, _) when approxs = [] ->
    Prim (Pmakeblock(0, Lambda.Blk_array, Asttypes.Immutable, Some []), [], dbg),
    A.value_block (Tag.create_exn 0) [||], C.Benefit.zero
  | Pmakearray (Pfloatarray, Mutable) ->
      let approx =
        A.value_mutable_float_array ~size:(List.length args)
      in
      expr, approx, C.Benefit.zero
  | Pmakearray (Pfloatarray, Immutable) ->
      let approx =
        A.value_immutable_float_array (Array.of_list approxs)
      in
      expr, approx, C.Benefit.zero
  | Pintcomp Ceq when phys_equal approxs ->
    S.const_bool_expr expr true
  | Pintcomp Cneq when phys_equal approxs ->
    S.const_bool_expr expr false
    (* N.B. Having [not (phys_equal approxs)] would not on its own tell us
       anything about whether the two values concerned are unequal.  To judge
       that, it would be necessary to prove that the approximations are
       different, which would in turn entail them being completely known.

       It may seem that in the case where we have two approximations each
       annotated with a symbol that we should be able to judge inequality
       even if part of the approximation description(s) are unknown.  This is
       unfortunately not the case.  Here is an example:

         let a = f 1
         let b = f 1
         let c = a, a
         let d = a, a

       If [Share_constants] is run before [f] is completely inlined (assuming
       [f] always generates the same result; effects of [f] aren't in fact
       relevant) then [c] and [d] will not be shared.  However if [f] is
       inlined later, [a] and [b] could be shared and thus [c] and [d] could
       be too.  As such, any intermediate non-aliasing judgement would be
       invalid. *)
  | Pintcomp Ceq when phys_different approxs ->
    S.const_bool_expr expr false
  | Pintcomp Cneq when phys_different approxs ->
    S.const_bool_expr expr true
    (* If two values are structurally different we are certain they can never
       be shared*)
  | _ ->
    match A.descrs approxs with
    | [Value_int x] ->
      begin match p with
      | Pidentity -> S.const_int_expr expr x
      | Pnot -> S.const_bool_expr expr (x = 0)
      | Pnegint -> S.const_int_expr expr (-x)
      | Pbswap16 -> S.const_int_expr expr (S.swap16 x)
      | Poffsetint y -> S.const_int_expr expr (x + y)
      | Pfloatofint when fpc -> S.const_float_expr expr (float_of_int x)
      | Pbintofint Pnativeint ->
        S.const_boxed_int_expr expr Nativeint (Nativeint.of_int x)
      | Pbintofint Pint32 -> S.const_boxed_int_expr expr Int32 (Int32.of_int x)
      | Pbintofint Pint64 -> S.const_boxed_int_expr expr Int64 (Int64.of_int x)
      | _ -> expr, A.value_unknown Other, C.Benefit.zero
      end
    | [(Value_int x | Value_constptr x); (Value_int y | Value_constptr y)] ->
      let shift_precond = 0 <= y && y < 8 * size_int in
      begin match p with
      | Paddint -> S.const_int_expr expr (x + y)
      | Psubint -> S.const_int_expr expr (x - y)
      | Pmulint -> S.const_int_expr expr (x * y)
      | Pdivint _ when y <> 0 -> S.const_int_expr expr (x / y)
      | Pmodint _ when y <> 0 -> S.const_int_expr expr (x mod y)
      | Pandint -> S.const_int_expr expr (x land y)
      | Porint -> S.const_int_expr expr (x lor y)
      | Pxorint -> S.const_int_expr expr (x lxor y)
      | Plslint when shift_precond -> S.const_int_expr expr (x lsl y)
      | Plsrint when shift_precond -> S.const_int_expr expr (x lsr y)
      | Pasrint when shift_precond -> S.const_int_expr expr (x asr y)
      | Pintcomp cmp -> S.const_comparison_expr expr cmp x y
      | Pisout -> S.const_bool_expr expr (y > x || y < 0)
      | _ -> expr, A.value_unknown Other, C.Benefit.zero
      end
    | [Value_char x; Value_char y] ->
      begin match p with
      | Pintcomp cmp -> S.const_comparison_expr expr cmp x y
      | _ -> expr, A.value_unknown Other, C.Benefit.zero
      end
    | [Value_constptr x] ->
      begin match p with
      (* [Pidentity] should probably never appear, but is here for
         completeness. *)
      | Pidentity -> S.const_ptr_expr expr x
      | Pnot -> S.const_bool_expr expr (x = 0)
      | Pisint -> S.const_bool_expr expr true
      | Poffsetint y -> S.const_ptr_expr expr (x + y)
      | Pctconst c ->
        begin match c with
        | Big_endian -> S.const_bool_expr expr big_endian
        | Word_size -> S.const_int_expr expr (8*size_int)
        | Int_size -> S.const_int_expr expr (8*size_int - 1)
        | Max_wosize ->
          (* CR-someday mshinwell: this function should maybe not live here. *)
          S.const_int_expr expr ((1 lsl ((8*size_int) - 10)) - 1)
        | Ostype_unix -> S.const_bool_expr expr (Sys.os_type = "Unix")
        | Ostype_win32 -> S.const_bool_expr expr (Sys.os_type = "Win32")
        | Ostype_cygwin -> S.const_bool_expr expr (Sys.os_type = "Cygwin")
        | Backend_type ->
          S.const_ptr_expr expr 0 (* tag 0 is the same as Native *)
        end
      | _ -> expr, A.value_unknown Other, C.Benefit.zero
      end
    | [Value_float (Some x)] when fpc ->
      begin match p with
      | Pintoffloat -> S.const_int_expr expr (int_of_float x)
      | Pnegfloat -> S.const_float_expr expr (-. x)
      | Pabsfloat -> S.const_float_expr expr (abs_float x)
      | _ -> expr, A.value_unknown Other, C.Benefit.zero
      end
    | [Value_float (Some n1); Value_float (Some n2)] when fpc ->
      begin match p with
      | Paddfloat -> S.const_float_expr expr (n1 +. n2)
      | Psubfloat -> S.const_float_expr expr (n1 -. n2)
      | Pmulfloat -> S.const_float_expr expr (n1 *. n2)
      | Pdivfloat -> S.const_float_expr expr (n1 /. n2)
      | Pfloatcomp c  -> S.const_comparison_expr expr c n1 n2
      | _ -> expr, A.value_unknown Other, C.Benefit.zero
      end
    | [A.Value_boxed_int(A.Nativeint, n)] ->
      I.Simplify_boxed_nativeint.simplify_unop p Nativeint expr n
    | [A.Value_boxed_int(A.Int32, n)] ->
      I.Simplify_boxed_int32.simplify_unop p Int32 expr n
    | [A.Value_boxed_int(A.Int64, n)] ->
      I.Simplify_boxed_int64.simplify_unop p Int64 expr n
    | [A.Value_boxed_int(A.Nativeint, n1);
       A.Value_boxed_int(A.Nativeint, n2)] ->
      I.Simplify_boxed_nativeint.simplify_binop p Nativeint expr n1 n2
    | [A.Value_boxed_int(A.Int32, n1); A.Value_boxed_int(A.Int32, n2)] ->
      I.Simplify_boxed_int32.simplify_binop p Int32 expr n1 n2
    | [A.Value_boxed_int(A.Int64, n1); A.Value_boxed_int(A.Int64, n2)] ->
      I.Simplify_boxed_int64.simplify_binop p Int64 expr n1 n2
    | [A.Value_boxed_int(A.Nativeint, n1); Value_int n2] ->
      I.Simplify_boxed_nativeint.simplify_binop_int p Nativeint expr n1 n2
        ~size_int
    | [A.Value_boxed_int(A.Int32, n1); Value_int n2] ->
      I.Simplify_boxed_int32.simplify_binop_int p Int32 expr n1 n2
        ~size_int
    | [A.Value_boxed_int(A.Int64, n1); Value_int n2] ->
      I.Simplify_boxed_int64.simplify_binop_int p Int64 expr n1 n2
        ~size_int
    | [Value_block _] when p = Lambda.Pisint ->
      S.const_bool_expr expr false
    | [Value_string { size }]
      when (p = Lambda.Pstringlength || p = Lambda.Pbyteslength) ->
      S.const_int_expr expr size
    | [Value_string { size; contents = Some s };
       (Value_int x | Value_constptr x)] when x >= 0 && x < size ->
        begin match p with
        | Pstringrefu
        | Pstringrefs
        | Pbytesrefu
        | Pbytesrefs ->
          S.const_char_expr (Prim(Pstringrefu, args, dbg)) s.[x]
        | _ -> expr, A.value_unknown Other, C.Benefit.zero
        end
    | [Value_string { size; contents = None };
       (Value_int x | Value_constptr x)]
      when x >= 0 && x < size && p = Lambda.Pstringrefs ->
        Flambda.Prim (Pstringrefu, args, dbg),
          A.value_unknown Other,
          (* we improved it, but there is no way to account for that: *)
          C.Benefit.zero
    | [Value_string { size; contents = None };
       (Value_int x | Value_constptr x)]
      when x >= 0 && x < size && p = Lambda.Pbytesrefs ->
        Flambda.Prim (Pbytesrefu, args, dbg),
          A.value_unknown Other,
          (* we improved it, but there is no way to account for that: *)
          C.Benefit.zero

    | [Value_float_array { size; contents }] ->
        begin match p with
        | Parraylength _ -> S.const_int_expr expr size
        | Pfloatfield (i,_) ->
          begin match contents with
          | A.Contents a when i >= 0 && i < size ->
            begin match A.check_approx_for_float a.(i) with
            | None -> expr, a.(i), C.Benefit.zero
            | Some v -> S.const_float_expr expr v
            end
          | Contents _ | Unknown_or_mutable ->
            expr, A.value_unknown Other, C.Benefit.zero
          end
        | _ -> expr, A.value_unknown Other, C.Benefit.zero
        end
    | _ ->
      match Semantics_of_primitives.return_type_of_primitive p with
      | Float ->
        expr, A.value_any_float, C.Benefit.zero
      | Other ->
        expr, A.value_unknown Other, C.Benefit.zero
