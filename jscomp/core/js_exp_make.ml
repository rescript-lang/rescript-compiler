(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017 - Hongbo Zhang, Authors of ReScript
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

let no_side_effect = Js_analyzer.no_side_effect_expression

type t = J.expression

(*
  [remove_pure_sub_exp x]
  Remove pure part of the expression (minor optimization)
  and keep the non-pure part while preserve the semantics 
  (modulo return value)
  It will return None if  [x] is pure
 *)
let rec remove_pure_sub_exp (x : t) : t option =
  match x.expression_desc with
  | Var _ | Str _ | Number _ -> None (* Can be refined later *)
  | Array_index (a, b) ->
      if is_pure_sub_exp a && is_pure_sub_exp b then None else Some x
  | Array (xs, _mutable_flag) ->
      if Ext_list.for_all xs is_pure_sub_exp then None else Some x
  | Seq (a, b) -> (
      match (remove_pure_sub_exp a, remove_pure_sub_exp b) with
      | None, None -> None
      | Some u, Some v -> Some { x with expression_desc = Seq (u, v) }
      (* may still have some simplification*)
      | None, (Some _ as v) -> v
      | (Some _ as u), None -> u)
  | _ -> Some x

and is_pure_sub_exp (x : t) = remove_pure_sub_exp x = None

(* let mk ?comment exp : t =
   {expression_desc = exp ; comment  } *)

let var ?comment id : t = { expression_desc = Var (Id id); comment }

(* only used in property access,
    Invariant: it should not call an external module .. *)

let js_global ?comment (v : string) = var ?comment (Ext_ident.create_js v)
let undefined : t = { expression_desc = Undefined {is_unit = false}; comment = None }
let nil : t = { expression_desc = Null; comment = None }

let call ?comment ~info e0 args : t =
  { expression_desc = Call (e0, args, info); comment }

(* TODO: optimization when es is known at compile time
    to be an array
*)
let flat_call ?comment e0 es : t =
  { expression_desc = FlatCall (e0, es); comment }

let tagged_template ?comment call_expr string_args value_args : t =
  { expression_desc = Tagged_template (call_expr, string_args, value_args); comment }

let runtime_var_dot ?comment (x : string) (e1 : string) : J.expression =
  {
    expression_desc =
      Var
        (Qualified ({ id = Ident.create_persistent x; kind = Runtime; dynamic_import = false }, Some e1));
    comment;
  }

let ml_var_dot ?comment ?(dynamic_import = false) (id : Ident.t) e : J.expression =
  { expression_desc = Var (Qualified ({ id; kind = Ml; dynamic_import }, Some e)); comment }

(** 
   module as a value 
   {[
     var http = require("http")
   ]}
*)
let external_var_field ?import_attributes ?comment ~external_name:name (id : Ident.t) ~field
    ~default : t =
  {
    expression_desc =
      Var (Qualified ({ id; kind = External { name; default; import_attributes }; dynamic_import = false }, Some field));
    comment;
  }

let external_var ?import_attributes ?comment ~external_name (id : Ident.t) : t =
  {
    expression_desc =
      Var
        (Qualified
           ( { id; kind = External { name = external_name; default = false; import_attributes }; dynamic_import = false },
             None ));
    comment;
  }

let ml_module_as_var ?comment ?(dynamic_import = false) (id : Ident.t) : t =
  { expression_desc = Var (Qualified ({ id; kind = Ml; dynamic_import }, None)); comment }

(* Static_index .....................**)
let runtime_call module_name fn_name args =
  call ~info:Js_call_info.builtin_runtime_call
    (runtime_var_dot module_name fn_name)
    args

let pure_runtime_call module_name fn_name args =
  call ~comment:Literals.pure ~info:Js_call_info.builtin_runtime_call
    (runtime_var_dot module_name fn_name)
    args

let runtime_ref module_name fn_name = runtime_var_dot module_name fn_name

let str ?(delim = J.DNone) ?comment txt : t =
  { expression_desc = Str { txt; delim }; comment }

let raw_js_code ?comment info s : t =
  {
    expression_desc = Raw_js_code { code = String.trim s; code_info = info };
    comment;
  }

let array ?comment mt es : t = { expression_desc = Array (es, mt); comment }
let some_comment = None

let optional_block e : J.expression =
  { expression_desc = Optional_block (e, false); comment = some_comment }

let optional_not_nest_block e : J.expression =
  { expression_desc = Optional_block (e, true); comment = None }

(** used in normal property
    like [e.length], no dependency introduced
*)
let dot ?comment (e0 : t) (e1 : string) : t =
  { expression_desc = Static_index (e0, e1, None); comment }

let module_access (e : t) (name : string) (pos : int32) =
  let name = Ext_ident.convert name in
  match e.expression_desc with
  | Caml_block (l, _, _, _) when no_side_effect e -> (
      match Ext_list.nth_opt l (Int32.to_int pos) with
      | Some x -> x
      | None ->
          { expression_desc = Static_index (e, name, Some pos); comment = None }
      )
  | _ -> { expression_desc = Static_index (e, name, Some pos); comment = None }

let make_block ?comment (tag : t) (tag_info : J.tag_info) (es : t list)
    (mutable_flag : J.mutable_flag) : t =
  { expression_desc = Caml_block (es, mutable_flag, tag, tag_info); comment }

module L = Literals

(* ATTENTION: this is relevant to how we encode string, boolean *)
let typeof ?comment (e : t) : t =
  match e.expression_desc with
  | Number _ | Length _ -> str ?comment L.js_type_number
  | Str _ -> str ?comment L.js_type_string
  | Array _ -> str ?comment L.js_type_object
  | Bool _ -> str ?comment L.js_type_boolean
  | _ -> { expression_desc = Typeof e; comment }

let instanceof ?comment (e0 : t) (e1: t) : t =
  { expression_desc = Bin (InstanceOf, e0, e1); comment }

let is_array  (e0 : t) : t =
  let f = str "Array.isArray" ~delim:DNoQuotes in
  { expression_desc = Call (f, [e0], Js_call_info.ml_full_call); comment=None }
  
let new_ ?comment e0 args : t =
  { expression_desc = New (e0, Some args); comment }

let unit : t = { expression_desc = Undefined {is_unit = true}; comment = None }

(* let math ?comment v args  : t =
   {comment ; expression_desc = Math(v,args)} *)

(* we can do constant folding here, but need to make sure the result is consistent
   {[
     let f x = string_of_int x
     ;; f 3
   ]}
   {[
     string_of_int 3
   ]}
   Used in [string_of_int] and format "%d"
   TODO: optimize
*)

(* Attention: Shared *mutable state* is evil,
   [Js_fun_env.empty] is a mutable state ..
*)

let ocaml_fun ?comment ?immutable_mask ~return_unit ~async ~one_unit_arg ?directive params body : t =
  let params = if one_unit_arg then [] else params in
  let len = List.length params in
  {
    expression_desc =
      Fun
        {
          is_method = false;
          params;
          body;
          env = Js_fun_env.make ?immutable_mask len;
          return_unit;
          async;
          directive;
        };
    comment;
  }

let method_ ?comment ?immutable_mask ~return_unit params body : t =
  let len = List.length params in
  {
    expression_desc =
      Fun
        {
          is_method = true;
          params;
          body;
          env = Js_fun_env.make ?immutable_mask len;
          return_unit;
          async = false;
          directive = None;
        };
    comment;
  }

(** ATTENTION: This is coupuled with {!Caml_obj.caml_update_dummy} *)
let dummy_obj ?comment (info : Lam_tag_info.t) : t =
  (* TODO:
     for record it is [{}]
     for other it is [[]]
  *)
  match info with
  | Blk_record _ | Blk_module _ | Blk_constructor _ | Blk_record_inlined _
  | Blk_poly_var _ | Blk_extension | Blk_record_ext _ ->
      { comment; expression_desc = Object [] }
  | Blk_tuple | Blk_module_export _ ->
      { comment; expression_desc = Array ([], Mutable) }
  | Blk_some | Blk_some_not_nested | Blk_lazy_general -> assert false

(* TODO: complete
    pure ...
*)
let rec seq ?comment (e0 : t) (e1 : t) : t =
  match (e0.expression_desc, e1.expression_desc) with
  | ( ( Seq (a, { expression_desc = Number _ | Undefined _ })
      | Seq ({ expression_desc = Number _ | Undefined _ }, a) ),
      _ ) ->
      seq ?comment a e1
  | _, Seq ({ expression_desc = Number _ | Undefined _ }, a) ->
      (* Return value could not be changed*)
      seq ?comment e0 a
  | _, Seq (a, ({ expression_desc = Number _ | Undefined _ } as v)) ->
      (* Return value could not be changed*)
      seq ?comment (seq e0 a) v
  | (Number _ | Var _ | Undefined _), _ -> e1
  | _ -> { expression_desc = Seq (e0, e1); comment }

let fuse_to_seq x xs = if xs = [] then x else Ext_list.fold_left xs x seq

(* let empty_string_literal : t =
   {expression_desc = Str (true,""); comment = None} *)

let zero_int_literal : t =
  { expression_desc = Number (Int { i = 0l; c = None }); comment = None }

let one_int_literal : t =
  { expression_desc = Number (Int { i = 1l; c = None }); comment = None }

let two_int_literal : t =
  { expression_desc = Number (Int { i = 2l; c = None }); comment = None }

let three_int_literal : t =
  { expression_desc = Number (Int { i = 3l; c = None }); comment = None }

let four_int_literal : t =
  { expression_desc = Number (Int { i = 4l; c = None }); comment = None }

let five_int_literal : t =
  { expression_desc = Number (Int { i = 5l; c = None }); comment = None }

let six_int_literal : t =
  { expression_desc = Number (Int { i = 6l; c = None }); comment = None }

let seven_int_literal : t =
  { expression_desc = Number (Int { i = 7l; c = None }); comment = None }

let eight_int_literal : t =
  { expression_desc = Number (Int { i = 8l; c = None }); comment = None }

let nine_int_literal : t =
  { expression_desc = Number (Int { i = 9l; c = None }); comment = None }

let int ?comment ?c i : t = { expression_desc = Number (Int { i; c }); comment }

let bigint ?comment sign i : t = { expression_desc = Number (BigInt {positive=sign; value=i}); comment}

let zero_bigint_literal : t = {expression_desc = Number (BigInt {positive=true; value="0"}); comment = None}

let small_int i : t =
  match i with
  | 0 -> zero_int_literal
  | 1 -> one_int_literal
  | 2 -> two_int_literal
  | 3 -> three_int_literal
  | 4 -> four_int_literal
  | 5 -> five_int_literal
  | 6 -> six_int_literal
  | 7 -> seven_int_literal
  | 8 -> eight_int_literal
  | 9 -> nine_int_literal
  | i -> int (Int32.of_int i)

let true_ : t = { comment = None; expression_desc = Bool true }
let false_ : t = { comment = None; expression_desc = Bool false }
let bool v = if v then true_ else false_

let float ?comment f : t = { expression_desc = Number (Float { f }); comment }

let zero_float_lit : t =
  { expression_desc = Number (Float { f = "0." }); comment = None }

let float_mod ?comment e1 e2 : J.expression =
  { comment; expression_desc = Bin (Mod, e1, e2) }

let array_index ?comment (e0 : t) (e1 : t) : t =
  match (e0.expression_desc, e1.expression_desc) with
  | Array (l, _), Number (Int { i; _ })
  (* Float i -- should not appear here *)
    when no_side_effect e0 -> (
      match Ext_list.nth_opt l (Int32.to_int i) with
      | None -> { expression_desc = Array_index (e0, e1); comment }
      | Some x -> x (* FIX #3084*))
  | _ -> { expression_desc = Array_index (e0, e1); comment }

let array_index_by_int ?comment (e : t) (pos : int32) : t =
  match e.expression_desc with
  | Array (l, _) (* Float i -- should not appear here *)
  | Caml_block (l, _, _, _)
    when no_side_effect e -> (
      match Ext_list.nth_opt l (Int32.to_int pos) with
      | Some x -> x
      | None ->
          {
            expression_desc = Array_index (e, int ?comment pos);
            comment = None;
          })
  | _ -> { expression_desc = Array_index (e, int ?comment pos); comment = None }

let record_access (e : t) (name : string) (pos : int32) =
  (* let name = Ext_ident.convert name in  *)
  match e.expression_desc with
  | Array (l, _) (* Float i -- should not appear here *)
  | Caml_block (l, _, _, _)
    when no_side_effect e -> (
      match Ext_list.nth_opt l (Int32.to_int pos) with
      | Some x -> x
      | None ->
          { expression_desc = Static_index (e, name, Some pos); comment = None }
      )
  | _ -> { expression_desc = Static_index (e, name, Some pos); comment = None }

(* The same as {!record_access} except tag*)
let inline_record_access = record_access

let variant_access (e : t) (pos : int32) =
  inline_record_access e ("_" ^ Int32.to_string pos) pos

let cons_access (e : t) (pos : int32) =
  inline_record_access e
    (match pos with
    | 0l -> Literals.hd
    | 1l -> Literals.tl
    | _ -> "_" ^ Int32.to_string pos)
    pos

let poly_var_tag_access (e : t) =
  match e.expression_desc with
  | Caml_block (l, _, _, _) when no_side_effect e -> (
      match l with x :: _ -> x | [] -> assert false)
  | _ ->
      {
        expression_desc = Static_index (e, Literals.polyvar_hash, Some 0l);
        comment = None;
      }

let poly_var_value_access (e : t) =
  match e.expression_desc with
  | Caml_block (l, _, _, _) when no_side_effect e -> (
      match l with _ :: v :: _ -> v | _ -> assert false)
  | _ ->
      {
        expression_desc = Static_index (e, Literals.polyvar_value, Some 1l);
        comment = None;
      }

let extension_access (e : t) ?name (pos : int32) : t =
  match e.expression_desc with
  | Array (l, _) (* Float i -- should not appear here *)
  | Caml_block (l, _, _, _)
    when no_side_effect e -> (
      match Ext_list.nth_opt l (Int32.to_int pos) with
      | Some x -> x
      | None ->
          let name =
            match name with Some n -> n | None -> "_" ^ Int32.to_string pos
          in
          { expression_desc = Static_index (e, name, Some pos); comment = None }
      )
  | _ ->
      let name =
        match name with Some n -> n | None -> "_" ^ Int32.to_string pos
      in
      { expression_desc = Static_index (e, name, Some pos); comment = None }

let string_index ?comment (e0 : t) (e1 : t) : t =
  match (e0.expression_desc, e1.expression_desc) with
  | Str { txt }, Number (Int { i; _ }) ->
      (* Don't optimize {j||j} *)
      let i = Int32.to_int i in
      if i >= 0 && i < String.length txt then
        (* TODO: check exception when i is out of range..
           RangeError?
        *)
        str (String.make 1 txt.[i])
      else { expression_desc = String_index (e0, e1); comment }
  | _ -> { expression_desc = String_index (e0, e1); comment }

let assign ?comment e0 e1 : t = { expression_desc = Bin (Eq, e0, e1); comment }

let assign_by_exp (e : t) index value : t =
  match e.expression_desc with
  | Array _
  (*
     Temporary block -- address not held
     Optimize cases like this which is really 
     rare {[
                  (ref x) :=  3
                ]}
             *)
  | Caml_block _
    when no_side_effect e && no_side_effect index ->
      value
  | _ ->
      assign { expression_desc = Array_index (e, index); comment = None } value

let assign_by_int ?comment e0 (index : int32) value =
  assign_by_exp e0 (int ?comment index) value

let record_assign (e : t) (pos : int32) (name : string) (value : t) =
  match e.expression_desc with
  | Array _
  (*
     Temporary block -- address not held
     Optimize cases like this which is really 
     rare {[
                  (ref x) :=  3
                ]}
             *)
  | Caml_block _
    when no_side_effect e ->
      value
  | _ ->
      assign
        { expression_desc = Static_index (e, name, Some pos); comment = None }
        value

let extension_assign (e : t) (pos : int32) name (value : t) =
  match e.expression_desc with
  | Array _
  (*
           Temporary block -- address not held
           Optimize cases like this which is really 
           rare {[
                  (ref x) :=  3
                ]}
             *)
  | Caml_block _
    when no_side_effect e ->
      value
  | _ ->
      assign
        { expression_desc = Static_index (e, name, Some pos); comment = None }
        value

(* This is a property access not external module *)

let array_length ?comment (e : t) : t =
  match e.expression_desc with
  (* TODO: use array instead? *)
  | (Array (l, _) | Caml_block (l, _, _, _)) when no_side_effect e ->
      int ?comment (Int32.of_int (List.length l))
  | _ -> { expression_desc = Length (e, Array); comment }

let string_length ?comment (e : t) : t =
  match e.expression_desc with
  | Str { txt; delim = DNone } -> int ?comment (Int32.of_int (String.length txt))
  (* No optimization for {j||j}*)
  | _ -> { expression_desc = Length (e, String); comment }

(* TODO: use [Buffer] instead? *)
let bytes_length ?comment (e : t) : t =
  match e.expression_desc with
  | Array (l, _) -> int ?comment (Int32.of_int (List.length l))
  | _ -> { expression_desc = Length (e, Bytes); comment }

let function_length ?comment (e : t) : t =
  match e.expression_desc with
  | Fun { is_method; params } ->
      let params_length = List.length params in
      int ?comment
        (Int32.of_int (if is_method then params_length - 1 else params_length))
  | _ -> { expression_desc = Length (e, Function); comment }

(** no dependency introduced *)
(* let js_global_dot ?comment (x : string)  (e1 : string) : t =
   { expression_desc = Static_index (js_global x,  e1,None); comment}
*)

let rec string_append ?comment (e : t) (el : t) : t =
  let concat a b ~delim =
    { e with expression_desc = Str { txt = a ^ b; delim } }
  in
  match (e.expression_desc, el.expression_desc) with
  | Str { txt = ""}, _ -> el
  | _, Str { txt = ""} -> e
  | ( Str { txt = a; delim },
      String_append ({ expression_desc = Str { txt = b; delim = delim_ } }, c) )
    when delim = delim_ ->
      string_append ?comment (concat a b ~delim) c
  | ( String_append (c, { expression_desc = Str { txt = b; delim } }),
      Str { txt = a; delim = delim_ } )
    when delim = delim_ ->
      string_append ?comment c (concat b a ~delim)
  | ( String_append (a, { expression_desc = Str { txt = b; delim } }),
      String_append ({ expression_desc = Str { txt = c; delim = delim_ } }, d) )
    when delim = delim_ ->
      string_append ?comment (string_append a (concat b c ~delim)) d
  | Str { txt = a; delim }, Str { txt = b; delim = delim_ } when delim = delim_
    ->
      { (concat a b ~delim) with comment }
  | _, _ -> { comment; expression_desc = String_append (e, el) }

let obj ?comment properties : t =
  { expression_desc = Object properties; comment }

let str_equal (txt0:string) (delim0:External_arg_spec.delim) txt1 delim1 =
  if delim0 = delim1 then
    if Ext_string.equal txt0 txt1 then Some true
    else if Ast_utf8_string.simple_comparison txt0 && Ast_utf8_string.simple_comparison txt1
    then Some false
    else None
  else None

let rec triple_equal ?comment (e0 : t) (e1 : t) : t =
  match (e0.expression_desc, e1.expression_desc) with
  | ( (Null | Undefined _),
      (Bool _ | Number _ | Typeof _ | Fun _ | Array _ | Caml_block _) )
    when no_side_effect e1 ->
      false_
  | ( (Bool _ | Number _ | Typeof _ | Fun _ | Array _ | Caml_block _),
      (Null | Undefined _) )
    when no_side_effect e0 ->
      false_
  | Number (Int { i = i0; _ }), Number (Int { i = i1; _ }) -> bool (i0 = i1)
  | Optional_block (a, _), Optional_block (b, _) -> triple_equal ?comment a b
  | Undefined _, Optional_block _
  | Optional_block _, Undefined _
  | Null, Undefined _
  | Undefined _, Null ->
      false_
  | Null, Null | Undefined _, Undefined _ -> true_
  | _ -> { expression_desc = Bin (EqEqEq, e0, e1); comment }

let bin ?comment (op : J.binop) (e0 : t) (e1 : t) : t =
  match (op, e0.expression_desc, e1.expression_desc) with
  | EqEqEq, _, _ -> triple_equal ?comment e0 e1
  | Ge, Length (e, _), Number (Int { i = 0l }) when no_side_effect e ->
      true_ (* x.length >=0 | [x] is pure  -> true*)
  | Gt, Length (_, _), Number (Int { i = 0l }) ->
      (* [e] is kept so no side effect check needed *)
      { expression_desc = Bin (NotEqEq, e0, e1); comment }
  | _ -> { expression_desc = Bin (op, e0, e1); comment }

(* TODO: Constant folding, Google Closure will do that?,
   Even if Google Clsoure can do that, we will see how it interact with other
   optimizations
   We wrap all boolean functions here, since OCaml boolean is a
   bit different from Javascript, so that we can change it in the future

   {[ a && (b && c) === (a && b ) && c ]}
     is not used: benefit is not clear
     | Int_of_boolean e10, Bin(And, {expression_desc = Int_of_boolean e20 }, e3)
      ->
      and_ ?comment
        { e1 with expression_desc
                  =
                    J.Int_of_boolean { expression_desc = Bin (And, e10,e20); comment = None}
        }
        e3
   Note that
   {[ "" && 3 ]}
     return  "" instead of false, so [e1] is indeed useful
   optimization if [e1 = e2], then and_ e1 e2 -> e2
     be careful for side effect
*)

let rec filter_bool (e: t) ~j ~b = match e.expression_desc with
  | Bin (And, e1, e2) ->
    (match (filter_bool e1 ~j ~b, filter_bool e2 ~j ~b) with
    | None, None -> None
    | Some e, None
    | None, Some e -> Some e
    | Some e1, Some e2 ->
      Some {e with expression_desc = Bin (And, e1, e2)} )
  | Bin (Or, e1, e2) ->
    (match (filter_bool e1 ~j ~b, filter_bool e2 ~j ~b) with
    | None, _ | _, None ->
      None
    | Some e1, Some e2 ->
      Some {e with expression_desc = Bin (Or, e1, e2)} )
  | Bin
    ( NotEqEq,
      {expression_desc = Typeof {expression_desc = Var i}},
      {expression_desc = Str {txt}}) when Js_op_util.same_vident i j ->
    if txt <> "bool"
    then None
    else assert false
  | Js_not {expression_desc =
    Call ({expression_desc = Str {txt = "Array.isArray"}},
      [{expression_desc = Var i}], _)} when Js_op_util.same_vident i j ->
    None
  | _ -> Some e

let and_ ?comment (e1 : t) (e2 : t) : t =
  match (e1.expression_desc, e2.expression_desc) with
  | Var i, Var j when Js_op_util.same_vident i j -> e1
  | Var i, Bin (And, { expression_desc = Var j; _ }, _)
    when Js_op_util.same_vident i j ->
      e2
  | Var i, Bin (And, l, ({ expression_desc = Var j; _ } as r))
    when Js_op_util.same_vident i j ->
      { e2 with expression_desc = Bin (And, r, l) }
  | ( Bin (NotEqEq, { expression_desc = Var i }, { expression_desc = Undefined _ }),
      Bin
        ( EqEqEq,
          { expression_desc = Var j },
          { expression_desc = Str _ | Number _ } ) )
    when Js_op_util.same_vident i j ->
      e2
  | ( _,
      Bin
      ( EqEqEq,
        { expression_desc = Var j },
        { expression_desc = Bool b } )
        ) ->
    (match filter_bool e1 ~j ~b with
     | None -> e2
     | Some e1 -> { expression_desc = Bin (And, e1, e2); comment })
  | _, _ -> { expression_desc = Bin (And, e1, e2); comment }

let or_ ?comment (e1 : t) (e2 : t) =
  match (e1.expression_desc, e2.expression_desc) with
  | Var i, Var j when Js_op_util.same_vident i j -> e1
  | Var i, Bin (Or, { expression_desc = Var j; _ }, _)
    when Js_op_util.same_vident i j ->
      e2
  | Var i, Bin (Or, l, ({ expression_desc = Var j; _ } as r))
    when Js_op_util.same_vident i j ->
      { e2 with expression_desc = Bin (Or, r, l) }
  | _, _ -> { expression_desc = Bin (Or, e1, e2); comment }

(* return a value of type boolean *)
(* TODO:
     when comparison with Int
     it is right that !(x > 3 ) -> x <= 3 *)
let not (e : t) : t =
  match e.expression_desc with
  | Number (Int { i; _ }) -> bool (i = 0l)
  | Js_not e -> e
  | Bool b -> if b then false_ else true_
  | Bin (EqEqEq, e0, e1) -> { e with expression_desc = Bin (NotEqEq, e0, e1) }
  | Bin (NotEqEq, e0, e1) -> { e with expression_desc = Bin (EqEqEq, e0, e1) }
  | Bin (Lt, a, b) -> { e with expression_desc = Bin (Ge, a, b) }
  | Bin (Ge, a, b) -> { e with expression_desc = Bin (Lt, a, b) }
  | Bin (Le, a, b) -> { e with expression_desc = Bin (Gt, a, b) }
  | Bin (Gt, a, b) -> { e with expression_desc = Bin (Le, a, b) }
  | _ -> { expression_desc = Js_not e; comment = None }

let not_empty_branch (x : t) =
  match x.expression_desc with
  | Number (Int { i = 0l }) | Undefined _ -> false
  | _ -> true

let rec econd ?comment (pred : t) (ifso : t) (ifnot : t) : t =
  match (pred.expression_desc, ifso.expression_desc, ifnot.expression_desc) with
  | Bool false, _, _ -> ifnot
  | Number (Int { i = 0l; _ }), _, _ -> ifnot
  | (Number _ | Array _ | Caml_block _), _, _ when no_side_effect pred ->
      ifso (* a block can not be false in OCAML, CF - relies on flow inference*)
  | Bool true, _, _ -> ifso
  | _, Cond (pred1, ifso1, ifnot1), _
    when Js_analyzer.eq_expression ifnot1 ifnot ->
      (* {[
           if b then (if p1 then branch_code0 else branch_code1)
           else branch_code1
         ]}
         is equivalent to
         {[
           if b && p1 then branch_code0 else branch_code1
         ]}
      *)
      econd (and_ pred pred1) ifso1 ifnot
  | _, Cond (pred1, ifso1, ifnot1), _ when Js_analyzer.eq_expression ifso1 ifnot
    ->
      econd (and_ pred (not pred1)) ifnot1 ifnot
  | _, _, Cond (pred1, ifso1, ifnot1) when Js_analyzer.eq_expression ifso ifso1
    ->
      econd (or_ pred pred1) ifso ifnot1
  | _, _, Cond (pred1, ifso1, ifnot1) when Js_analyzer.eq_expression ifso ifnot1
    ->
      econd (or_ pred (not pred1)) ifso ifso1
  | Js_not e, _, _ when not_empty_branch ifnot -> econd ?comment e ifnot ifso
  | ( _,
      Seq (a, { expression_desc = Undefined _ }),
      Seq (b, { expression_desc = Undefined _ }) ) ->
      seq (econd ?comment pred a b) undefined
  | _ ->
      if Js_analyzer.eq_expression ifso ifnot then
        if no_side_effect pred then ifso else seq ?comment pred ifso
      else { expression_desc = Cond (pred, ifso, ifnot); comment }

let rec float_equal ?comment (e0 : t) (e1 : t) : t =
  match (e0.expression_desc, e1.expression_desc) with
  | Number (Int { i = i0; _ }), Number (Int { i = i1 }) -> bool (i0 = i1)
  | Undefined _, Undefined _ -> true_
  (* | (Bin(Bor,
         {expression_desc = Number(Int {i = 0l; _})},
         ({expression_desc = Caml_block_tag _; _} as a ))
     |
      Bin(Bor,
          ({expression_desc = Caml_block_tag _; _} as a),
          {expression_desc = Number (Int {i = 0l; _})})),
     Number (Int {i = 0l;}) when e1.comment = None
     ->  (** (x.tag | 0) === 0  *)
     not  a *)
  | ( ( Bin
          ( Bor,
            { expression_desc = Number (Int { i = 0l; _ }) },
            ({ expression_desc = Caml_block_tag _; _ } as a) )
      | Bin
          ( Bor,
            ({ expression_desc = Caml_block_tag _; _ } as a),
            { expression_desc = Number (Int { i = 0l; _ }) } ) ),
      Number _ ) ->
      (* for sure [i <> 0 ]*)
      (* since a is integer, if we guarantee there is no overflow
         of a
         then [a | 0] is a nop unless a is undefined
         (which is applicable when applied to tag),
         obviously tag can not be overflowed.
         if a is undefined, then [ a|0===0 ] is true
         while [a === 0 ] is not true
         [a|0 === non_zero] is false and [a===non_zero] is false
         so we can not eliminate when the tag is zero
      *)
      float_equal ?comment a e1
  | Number (Float { f = f0; _ }), Number (Float { f = f1 }) when f0 = f1 ->
      true_
  | _ -> { expression_desc = Bin (EqEqEq, e0, e1); comment }

let int_equal = float_equal

let string_equal ?comment (e0 : t) (e1 : t) : t =
  let default () : t = { expression_desc = Bin (EqEqEq, e0, e1); comment } in
  match (e0.expression_desc, e1.expression_desc) with
  | Str { txt = a0; delim = d0 }, Str { txt = a1; delim = d1 } when d0 = d1 ->
    (match str_equal a0 d0 a1 d1 with
    | Some b -> bool b
    | None -> default ())
  | _, _ -> default ()

let is_type_number ?comment (e : t) : t =
  string_equal ?comment (typeof e) (str "number")

let tag_type = function
  | Ast_untagged_variants.String s -> str s ~delim:DStarJ
  | Int i -> small_int i
  | Float f -> float f
  | BigInt i -> 
    let sign, i = Bigint_utils.parse_bigint i in
    bigint sign i
  | Bool b -> bool b
  | Null -> nil
  | Undefined -> undefined
  | Untagged IntType -> str "number"
  | Untagged FloatType -> str "number"
  | Untagged BigintType -> str "bigint"
  | Untagged BooleanType -> str "boolean"
  | Untagged FunctionType -> str "function"
  | Untagged StringType -> str "string"
  | Untagged (InstanceType i) -> str (Ast_untagged_variants.Instance.to_string i) ~delim:DNoQuotes
  | Untagged ObjectType -> str "object"
  | Untagged UnknownType ->
    (* TODO: this should not happen *)
    assert false

let rec emit_check (check : t Ast_untagged_variants.DynamicChecks.t) = match check with
  | TagType t -> tag_type t
  | BinOp(op, x, y) ->
    let op = match op with
      | EqEqEq -> Js_op.EqEqEq
      | NotEqEq -> NotEqEq
      | And -> And
      | Or -> Or
    in
    bin op (emit_check x) (emit_check y)
  | TypeOf x -> typeof (emit_check x)
  | IsInstanceOf (Array, x) -> is_array (emit_check x)
  | IsInstanceOf (instance, x) ->
    let instance_name = Ast_untagged_variants.Instance.to_string instance in
    instanceof (emit_check x) (str instance_name ~delim:DNoQuotes) 
  | Not x -> not (emit_check x)
  | Expr x -> x

let is_a_literal_case ~literal_cases ~block_cases (e:t) =
  let check = Ast_untagged_variants.DynamicChecks.is_a_literal_case ~literal_cases ~block_cases (Expr e) in
  emit_check check

let is_int_tag ?has_null_undefined_other e =
    let check = Ast_untagged_variants.DynamicChecks.is_int_tag ?has_null_undefined_other (Expr e) in
    emit_check check

let is_type_string ?comment (e : t) : t =
  string_equal ?comment (typeof e) (str "string")

let is_type_object (e : t) : t = string_equal (typeof e) (str "object")

(* we are calling [Caml_primitive.primitive_name], since it's under our
   control, we should make it follow the javascript name convention, and
   call plain [dot]
*)

let tag ?comment ?(name=Js_dump_lit.tag) e : t =
  { expression_desc = Caml_block_tag (e, name); comment }

(* according to the compiler, [Btype.hash_variant],
   it's reduced to 31 bits for hash
*)

(* TODO: handle arbitrary length of args ..
   we can reduce part of the overhead by using
   `__js` -- a easy ppx {{ x ##.hh }}
   the downside is that no way to swap ocaml/js implementation
   for object part, also need encode arity..
   how about x#|getElementById|2|
*)

(* Note that [lsr] or [bor] are js semantics *)
let rec int32_bor ?comment (e1 : J.expression) (e2 : J.expression) :
    J.expression =
  match (e1.expression_desc, e2.expression_desc) with
  | Number (Int { i = i1 } | Uint i1), Number (Int { i = i2 }) ->
      int ?comment (Int32.logor i1 i2)
  | _, Bin (Lsr, e2, { expression_desc = Number (Int { i = 0l } | Uint 0l); _ })
    ->
      int32_bor e1 e2
  | Bin (Lsr, e1, { expression_desc = Number (Int { i = 0l } | Uint 0l); _ }), _
    ->
      int32_bor e1 e2
  | ( Bin (Lsr, _, { expression_desc = Number (Int { i } | Uint i); _ }),
      Number (Int { i = 0l } | Uint 0l) )
    when i > 0l ->
      (* a >>> 3 | 0 -> a >>> 3 *)
      e1
  | ( Bin (Bor, e1, { expression_desc = Number (Int { i = 0l } | Uint 0l); _ }),
      Number (Int { i = 0l } | Uint 0l) ) ->
      int32_bor e1 e2
  | _ -> { comment; expression_desc = Bin (Bor, e1, e2) }

(* Arithmatic operations
   TODO: distinguish between int and float
   TODO: Note that we have to use Int64 to avoid integer overflow, this is fine
   since Js only have .

   like code below
   {[
     MAX_INT_VALUE - (MAX_INT_VALUE - 100) + 20
   ]}

   {[
     MAX_INT_VALUE - x + 30
   ]}

   check: Re-association: avoid integer overflow
*)
let to_int32 ?comment (e : J.expression) : J.expression =
  int32_bor ?comment e zero_int_literal
(* TODO: if we already know the input is int32, [x|0] can be reduced into [x] *)

let uint32 ?comment n : J.expression =
  { expression_desc = Number (Uint n); comment }

let string_comp (cmp : J.binop) ?comment (e0 : t) (e1 : t) =
  match (e0.expression_desc, e1.expression_desc) with
  | Str { txt = a0; delim = d0 }, Str { txt = a1; delim = d1 } -> (
    match cmp, str_equal a0 d0 a1 d1 with
      | EqEqEq, Some b -> bool b
      | NotEqEq, Some b -> bool (b = false)
      | _ ->
        bin ?comment cmp e0 e1)
  | _ -> bin ?comment cmp e0 e1

let obj_length ?comment e : t =
  to_int32 { expression_desc = Length (e, Caml_block); comment }

let compare_int_aux (cmp : Lam_compat.comparison) (l : int) r =
  match cmp with
  | Ceq -> l = r
  | Cneq -> l <> r
  | Clt -> l < r
  | Cgt -> l > r
  | Cle -> l <= r
  | Cge -> l >= r

let rec int_comp (cmp : Lam_compat.comparison) ?comment (e0 : t) (e1 : t) =
  match (cmp, e0.expression_desc, e1.expression_desc) with
  | _, Number ((Int _ | Uint _) as l), Number ((Int _ | Uint _) as r) ->
      let l =
        match l with
        | Uint l -> Ext_int.int32_unsigned_to_int l
        | Int { i = l } -> Int32.to_int l
        | _ -> assert false
      in
      let r =
        match r with
        | Uint l -> Ext_int.int32_unsigned_to_int l
        | Int { i = l } -> Int32.to_int l
        | _ -> assert false
      in
      bool (compare_int_aux cmp l r)
  | ( _,
      Call
        ( {
            expression_desc =
              Var (Qualified ({ kind = Runtime }, Some "int_compare"));
            _;
          },
          [ l; r ],
          _ ),
      Number (Int { i = 0l }) ) ->
      int_comp cmp l r (* = 0 > 0 < 0 *)
  | ( Ceq,
      Call
        ( ({
             expression_desc =
               Var
                 (Qualified
                   (({ id = _; kind = Runtime } as iid), Some "compare"));
             _;
           } as fn),
          ([ _; _ ] as args),
          call_info ),
      Number (Int { i = 0l }) ) ->
      (* This is now generalized for runtime modules
         `RuntimeModule.compare x y = 0 ` -->
         `RuntimeModule.equal x y`
      *)
      {
        e0 with
        expression_desc =
          Call
            ( { fn with expression_desc = Var (Qualified (iid, Some "equal")) },
              args,
              call_info );
      }
  | Ceq, Optional_block _, Undefined _ | Ceq, Undefined _, Optional_block _ ->
      false_
  | Ceq, _, _ -> int_equal e0 e1
  | Cneq, Optional_block _, Undefined _
  | Cneq, Undefined _, Optional_block _
  | Cneq, Caml_block _, Number _
  | Cneq, Number _, Caml_block _ ->
      true_
  | _ -> bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1

let bool_comp (cmp : Lam_compat.comparison) ?comment (e0 : t) (e1 : t) =
  match (e0, e1) with
  | { expression_desc = Bool l }, { expression_desc = Bool r } ->
      bool
        (match cmp with
        | Ceq -> l = r
        | Cneq -> l <> r
        | Clt -> l < r
        | Cgt -> l > r
        | Cle -> l <= r
        | Cge -> l >= r)
  | { expression_desc = Bool true }, rest
  | rest, { expression_desc = Bool false } -> (
      match cmp with
      | Clt -> seq rest false_
      | Cge -> seq rest true_
      | Cle | Cgt | Ceq | Cneq ->
          bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1)
  | rest, { expression_desc = Bool true }
  | { expression_desc = Bool false }, rest -> (
      match cmp with
      | Cle -> seq rest true_
      | Cgt -> seq rest false_
      | Clt | Cge | Ceq | Cneq ->
          bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1)
  | _, _ -> bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1

let float_comp cmp ?comment e0 e1 =
  bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1

let js_comp cmp ?comment e0 e1 =
  bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1

let rec int32_lsr ?comment (e1 : J.expression) (e2 : J.expression) :
    J.expression =
  let aux i1 i = uint32 (Int32.shift_right_logical i1 i) in
  match (e1.expression_desc, e2.expression_desc) with
  | Number (Int { i = i1 } | Uint i1), Number (Int { i = i2 } | Uint i2) ->
      aux i1 (Int32.to_int i2)
  | Bin (Lsr, _, _), Number (Int { i = 0l } | Uint 0l) ->
      e1 (* TODO: more opportunities here *)
  | ( Bin
        (Bor, e1, { expression_desc = Number (Int { i = 0l; _ } | Uint 0l); _ }),
      Number (Int { i = 0l } | Uint 0l) ) ->
      int32_lsr ?comment e1 e2
  | _, _ -> { comment; expression_desc = Bin (Lsr, e1, e2) (* uint32 *) }

let to_uint32 ?comment (e : J.expression) : J.expression =
  int32_lsr ?comment e zero_int_literal

(* TODO:
   we can apply a more general optimization here,
   do some algebraic rewerite rules to rewrite [triple_equal]
*)
let rec is_out ?comment (e : t) (range : t) : t =
  match (range.expression_desc, e.expression_desc) with
  | Number (Int { i = 1l }), Var _ ->
      not
        (or_ (triple_equal e zero_int_literal) (triple_equal e one_int_literal))
  | ( Number (Int { i = 1l }),
      ( Bin
          ( Plus,
            { expression_desc = Number (Int { i; _ }) },
            ({ expression_desc = Var _; _ } as x) )
      | Bin
          ( Plus,
            ({ expression_desc = Var _; _ } as x),
            { expression_desc = Number (Int { i; _ }) } ) ) ) ->
      not
        (or_
           (triple_equal x (int (Int32.neg i)))
           (triple_equal x (int (Int32.sub Int32.one i))))
  | ( Number (Int { i = 1l }),
      Bin
        ( Minus,
          ({ expression_desc = Var _; _ } as x),
          { expression_desc = Number (Int { i; _ }) } ) ) ->
      not (or_ (triple_equal x (int (Int32.add i 1l))) (triple_equal x (int i)))
  (* (x - i >>> 0 ) > k *)
  | ( Number (Int { i = k }),
      Bin
        ( Minus,
          ({ expression_desc = Var _; _ } as x),
          { expression_desc = Number (Int { i; _ }) } ) ) ->
      or_ (int_comp Cgt x (int (Int32.add i k))) (int_comp Clt x (int i))
  | Number (Int { i = k }), Var _ ->
      (* Note that js support [ 1 < x < 3],
         we can optimize it into [ not ( 0<= x <=  k)]
      *)
      or_ (int_comp Cgt e (int k)) (int_comp Clt e zero_int_literal)
  | ( _,
      Bin
        ( Bor,
          ({
             expression_desc =
               ( Bin
                   ( (Plus | Minus),
                     { expression_desc = Number (Int { i = _; _ }) },
                     { expression_desc = Var _; _ } )
               | Bin
                   ( (Plus | Minus),
                     { expression_desc = Var _; _ },
                     { expression_desc = Number (Int { i = _; _ }) } ) );
           } as e),
          { expression_desc = Number (Int { i = 0l } | Uint 0l); _ } ) ) ->
      (* TODO: check correctness *)
      is_out ?comment e range
  | _, _ -> int_comp ?comment Cgt (to_uint32 e) range

let rec float_add ?comment (e1 : t) (e2 : t) =
  match (e1.expression_desc, e2.expression_desc) with
  | Number (Int { i; _ }), Number (Int { i = j; _ }) ->
      int ?comment (Int32.add i j)
  | _, Number (Int { i = j; c }) when j < 0l ->
      float_minus ?comment e1
        { e2 with expression_desc = Number (Int { i = Int32.neg j; c }) }
  | ( Bin (Plus, a1, { expression_desc = Number (Int { i = k; _ }) }),
      Number (Int { i = j; _ }) ) ->
      { comment; expression_desc = Bin (Plus, a1, int (Int32.add k j)) }
  (* bin ?comment Plus a1 (int (k + j)) *)
  (* TODO remove commented code  ?? *)
  (* | Bin(Plus, a0 , ({expression_desc = Number (Int a1)}  )), *)
  (*     Bin(Plus, b0 , ({expression_desc = Number (Int b1)}  )) *)
  (*   ->  *)
  (*   bin ?comment Plus a1 (int (a1 + b1)) *)

  (* | _, Bin(Plus,  b0, ({expression_desc = Number _}  as v)) *)
  (*   -> *)
  (*     bin ?comment Plus (bin ?comment Plus e1 b0) v *)
  (* | Bin(Plus, a1 , ({expression_desc = Number _}  as v)), _ *)
  (* | Bin(Plus, ({expression_desc = Number _}  as v),a1), _ *)
  (*   ->  *)
  (*     bin ?comment Plus (bin ?comment Plus a1 e2 ) v  *)
  (* | Number _, _ *)
  (*   ->  *)
  (*     bin ?comment Plus  e2 e1 *)
  | _ -> { comment; expression_desc = Bin (Plus, e1, e2) }

(* bin ?comment Plus e1 e2 *)
(* associative is error prone due to overflow *)
and float_minus ?comment (e1 : t) (e2 : t) : t =
  match (e1.expression_desc, e2.expression_desc) with
  | Number (Int { i; _ }), Number (Int { i = j; _ }) ->
      int ?comment (Int32.sub i j)
  | _ -> { comment; expression_desc = Bin (Minus, e1, e2) }
(* bin ?comment Minus e1 e2 *)

let unchecked_int32_add ?comment e1 e2 = float_add ?comment e1 e2
let int32_add ?comment e1 e2 = to_int32 (float_add ?comment e1 e2)

let offset e1 (offset : int) =
  if offset = 0 then e1 else int32_add e1 (small_int offset)

let int32_minus ?comment e1 e2 : J.expression =
  to_int32 (float_minus ?comment e1 e2)

let unchecked_int32_minus ?comment e1 e2 : J.expression =
  float_minus ?comment e1 e2

let float_div ?comment e1 e2 = bin ?comment Div e1 e2
let float_notequal ?comment e1 e2 = bin ?comment NotEqEq e1 e2

let int32_asr ?comment e1 e2 : J.expression =
  { comment; expression_desc = Bin (Asr, e1, e2) }

(** Division by zero is undefined behavior*)
let int32_div ~checked ?comment (e1 : t) (e2 : t) : t =
  match (e1.expression_desc, e2.expression_desc) with
  | Length _, Number (Int { i = 2l } | Uint 2l) -> int32_asr e1 one_int_literal
  | e1_desc, Number (Int { i = i1 }) when i1 <> 0l -> (
      match e1_desc with
      | Number (Int { i = i0 }) -> int (Int32.div i0 i1)
      | _ -> to_int32 (float_div ?comment e1 e2))
  | _, _ ->
      if checked then runtime_call Js_runtime_modules.int32 "div" [ e1; e2 ]
      else to_int32 (float_div ?comment e1 e2)

let int32_mod ~checked ?comment e1 (e2 : t) : J.expression =
  match e2.expression_desc with
  | Number (Int { i }) when i <> 0l ->
      { comment; expression_desc = Bin (Mod, e1, e2) }
  | _ ->
      if checked then runtime_call Js_runtime_modules.int32 "mod_" [ e1; e2 ]
      else { comment; expression_desc = Bin (Mod, e1, e2) }

let float_mul ?comment e1 e2 = bin ?comment Mul e1 e2

let int32_lsl ?comment (e1 : J.expression) (e2 : J.expression) : J.expression =
  match (e1, e2) with
  | ( { expression_desc = Number (Int { i = i0 } | Uint i0) },
      { expression_desc = Number (Int { i = i1 } | Uint i1) } ) ->
      int ?comment (Int32.shift_left i0 (Int32.to_int i1))
  | _ -> { comment; expression_desc = Bin (Lsl, e1, e2) }

let is_pos_pow n =
  let exception E in
  let rec aux c (n : Int32.t) =
    if n <= 0l then -2
    else if n = 1l then c
    else if Int32.logand n 1l = 0l then aux (c + 1) (Int32.shift_right n 1)
    else raise_notrace E
  in
  try aux 0 n with E -> -1

let int32_mul ?comment (e1 : J.expression) (e2 : J.expression) : J.expression =
  match (e1, e2) with
  | { expression_desc = Number (Int { i = 0l } | Uint 0l); _ }, x
    when Js_analyzer.no_side_effect_expression x ->
      zero_int_literal
  | x, { expression_desc = Number (Int { i = 0l } | Uint 0l); _ }
    when Js_analyzer.no_side_effect_expression x ->
      zero_int_literal
  | ( { expression_desc = Number (Int { i = i0 }); _ },
      { expression_desc = Number (Int { i = i1 }); _ } ) ->
      int (Int32.mul i0 i1)
  | e, { expression_desc = Number (Int { i = i0 } | Uint i0); _ }
  | { expression_desc = Number (Int { i = i0 } | Uint i0); _ }, e ->
      let i = is_pos_pow i0 in
      if i >= 0 then int32_lsl e (small_int i)
      else
        call ?comment ~info:Js_call_info.builtin_runtime_call
          (dot (js_global "Math") Literals.imul)
          [ e1; e2 ]
  | _ ->
      call ?comment ~info:Js_call_info.builtin_runtime_call
        (dot (js_global "Math") Literals.imul)
        [ e1; e2 ]

let unchecked_int32_mul ?comment e1 e2 : J.expression =
  { comment; expression_desc = Bin (Mul, e1, e2) }

let rec int32_bxor ?comment (e1 : t) (e2 : t) : J.expression =
  match (e1.expression_desc, e2.expression_desc) with
  | Number (Int { i = i1 }), Number (Int { i = i2 }) ->
      int ?comment (Int32.logxor i1 i2)
  | _, Bin (Lsr, e2, { expression_desc = Number (Int { i = 0l } | Uint 0l); _ })
    ->
      int32_bxor e1 e2
  | Bin (Lsr, e1, { expression_desc = Number (Int { i = 0l } | Uint 0l); _ }), _
    ->
      int32_bxor e1 e2
  | _ -> { comment; expression_desc = Bin (Bxor, e1, e2) }

let rec int32_band ?comment (e1 : J.expression) (e2 : J.expression) :
    J.expression =
  match e1.expression_desc with
  | Bin (Bor, a, { expression_desc = Number (Int { i = 0l }) }) ->
      (* Note that in JS
         {[ -1 >>> 0 & 0xffffffff = -1]} is the same as
         {[ (-1 >>> 0 | 0 ) & 0xffffff ]}
      *)
      int32_band a e2
  | _ -> { comment; expression_desc = Bin (Band, e1, e2) }

(* let int32_bin ?comment op e1 e2 : J.expression =  *)
(*   {expression_desc = Int32_bin(op,e1, e2); comment} *)

let bigint_op ?comment op (e1: t) (e2: t) = bin ?comment op e1 e2

let bigint_comp (cmp : Lam_compat.comparison) ?comment (e0: t) (e1: t) =
  bin ?comment (Lam_compile_util.jsop_of_comp cmp) e0 e1

let bigint_div ~checked ?comment (e0: t) (e1: t) =
  if checked then
    runtime_call Js_runtime_modules.bigint "div" [e0; e1]
  else
    bigint_op ?comment Div e0 e1
    
let bigint_mod ~checked ?comment (e0: t) (e1: t) =
  if checked then
    runtime_call Js_runtime_modules.bigint "mod_" [e0; e1]
  else
    bigint_op ?comment Mod e0 e1

(* TODO -- alpha conversion
    remember to add parens..
*)
let of_block ?comment ?e block : t =
  let return_unit = false in
  (* This case is not hit that much*)
  call ~info:Js_call_info.ml_full_call
    {
      comment;
      expression_desc =
        Fun
          {
            is_method = false;
            params = [];
            body =
              (match e with
              | None -> block
              | Some e ->
                  Ext_list.append block
                    [ { J.statement_desc = Return e; comment } ]);
            env = Js_fun_env.make 0;
            return_unit;
            async = false;
            directive = None;
          };
    }
    []

let is_null ?comment (x : t) = triple_equal ?comment x nil
let is_undef ?comment x = triple_equal ?comment x undefined

let for_sure_js_null_undefined (x : t) =
  match x.expression_desc with Null | Undefined _ -> true | _ -> false

let is_null_undefined ?comment (x : t) : t =
  match x.expression_desc with
  | Null | Undefined _ -> true_
  | Number _ | Array _ | Caml_block _ -> false_
  | _ -> { comment; expression_desc = Is_null_or_undefined x }

let eq_null_undefined_boolean ?comment (a : t) (b : t) =
  match (a.expression_desc, b.expression_desc) with
  | ( (Null | Undefined _),
      (Bool _ | Number _ | Typeof _ | Fun _ | Array _ | Caml_block _) ) ->
      false_
  | ( (Bool _ | Number _ | Typeof _ | Fun _ | Array _ | Caml_block _),
      (Null | Undefined _) ) ->
      false_
  | Null, Undefined _ | Undefined _, Null -> false_
  | Null, Null | Undefined _, Undefined _ -> true_
  | _ -> { expression_desc = Bin (EqEqEq, a, b); comment }

let neq_null_undefined_boolean ?comment (a : t) (b : t) =
  match (a.expression_desc, b.expression_desc) with
  | ( (Null | Undefined _),
      (Bool _ | Number _ | Typeof _ | Fun _ | Array _ | Caml_block _) ) ->
      true_
  | ( (Bool _ | Number _ | Typeof _ | Fun _ | Array _ | Caml_block _),
      (Null | Undefined _) ) ->
      true_
  | Null, Null | Undefined _, Undefined _ -> false_
  | Null, Undefined _ | Undefined _, Null -> true_
  | _ -> { expression_desc = Bin (NotEqEq, a, b); comment }

let make_exception (s : string) =
  pure_runtime_call Js_runtime_modules.exceptions Literals.create [ str s ]
