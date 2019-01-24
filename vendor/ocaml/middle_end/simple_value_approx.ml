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

module U = Flambda_utils

type 'a boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type value_string = {
  (* CR-soon mshinwell: use variant *)
  contents : string option; (* None if unknown or mutable *)
  size : int;
}

type unresolved_value =
  | Set_of_closures_id of Set_of_closures_id.t
  | Symbol of Symbol.t

type unknown_because_of =
  | Unresolved_value of unresolved_value
  | Other

type t = {
  descr : descr;
  var : Variable.t option;
  symbol : (Symbol.t * int option) option;
}

and descr =
  | Value_block of Tag.t * t array
  | Value_int of int
  | Value_char of char
  | Value_constptr of int
  | Value_float of float option
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_set_of_closures of value_set_of_closures
  | Value_closure of value_closure
  | Value_string of value_string
  | Value_float_array of value_float_array
  | Value_unknown of unknown_because_of
  | Value_bottom
  | Value_extern of Export_id.t
  | Value_symbol of Symbol.t
  | Value_unresolved of unresolved_value
    (* No description was found for this value *)

and value_closure = {
  set_of_closures : t;
  closure_id : Closure_id.t;
}

and value_set_of_closures = {
  function_decls : Flambda.function_declarations;
  bound_vars : t Var_within_closure.Map.t;
  invariant_params : Variable.Set.t Variable.Map.t lazy_t;
  size : int option Variable.Map.t lazy_t;
  specialised_args : Flambda.specialised_to Variable.Map.t;
  freshening : Freshening.Project_var.t;
  direct_call_surrogates : Closure_id.t Closure_id.Map.t;
}

and value_float_array_contents =
  | Contents of t array
  | Unknown_or_mutable

and value_float_array = {
  contents : value_float_array_contents;
  size : int;
}

let descr t = t.descr

let print_value_set_of_closures ppf
      { function_decls = { funs }; invariant_params; freshening; _ } =
  Format.fprintf ppf "(set_of_closures:@ %a invariant_params=%a freshening=%a)"
    (fun ppf -> Variable.Map.iter (fun id _ -> Variable.print ppf id)) funs
    (Variable.Map.print Variable.Set.print) (Lazy.force invariant_params)
    Freshening.Project_var.print freshening

let print_unresolved_value ppf = function
  | Set_of_closures_id set ->
    Format.fprintf ppf "Set_of_closures_id %a" Set_of_closures_id.print set
  | Symbol symbol ->
    Format.fprintf ppf "Symbol %a" Symbol.print symbol

let rec print_descr ppf = function
  | Value_int i -> Format.pp_print_int ppf i
  | Value_char c -> Format.fprintf ppf "%c" c
  | Value_constptr i -> Format.fprintf ppf "%ia" i
  | Value_block (tag,fields) ->
    let p ppf fields =
      Array.iter (fun v -> Format.fprintf ppf "%a@ " print v) fields in
    Format.fprintf ppf "[%i:@ @[<1>%a@]]" (Tag.to_int tag) p fields
  | Value_unknown reason ->
    begin match reason with
    | Unresolved_value value ->
      Format.fprintf ppf "?(due to unresolved %a)" print_unresolved_value value
    | Other -> Format.fprintf ppf "?"
    end;
  | Value_bottom -> Format.fprintf ppf "bottom"
  | Value_extern id -> Format.fprintf ppf "_%a_" Export_id.print id
  | Value_symbol sym -> Format.fprintf ppf "%a" Symbol.print sym
  | Value_closure { set_of_closures; closure_id; } ->
    Format.fprintf ppf "(closure:@ %a from@ %a)" Closure_id.print closure_id
      print set_of_closures
  | Value_set_of_closures set_of_closures ->
    print_value_set_of_closures ppf set_of_closures
  | Value_unresolved value ->
    Format.fprintf ppf "(unresolved %a)" print_unresolved_value value
  | Value_float (Some f) -> Format.pp_print_float ppf f
  | Value_float None -> Format.pp_print_string ppf "float"
  | Value_string { contents; size } -> begin
      match contents with
      | None ->
          Format.fprintf ppf "string %i" size
      | Some s ->
          let s =
            if size > 10
            then String.sub s 0 8 ^ "..."
            else s
          in
          Format.fprintf ppf "string %i %S" size s
    end
  | Value_float_array float_array ->
    begin match float_array.contents with
    | Unknown_or_mutable ->
      Format.fprintf ppf "float_array %i" float_array.size
    | Contents _ ->
      Format.fprintf ppf "float_array_imm %i" float_array.size
    end
  | Value_boxed_int (t, i) ->
    match t with
    | Int32 -> Format.fprintf ppf "%li" i
    | Int64 -> Format.fprintf ppf "%Li" i
    | Nativeint -> Format.fprintf ppf "%ni" i

and print ppf { descr; var; symbol; } =
  let print ppf = function
    | None -> Symbol.print_opt ppf None
    | Some (sym, None) -> Symbol.print ppf sym
    | Some (sym, Some field) ->
        Format.fprintf ppf "%a.(%i)" Symbol.print sym field
  in
  Format.fprintf ppf "{ descr=%a var=%a symbol=%a }"
    print_descr descr
    Variable.print_opt var
    print symbol

let approx descr = { descr; var = None; symbol = None }

let augment_with_variable t var = { t with var = Some var }
let augment_with_symbol t symbol = { t with symbol = Some (symbol, None) }
let augment_with_symbol_field t symbol field =
  match t.symbol with
  | None -> { t with symbol = Some (symbol, Some field) }
  | Some _ -> t
let replace_description t descr = { t with descr }

let augment_with_kind t (kind:Lambda.value_kind) =
  match kind with
  | Pgenval -> t
  | Pfloatval ->
    begin match t.descr with
    | Value_float _ ->
      t
    | Value_unknown _ | Value_unresolved _ ->
      { t with descr = Value_float None }
    | Value_block _
    | Value_int _
    | Value_char _
    | Value_constptr _
    | Value_boxed_int _
    | Value_set_of_closures _
    | Value_closure _
    | Value_string _
    | Value_float_array _
    | Value_bottom ->
      (* Unreachable *)
      { t with descr = Value_bottom }
    | Value_extern _ | Value_symbol _ ->
      (* We don't know yet *)
      t
    end
  | _ -> t

let augment_kind_with_approx t (kind:Lambda.value_kind) : Lambda.value_kind =
  match t.descr with
  | Value_float _ -> Pfloatval
  | Value_int _ -> Pintval
  | Value_boxed_int (Int32, _) -> Pboxedintval Pint32
  | Value_boxed_int (Int64, _) -> Pboxedintval Pint64
  | Value_boxed_int (Nativeint, _) -> Pboxedintval Pnativeint
  | _ -> kind

let value_unknown reason = approx (Value_unknown reason)
let value_int i = approx (Value_int i)
let value_char i = approx (Value_char i)
let value_constptr i = approx (Value_constptr i)
let value_float f = approx (Value_float (Some f))
let value_any_float = approx (Value_float None)
let value_boxed_int bi i = approx (Value_boxed_int (bi,i))

let value_closure ?closure_var ?set_of_closures_var ?set_of_closures_symbol
      value_set_of_closures closure_id =
  let approx_set_of_closures =
    { descr = Value_set_of_closures value_set_of_closures;
      var = set_of_closures_var;
      symbol = Misc.may_map (fun s -> s, None) set_of_closures_symbol;
    }
  in
  let value_closure =
    { set_of_closures = approx_set_of_closures;
      closure_id;
    }
  in
  { descr = Value_closure value_closure;
    var = closure_var;
    symbol = None;
  }

let create_value_set_of_closures
      ~(function_decls : Flambda.function_declarations) ~bound_vars
      ~invariant_params ~specialised_args ~freshening
      ~direct_call_surrogates =
  let size =
    lazy (
      let functions = Variable.Map.keys function_decls.funs in
      Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
          let params = Parameter.Set.vars function_decl.params in
          let free_vars =
            Variable.Set.diff
              (Variable.Set.diff function_decl.free_variables params)
              functions
          in
          let num_free_vars = Variable.Set.cardinal free_vars in
          let max_size =
            Inlining_cost.maximum_interesting_size_of_function_body
              num_free_vars
          in
          Inlining_cost.lambda_smaller' function_decl.body ~than:max_size)
        function_decls.funs)
  in
  { function_decls;
    bound_vars;
    invariant_params;
    size;
    specialised_args;
    freshening;
    direct_call_surrogates;
  }

let update_freshening_of_value_set_of_closures value_set_of_closures
      ~freshening =
  (* CR-someday mshinwell: We could maybe check that [freshening] is
     reasonable. *)
  { value_set_of_closures with freshening; }

let value_set_of_closures ?set_of_closures_var value_set_of_closures =
  { descr = Value_set_of_closures value_set_of_closures;
    var = set_of_closures_var;
    symbol = None;
  }

let value_block t b = approx (Value_block (t, b))
let value_extern ex = approx (Value_extern ex)
let value_symbol sym =
  { (approx (Value_symbol sym)) with symbol = Some (sym, None) }
let value_bottom = approx Value_bottom
let value_unresolved value = approx (Value_unresolved value)

let value_string size contents = approx (Value_string {size; contents })
let value_mutable_float_array ~size =
  approx (Value_float_array { contents = Unknown_or_mutable; size; } )
let value_immutable_float_array (contents:t array) =
  let size = Array.length contents in
  let contents =
    Array.map (fun t -> augment_with_kind t Pfloatval) contents
  in
  approx (Value_float_array { contents = Contents contents; size; } )

let name_expr_fst (named, thing) ~name =
  (Flambda_utils.name_expr named ~name), thing

let make_const_int_named n : Flambda.named * t =
  Const (Int n), value_int n
let make_const_int (n : int) =
  let name =
    match n with
    | 0 -> "const_zero"
    | 1 -> "const_one"
    | _ -> "const_int"
  in
  name_expr_fst (make_const_int_named n) ~name

let make_const_char_named n : Flambda.named * t =
  Const (Char n), value_char n
let make_const_char n =
  name_expr_fst (make_const_char_named n) ~name:"const_char"

let make_const_ptr_named n : Flambda.named * t =
  Const (Const_pointer n), value_constptr n
let make_const_ptr (n : int) =
  let name =
    match n with
    | 0 -> "const_ptr_zero"
    | 1 -> "const_ptr_one"
    | _ -> "const_ptr"
  in
  name_expr_fst (make_const_ptr_named n) ~name

let make_const_bool_named b : Flambda.named * t =
  make_const_ptr_named (if b then 1 else 0)
let make_const_bool b =
  name_expr_fst (make_const_bool_named b) ~name:"const_bool"

let make_const_float_named f : Flambda.named * t =
  Allocated_const (Float f), value_float f
let make_const_float f =
  name_expr_fst (make_const_float_named f) ~name:"const_float"

let make_const_boxed_int_named (type bi) (t:bi boxed_int) (i:bi)
      : Flambda.named * t =
  let c : Allocated_const.t =
    match t with
    | Int32 -> Int32 i
    | Int64 -> Int64 i
    | Nativeint -> Nativeint i
  in
  Allocated_const c, value_boxed_int t i
let make_const_boxed_int t i =
  name_expr_fst (make_const_boxed_int_named t i) ~name:"const_boxed_int"

type simplification_summary =
  | Nothing_done
  | Replaced_term

type simplification_result = Flambda.t * simplification_summary * t
type simplification_result_named = Flambda.named * simplification_summary * t

let simplify t (lam : Flambda.t) : simplification_result =
  if Effect_analysis.no_effects lam then
    match t.descr with
    | Value_int n ->
      let const, approx = make_const_int n in
      const, Replaced_term, approx
    | Value_char n ->
      let const, approx = make_const_char n in
      const, Replaced_term, approx
    | Value_constptr n ->
      let const, approx = make_const_ptr n in
      const, Replaced_term, approx
    | Value_float (Some f) ->
      let const, approx = make_const_float f in
      const, Replaced_term, approx
    | Value_boxed_int (t, i) ->
      let const, approx = make_const_boxed_int t i in
      const, Replaced_term, approx
    | Value_symbol sym ->
      U.name_expr (Symbol sym) ~name:"symbol", Replaced_term, t
    | Value_string _ | Value_float_array _ | Value_float None
    | Value_block _ | Value_set_of_closures _ | Value_closure _
    | Value_unknown _ | Value_bottom | Value_extern _ | Value_unresolved _ ->
      lam, Nothing_done, t
  else
    lam, Nothing_done, t

let simplify_named t (named : Flambda.named) : simplification_result_named =
  if Effect_analysis.no_effects_named named then
    match t.descr with
    | Value_int n ->
      let const, approx = make_const_int_named n in
      const, Replaced_term, approx
    | Value_char n ->
      let const, approx = make_const_char_named n in
      const, Replaced_term, approx
    | Value_constptr n ->
      let const, approx = make_const_ptr_named n in
      const, Replaced_term, approx
    | Value_float (Some f) ->
      let const, approx = make_const_float_named f in
      const, Replaced_term, approx
    | Value_boxed_int (t, i) ->
      let const, approx = make_const_boxed_int_named t i in
      const, Replaced_term, approx
    | Value_symbol sym ->
      Symbol sym, Replaced_term, t
    | Value_string _ | Value_float_array _ | Value_float None
    | Value_block _ | Value_set_of_closures _ | Value_closure _
    | Value_unknown _ | Value_bottom | Value_extern _ | Value_unresolved _ ->
      named, Nothing_done, t
  else
    named, Nothing_done, t

(* CR-soon mshinwell: bad name.  This function and its call site in
   [Inline_and_simplify] is also messy. *)
let simplify_var t : (Flambda.named * t) option =
  match t.descr with
  | Value_int n -> Some (make_const_int_named n)
  | Value_char n -> Some (make_const_char_named n)
  | Value_constptr n -> Some (make_const_ptr_named n)
  | Value_float (Some f) -> Some (make_const_float_named f)
  | Value_boxed_int (t, i) -> Some (make_const_boxed_int_named t i)
  | Value_symbol sym -> Some (Symbol sym, t)
  | Value_string _ | Value_float_array _ | Value_float None
  | Value_block _ | Value_set_of_closures _ | Value_closure _
  | Value_unknown _ | Value_bottom | Value_extern _
  | Value_unresolved _ ->
    match t.symbol with
    | Some (sym, None) -> Some (Symbol sym, t)
    | Some (sym, Some field) -> Some (Read_symbol_field (sym, field), t)
    | None -> None

let join_summaries summary ~replaced_by_var_or_symbol =
  match replaced_by_var_or_symbol, summary with
  | true, Nothing_done
  | true, Replaced_term
  | false, Replaced_term -> Replaced_term
  | false, Nothing_done -> Nothing_done

let simplify_using_env t ~is_present_in_env flam =
  let replaced_by_var_or_symbol, flam =
    match t.var with
    | Some var when is_present_in_env var -> true, Flambda.Var var
    | _ ->
      match t.symbol with
      | Some (sym, None) -> true,
        U.name_expr (Symbol sym) ~name:"symbol"
      | Some (sym, Some field) ->
        true, U.name_expr (Read_symbol_field (sym, field)) ~name:"symbol_field"
      | None -> false, flam
  in
  let const, summary, approx = simplify t flam in
  const, join_summaries summary ~replaced_by_var_or_symbol, approx

let simplify_named_using_env t ~is_present_in_env named =
  let replaced_by_var_or_symbol, named =
    match t.var with
    | Some var when is_present_in_env var ->
      true, Flambda.Expr (Var var)
    | _ ->
      match t.symbol with
      | Some (sym, None) -> true, (Flambda.Symbol sym:Flambda.named)
      | Some (sym, Some field) ->
        true, Flambda.Read_symbol_field (sym, field)
      | None -> false, named
  in
  let const, summary, approx = simplify_named t named in
  const, join_summaries summary ~replaced_by_var_or_symbol, approx

let simplify_var_to_var_using_env t ~is_present_in_env =
  match t.var with
  | Some var when is_present_in_env var -> Some var
  | _ -> None

let known t =
  match t.descr with
  | Value_unresolved _
  | Value_unknown _ -> false
  | Value_string _ | Value_float_array _
  | Value_bottom | Value_block _ | Value_int _ | Value_char _
  | Value_constptr _ | Value_set_of_closures _ | Value_closure _
  | Value_extern _ | Value_float _ | Value_boxed_int _ | Value_symbol _ -> true

let useful t =
  match t.descr with
  | Value_unresolved _ | Value_unknown _ | Value_bottom -> false
  | Value_string _ | Value_float_array _ | Value_block _ | Value_int _
  | Value_char _ | Value_constptr _ | Value_set_of_closures _
  | Value_float _ | Value_boxed_int _ | Value_closure _ | Value_extern _
  | Value_symbol _ -> true

let all_not_useful ts = List.for_all (fun t -> not (useful t)) ts

let warn_on_mutation t =
  match t.descr with
  | Value_block(_, fields) -> Array.length fields > 0
  | Value_string { contents = Some _ }
  | Value_int _ | Value_char _ | Value_constptr _
  | Value_set_of_closures _ | Value_float _ | Value_boxed_int _
  | Value_closure _ -> true
  | Value_string { contents = None } | Value_float_array _
  | Value_unresolved _ | Value_unknown _ | Value_bottom -> false
  | Value_extern _ | Value_symbol _ -> assert false

type get_field_result =
  | Ok of t
  | Unreachable

let get_field t ~field_index:i : get_field_result =
  match t.descr with
  | Value_block (_tag, fields) ->
    if i >= 0 && i < Array.length fields then begin
      Ok fields.(i)
    end else begin
      (* This (unfortunately) cannot be a fatal error; it can happen if a
         .cmx file is missing.  However for debugging the compiler this can
         be a useful point to put a [Misc.fatal_errorf]. *)
      Unreachable
    end
  (* CR-someday mshinwell: This should probably return Unreachable in more
     cases.  I added a couple more. *)
  | Value_bottom
  | Value_int _ | Value_char _ | Value_constptr _ ->
    (* Something seriously wrong is happening: either the user is doing
       something exceptionally unsafe, or it is an unreachable branch.
       We consider this as unreachable and mark the result accordingly. *)
    Ok value_bottom
  | Value_float_array _ ->
    (* For the moment we return "unknown" even for immutable arrays, since
       it isn't possible for user code to project from an immutable array. *)
    (* CR-someday mshinwell: If Leo's array's patch lands, then we can
       change this, although it's probably not Pfield that is used to
       do the projection. *)
    Ok (value_unknown Other)
  | Value_string _ | Value_float _ | Value_boxed_int _ ->
    (* The user is doing something unsafe. *)
    Unreachable
  | Value_set_of_closures _ | Value_closure _
    (* This is used by [CamlinternalMod]. *)
  | Value_symbol _ | Value_extern _ ->
    (* These should have been resolved. *)
    Ok (value_unknown Other)
  | Value_unknown reason ->
    Ok (value_unknown reason)
  | Value_unresolved value ->
    (* We don't know anything, but we must remember that it comes
       from another compilation unit in case it contains a closure. *)
    Ok (value_unknown (Unresolved_value value))

type checked_approx_for_block =
  | Wrong
  | Ok of Tag.t * t array

let check_approx_for_block t =
  match t.descr with
  | Value_block (tag, fields) ->
    Ok (tag, fields)
  | Value_bottom
  | Value_int _ | Value_char _ | Value_constptr _
  | Value_float_array _
  | Value_string _ | Value_float _ | Value_boxed_int _
  | Value_set_of_closures _ | Value_closure _
  | Value_symbol _ | Value_extern _
  | Value_unknown _
  | Value_unresolved _ ->
    Wrong

let descrs approxs = List.map (fun v -> v.descr) approxs

let equal_boxed_int (type t1) (type t2)
    (bi1:t1 boxed_int) (i1:t1)
    (bi2:t2 boxed_int) (i2:t2) =
  match bi1, bi2 with
  | Int32, Int32 -> Int32.equal i1 i2
  | Int64, Int64 -> Int64.equal i1 i2
  | Nativeint, Nativeint -> Nativeint.equal i1 i2
  | _ -> false

(* Closures and set of closures descriptions cannot be merged.

   let f x =
     let g y -> x + y in
     g
   in
   let v =
     if ...
     then f 1
     else f 2
   in
   v 3

   The approximation for [f 1] and [f 2] could both contain the
   description of [g]. But if [f] where inlined, a new [g] would
   be created in each branch, leading to incompatible description.
   And we must never make the descrition for a function less
   precise that it used to be: its information are needed for
   rewriting [Project_var] and [Project_closure] constructions
   in [Flambdainline.loop]
*)
let rec meet_descr ~really_import_approx d1 d2 = match d1, d2 with
  | Value_int i, Value_int j when i = j ->
      d1
  | Value_constptr i, Value_constptr j when i = j ->
      d1
  | Value_symbol s1, Value_symbol s2 when Symbol.equal s1 s2 ->
      d1
  | Value_extern e1, Value_extern e2 when Export_id.equal e1 e2 ->
      d1
  | Value_float i, Value_float j when i = j ->
      d1
  | Value_boxed_int (bi1, i1), Value_boxed_int (bi2, i2) when
      equal_boxed_int bi1 i1 bi2 i2 ->
      d1
  | Value_block (tag1, a1), Value_block (tag2, a2)
    when tag1 = tag2 && Array.length a1 = Array.length a2 ->
    let fields =
      Array.mapi (fun i v -> meet ~really_import_approx v a2.(i)) a1
    in
    Value_block (tag1, fields)
  | _ -> Value_unknown Other

and meet ~really_import_approx a1 a2 =
  match a1, a2 with
  | { descr = Value_bottom }, a
  | a, { descr = Value_bottom } -> a
  | { descr = (Value_symbol _ | Value_extern _) }, _
  | _, { descr = (Value_symbol _ | Value_extern _) } ->
    meet ~really_import_approx
      (really_import_approx a1) (really_import_approx a2)
  | _ ->
      let var =
        match a1.var, a2.var with
        | None, _ | _, None -> None
        | Some v1, Some v2 ->
            if Variable.equal v1 v2
            then Some v1
            else None
      in
      let symbol =
        match a1.symbol, a2.symbol with
        | None, _ | _, None -> None
        | Some (v1, field1), Some (v2, field2) ->
            if Symbol.equal v1 v2
            then match field1, field2 with
              | None, None -> a1.symbol
              | Some f1, Some f2 when f1 = f2 ->
                  a1.symbol
              | _ -> None
            else None
      in
      { descr = meet_descr ~really_import_approx a1.descr a2.descr;
        var;
        symbol }

(* Given a set-of-closures approximation and a closure ID, apply any
   freshening specified in the approximation to the closure ID, and return
   that new closure ID.  A fatal error is produced if the new closure ID
   does not correspond to a function declaration in the given approximation. *)
let freshen_and_check_closure_id
      (value_set_of_closures : value_set_of_closures) closure_id =
  let closure_id =
    Freshening.Project_var.apply_closure_id
      value_set_of_closures.freshening closure_id
  in
  try
    ignore (Flambda_utils.find_declaration closure_id
      value_set_of_closures.function_decls);
    closure_id
  with Not_found ->
    Misc.fatal_error (Format.asprintf
      "Function %a not found in the set of closures@ %a@.%a@."
      Closure_id.print closure_id
      print_value_set_of_closures value_set_of_closures
      Flambda.print_function_declarations value_set_of_closures.function_decls)

type checked_approx_for_set_of_closures =
  | Wrong
  | Unresolved of unresolved_value
  | Unknown
  | Unknown_because_of_unresolved_value of unresolved_value
  | Ok of Variable.t option * value_set_of_closures

let check_approx_for_set_of_closures t : checked_approx_for_set_of_closures =
  match t.descr with
  | Value_unresolved value -> Unresolved value
  | Value_unknown (Unresolved_value value) ->
    Unknown_because_of_unresolved_value value
  | Value_set_of_closures value_set_of_closures ->
    (* Note that [var] might be [None]; we might be reaching the set of
       closures via approximations only, with the variable originally bound
       to the set now out of scope. *)
    Ok (t.var, value_set_of_closures)
  | Value_closure _ | Value_block _ | Value_int _ | Value_char _
  | Value_constptr _ | Value_float _ | Value_boxed_int _ | Value_unknown _
  | Value_bottom | Value_extern _ | Value_string _ | Value_float_array _
  | Value_symbol _ ->
    Wrong

type strict_checked_approx_for_set_of_closures =
  | Wrong
  | Ok of Variable.t option * value_set_of_closures

let strict_check_approx_for_set_of_closures t
      : strict_checked_approx_for_set_of_closures =
  match check_approx_for_set_of_closures t with
  | Ok (var, value_set_of_closures) -> Ok (var, value_set_of_closures)
  | Wrong | Unresolved _
  | Unknown | Unknown_because_of_unresolved_value _ -> Wrong

type checked_approx_for_closure_allowing_unresolved =
  | Wrong
  | Unresolved of unresolved_value
  | Unknown
  | Unknown_because_of_unresolved_value of unresolved_value
  | Ok of value_closure * Variable.t option
          * Symbol.t option * value_set_of_closures

let check_approx_for_closure_allowing_unresolved t
      : checked_approx_for_closure_allowing_unresolved =
  match t.descr with
  | Value_closure value_closure ->
    begin match value_closure.set_of_closures.descr with
    | Value_set_of_closures value_set_of_closures ->
      let symbol = match value_closure.set_of_closures.symbol with
        | Some (symbol, None) -> Some symbol
        | None | Some (_, Some _) -> None
      in
      Ok (value_closure, value_closure.set_of_closures.var,
          symbol, value_set_of_closures)
    | Value_unresolved _
    | Value_closure _ | Value_block _ | Value_int _ | Value_char _
    | Value_constptr _ | Value_float _ | Value_boxed_int _ | Value_unknown _
    | Value_bottom | Value_extern _ | Value_string _ | Value_float_array _
    | Value_symbol _ ->
      Wrong
    end
  | Value_unknown (Unresolved_value value) ->
    Unknown_because_of_unresolved_value value
  | Value_unresolved symbol -> Unresolved symbol
  | Value_set_of_closures _ | Value_block _ | Value_int _ | Value_char _
  | Value_constptr _ | Value_float _ | Value_boxed_int _
  | Value_bottom | Value_extern _ | Value_string _ | Value_float_array _
  | Value_symbol _ ->
    Wrong
  (* CR-soon mshinwell: This should be unwound once the reason for a value
     being unknown can be correctly propagated through the export info. *)
  | Value_unknown Other -> Unknown

type checked_approx_for_closure =
  | Wrong
  | Ok of value_closure * Variable.t option
          * Symbol.t option * value_set_of_closures

let check_approx_for_closure t : checked_approx_for_closure =
  match check_approx_for_closure_allowing_unresolved t with
  | Ok (value_closure, set_of_closures_var, set_of_closures_symbol,
      value_set_of_closures) ->
    Ok (value_closure, set_of_closures_var, set_of_closures_symbol,
      value_set_of_closures)
  | Wrong | Unknown | Unresolved _ | Unknown_because_of_unresolved_value _ ->
    Wrong

let approx_for_bound_var value_set_of_closures var =
  try
    Var_within_closure.Map.find var value_set_of_closures.bound_vars
  with
  | Not_found ->
    Misc.fatal_errorf "The set-of-closures approximation %a@ does not \
        bind the variable %a@.%s@."
      print_value_set_of_closures value_set_of_closures
      Var_within_closure.print var
      (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int))

let check_approx_for_float t : float option =
  match t.descr with
  | Value_float f -> f
  | Value_unresolved _
  | Value_unknown _ | Value_string _ | Value_float_array _
  | Value_bottom | Value_block _ | Value_int _ | Value_char _
  | Value_constptr _ | Value_set_of_closures _ | Value_closure _
  | Value_extern _ | Value_boxed_int _ | Value_symbol _ ->
      None

let float_array_as_constant (t:value_float_array) : float list option =
  match t.contents with
  | Unknown_or_mutable -> None
  | Contents contents ->
    Array.fold_right (fun elt acc ->
      match acc, elt.descr with
      | Some acc, Value_float (Some f) ->
        Some (f :: acc)
      | None, _
      | Some _,
        (Value_float None | Value_unresolved _
        | Value_unknown _ | Value_string _ | Value_float_array _
        | Value_bottom | Value_block _ | Value_int _ | Value_char _
        | Value_constptr _ | Value_set_of_closures _ | Value_closure _
        | Value_extern _ | Value_boxed_int _ | Value_symbol _)
        -> None)
      contents (Some [])

let check_approx_for_string t : string option =
  match t.descr with
  | Value_string { contents } -> contents
  | Value_float _
  | Value_unresolved _
  | Value_unknown _ | Value_float_array _
  | Value_bottom | Value_block _ | Value_int _ | Value_char _
  | Value_constptr _ | Value_set_of_closures _ | Value_closure _
  | Value_extern _ | Value_boxed_int _ | Value_symbol _ ->
      None

type switch_branch_selection =
  | Cannot_be_taken
  | Can_be_taken
  | Must_be_taken

let potentially_taken_const_switch_branch t branch =
  match t.descr with
  | Value_unresolved _
  | Value_unknown _
  | Value_extern _
  | Value_symbol _ ->
    (* In theory symbol cannot contain integers but this shouldn't
       matter as this will always be an imported approximation *)
    Can_be_taken
  | Value_constptr i | Value_int i when i = branch ->
    Must_be_taken
  | Value_char c when Char.code c = branch ->
    Must_be_taken
  | Value_constptr _ | Value_int _ | Value_char _ ->
    Cannot_be_taken
  | Value_block _ | Value_float _ | Value_float_array _
  | Value_string _ | Value_closure _ | Value_set_of_closures _
  | Value_boxed_int _ | Value_bottom ->
    Cannot_be_taken

let potentially_taken_block_switch_branch t tag =
  match t.descr with
  | (Value_unresolved _
    | Value_unknown _
    | Value_extern _
    | Value_symbol _) ->
    Can_be_taken
  | (Value_constptr _ | Value_int _| Value_char _) ->
    Cannot_be_taken
  | Value_block (block_tag, _) when Tag.to_int block_tag = tag ->
    Must_be_taken
  | Value_float _ when tag = Obj.double_tag ->
    Must_be_taken
  | Value_float_array _ when tag = Obj.double_array_tag ->
    Must_be_taken
  | Value_string _ when tag = Obj.string_tag ->
    Must_be_taken
  | (Value_closure _ | Value_set_of_closures _)
    when tag = Obj.closure_tag || tag = Obj.infix_tag ->
    Can_be_taken
  | Value_boxed_int _ when tag = Obj.custom_tag ->
    Must_be_taken
  | Value_block _ | Value_float _ | Value_set_of_closures _ | Value_closure _
  | Value_string _ | Value_float_array _ | Value_boxed_int _ ->
    Cannot_be_taken
  | Value_bottom ->
    Cannot_be_taken
