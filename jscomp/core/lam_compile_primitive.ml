(* Copyright (C) 2015 - 2016 Bloomberg Finance L.P.
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

module E = Js_exp_make

(* If it is the return value, since it is a side-effect call,
   we return unit, otherwise just return it
*)
let ensure_value_unit (st : Lam_compile_context.continuation) e : E.t =
  match st with
  | EffectCall (Maybe_tail_is_return _)
  | NeedValue (Maybe_tail_is_return _)
  | Assign _ | Declare _ | NeedValue _ ->
      E.seq e E.unit
  | EffectCall Not_tail -> e
(* NeedValue should return a meaningful expression*)

let module_of_expression = function
  | J.Var (J.Qualified (module_id, value)) -> [ (module_id, value) ]
  | _ -> []

let get_module_system () =
    let package_info = Js_packages_state.get_packages_info () in
    let module_system =
        if Js_packages_info.is_empty package_info && !Js_config.js_stdout then
            [Ext_module_system.Commonjs]
        else Js_packages_info.map package_info (fun {module_system} -> module_system)
    in
    match module_system with
    | [module_system] -> module_system
    | _ -> Commonjs

let import_of_path path =
  E.call
    ~info:{ arity = Full; call_info = Call_na }
    (E.js_global "import")
    [ E.str path ]

let wrap_then import value =
  let arg = Ident.create "m" in
  E.call
    ~info:{ arity = Full; call_info = Call_na }
    (E.dot import "then")
    [
      E.ocaml_fun ~return_unit:false ~async:false ~one_unit_arg:false  [ arg ]
        [
          {
            statement_desc = J.Return (E.dot (E.var arg) value);
            comment = None;
          };
        ];
    ]

let translate output_prefix loc (cxt : Lam_compile_context.t)
    (prim : Lam_primitive.t) (args : J.expression list) : J.expression =
  match prim with
  | Pis_not_none -> Js_of_lam_option.is_not_none (Ext_list.singleton_exn args)
  | Pcreate_extension s -> E.make_exception s
  | Pwrap_exn ->
      E.runtime_call Js_runtime_modules.caml_js_exceptions
        "internalToOCamlException" args
  | Praw_js_code { code; code_info } -> E.raw_js_code code_info code
  (* FIXME: save one allocation
     trim can not be done before syntax checking
     otherwise location is incorrect
  *)
  | Pjs_runtime_apply -> (
      match args with [ f; args ] -> E.flat_call f args | _ -> assert false)
  | Pjs_apply -> (
      match args with
      | fn :: rest -> E.call ~info:{ arity = Full; call_info = Call_na } fn rest
      | _ -> assert false)
  | Pnull_to_opt -> (
      match args with
      | [ e ] -> (
          match e.expression_desc with
          | Var _ | Undefined _ | Null -> Js_of_lam_option.null_to_opt e
          | _ -> E.runtime_call Js_runtime_modules.option "null_to_opt" args)
      | _ -> assert false)
  | Pundefined_to_opt -> (
      match args with
      | [ e ] -> (
          match e.expression_desc with
          | Var _ | Undefined _ | Null -> Js_of_lam_option.undef_to_opt e
          | _ ->
              E.runtime_call Js_runtime_modules.option "undefined_to_opt" args)
      | _ -> assert false)
  | Pnull_undefined_to_opt -> (
      match args with
      | [ e ] -> (
          match e.expression_desc with
          | Var _ | Undefined _ | Null -> Js_of_lam_option.null_undef_to_opt e
          | _ -> E.runtime_call Js_runtime_modules.option "nullable_to_opt" args
          )
      | _ -> assert false)
  (* Compile #import: The module argument for dynamic import is represented as a path,
     and the module value is expressed through wrapping it with promise.then *)
  | Pimport -> (
      match args with
      | [ e ] -> (
          let output_dir = Filename.dirname output_prefix in

          let module_id, module_value =
            match module_of_expression e.expression_desc with
            | [ module_ ] -> module_
            | _ -> Location.raise_errorf ~loc
                "Invalid argument: Dynamic import requires a module or module value that is a file as argument. Passing a value or local module is not allowed."
          in

          let path =
            let module_system = get_module_system () in
            Js_name_of_module_id.string_of_module_id {module_id with dynamic_import = true} ~output_dir module_system
          in

          match module_value with
          | Some value -> wrap_then (import_of_path path) value
          | None -> import_of_path path)
      | [] | _ ->
          Location.raise_errorf ~loc
            "Invalid argument: Dynamic import must take a single module or module value as its argument.")
  | Pjs_function_length -> E.function_length (Ext_list.singleton_exn args)
  | Pcaml_obj_length -> E.obj_length (Ext_list.singleton_exn args)
  | Pis_null -> E.is_null (Ext_list.singleton_exn args)
  | Pis_undefined -> E.is_undef (Ext_list.singleton_exn args)
  | Pis_null_undefined -> E.is_null_undefined (Ext_list.singleton_exn args)
  | Pjs_typeof -> E.typeof (Ext_list.singleton_exn args)
  | Pjs_unsafe_downgrade _ | Pdebugger | Pvoid_run | Pfull_apply | Pjs_fn_make _ | Pjs_fn_make_unit
    ->
      assert false (* already handled by {!Lam_compile} *)
  | Pjs_fn_method -> assert false
  | Pstringadd -> (
      match args with [ a; b ] -> E.string_append a b | _ -> assert false)
  | Pinit_mod -> E.runtime_call Js_runtime_modules.module_ "init_mod" args
  | Pupdate_mod -> E.runtime_call Js_runtime_modules.module_ "update_mod" args
  | Psome -> (
      let arg = Ext_list.singleton_exn args in
      match arg.expression_desc with
      | Null | Object _ | Number _ | Caml_block _ | Array _ | Str _ ->
          (* This makes sense when type info
             is not available at the definition
             site, and inline recovered it
          *)
          E.optional_not_nest_block arg
      | _ -> E.optional_block arg)
  | Psome_not_nest -> E.optional_not_nest_block (Ext_list.singleton_exn args)
  | Pmakeblock (tag, tag_info, mutable_flag) ->
      (* RUNTIME *)
      Js_of_lam_block.make_block
        (Js_op_util.of_lam_mutable_flag mutable_flag)
        tag_info (E.small_int tag) args
  | Pval_from_option ->
      Js_of_lam_option.val_from_option (Ext_list.singleton_exn args)
  | Pval_from_option_not_nest -> Ext_list.singleton_exn args
  | Pfield (i, fld_info) ->
      Js_of_lam_block.field fld_info
        (Ext_list.singleton_exn args)
        (Int32.of_int i)
  (* Invariant depends on runtime *)
  (* Negate boxed int *)
  | Pnegint ->
      (* #977 *)
      E.int32_minus E.zero_int_literal (Ext_list.singleton_exn args)
  | Pnegint64 -> Js_long.neg args
  | Pnegfloat -> E.float_minus E.zero_float_lit (Ext_list.singleton_exn args)
  | Pnegbigint -> E.bigint_op Minus E.zero_bigint_literal (Ext_list.singleton_exn args)
  (* Negate boxed int end*)
  (* Int addition and subtraction *)
  | Paddint -> (
      match args with [ e1; e2 ] -> E.int32_add e1 e2 | _ -> assert false)
  | Paddint64 -> Js_long.add args
  | Paddfloat -> (
      match args with [ e1; e2 ] -> E.float_add e1 e2 | _ -> assert false)
  | Paddbigint -> (
    match args with [ e1; e2 ] -> E.bigint_op Plus e1 e2 | _ -> assert false)
  | Psubint -> (
      match args with [ e1; e2 ] -> E.int32_minus e1 e2 | _ -> assert false)
  | Psubint64 -> Js_long.sub args
  | Psubfloat -> (
      match args with [ e1; e2 ] -> E.float_minus e1 e2 | _ -> assert false)
  | Psubbigint -> (
      match args with [ e1; e2 ] -> E.bigint_op Minus e1 e2 | _ -> assert false)
  | Pmulint -> (
      match args with [ e1; e2 ] -> E.int32_mul e1 e2 | _ -> assert false)
  | Pmulint64 -> Js_long.mul args
  | Pmulfloat -> (
      match args with [ e1; e2 ] -> E.float_mul e1 e2 | _ -> assert false)
  | Pmulbigint -> (
      match args with [ e1; e2 ] -> E.bigint_op Mul e1 e2 | _ -> assert false)
  | Pdivfloat -> (
      match args with [ e1; e2 ] -> E.float_div e1 e2 | _ -> assert false)
  | Pdivint -> (
      match args with
      | [ e1; e2 ] -> E.int32_div ~checked:!Js_config.check_div_by_zero e1 e2
      | _ -> assert false)
  | Pdivint64 -> Js_long.div args
  | Pdivbigint -> (
      match args with
      | [ e1; e2 ] -> E.bigint_div ~checked:!Js_config.check_div_by_zero e1 e2
      | _ -> assert false)
  | Pmodint -> (
      match args with
      | [ e1; e2 ] -> E.int32_mod ~checked:!Js_config.check_div_by_zero e1 e2
      | _ -> assert false)
  | Pmodint64 -> Js_long.mod_ args
  | Pmodbigint -> (
      match args with
      | [ e1; e2 ] -> E.bigint_mod ~checked:!Js_config.check_div_by_zero e1 e2
      | _ -> assert false)
  | Ppowbigint -> (match args with [ e1; e2 ] -> E.bigint_op Pow e1 e2 | _ -> assert false)
  | Plslint -> (
      match args with [ e1; e2 ] -> E.int32_lsl e1 e2 | _ -> assert false)
  | Plslint64 -> Js_long.lsl_ args
  | Plslbigint -> (
      match args with [ e1; e2 ] -> E.bigint_op Lsl e1 e2 | _ -> assert false)
  | Plsrint -> (
      match args with
      | [ e1; { J.expression_desc = Number (Int { i = 0l; _ } | Uint 0l); _ } ]
        ->
          e1
      | [ e1; e2 ] -> E.to_int32 @@ E.int32_lsr e1 e2
      | _ -> assert false)
  | Plsrint64 -> Js_long.lsr_ args
  | Pasrint -> (
      match args with [ e1; e2 ] -> E.int32_asr e1 e2 | _ -> assert false)
  | Pasrint64 -> Js_long.asr_ args
  | Pasrbigint -> (
      match args with [ e1; e2 ] -> E.bigint_op Asr e1 e2 | _ -> assert false)
  | Pandint -> (
      match args with [ e1; e2 ] -> E.int32_band e1 e2 | _ -> assert false)
  | Pandint64 -> Js_long.and_ args
  | Pandbigint -> (
      match args with [ e1; e2 ] -> E.bigint_op Band e1 e2 | _ -> assert false)
  | Porint -> (
      match args with [ e1; e2 ] -> E.int32_bor e1 e2 | _ -> assert false)
  | Porint64 -> Js_long.or_ args
  | Porbigint -> (
      match args with [ e1; e2 ] -> E.bigint_op Bor e1 e2 | _ -> assert false)
  | Pxorint -> (
      match args with [ e1; e2 ] -> E.int32_bxor e1 e2 | _ -> assert false)
  | Pxorint64 -> Js_long.xor args
  | Pxorbigint -> (
      match args with [ e1; e2 ] -> E.bigint_op Bxor e1 e2 | _ -> assert false)
  | Pjscomp cmp -> (
      match args with [ l; r ] -> E.js_comp cmp l r | _ -> assert false)
  | Pfloatcomp cmp | Pintcomp cmp -> (
      (* Global Builtin Exception is an int, like
         [Not_found] or [Invalid_argument] ?
      *)
      match args with [ e1; e2 ] -> E.int_comp cmp e1 e2 | _ -> assert false)
  | Pbigintcomp cmp -> (
      match args with [ e1; e2 ] -> E.bigint_comp cmp e1 e2 | _ -> assert false)
  (* List --> stamp = 0
     Assert_false --> stamp = 26
  *)
  | Pint64comp cmp -> Js_long.comp cmp args
  | Pintoffloat -> (
      match args with [ e ] -> E.to_int32 e | _ -> assert false)
  | Pint64ofint -> Js_long.of_int32 args
  | Pfloatofint -> Ext_list.singleton_exn args
  | Pintofint64 -> Js_long.to_int32 args
  | Pnot -> E.not (Ext_list.singleton_exn args)
  | Poffsetint n -> E.offset (Ext_list.singleton_exn args) n
  | Poffsetref n ->
      let v =
        Js_of_lam_block.field Lambda.ref_field_info
          (Ext_list.singleton_exn args)
          0l
      in
      E.seq (E.assign v (E.offset v n)) E.unit
  | Psequand -> (
      (* TODO: rhs is possibly a tail call *)
      match args with [ e1; e2 ] -> E.and_ e1 e2 | _ -> assert false)
  | Psequor -> (
      (* TODO: rhs is possibly a tail call *)
      match args with [ e1; e2 ] -> E.or_ e1 e2 | _ -> assert false)
  | Pisout off -> (
      match args with
      (* predicate: [x > range  or x < 0 ]
         can be simplified if x is positive , x > range
         if x is negative, fine, its uint is for sure larger than range,
         the output is not readable, we might change it back.

         Note that if range is small like [1], then the negative of
         it can be more precise (given integer)
         a normal case of the compiler is  that it will do a shift
         in the first step [ (x - 1) > 1 or ( x - 1 ) < 0 ]
      *)
      | [ range; e ] -> E.is_out (E.offset e off) range
      | _ -> assert false)
  | Pbytes_to_string ->
      Js_of_lam_string.bytes_to_string (Ext_list.singleton_exn args)
  | Pstringlength -> E.string_length (Ext_list.singleton_exn args)
  | Pbyteslength -> E.bytes_length (Ext_list.singleton_exn args)
  (* This should only be Pbyteset(u|s), which in js, is an int array
     Bytes is an int array in javascript
  *)
  | Pbytessetu -> (
      match args with
      | [ e; e0; e1 ] ->
          ensure_value_unit cxt.continuation (Js_of_lam_string.set_byte e e0 e1)
      | _ -> assert false)
  | Pbytessets -> E.runtime_call Js_runtime_modules.bytes "set" args
  | Pbytesrefu -> (
      match args with
      | [ e; e1 ] -> Js_of_lam_string.ref_byte e e1
      | _ -> assert false)
  | Pbytesrefs -> E.runtime_call Js_runtime_modules.bytes "get" args
  | Pstringrefs -> E.runtime_call Js_runtime_modules.string "get" args
  (* For bytes and string, they both return [int] in ocaml
      we need tell Pbyteref from Pstringref
      1. Pbyteref -> a[i]
      2. Pstringref -> a.charCodeAt (a[i] is wrong)
  *)
  | Pstringrefu -> (
      match args with
      | [ e; e1 ] -> Js_of_lam_string.ref_string e e1
      | _ -> assert false)
  (* only when Lapply -> expand = true*)
  | Praise -> assert false (* handled before here *)
  (* Runtime encoding relevant *)
  | Parraylength -> E.array_length (Ext_list.singleton_exn args)
  | Psetfield (i, field_info) -> (
      match args with
      | [ e0; e1 ] ->
          (* RUNTIME *)
          ensure_value_unit cxt.continuation
            (Js_of_lam_block.set_field field_info e0 (Int32.of_int i) e1)
      (*TODO: get rid of [E.unit ()]*)
      | _ -> assert false)
  | Parrayrefu -> (
      match args with
      | [ e; e1 ] -> Js_of_lam_array.ref_array e e1 (* Todo: Constant Folding *)
      | _ -> assert false)
  | Parrayrefs -> E.runtime_call Js_runtime_modules.array "get" args
  | Parraysets -> E.runtime_call Js_runtime_modules.array "set" args
  | Pmakearray -> Js_of_lam_array.make_array Mutable args
  | Parrayspread -> Js_of_lam_array.spread_array args
  | Parraysetu -> (
      match args with
      (* wrong*)
      | [ e; e0; e1 ] ->
          ensure_value_unit cxt.continuation (Js_of_lam_array.set_array e e0 e1)
      | _ -> assert false)
  | Pccall prim -> Lam_dispatch_primitive.translate loc prim.prim_name args
  (* Lam_compile_external_call.translate loc cxt prim args *)
  (* Test if the argument is a block or an immediate integer *)
  | Pjs_object_create _ -> assert false
  | Pjs_call { arg_types; ffi; dynamic_import } ->
      Lam_compile_external_call.translate_ffi cxt arg_types ffi args ~dynamic_import
  (* FIXME, this can be removed later *)
  | Pisint -> E.is_type_number (Ext_list.singleton_exn args)
  | Pis_poly_var_block -> E.is_type_object (Ext_list.singleton_exn args)
  | Pctconst ct -> (
      match ct with
      | Big_endian -> E.bool Sys.big_endian
      | Ostype -> E.runtime_call Js_runtime_modules.sys "os_type" args
      | Ostype_unix ->
          E.string_equal
            (E.runtime_call Js_runtime_modules.sys "os_type" args)
            (E.str "Unix")
      | Ostype_win32 ->
          E.string_equal
            (E.runtime_call Js_runtime_modules.sys "os_type" args)
            (E.str "Win32")
      (* | Max_wosize ->
         (* max_array_length*)
         E.int 2147483647l (* 2 ^ 31 - 1 *) *)
      (* 4_294_967_295l  not representable*)
      (* 2 ^ 32 - 1*)
      | Backend_type ->
          E.make_block E.zero_int_literal
            (Blk_constructor { name = "Other"; num_nonconst = 1; tag = 0; attrs = [] })
            [ E.str "BS" ] Immutable)
  | Pduprecord -> Lam_dispatch_primitive.translate loc "?obj_dup" args
  | Plazyforce
  (* FIXME: we don't inline lazy force or at least
     let buckle handle it
  *)
  (* let parm = Ident.create "prim" in
     Lfunction(Curried, [parm],
               Matching.inline_lazy_force (Lvar parm) Location.none)
     It is inlined, this should not appear here *) ->
    assert false
