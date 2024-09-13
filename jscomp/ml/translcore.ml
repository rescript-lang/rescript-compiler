(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Typeopt
open Lambda

type error = Unknown_builtin_primitive of string | Unreachable_reached

exception Error of Location.t * error

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref
    (fun _cc _rootpath _modl -> assert false
      : module_coercion -> Path.t option -> module_expr -> lambda)

(* Compile an exception/extension definition *)

let transl_extension_constructor env path ext =
  let name =
    match path (*!Clflags.for_package*) with
    | None -> Ident.name ext.ext_id
    | Some p -> Path.name p
  in
  let loc = ext.ext_loc in
  match ext.ext_kind with
  | Text_decl _ -> Lprim (Pcreate_extension name, [], loc)
  | Text_rebind (path, _lid) -> transl_extension_path ~loc env path

(* Translation of primitives *)

type specialized = {
  gencomp : Lambda.primitive;
  intcomp : Lambda.primitive;
  boolcomp : Lambda.primitive;
  floatcomp : Lambda.primitive;
  stringcomp : Lambda.primitive;
  bytescomp : Lambda.primitive;
  int64comp : Lambda.primitive;
  bigintcomp : Lambda.primitive;
  simplify_constant_constructor : bool;
}

let arity2 name : Lambda.primitive =
  Lambda.Pccall (Primitive.simple ~name ~arity:2 ~alloc:true)

let comparisons_table =
  create_hashtable 11
    [
      ( "%equal",
        {
          gencomp =
            Pccall (Primitive.simple ~name:"caml_equal" ~arity:2 ~alloc:true);
          intcomp = Pintcomp Ceq;
          boolcomp =
            Pccall
              (Primitive.simple ~name:"caml_bool_equal" ~arity:2 ~alloc:false);
          floatcomp = Pfloatcomp Ceq;
          stringcomp =
            Pccall
              (Primitive.simple ~name:"caml_string_equal" ~arity:2 ~alloc:false);
          bytescomp =
            Pccall
              (Primitive.simple ~name:"caml_bytes_equal" ~arity:2 ~alloc:false);
          int64comp = Pbintcomp (Pint64, Ceq);
          bigintcomp = Pbigintcomp Ceq;
          simplify_constant_constructor = true;
        } );
      ( "%notequal",
        {
          gencomp =
            Pccall (Primitive.simple ~name:"caml_notequal" ~arity:2 ~alloc:true);
          intcomp = Pintcomp Cneq;
          boolcomp =
            Pccall
              (Primitive.simple ~name:"caml_bool_notequal" ~arity:2 ~alloc:false);
          floatcomp = Pfloatcomp Cneq;
          stringcomp =
            Pccall
              (Primitive.simple ~name:"caml_string_notequal" ~arity:2
                 ~alloc:false);
          bytescomp =
            Pccall
              (Primitive.simple ~name:"caml_bytes_notequal" ~arity:2
                 ~alloc:false);
          int64comp = Pbintcomp (Pint64, Cneq);
          bigintcomp = Pbigintcomp Cneq;
          simplify_constant_constructor = true;
        } );
      ( "%lessthan",
        {
          gencomp =
            Pccall (Primitive.simple ~name:"caml_lessthan" ~arity:2 ~alloc:true);
          intcomp = Pintcomp Clt;
          boolcomp =
            Pccall
              (Primitive.simple ~name:"caml_bool_lessthan" ~arity:2 ~alloc:false);
          floatcomp = Pfloatcomp Clt;
          stringcomp =
            Pccall
              (Primitive.simple ~name:"caml_string_lessthan" ~arity:2
                 ~alloc:false);
          bytescomp =
            Pccall
              (Primitive.simple ~name:"caml_bytes_lessthan" ~arity:2
                 ~alloc:false);
          int64comp = Pbintcomp (Pint64, Clt);
          bigintcomp = Pbigintcomp Clt;
          simplify_constant_constructor = false;
        } );
      ( "%greaterthan",
        {
          gencomp =
            Pccall
              (Primitive.simple ~name:"caml_greaterthan" ~arity:2 ~alloc:true);
          intcomp = Pintcomp Cgt;
          boolcomp =
            Pccall
              (Primitive.simple ~name:"caml_bool_greaterthan" ~arity:2
                 ~alloc:false);
          floatcomp = Pfloatcomp Cgt;
          stringcomp =
            Pccall
              (Primitive.simple ~name:"caml_string_greaterthan" ~arity:2
                 ~alloc:false);
          bytescomp =
            Pccall
              (Primitive.simple ~name:"caml_bytes_greaterthan" ~arity:2
                 ~alloc:false);
          int64comp = Pbintcomp (Pint64, Cgt);
          bigintcomp = Pbigintcomp Cgt;
          simplify_constant_constructor = false;
        } );
      ( "%lessequal",
        {
          gencomp =
            Pccall
              (Primitive.simple ~name:"caml_lessequal" ~arity:2 ~alloc:true);
          intcomp = Pintcomp Cle;
          boolcomp =
            Pccall
              (Primitive.simple ~name:"caml_bool_lessequal" ~arity:2
                 ~alloc:false);
          floatcomp = Pfloatcomp Cle;
          stringcomp =
            Pccall
              (Primitive.simple ~name:"caml_string_lessequal" ~arity:2
                 ~alloc:false);
          bytescomp =
            Pccall
              (Primitive.simple ~name:"caml_bytes_lessequal" ~arity:2
                 ~alloc:false);
          int64comp = Pbintcomp (Pint64, Cle);
          bigintcomp = Pbigintcomp Cle;
          simplify_constant_constructor = false;
        } );
      ( "%greaterequal",
        {
          gencomp =
            Pccall
              (Primitive.simple ~name:"caml_greaterequal" ~arity:2 ~alloc:true);
          intcomp = Pintcomp Cge;
          boolcomp =
            Pccall
              (Primitive.simple ~name:"caml_bool_greaterequal" ~arity:2
                 ~alloc:false);
          floatcomp = Pfloatcomp Cge;
          stringcomp =
            Pccall
              (Primitive.simple ~name:"caml_string_greaterequal" ~arity:2
                 ~alloc:false);
          bytescomp =
            Pccall
              (Primitive.simple ~name:"caml_bytes_greaterequal" ~arity:2
                 ~alloc:false);
          int64comp = Pbintcomp (Pint64, Cge);
          bigintcomp = Pbigintcomp Cge;
          simplify_constant_constructor = false;
        } );
      ( "%compare",
        {
          gencomp =
            Pccall (Primitive.simple ~name:"caml_compare" ~arity:2 ~alloc:true);
          (* Not unboxed since the comparison is done directly on tagged int *)
          intcomp =
            Pccall
              (Primitive.simple ~name:"caml_int_compare" ~arity:2 ~alloc:false);
          boolcomp =
            Pccall
              (Primitive.simple ~name:"caml_bool_compare" ~arity:2 ~alloc:false);
          floatcomp =
            Pccall
              (Primitive.simple ~name:"caml_float_compare" ~arity:2 ~alloc:false);
          stringcomp =
            Pccall
              (Primitive.simple ~name:"caml_string_compare" ~arity:2
                 ~alloc:false);
          bytescomp =
            Pccall
              (Primitive.simple ~name:"caml_bytes_compare" ~arity:2 ~alloc:false);
          int64comp =
            Pccall
              (Primitive.simple ~name:"caml_int64_compare" ~arity:2 ~alloc:false);
          bigintcomp =
            Pccall
              (Primitive.simple ~name:"caml_bigint_compare" ~arity:2 ~alloc:false);
          simplify_constant_constructor = false;
        } );
      ( "%bs_max",
        {
          gencomp = arity2 "caml_max";
          bytescomp = arity2 "caml_max";
          (* FIXME bytescomp*)
          intcomp = arity2 "caml_int_max";
          boolcomp = arity2 "caml_bool_max";
          floatcomp = arity2 "caml_float_max";
          stringcomp = arity2 "caml_string_max";
          int64comp = arity2 "caml_int64_max";
          bigintcomp = arity2 "caml_bigint_max";
          simplify_constant_constructor = false;
        } );
      ( "%bs_min",
        {
          gencomp = arity2 "caml_min";
          bytescomp = arity2 "caml_min";
          intcomp = arity2 "caml_int_min";
          boolcomp = arity2 "caml_bool_min";
          floatcomp = arity2 "caml_float_min";
          stringcomp = arity2 "caml_string_min";
          int64comp = arity2 "caml_int64_min";
          bigintcomp = arity2 "caml_bigint_min";
          simplify_constant_constructor = false;
        } );
      ( "%bs_equal_null",
        {
          gencomp = arity2 "caml_equal_null";
          bytescomp = arity2 "caml_equal_null";
          (* FIXME*)
          intcomp = arity2 "caml_int_equal_null";
          boolcomp = arity2 "caml_bool_equal_null";
          floatcomp = arity2 "caml_float_equal_null";
          stringcomp = arity2 "caml_string_equal_null";
          int64comp = arity2 "caml_int64_equal_null";
          bigintcomp = arity2 "caml_bigint_equal_null";
          simplify_constant_constructor = true;
        } );
      ( "%bs_equal_undefined",
        {
          gencomp = arity2 "caml_equal_undefined";
          bytescomp = arity2 "caml_equal_undefined";
          (* FIXME*)
          intcomp = arity2 "caml_int_equal_undefined";
          boolcomp = arity2 "caml_bool_equal_undefined";
          floatcomp = arity2 "caml_float_equal_undefined";
          stringcomp = arity2 "caml_string_equal_undefined";
          int64comp = arity2 "caml_int64_equal_undefined";
          bigintcomp = arity2 "caml_bigint_equal_undefined";
          simplify_constant_constructor = true;
        } );
      ( "%bs_equal_nullable",
        {
          gencomp = arity2 "caml_equal_nullable";
          bytescomp = arity2 "caml_equal_nullable";
          (* FIXME *)
          intcomp = arity2 "caml_int_equal_nullable";
          boolcomp = arity2 "caml_bool_equal_nullable";
          floatcomp = arity2 "caml_float_equal_nullable";
          stringcomp = arity2 "caml_string_equal_nullable";
          int64comp = arity2 "caml_int64_equal_nullable";
          bigintcomp = arity2 "caml_bigint_equal_nullable";
          simplify_constant_constructor = true;
        } );
    ]

let primitives_table =
  create_hashtable 57
    [
      ("%identity", Pidentity);
      ("%bytes_to_string", Pbytes_to_string);
      ("%ignore", Pignore);
      ("%revapply", Prevapply);
      ("%apply", Pdirapply);
      ("%loc_LOC", Ploc Loc_LOC);
      ("%loc_FILE", Ploc Loc_FILE);
      ("%loc_LINE", Ploc Loc_LINE);
      ("%loc_POS", Ploc Loc_POS);
      ("%loc_MODULE", Ploc Loc_MODULE);
      (* BEGIN Triples for  ref data type *)
      ("%bs_ref_setfield0", Psetfield (0, Lambda.ref_field_set_info));
      ("%bs_ref_field0", Pfield (0, Lambda.ref_field_info));
      ("%makemutable", Pmakeblock Lambda.ref_tag_info);
      ("%incr", Poffsetref 1);
      ("%decr", Poffsetref (-1));
      (* Finish Triples for  ref data type *)
      ("%field0", Pfield (0, Fld_tuple));
      ("%field1", Pfield (1, Fld_tuple));
      ("%obj_field", Parrayrefu);
      ("%obj_set_field", Parraysetu);
      ("%obj_is_int", Pisint);
      ("%raise", Praise Raise_regular);
      ("%reraise", Praise Raise_reraise);
      ("%raise_notrace", Praise Raise_notrace);
      ("%sequand", Psequand);
      ("%sequor", Psequor);
      ("%boolnot", Pnot);
      ("%big_endian", Pctconst Big_endian);
      ("%backend_type", Pctconst Backend_type);
      ("%word_size", Pctconst Word_size);
      ("%int_size", Pctconst Int_size);
      ("%max_wosize", Pctconst Max_wosize);
      ("%ostype_unix", Pctconst Ostype_unix);
      ("%ostype_win32", Pctconst Ostype_win32);
      ("%ostype_cygwin", Pctconst Ostype_cygwin);
      ("%negint", Pnegint);
      ("%succint", Poffsetint 1);
      ("%predint", Poffsetint (-1));
      ("%addint", Paddint);
      ("%subint", Psubint);
      ("%mulint", Pmulint);
      ("%divint", Pdivint Safe);
      ("%modint", Pmodint Safe);
      ("%andint", Pandint);
      ("%orint", Porint);
      ("%xorint", Pxorint);
      ("%lslint", Plslint);
      ("%lsrint", Plsrint);
      ("%asrint", Pasrint);
      ("%andbigint", Pandbigint);
      ("%orbigint", Porbigint);
      ("%xorbigint", Pxorbigint);
      ("%lslbigint", Plslbigint);
      ("%asrbigint", Pasrbigint);
      ("%eq", Pintcomp Ceq);
      ("%noteq", Pintcomp Cneq);
      ("%ltint", Pintcomp Clt);
      ("%leint", Pintcomp Cle);
      ("%gtint", Pintcomp Cgt);
      ("%geint", Pintcomp Cge);
      ("%intoffloat", Pintoffloat);
      ("%floatofint", Pfloatofint);
      ("%negfloat", Pnegfloat);
      ("%absfloat", Pabsfloat);
      ("%addfloat", Paddfloat);
      ("%subfloat", Psubfloat);
      ("%mulfloat", Pmulfloat);
      ("%divfloat", Pdivfloat);
      ("%eqfloat", Pfloatcomp Ceq);
      ("%noteqfloat", Pfloatcomp Cneq);
      ("%ltfloat", Pfloatcomp Clt);
      ("%lefloat", Pfloatcomp Cle);
      ("%gtfloat", Pfloatcomp Cgt);
      ("%gefloat", Pfloatcomp Cge);
      ("%negbigint", Pnegbigint);
      ("%addbigint", Paddbigint);
      ("%subbigint", Psubbigint);
      ("%mulbigint", Pmulbigint);
      ("%divbigint", Pdivbigint);
      ("%powbigint", Ppowbigint);
      ("%modbigint", Pmodbigint);
      ("%eqbigint", Pbigintcomp Ceq);
      ("%noteqbigint", Pbigintcomp Cneq);
      ("%ltbigint", Pbigintcomp Clt);
      ("%lebigint", Pbigintcomp Cle);
      ("%gtbigint", Pbigintcomp Cgt);
      ("%gebigint", Pbigintcomp Cge);
      ("%string_length", Pstringlength);
      ("%string_safe_get", Pstringrefs);
      ("%string_unsafe_get", Pstringrefu);
      ("%bytes_length", Pbyteslength);
      ("%bytes_safe_get", Pbytesrefs);
      ("%bytes_safe_set", Pbytessets);
      ("%bytes_unsafe_get", Pbytesrefu);
      ("%bytes_unsafe_set", Pbytessetu);
      ("%array_length", Parraylength);
      ("%array_safe_get", Parrayrefs);
      ("%array_safe_set", Parraysets);
      ("%array_unsafe_get", Parrayrefu);
      ("%array_unsafe_set", Parraysetu);
      ("%array_spread", Parrayspread);
      ("%floatarray_length", Parraylength);
      ("%floatarray_safe_get", Parrayrefs);
      ("%floatarray_safe_set", Parraysets);
      ("%floatarray_unsafe_get", Parrayrefu);
      ("%floatarray_unsafe_set", Parraysetu);
      ("%lazy_force", Plazyforce);
      ("%int64_of_int", Pbintofint Pint64);
      ("%int64_to_int", Pintofbint Pint64);
      ("%int64_neg", Pnegbint Pint64);
      ("%int64_add", Paddbint Pint64);
      ("%int64_sub", Psubbint Pint64);
      ("%int64_mul", Pmulbint Pint64);
      ("%int64_div", Pdivbint { size = Pint64; is_safe = Safe });
      ("%int64_mod", Pmodbint { size = Pint64; is_safe = Safe });
      ("%int64_and", Pandbint Pint64);
      ("%int64_or", Porbint Pint64);
      ("%int64_xor", Pxorbint Pint64);
      ("%int64_lsl", Plslbint Pint64);
      ("%int64_lsr", Plsrbint Pint64);
      ("%int64_asr", Pasrbint Pint64);
      ("%bigint_of_int32", Pcvtbint (Pint32, Pbigint));
      ("%bigint_to_int32", Pcvtbint (Pbigint, Pint32));
      ("%int64_of_int32", Pcvtbint (Pint32, Pint64));
      ("%int64_to_int32", Pcvtbint (Pint64, Pint32));
      ("%int64_of_bigint", Pcvtbint (Pbigint, Pint64));
      ("%int64_to_bigint", Pcvtbint (Pint64, Pbigint));
    ]

let find_primitive prim_name = Hashtbl.find primitives_table prim_name

let specialize_comparison
    ({ gencomp; intcomp; floatcomp; stringcomp; bytescomp; int64comp; bigintcomp; boolcomp } :
      specialized) env ty =
  match () with
  | ()
    when is_base_type env ty Predef.path_int
         || is_base_type env ty Predef.path_char
         || maybe_pointer_type env ty = Immediate ->
      intcomp
  | () when is_base_type env ty Predef.path_float -> floatcomp
  | () when is_base_type env ty Predef.path_string -> stringcomp
  | () when is_base_type env ty Predef.path_bytes -> bytescomp
  | () when is_base_type env ty Predef.path_int64 -> int64comp
  | () when is_base_type env ty Predef.path_bigint -> bigintcomp
  | () when is_base_type env ty Predef.path_bool -> boolcomp
  | () -> gencomp

(* Specialize a primitive from available type information,
   raise Not_found if primitive is unknown  *)

let specialize_primitive p env ty (* ~has_constant_constructor *) =
  try
    let table = Hashtbl.find comparisons_table p.prim_name in
    match is_function_type env ty with
    | Some (lhs, _rhs) -> specialize_comparison table env lhs
    | None -> table.gencomp
  with Not_found -> find_primitive p.prim_name

(* Eta-expand a primitive *)

let transl_primitive loc p env ty =
  (* Printf.eprintf "----transl_primitive %s----\n" p.prim_name; *)
  let prim =
    try specialize_primitive p env ty (* ~has_constant_constructor:false *)
    with Not_found -> Pccall p
  in
  match prim with
  | Plazyforce ->
      let parm = Ident.create "prim" in
      Lfunction
        {
          params = [ parm ];
          body = Matching.inline_lazy_force (Lvar parm) Location.none;
          loc;
          attr = default_function_attribute;
        }
  | Ploc kind -> (
      let lam = lam_of_loc kind loc in
      match p.prim_arity with
      | 0 -> lam
      | 1 ->
          (* TODO: we should issue a warning ? *)
          let param = Ident.create "prim" in
          Lfunction
            {
              params = [ param ];
              attr = default_function_attribute;
              loc;
              body = Lprim (Pmakeblock Blk_tuple, [ lam; Lvar param ], loc);
            }
      | _ -> assert false)
  | _ ->
      let rec make_params n total =
        if n <= 0 then []
        else
          Ident.create ("prim" ^ string_of_int (total - n))
          :: make_params (n - 1) total
      in
      let prim_arity = p.prim_arity in
      if p.prim_from_constructor || prim_arity = 0 then Lprim (prim, [], loc)
      else
        let params =
          if prim_arity = 1 then [ Ident.create "prim" ]
          else make_params prim_arity prim_arity
        in
        Lfunction
          {
            params;
            attr = default_function_attribute;
            loc;
            body = Lprim (prim, List.map (fun id -> Lvar id) params, loc);
          }

let transl_primitive_application loc prim env ty args =
  let prim_name = prim.prim_name in
  try
    match args with
    | [ arg1; _ ]
      when is_base_type env arg1.exp_type Predef.path_bool
           && Hashtbl.mem comparisons_table prim_name ->
        (Hashtbl.find comparisons_table prim_name).boolcomp
    | _ ->
        let has_constant_constructor =
          match args with
          | [
              _;
              {
                exp_desc = Texp_construct (_, { cstr_tag = Cstr_constant _ }, _);
              };
            ]
          | [
              {
                exp_desc = Texp_construct (_, { cstr_tag = Cstr_constant _ }, _);
              };
              _;
            ]
          | [ _; { exp_desc = Texp_variant (_, None) } ]
          | [ { exp_desc = Texp_variant (_, None) }; _ ] ->
              true
          | _ -> false
        in
        if has_constant_constructor then
          match Hashtbl.find_opt comparisons_table prim_name with
          | Some table when table.simplify_constant_constructor -> table.intcomp
          | Some _ | None -> specialize_primitive prim env ty
          (* ~has_constant_constructor*)
        else specialize_primitive prim env ty
  with Not_found ->
    if String.length prim_name > 0 && prim_name.[0] = '%' then
      raise (Error (loc, Unknown_builtin_primitive prim_name));
    Pccall prim

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
  | Lconst sc -> sc
  | _ -> raise_notrace Not_constant

(* Push the default values under the functional abstractions *)
(* Also push bindings of module patterns, since this sound *)

type binding =
  | Bind_value of value_binding list
  | Bind_module of Ident.t * string loc * module_expr

let rec push_defaults loc bindings cases partial =
  match cases with
  | [
   {
     c_lhs = pat;
     c_guard = None;
     c_rhs =
       { exp_desc = Texp_function { arg_label; param; cases; partial } } as exp;
   };
  ] ->
      let cases = push_defaults exp.exp_loc bindings cases partial in
      [
        {
          c_lhs = pat;
          c_guard = None;
          c_rhs =
            {
              exp with
              exp_desc = Texp_function { arg_label; param; cases; partial };
            };
        };
      ]
  | [
   {
     c_lhs = pat;
     c_guard = None;
     c_rhs =
       {
         exp_attributes = [ ({ txt = "#default" }, _) ];
         exp_desc =
           Texp_let (Nonrecursive, binds, ({ exp_desc = Texp_function _ } as e2));
       };
   };
  ] ->
      push_defaults loc
        (Bind_value binds :: bindings)
        [ { c_lhs = pat; c_guard = None; c_rhs = e2 } ]
        partial
  | [
   {
     c_lhs = pat;
     c_guard = None;
     c_rhs =
       {
         exp_attributes = [ ({ txt = "#modulepat" }, _) ];
         exp_desc =
           Texp_letmodule
             (id, name, mexpr, ({ exp_desc = Texp_function _ } as e2));
       };
   };
  ] ->
      push_defaults loc
        (Bind_module (id, name, mexpr) :: bindings)
        [ { c_lhs = pat; c_guard = None; c_rhs = e2 } ]
        partial
  | [ case ] ->
      let exp =
        List.fold_left
          (fun exp binds ->
            {
              exp with
              exp_desc =
                (match binds with
                | Bind_value binds -> Texp_let (Nonrecursive, binds, exp)
                | Bind_module (id, name, mexpr) ->
                    Texp_letmodule (id, name, mexpr, exp));
            })
          case.c_rhs bindings
      in
      [ { case with c_rhs = exp } ]
  | { c_lhs = pat; c_rhs = exp; c_guard = _ } :: _ when bindings <> [] ->
      let param = Typecore.name_pattern "param" cases in
      let name = Ident.name param in
      let exp =
        {
          exp with
          exp_loc = loc;
          exp_desc =
            Texp_match
              ( {
                  exp with
                  exp_type = pat.pat_type;
                  exp_desc =
                    Texp_ident
                      ( Path.Pident param,
                        mknoloc (Longident.Lident name),
                        {
                          val_type = pat.pat_type;
                          val_kind = Val_reg;
                          val_attributes = [];
                          Types.val_loc = Location.none;
                        } );
                },
                cases,
                [],
                partial );
        }
      in
      push_defaults loc bindings
        [
          {
            c_lhs = { pat with pat_desc = Tpat_var (param, mknoloc name) };
            c_guard = None;
            c_rhs = exp;
          };
        ]
        Total
  | _ -> cases



(* Assertions *)

let assert_failed exp =
  let fname, line, char =
    Location.get_pos_info exp.exp_loc.Location.loc_start
  in
  let fname = Filename.basename fname in
  Lprim
    ( Praise Raise_regular,
      [
        Lprim
          ( Pmakeblock Blk_extension,
            [
              transl_normal_path Predef.path_assert_failure;
              Lconst
                (Const_block
                   ( Blk_tuple,
                     [
                       Const_base (Const_string (fname, None));
                       Const_base (Const_int line);
                       Const_base (Const_int char);
                     ] ));
            ],
            exp.exp_loc );
      ],
      exp.exp_loc )

let rec cut n l =
  if n = 0 then ([], l)
  else
    match l with
    | [] -> failwith "Translcore.cut"
    | a :: l ->
        let l1, l2 = cut (n - 1) l in
        (a :: l1, l2)

(* Translation of expressions *)

let try_ids = Hashtbl.create 8

let has_async_attribute exp = exp.exp_attributes |> List.exists (fun ({txt}, _payload) -> txt = "res.async")

let extract_directive_for_fn exp = 
  exp.exp_attributes |> List.find_map (
    fun ({txt}, payload) -> if txt = "directive" then Ast_payload.is_single_string payload else None)


let rec transl_exp e =
  List.iter (Translattribute.check_attribute e) e.exp_attributes;
  transl_exp0 e

and transl_exp0 (e : Typedtree.expression) : Lambda.lambda =
  match e.exp_desc with
  | Texp_ident (_, _, { val_kind = Val_prim p }) ->
      transl_primitive e.exp_loc p e.exp_env e.exp_type
  | Texp_ident (path, _, { val_kind = Val_reg }) ->
      transl_value_path ~loc:e.exp_loc e.exp_env path
  | Texp_constant cst -> Lconst (Const_base cst)
  | Texp_let (rec_flag, pat_expr_list, body) ->
      transl_let rec_flag pat_expr_list (transl_exp body)
  | Texp_function { arg_label = _; param; cases; partial } ->
      let async = has_async_attribute e in
      let directive = (
        match extract_directive_for_fn e with 
        | None -> None 
        | Some (directive, _) -> Some directive
      ) in
      let params, body, return_unit =
        let pl = push_defaults e.exp_loc [] cases partial in
        transl_function e.exp_loc partial param pl
      in
      let attr =
        {
          default_function_attribute with
          inline = Translattribute.get_inline_attribute e.exp_attributes;
          async;
          return_unit;
          directive;
        }
      in
      let loc = e.exp_loc in
      Lfunction { params; body; attr; loc }
  | Texp_apply
      ( ({
           exp_desc = Texp_ident (_, _, { val_kind = Val_prim p });
           exp_type = prim_type;
         } as funct),
        oargs )
    when List.length oargs >= p.prim_arity
         && List.for_all (fun (_, arg) -> arg <> None) oargs -> (
      let args, args' = cut p.prim_arity oargs in
      let wrap f =
        if args' = [] then f
        else
          let inlined, _ =
            Translattribute.get_and_remove_inlined_attribute funct
          in
          transl_apply ~inlined f args' e.exp_loc
      in
      let args =
        List.map (function _, Some x -> x | _ -> assert false) args
      in
      let argl = transl_list args in
      let prim =
        transl_primitive_application e.exp_loc p e.exp_env prim_type args
      in
      match (prim, args) with
      | Praise k, [ _ ] ->
          let targ = List.hd argl in
          let k =
            match (k, targ) with
            | Raise_regular, Lvar id when Hashtbl.mem try_ids id ->
                Raise_reraise
            | _ -> k
          in
          wrap (Lprim (Praise k, [ targ ], e.exp_loc))
      | Ploc kind, [] -> lam_of_loc kind e.exp_loc
      | Ploc kind, [ arg1 ] ->
          let lam = lam_of_loc kind arg1.exp_loc in
          Lprim (Pmakeblock Blk_tuple, lam :: argl, e.exp_loc)
      | Ploc _, _ -> assert false
      | _, _ -> (
          match (prim, argl) with
          | Plazyforce, [ a ] -> wrap (Matching.inline_lazy_force a e.exp_loc)
          | Plazyforce, _ -> assert false
          | _ ->
              wrap (Lprim (prim, argl, e.exp_loc))
              ))
  | Texp_apply (funct, oargs) ->
      let inlined, funct =
        Translattribute.get_and_remove_inlined_attribute funct
      in
      let uncurried_partial_application =
        (* In case of partial application foo(args, ...) when some args are missing,
           get the arity *)
        let uncurried_partial_app = Ext_list.exists e.exp_attributes (fun ({txt },_) -> txt = "res.partial") in
        if uncurried_partial_app then
          let arity_opt = Ast_uncurried.uncurried_type_get_arity_opt ~env:funct.exp_env funct.exp_type in
          match arity_opt with
            | Some arity ->
              let real_args = List.filter (fun (_, x) -> Option.is_some x) oargs in
              if arity > List.length real_args then
                Some arity
              else None 
            | None -> None
        else
          None in
      transl_apply ~inlined ~uncurried_partial_application (transl_exp funct) oargs e.exp_loc
  | Texp_match (arg, pat_expr_list, exn_pat_expr_list, partial) ->
      transl_match e arg pat_expr_list exn_pat_expr_list partial
  | Texp_try (body, pat_expr_list) ->
      let id = Typecore.name_pattern "exn" pat_expr_list in
      Ltrywith
        ( transl_exp body,
          id,
          Matching.for_trywith (Lvar id) (transl_cases_try pat_expr_list) )
  | Texp_tuple el -> (
      let ll = transl_list el in
      try Lconst (Const_block (Blk_tuple, List.map extract_constant ll))
      with Not_constant -> Lprim (Pmakeblock Blk_tuple, ll, e.exp_loc))
  | Texp_construct ({ txt = Lident "false" }, _, []) -> Lconst Const_false
  | Texp_construct ({ txt = Lident "true" }, _, []) -> Lconst Const_true
  | Texp_construct ({ txt = Lident "Function$"}, _, [expr]) ->
      (* ReScript uncurried encoding *)
      let loc = expr.exp_loc in
      let lambda = transl_exp expr in
      let arity = Ast_uncurried.uncurried_type_get_arity ~env:e.exp_env e.exp_type in
      let arity_s = arity |> string_of_int in
      let name = match (Ctype.expand_head expr.exp_env expr.exp_type).desc with
      | Tarrow (Nolabel, t, _, _) -> (
        match (Ctype.expand_head expr.exp_env t).desc with
        | Tconstr (Pident {name= "unit"}, [], _) -> "#fn_mk_unit"
        | _ -> "#fn_mk"
      )
      | _ -> "#fn_mk" in
      let prim =
        Primitive.make ~name ~alloc:true ~native_name:arity_s ~arity:1
      in
      Lprim
        ( Pccall prim
          (* could be replaced with Opaque in the future except arity 0*),
          [ lambda ],
          loc )
  | Texp_construct (lid, cstr, args) -> (
      let ll = transl_list args in
      if cstr.cstr_inlined <> None then
        match ll with [ x ] -> x | _ -> assert false
      else
        match cstr.cstr_tag with
        | Cstr_constant n ->
            Lconst
              (Const_pointer
                 ( n,
                   match lid.txt with
                   | Longident.Ldot (Longident.Lident "*predef*", "None")
                   | Longident.Lident "None"
                     when Datarepr.constructor_has_optional_shape cstr ->
                       Pt_shape_none
                   | _ ->
                       if Datarepr.constructor_has_optional_shape cstr then
                         Pt_shape_none
                       else
                         Pt_constructor
                           {
                             name = cstr.cstr_name;
                             const = cstr.cstr_consts;
                             non_const = cstr.cstr_nonconsts;
                             attrs = cstr.cstr_attributes;
                           } ))
        | Cstr_unboxed -> ( match ll with [ v ] -> v | _ -> assert false)
        | Cstr_block n -> (
            let tag_info : Lambda.tag_info =
              if Datarepr.constructor_has_optional_shape cstr then
                match args with
                | [ arg ]
                  when Typeopt.type_cannot_contain_undefined arg.exp_type
                         arg.exp_env ->
                    (* Format.fprintf Format.err_formatter "@[special boxingl@]@."; *)
                    Blk_some_not_nested
                | _ -> Blk_some
              else
                Blk_constructor
                  {
                    name = cstr.cstr_name;
                    num_nonconst = cstr.cstr_nonconsts;
                    tag = n;
                    attrs = cstr.cstr_attributes;
                  }
            in
            try Lconst (Const_block (tag_info, List.map extract_constant ll))
            with Not_constant -> Lprim (Pmakeblock tag_info, ll, e.exp_loc))
        | Cstr_extension (path) ->
            Lprim
              ( Pmakeblock Blk_extension,
                transl_extension_path e.exp_env path :: ll,
                e.exp_loc ))
  | Texp_extension_constructor (_, path) -> transl_extension_path e.exp_env path
  | Texp_variant (l, arg) -> (
      let tag = Btype.hash_variant l in
      match arg with
      | None -> Lconst (Const_pointer (tag, Pt_variant { name = l }))
      | Some arg -> (
          let lam = transl_exp arg in
          let tag_info = Blk_poly_var l in
          try
            Lconst
              (Const_block
                 (tag_info, [ Const_base (Const_int tag); extract_constant lam ]))
          with Not_constant ->
            Lprim
              ( Pmakeblock tag_info,
                [ Lconst (Const_base (Const_int tag)); lam ],
                e.exp_loc )))
  | Texp_record { fields; representation; extended_expression } ->
      transl_record e.exp_loc e.exp_env fields representation
        extended_expression
  | Texp_field (arg, _, lbl) -> (
      let targ = transl_exp arg in
      match lbl.lbl_repres with
      | Record_float_unused -> assert false
      | Record_regular | Record_optional_labels _ ->
          Lprim
            (Pfield (lbl.lbl_pos, Lambda.fld_record lbl), [ targ ], e.exp_loc)
      | Record_inlined _ ->
          Lprim
            ( Pfield (lbl.lbl_pos, Lambda.fld_record_inline lbl),
              [ targ ],
              e.exp_loc )
      | Record_unboxed _ -> targ
      | Record_extension ->
          Lprim
            ( Pfield
                (lbl.lbl_pos + 1, Lambda.fld_record_extension lbl),
              [ targ ],
              e.exp_loc ))
  | Texp_setfield (arg, _, lbl, newval) ->
      let access =
        match lbl.lbl_repres with
        | Record_float_unused -> assert false
        | Record_regular | Record_optional_labels _ ->
            Psetfield (lbl.lbl_pos, Lambda.fld_record_set lbl)
        | Record_inlined _ ->
            Psetfield (lbl.lbl_pos, Lambda.fld_record_inline_set lbl)
        | Record_unboxed _ -> assert false
        | Record_extension ->
            Psetfield (lbl.lbl_pos + 1, Lambda.fld_record_extension_set lbl)
      in
      Lprim (access, [ transl_exp arg; transl_exp newval ], e.exp_loc)
  | Texp_array expr_list ->
      let ll = transl_list expr_list in
      Lprim (Pmakearray Mutable, ll, e.exp_loc)
  | Texp_ifthenelse (cond, ifso, Some ifnot) ->
      Lifthenelse (transl_exp cond, transl_exp ifso, transl_exp ifnot)
  | Texp_ifthenelse (cond, ifso, None) ->
      Lifthenelse (transl_exp cond, transl_exp ifso, lambda_unit)
  | Texp_sequence (expr1, expr2) ->
      Lsequence (transl_exp expr1, transl_exp expr2)
  | Texp_while (cond, body) -> Lwhile (transl_exp cond, transl_exp body)
  | Texp_for (param, _, low, high, dir, body) ->
      Lfor (param, transl_exp low, transl_exp high, dir, transl_exp body)
  | Texp_send (expr, Tmeth_name nm, _) ->
      let obj = transl_exp expr in
      Lsend (nm, obj, e.exp_loc)
  | Texp_new _ | Texp_instvar _ | Texp_setinstvar _ | Texp_override _ ->
      assert false
  | Texp_letmodule (id, _loc, modl, body) ->
      let defining_expr = !transl_module Tcoerce_none None modl in
      Llet (Strict, Pgenval, id, defining_expr, transl_exp body)
  | Texp_letexception (cd, body) ->
      Llet
        ( Strict,
          Pgenval,
          cd.ext_id,
          transl_extension_constructor e.exp_env None cd,
          transl_exp body )
  | Texp_pack modl -> !transl_module Tcoerce_none None modl
  | Texp_assert { exp_desc = Texp_construct (_, { cstr_name = "false" }, _) } ->
      if !Clflags.no_assert_false then Lambda.lambda_assert_false
      else assert_failed e
  | Texp_assert cond ->
      if !Clflags.noassert then lambda_unit
      else Lifthenelse (transl_exp cond, lambda_unit, assert_failed e)
  | Texp_lazy e ->
      (* when e needs no computation (constants, identifiers, ...), we
         optimize the translation just as Lazy.lazy_from_val would
         do *)
      Lprim (Pmakeblock Blk_lazy_general, [ transl_exp e ], e.exp_loc)
  | Texp_object () -> assert false
  | Texp_unreachable -> raise (Error (e.exp_loc, Unreachable_reached))

and transl_list expr_list = List.map transl_exp expr_list

and transl_guard guard rhs =
  let expr = transl_exp rhs in
  match guard with
  | None -> expr
  | Some cond -> Lifthenelse (transl_exp cond, expr, staticfail)

and transl_case { c_lhs; c_guard; c_rhs } = (c_lhs, transl_guard c_guard c_rhs)

and transl_cases cases =
  let cases =
    Ext_list.filter cases (fun c -> c.c_rhs.exp_desc <> Texp_unreachable)
  in
  List.map transl_case cases

and transl_case_try { c_lhs; c_guard; c_rhs } =
  match c_lhs.pat_desc with
  | Tpat_var (id, _) | Tpat_alias (_, id, _) ->
      Hashtbl.replace try_ids id ();
      Misc.try_finally
        (fun () -> (c_lhs, transl_guard c_guard c_rhs))
        (fun () -> Hashtbl.remove try_ids id)
  | _ -> (c_lhs, transl_guard c_guard c_rhs)

and transl_cases_try cases =
  let cases =
    Ext_list.filter cases (fun c -> c.c_rhs.exp_desc <> Texp_unreachable)
  in
  List.map transl_case_try cases

and transl_apply ?(inlined = Default_inline) ?(uncurried_partial_application=None) lam sargs loc =
  let lapply ap_func ap_args =
    Lapply { ap_loc = loc; ap_func; ap_args; ap_inlined = inlined } in
  let rec build_apply lam args = function
    | (None, optional) :: l ->
        let defs = ref [] in
        let protect name lam =
          match lam with
          | Lvar _ | Lconst _ -> lam
          | _ ->
              let id = Ident.create name in
              defs := (id, lam) :: !defs;
              Lvar id
        in
        let args, args' =
          if List.for_all (fun (_, opt) -> opt) args then ([], args)
          else (args, [])
        in
        let lam =
          if args = [] then lam else lapply lam (List.rev_map fst args)
        in
        let handle = protect "func" lam
        and l =
          List.map (fun (arg, opt) -> (may_map (protect "arg") arg, opt)) l
        and id_arg = Ident.create "param" in
        let body =
          match build_apply handle ((Lvar id_arg, optional) :: args') l with
          | Lfunction { params = ids; body = lam; attr; loc } ->
              Lfunction { params = id_arg :: ids; body = lam; attr; loc }
          | lam ->
              Lfunction
                {
                  params = [ id_arg ];
                  body = lam;
                  attr = default_function_attribute;
                  loc;
                }
        in
        List.fold_left
          (fun body (id, lam) -> Llet (Strict, Pgenval, id, lam, body))
          body !defs
    | (Some arg, optional) :: l -> build_apply lam ((arg, optional) :: args) l
    | [] -> lapply lam (List.rev_map fst args)
  in
  match uncurried_partial_application with
  | Some arity ->
    let extra_arity = arity - List.length sargs in
    let none_ids = ref [] in
    let args = Ext_list.filter_map sargs (function
     | _, Some e ->
        Some (transl_exp e)
     | _, None ->
        let id_arg = Ident.create "none" in
        none_ids := id_arg :: !none_ids;
        Some (Lvar id_arg)) in
    let extra_ids = Array.init extra_arity (fun _ -> Ident.create "extra") |> Array.to_list in
    let extra_args = Ext_list.map extra_ids (fun id -> Lvar id) in
    let ap_args = args @ extra_args in
    let l0 = Lapply { ap_func = lam; ap_args; ap_inlined = inlined; ap_loc = loc } in
    Lfunction
      {
        params = List.rev_append !none_ids extra_ids ;
        body = l0;
        attr = default_function_attribute;
        loc;
      }
  | _ ->
    (build_apply lam []
      (List.map
          (fun (l, x) -> (may_map transl_exp x, Btype.is_optional l))
          sargs)
      : Lambda.lambda)

and transl_function loc partial param cases =
  match cases with
  | [
   {
     c_lhs = pat;
     c_guard = None;
     c_rhs =
       {
         exp_desc =
           Texp_function
             { arg_label = _; param = param'; cases; partial = partial' };
       } as exp;
   };
  ]
    when Parmatch.inactive ~partial pat && not (exp |> has_async_attribute) ->
      let params, body, return_unit =
        transl_function exp.exp_loc partial' param' cases
      in
      ( param :: params,
        Matching.for_function loc None (Lvar param) [ (pat, body) ] partial,
        return_unit )
  | { c_rhs = { exp_env; exp_type }; _ } :: _ ->
      ( [ param ],
        Matching.for_function loc None (Lvar param) (transl_cases cases) partial,
        is_base_type exp_env exp_type Predef.path_unit )
  | _ -> assert false

and transl_let rec_flag pat_expr_list body =
  match rec_flag with
  | Nonrecursive ->
      let rec transl = function
        | [] -> body
        | { vb_pat = pat; vb_expr = expr; vb_attributes = attr; vb_loc } :: rem
          ->
            let lam = transl_exp expr in
            let lam = Translattribute.add_inline_attribute lam vb_loc attr in
            Matching.for_let pat.pat_loc lam pat (transl rem)
      in
      transl pat_expr_list
  | Recursive ->
      let transl_case { vb_expr = expr; vb_attributes; vb_loc; vb_pat = pat } =
        let id =
          match pat.pat_desc with
          | Tpat_var (id, _) -> id
          | Tpat_alias ({ pat_desc = Tpat_any }, id, _) -> id
          | _ -> assert false
          (* Illegal_letrec_pat
             Only variables are allowed as left-hand side of `let rec'
          *)
        in
        let lam = transl_exp expr in
        let lam =
          Translattribute.add_inline_attribute lam vb_loc vb_attributes
        in
        (id, lam)
      in
      Lletrec (Ext_list.map pat_expr_list transl_case, body)

and transl_record loc env fields repres opt_init_expr =
  match (opt_init_expr, repres, fields) with
  | None, Record_unboxed _, [| ({ lbl_name; lbl_loc }, Overridden (_, expr)) |]
    ->
      (* ReScript uncurried encoding *)
      let loc = lbl_loc in
      let lambda = transl_exp expr in
      if lbl_name.[0] = 'I' then
        let arity_s = String.sub lbl_name 1 (String.length lbl_name - 1) in
        let prim =
          Primitive.make ~name:"#fn_mk" ~alloc:true ~native_name:arity_s
            ~arity:1
        in
        Lprim
          ( Pccall prim
            (* could be replaced with Opaque in the future except arity 0*),
            [ lambda ],
            loc )
      else lambda
  | _ -> (
      let size = Array.length fields in
      (* Determine if there are "enough" fields (only relevant if this is a
         functional-style record update *)
      let no_init = match opt_init_expr with None -> true | _ -> false in
      if
        no_init || (size < 20 && (match repres with Record_optional_labels _ -> false | _ -> true))
        (* TODO: More strategies
           3 + 2 * List.length lbl_expr_list >= size (density)
        *)
      then
        (* Allocate new record with given fields (and remaining fields
           taken from init_expr if any *)
        let init_id = Ident.create "init" in
        let lv =
          Array.mapi
            (fun i (lbl, definition) ->
              match definition with
              | Kept _ ->
                  let access =
                    match repres with
                    | Record_float_unused -> assert false
                    | Record_regular | Record_optional_labels _ ->
                        Pfield (i, Lambda.fld_record lbl)
                    | Record_inlined _ ->
                        Pfield (i, Lambda.fld_record_inline lbl)
                    | Record_unboxed _ -> assert false
                    | Record_extension ->
                        Pfield
                          (i + 1, Lambda.fld_record_extension lbl)
                  in
                  Lprim (access, [ Lvar init_id ], loc)
              | Overridden (_lid, expr) -> transl_exp expr)
            fields
        in
        let ll = Array.to_list lv in
        let mut =
          if Array.exists (fun (lbl, _) -> lbl.lbl_mut = Mutable) fields then
            Mutable
          else Immutable
        in
        let lam =
          try
            if mut = Mutable then raise Not_constant;
            let cl = List.map extract_constant ll in
            match repres with
            | Record_float_unused -> assert false
            | Record_regular ->
                Lconst
                  (Const_block (Lambda.blk_record fields mut Record_regular, cl))
            | Record_optional_labels _ ->
                Lconst
                  (Const_block (Lambda.blk_record fields mut Record_optional, cl))
            | Record_inlined { tag; name; num_nonconsts; optional_labels; attrs } ->
                Lconst
                  (Const_block
                     ( Lambda.blk_record_inlined fields name num_nonconsts optional_labels ~tag ~attrs
                         mut,
                       cl ))
            | Record_unboxed _ ->
                Lconst (match cl with [ v ] -> v | _ -> assert false)
            | Record_extension -> raise Not_constant
          with Not_constant -> (
            match repres with
            | Record_regular ->
                Lprim
                  ( Pmakeblock (Lambda.blk_record fields mut Record_regular),
                    ll,
                    loc )
            | Record_optional_labels _ ->
                Lprim
                  ( Pmakeblock (Lambda.blk_record fields mut Record_optional),
                    ll,
                    loc )
            | Record_float_unused -> assert false
            | Record_inlined { tag; name; num_nonconsts; optional_labels; attrs } ->
                Lprim
                  ( Pmakeblock
                      (Lambda.blk_record_inlined fields name num_nonconsts optional_labels ~tag ~attrs
                         mut),
                    ll,
                    loc )
            | Record_unboxed _ -> (
                match ll with [ v ] -> v | _ -> assert false)
            | Record_extension ->
                let path =
                  let label, _ = fields.(0) in
                  match label.lbl_res.desc with
                  | Tconstr (p, _, _) -> p
                  | _ -> assert false
                in
                let slot = transl_extension_path env path in
                Lprim
                  ( Pmakeblock (Lambda.blk_record_ext fields mut),
                    slot :: ll,
                    loc ))
        in
        match opt_init_expr with
        | None -> lam
        | Some init_expr ->
            Llet (Strict, Pgenval, init_id, transl_exp init_expr, lam)
      else
        (* Take a shallow copy of the init record, then mutate the fields
           of the copy *)
        let copy_id = Ident.create "newrecord" in
        let update_field cont (lbl, definition) =
          match definition with
          | Kept _type -> cont
          | Overridden (_lid, expr) ->
              let upd =
                match repres with
                | Record_float_unused -> assert false
                | Record_regular | Record_optional_labels _ ->
                    Psetfield (lbl.lbl_pos, Lambda.fld_record_set lbl)
                | Record_inlined _ ->
                    Psetfield (lbl.lbl_pos, Lambda.fld_record_inline_set lbl)
                | Record_unboxed _ -> assert false
                | Record_extension ->
                    Psetfield
                      (lbl.lbl_pos + 1, Lambda.fld_record_extension_set lbl)
              in
              Lsequence
                (Lprim (upd, [ Lvar copy_id; transl_exp expr ], loc), cont)
        in
        match opt_init_expr with
        | None -> assert false
        | Some init_expr ->
            Llet
              ( Strict,
                Pgenval,
                copy_id,
                Lprim (Pduprecord, [ transl_exp init_expr ], loc),
                Array.fold_left update_field (Lvar copy_id) fields ))

and transl_match e arg pat_expr_list exn_pat_expr_list partial =
  let id = Typecore.name_pattern "exn" exn_pat_expr_list
  and cases = transl_cases pat_expr_list
  and exn_cases = transl_cases_try exn_pat_expr_list in
  let static_catch body val_ids handler =
    let static_exception_id = next_negative_raise_count () in
    Lstaticcatch
      ( Ltrywith
          ( Lstaticraise (static_exception_id, body),
            id,
            Matching.for_trywith (Lvar id) exn_cases ),
        (static_exception_id, val_ids),
        handler )
  in
  match (arg, exn_cases) with
  | { exp_desc = Texp_tuple argl }, [] ->
      Matching.for_multiple_match e.exp_loc (transl_list argl) cases partial
  | { exp_desc = Texp_tuple argl }, _ :: _ ->
      let val_ids = List.map (fun _ -> Typecore.name_pattern "val" []) argl in
      let lvars = List.map (fun id -> Lvar id) val_ids in
      static_catch (transl_list argl) val_ids
        (Matching.for_multiple_match e.exp_loc lvars cases partial)
  | arg, [] ->
      Matching.for_function e.exp_loc None (transl_exp arg) cases partial
  | arg, _ :: _ ->
      let val_id = Typecore.name_pattern "val" pat_expr_list in
      static_catch [ transl_exp arg ] [ val_id ]
        (Matching.for_function e.exp_loc None (Lvar val_id) cases partial)

open Format

let report_error ppf = function
  | Unknown_builtin_primitive prim_name ->
      fprintf ppf "Unknown builtin primitive \"%s\"" prim_name
  | Unreachable_reached -> fprintf ppf "Unreachable expression was reached"

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None)
