(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(**** Typing of type definitions ****)

open Misc
open Asttypes
open Parsetree
open Primitive
open Types
open Typetexp

type native_repr_kind = Unboxed | Untagged

type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string
  | Cycle_in_def of string * type_expr
  | Definition_mismatch of type_expr * Includecore.type_mismatch list
  | Constraint_failed of type_expr * type_expr
  | Inconsistent_constraint of Env.t * (type_expr * type_expr) list
  | Type_clash of Env.t * (type_expr * type_expr) list
  | Parameters_differ of Path.t * type_expr * type_expr
  | Null_arity_external
  | Missing_native_external
  | Unbound_type_var of type_expr * type_declaration
  | Cannot_extend_private_type of Path.t
  | Not_extensible_type of Path.t
  | Extension_mismatch of Path.t * Includecore.type_mismatch list
  | Rebind_wrong_type of Longident.t * Env.t * (type_expr * type_expr) list
  | Rebind_mismatch of Longident.t * Path.t * Path.t
  | Rebind_private of Longident.t
  | Bad_variance of int * (bool * bool * bool) * (bool * bool * bool)
  | Unavailable_type_constructor of Path.t
  | Bad_fixed_type of string
  | Unbound_type_var_ext of type_expr * extension_constructor
  | Varying_anonymous
  | Val_in_structure
  | Multiple_native_repr_attributes
  | Cannot_unbox_or_untag_type of native_repr_kind
  | Deep_unbox_or_untag_attribute of native_repr_kind
  | Bad_immediate_attribute
  | Bad_unboxed_attribute of string
  | Wrong_unboxed_type_float
  | Boxed_and_unboxed
  | Nonrec_gadt

open Typedtree

exception Error of Location.t * error

(* Note: do not factor the branches in the following pattern-matching:
   the records must be constants for the compiler to do sharing on them.
*)
let get_unboxed_from_attributes sdecl =
  let unboxed = Builtin_attributes.has_unboxed sdecl.ptype_attributes in
  let boxed = Builtin_attributes.has_boxed sdecl.ptype_attributes in
  match boxed, unboxed, !Clflags.unboxed_types with
  | true, true, _ -> raise (Error(sdecl.ptype_loc, Boxed_and_unboxed))
  | true, false, _ -> unboxed_false_default_false
  | false, true, _ -> unboxed_true_default_false
  | false, false, false -> unboxed_false_default_true
  | false, false, true -> unboxed_true_default_true

(* Enter all declared types in the environment as abstract types *)

let enter_type rec_flag env sdecl id =
  let needed =
    match rec_flag with
    | Asttypes.Nonrecursive ->
        begin match sdecl.ptype_kind with
        | Ptype_variant scds ->
            List.iter (fun cd ->
              if cd.pcd_res <> None then raise (Error(cd.pcd_loc, Nonrec_gadt)))
              scds
        | _ -> ()
        end;
        Btype.is_row_name (Ident.name id)
    | Asttypes.Recursive -> true
  in
  if not needed then env else
  let decl =
    { type_params =
        List.map (fun _ -> Btype.newgenvar ()) sdecl.ptype_params;
      type_arity = List.length sdecl.ptype_params;
      type_kind = Type_abstract;
      type_private = sdecl.ptype_private;
      type_manifest =
        begin match sdecl.ptype_manifest with None -> None
        | Some _ -> Some(Ctype.newvar ()) end;
      type_variance = List.map (fun _ -> Variance.full) sdecl.ptype_params;
      type_newtype_level = None;
      type_loc = sdecl.ptype_loc;
      type_attributes = sdecl.ptype_attributes;
      type_immediate = false;
      type_unboxed = unboxed_false_default_false;
    }
  in
  Env.add_type ~check:true id decl env

let update_type temp_env env id loc =
  let path = Path.Pident id in
  let decl = Env.find_type path temp_env in
  match decl.type_manifest with None -> ()
  | Some ty ->
      let params = List.map (fun _ -> Ctype.newvar ()) decl.type_params in
      try Ctype.unify env (Ctype.newconstr path params) ty
      with Ctype.Unify trace ->
        raise (Error(loc, Type_clash (env, trace)))

(* We use the Ctype.expand_head_opt version of expand_head to get access
   to the manifest type of private abbreviations. *)
let rec get_unboxed_type_representation env ty fuel =
  if fuel < 0 then None else
  let ty = Ctype.repr (Ctype.expand_head_opt env ty) in
  match ty.desc with
  | Tconstr (p, args, _) ->
    begin match Env.find_type p env with
    | exception Not_found -> Some ty
    | {type_unboxed = {unboxed = false}} -> Some ty
    | {type_params; type_kind =
         Type_record ([{ld_type = ty2; _}], _)
       | Type_variant [{cd_args = Cstr_tuple [ty2]; _}]
       | Type_variant [{cd_args = Cstr_record [{ld_type = ty2; _}]; _}]}

         -> get_unboxed_type_representation env
             (Ctype.apply env type_params ty2 args) (fuel - 1)
    | {type_kind=Type_abstract} -> None
          (* This case can occur when checking a recursive unboxed type
             declaration. *)
    | _ -> assert false (* only the above can be unboxed *)
    end
  | _ -> Some ty

let get_unboxed_type_representation env ty =
  (* Do not give too much fuel: PR#7424 *)
  get_unboxed_type_representation env ty 100
;;

(* Determine if a type's values are represented by floats at run-time. *)
let is_float env ty =
  match get_unboxed_type_representation env ty with
    Some {desc = Tconstr(p, _, _); _} -> Path.same p Predef.path_float
  | _ -> false

(* Determine if a type definition defines a fixed type. (PW) *)
let is_fixed_type sd =
  let rec has_row_var sty =
    match sty.ptyp_desc with
      Ptyp_alias (sty, _) -> has_row_var sty
    | Ptyp_class _
    | Ptyp_object (_, Open)
    | Ptyp_variant (_, Open, _)
    | Ptyp_variant (_, Closed, Some _) -> true
    | _ -> false
  in
  match sd.ptype_manifest with
    None -> false
  | Some sty ->
      sd.ptype_kind = Ptype_abstract &&
      sd.ptype_private = Private &&
      has_row_var sty

(* Set the row variable in a fixed type *)
let set_fixed_row env loc p decl =
  let tm =
    match decl.type_manifest with
      None -> assert false
    | Some t -> Ctype.expand_head env t
  in
  let rv =
    match tm.desc with
      Tvariant row ->
        let row = Btype.row_repr row in
        tm.desc <- Tvariant {row with row_fixed = true};
        if Btype.static_row row then Btype.newgenty Tnil
        else row.row_more
    | Tobject (ty, _) ->
        snd (Ctype.flatten_fields ty)
    | _ ->
        raise (Error (loc, Bad_fixed_type "is not an object or variant"))
  in
  if not (Btype.is_Tvar rv) then
    raise (Error (loc, Bad_fixed_type "has no row variable"));
  rv.desc <- Tconstr (p, decl.type_params, ref Mnil)

(* Translate one type declaration *)

module StringSet =
  Set.Make(struct
    type t = string
    let compare (x:t) y = compare x y
  end)

let make_params env params =
  let make_param (sty, v) =
    try
      (transl_type_param env sty, v)
    with Already_bound ->
      raise(Error(sty.ptyp_loc, Repeated_parameter))
  in
    List.map make_param params

let transl_labels env closed lbls =
  assert (lbls <> []);
  let all_labels = ref StringSet.empty in
  List.iter
    (fun {pld_name = {txt=name; loc}} ->
       if StringSet.mem name !all_labels then
         raise(Error(loc, Duplicate_label name));
       all_labels := StringSet.add name !all_labels)
    lbls;
  let mk {pld_name=name;pld_mutable=mut;pld_type=arg;pld_loc=loc;
          pld_attributes=attrs} =
    Builtin_attributes.warning_scope attrs
      (fun () ->
         let arg = Ast_helper.Typ.force_poly arg in
         let cty = transl_simple_type env closed arg in
         {ld_id = Ident.create name.txt; ld_name = name; ld_mutable = mut;
          ld_type = cty; ld_loc = loc; ld_attributes = attrs}
      )
  in
  let lbls = List.map mk lbls in
  let lbls' =
    List.map
      (fun ld ->
         let ty = ld.ld_type.ctyp_type in
         let ty = match ty.desc with Tpoly(t,[]) -> t | _ -> ty in
         {Types.ld_id = ld.ld_id;
          ld_mutable = ld.ld_mutable;
          ld_type = ty;
          ld_loc = ld.ld_loc;
          ld_attributes = ld.ld_attributes
         }
      )
      lbls in
  lbls, lbls'

let transl_constructor_arguments env closed = function
  | Pcstr_tuple l ->
      let l = List.map (transl_simple_type env closed) l in
      Types.Cstr_tuple (List.map (fun t -> t.ctyp_type) l),
      Cstr_tuple l
  | Pcstr_record l ->
      let lbls, lbls' = transl_labels env closed l in
      Types.Cstr_record lbls',
      Cstr_record lbls

let make_constructor env type_path type_params sargs sret_type =
  match sret_type with
  | None ->
      let args, targs =
        transl_constructor_arguments env true sargs
      in
        targs, None, args, None, type_params
  | Some sret_type ->
      (* if it's a generalized constructor we must first narrow and
         then widen so as to not introduce any new constraints *)
      let z = narrow () in
      reset_type_variables ();
      let args, targs =
        transl_constructor_arguments env false sargs
      in
      let tret_type = transl_simple_type env false sret_type in
      let ret_type = tret_type.ctyp_type in
      let params =
        match (Ctype.repr ret_type).desc with
        | Tconstr (p', params, _) when Path.same type_path p' ->
            params
        | _ ->
            raise (Error (sret_type.ptyp_loc, Constraint_failed
                            (ret_type, Ctype.newconstr type_path type_params)))
      in
      widen z;
      targs, Some tret_type, args, Some ret_type, params

(* Check that the variable [id] is present in the [univ] list. *)
let check_type_var loc univ id =
  let f t = (Btype.repr t).id = id in
  if not (List.exists f univ) then raise (Error (loc, Wrong_unboxed_type_float))

(* Check that all the variables found in [ty] are in [univ].
   Because [ty] is the argument to an abstract type, the representation
   of that abstract type could be any subexpression of [ty], in particular
   any type variable present in [ty].
*)
let rec check_unboxed_abstract_arg loc univ ty =
  match ty.desc with
  | Tvar _ -> check_type_var loc univ ty.id
  | Tarrow (_, t1, t2, _)
  | Tfield (_, _, t1, t2) ->
    check_unboxed_abstract_arg loc univ t1;
    check_unboxed_abstract_arg loc univ t2
  | Ttuple args
  | Tconstr (_, args, _)
  | Tpackage (_, _, args) ->
    List.iter (check_unboxed_abstract_arg loc univ) args
  | Tobject (fields, r) ->
    check_unboxed_abstract_arg loc univ fields;
    begin match !r with
    | None -> ()
    | Some (_, args) -> List.iter (check_unboxed_abstract_arg loc univ) args
    end
  | Tnil
  | Tunivar _ -> ()
  | Tlink e -> check_unboxed_abstract_arg loc univ e
  | Tsubst _ -> assert false
  | Tvariant { row_fields; row_more; row_name } ->
    List.iter (check_unboxed_abstract_row_field loc univ) row_fields;
    check_unboxed_abstract_arg loc univ row_more;
    begin match row_name with
    | None -> ()
    | Some (_, args) -> List.iter (check_unboxed_abstract_arg loc univ) args
    end
  | Tpoly (t, _) -> check_unboxed_abstract_arg loc univ t

and check_unboxed_abstract_row_field loc univ (_, field) =
  match field with
  | Rpresent (Some ty) -> check_unboxed_abstract_arg loc univ ty
  | Reither (_, args, _, r) ->
    List.iter (check_unboxed_abstract_arg loc univ) args;
    begin match !r with
    | None -> ()
    | Some f -> check_unboxed_abstract_row_field loc univ ("", f)
    end
  | Rabsent
  | Rpresent None -> ()

(* Check that the argument to a GADT constructor is compatible with unboxing
   the type, given the universal parameters of the type. *)
let rec check_unboxed_gadt_arg loc univ env ty =
  match get_unboxed_type_representation env ty with
  | Some {desc = Tvar _; id} -> check_type_var loc univ id
  | Some {desc = Tarrow _ | Ttuple _ | Tpackage _ | Tobject _ | Tnil
                 | Tvariant _; _} ->
    ()
    (* A comment in [Translcore.transl_exp0] claims the above cannot be
       represented by floats. *)
  | Some {desc = Tconstr (p, args, _); _} ->
    let tydecl = Env.find_type p env in
    assert (not tydecl.type_unboxed.unboxed);
    if tydecl.type_kind = Type_abstract then
      List.iter (check_unboxed_abstract_arg loc univ) args
  | Some {desc = Tfield _ | Tlink _ | Tsubst _; _} -> assert false
  | Some {desc = Tunivar _; _} -> ()
  | Some {desc = Tpoly (t2, _); _} -> check_unboxed_gadt_arg loc univ env t2
  | None -> ()
      (* This case is tricky: the argument is another (or the same) type
         in the same recursive definition. In this case we don't have to
         check because we will also check that other type for correctness. *)

let transl_declaration env sdecl id =
  (* Bind type parameters *)
  reset_type_variables();
  Ctype.begin_def ();
  let tparams = make_params env sdecl.ptype_params in
  let params = List.map (fun (cty, _) -> cty.ctyp_type) tparams in
  let cstrs = List.map
    (fun (sty, sty', loc) ->
      transl_simple_type env false sty,
      transl_simple_type env false sty', loc)
    sdecl.ptype_cstrs
  in
  let raw_status = get_unboxed_from_attributes sdecl in
  if raw_status.unboxed && not raw_status.default then begin
    match sdecl.ptype_kind with
    | Ptype_abstract ->
        raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute
                      "it is abstract"))
    | Ptype_variant [{pcd_args = Pcstr_tuple []; _}] ->
      raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute
                    "its constructor has no argument"))
    | Ptype_variant [{pcd_args = Pcstr_tuple [_]; _}] -> ()
    | Ptype_variant [{pcd_args = Pcstr_tuple _; _}] ->
      raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute
                    "its constructor has more than one argument"))
    | Ptype_variant [{pcd_args = Pcstr_record
                        [{pld_mutable=Immutable; _}]; _}] -> ()
    | Ptype_variant [{pcd_args = Pcstr_record [{pld_mutable=Mutable; _}]; _}] ->
      raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute "it is mutable"))
    | Ptype_variant [{pcd_args = Pcstr_record _; _}] ->
      raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute
                    "its constructor has more than one argument"))
    | Ptype_variant _ ->
      raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute
                    "it has more than one constructor"))
    | Ptype_record [{pld_mutable=Immutable; _}] -> ()
    | Ptype_record [{pld_mutable=Mutable; _}] ->
      raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute
                    "it is mutable"))
    | Ptype_record _ ->
      raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute
                    "it has more than one field"))
    | Ptype_open ->
      raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute
                    "extensible variant types cannot be unboxed"))
  end;
  let unboxed_status =
    match sdecl.ptype_kind with
    | Ptype_variant [{pcd_args = Pcstr_tuple [_]; _}]
      | Ptype_variant [{pcd_args = Pcstr_record
                          [{pld_mutable = Immutable; _}]; _}]
      | Ptype_record [{pld_mutable = Immutable; _}] ->
    raw_status
    | _ -> (* The type is not unboxable, mark it as boxed *)
      unboxed_false_default_false
  in
  let unbox = unboxed_status.unboxed in
  let (tkind, kind) =
    match sdecl.ptype_kind with
      | Ptype_abstract -> Ttype_abstract, Type_abstract
      | Ptype_variant scstrs ->
        assert (scstrs <> []);
        if List.exists (fun cstr -> cstr.pcd_res <> None) scstrs then begin
          match cstrs with
            [] -> ()
          | (_,_,loc)::_ ->
              Location.prerr_warning loc Warnings.Constraint_on_gadt
        end;
        let all_constrs = ref StringSet.empty in
        List.iter
          (fun {pcd_name = {txt = name}} ->
            if StringSet.mem name !all_constrs then
              raise(Error(sdecl.ptype_loc, Duplicate_constructor name));
            all_constrs := StringSet.add name !all_constrs)
          scstrs;
        if List.length
            (List.filter (fun cd -> cd.pcd_args <> Pcstr_tuple []) scstrs)
           > (Config.max_tag + 1) then
          raise(Error(sdecl.ptype_loc, Too_many_constructors));
        let make_cstr scstr =
          let name = Ident.create scstr.pcd_name.txt in
          let targs, tret_type, args, ret_type, cstr_params =
            make_constructor env (Path.Pident id) params
                             scstr.pcd_args scstr.pcd_res
          in
          if Config.flat_float_array && unbox then begin
            (* Cannot unbox a type when the argument can be both float and
               non-float because it interferes with the dynamic float array
               optimization. This can only happen when the type is a GADT
               and the argument is an existential type variable or an
               unboxed (or abstract) type constructor applied to some
               existential type variable. Of course we also have to rule
               out any abstract type constructor applied to anything that
               might be an existential type variable.
               There is a difficulty with existential variables created
               out of thin air (rather than bound by the declaration).
               See PR#7511 and GPR#1133 for details. *)
            match Datarepr.constructor_existentials args ret_type with
            | _, [] -> ()
            | [argty], _ex ->
                check_unboxed_gadt_arg sdecl.ptype_loc cstr_params env argty
            | _ -> assert false
          end;
          let tcstr =
            { cd_id = name;
              cd_name = scstr.pcd_name;
              cd_args = targs;
              cd_res = tret_type;
              cd_loc = scstr.pcd_loc;
              cd_attributes = scstr.pcd_attributes }
          in
          let cstr =
            { Types.cd_id = name;
              cd_args = args;
              cd_res = ret_type;
              cd_loc = scstr.pcd_loc;
              cd_attributes = scstr.pcd_attributes }
          in
            tcstr, cstr
        in
        let make_cstr scstr =
          Builtin_attributes.warning_scope scstr.pcd_attributes
            (fun () -> make_cstr scstr)
        in
        let tcstrs, cstrs = List.split (List.map make_cstr scstrs) in
          Ttype_variant tcstrs, Type_variant cstrs
      | Ptype_record lbls ->
          let lbls, lbls' = transl_labels env true lbls in
          let rep =
            if unbox then Record_unboxed false
            else if List.for_all (fun l -> is_float env l.Types.ld_type) lbls'
            then Record_float
            else Record_regular
          in
          Ttype_record lbls, Type_record(lbls', rep)
      | Ptype_open -> Ttype_open, Type_open
      in
    let (tman, man) = match sdecl.ptype_manifest with
        None -> None, None
      | Some sty ->
        let no_row = not (is_fixed_type sdecl) in
        let cty = transl_simple_type env no_row sty in
        Some cty, Some cty.ctyp_type
    in
    let decl =
      { type_params = params;
        type_arity = List.length params;
        type_kind = kind;
        type_private = sdecl.ptype_private;
        type_manifest = man;
        type_variance = List.map (fun _ -> Variance.full) params;
        type_newtype_level = None;
        type_loc = sdecl.ptype_loc;
        type_attributes = sdecl.ptype_attributes;
        type_immediate = false;
        type_unboxed = unboxed_status;
      } in

  (* Check constraints *)
    List.iter
      (fun (cty, cty', loc) ->
        let ty = cty.ctyp_type in
        let ty' = cty'.ctyp_type in
        try Ctype.unify env ty ty' with Ctype.Unify tr ->
          raise(Error(loc, Inconsistent_constraint (env, tr))))
      cstrs;
    Ctype.end_def ();
  (* Add abstract row *)
    if is_fixed_type sdecl then begin
      let p =
        try Env.lookup_type (Longident.Lident(Ident.name id ^ "#row")) env
        with Not_found -> assert false in
      set_fixed_row env sdecl.ptype_loc p decl
    end;
  (* Check for cyclic abbreviations *)
    begin match decl.type_manifest with None -> ()
      | Some ty ->
        if Ctype.cyclic_abbrev env id ty then
          raise(Error(sdecl.ptype_loc, Recursive_abbrev sdecl.ptype_name.txt));
    end;
    {
      typ_id = id;
      typ_name = sdecl.ptype_name;
      typ_params = tparams;
      typ_type = decl;
      typ_cstrs = cstrs;
      typ_loc = sdecl.ptype_loc;
      typ_manifest = tman;
      typ_kind = tkind;
      typ_private = sdecl.ptype_private;
      typ_attributes = sdecl.ptype_attributes;
    }

(* Generalize a type declaration *)

let generalize_decl decl =
  List.iter Ctype.generalize decl.type_params;
  Btype.iter_type_expr_kind Ctype.generalize decl.type_kind;
  begin match decl.type_manifest with
  | None    -> ()
  | Some ty -> Ctype.generalize ty
  end

(* Check that all constraints are enforced *)

module TypeSet = Btype.TypeSet
module TypeMap = Btype.TypeMap

let rec check_constraints_rec env loc visited ty =
  let ty = Ctype.repr ty in
  if TypeSet.mem ty !visited then () else begin
  visited := TypeSet.add ty !visited;
  match ty.desc with
  | Tconstr (path, args, _) ->
      let args' = List.map (fun _ -> Ctype.newvar ()) args in
      let ty' = Ctype.newconstr path args' in
      begin try Ctype.enforce_constraints env ty'
      with Ctype.Unify _ -> assert false
      | Not_found -> raise (Error(loc, Unavailable_type_constructor path))
      end;
      if not (Ctype.matches env ty ty') then
        raise (Error(loc, Constraint_failed (ty, ty')));
      List.iter (check_constraints_rec env loc visited) args
  | Tpoly (ty, tl) ->
      let _, ty = Ctype.instance_poly false tl ty in
      check_constraints_rec env loc visited ty
  | _ ->
      Btype.iter_type_expr (check_constraints_rec env loc visited) ty
  end

module SMap = Map.Make(String)

let check_constraints_labels env visited l pl =
  let rec get_loc name = function
      [] -> assert false
    | pld :: tl ->
        if name = pld.pld_name.txt then pld.pld_type.ptyp_loc
        else get_loc name tl
  in
  List.iter
    (fun {Types.ld_id=name; ld_type=ty} ->
       check_constraints_rec env (get_loc (Ident.name name) pl) visited ty)
    l

let check_constraints env sdecl (_, decl) =
  let visited = ref TypeSet.empty in
  begin match decl.type_kind with
  | Type_abstract -> ()
  | Type_variant l ->
      let find_pl = function
          Ptype_variant pl -> pl
        | Ptype_record _ | Ptype_abstract | Ptype_open -> assert false
      in
      let pl = find_pl sdecl.ptype_kind in
      let pl_index =
        let foldf acc x =
          SMap.add x.pcd_name.txt x acc
        in
        List.fold_left foldf SMap.empty pl
      in
      List.iter
        (fun {Types.cd_id=name; cd_args; cd_res} ->
          let {pcd_args; pcd_res; _} =
            try SMap.find (Ident.name name) pl_index
            with Not_found -> assert false in
          begin match cd_args, pcd_args with
          | Cstr_tuple tyl, Pcstr_tuple styl ->
              List.iter2
                (fun sty ty ->
                   check_constraints_rec env sty.ptyp_loc visited ty)
                styl tyl
          | Cstr_record tyl, Pcstr_record styl ->
              check_constraints_labels env visited tyl styl
          | _ -> assert false
          end;
          match pcd_res, cd_res with
          | Some sr, Some r ->
              check_constraints_rec env sr.ptyp_loc visited r
          | _ ->
              () )
        l
  | Type_record (l, _) ->
      let find_pl = function
          Ptype_record pl -> pl
        | Ptype_variant _ | Ptype_abstract | Ptype_open -> assert false
      in
      let pl = find_pl sdecl.ptype_kind in
      check_constraints_labels env visited l pl
  | Type_open -> ()
  end;
  begin match decl.type_manifest with
  | None -> ()
  | Some ty ->
      let sty =
        match sdecl.ptype_manifest with Some sty -> sty | _ -> assert false
      in
      check_constraints_rec env sty.ptyp_loc visited ty
  end

(*
   If both a variant/record definition and a type equation are given,
   need to check that the equation refers to a type of the same kind
   with the same constructors and labels.
*)
let check_coherence env loc id decl =
  match decl with
    { type_kind = (Type_variant _ | Type_record _| Type_open);
      type_manifest = Some ty } ->
      begin match (Ctype.repr ty).desc with
        Tconstr(path, args, _) ->
          begin try
            let decl' = Env.find_type path env in
            let err =
              if List.length args <> List.length decl.type_params
              then [Includecore.Arity]
              else if not (Ctype.equal env false args decl.type_params)
              then [Includecore.Constraint]
              else
                Includecore.type_declarations ~loc ~equality:true env
                  (Path.last path)
                  decl'
                  id
                  (Subst.type_declaration
                     (Subst.add_type id path Subst.identity) decl)
            in
            if err <> [] then
              raise(Error(loc, Definition_mismatch (ty, err)))
          with Not_found ->
            raise(Error(loc, Unavailable_type_constructor path))
          end
      | _ -> raise(Error(loc, Definition_mismatch (ty, [])))
      end
  | _ -> ()

let check_abbrev env sdecl (id, decl) =
  check_coherence env sdecl.ptype_loc id decl

(* Check that recursion is well-founded *)

let check_well_founded env loc path to_check ty =
  let visited = ref TypeMap.empty in
  let rec check ty0 parents ty =
    let ty = Btype.repr ty in
    if TypeSet.mem ty parents then begin
      (*Format.eprintf "@[%a@]@." Printtyp.raw_type_expr ty;*)
      if match ty0.desc with
      | Tconstr (p, _, _) -> Path.same p path
      | _ -> false
      then raise (Error (loc, Recursive_abbrev (Path.name path)))
      else raise (Error (loc, Cycle_in_def (Path.name path, ty0)))
    end;
    let (fini, parents) =
      try
        let prev = TypeMap.find ty !visited in
        if TypeSet.subset parents prev then (true, parents) else
        (false, TypeSet.union parents prev)
      with Not_found ->
        (false, parents)
    in
    if fini then () else
    let rec_ok =
      match ty.desc with
        Tconstr(p,_,_) ->
          !Clflags.recursive_types && Ctype.is_contractive env p
      | Tobject _ | Tvariant _ -> true
      | _ -> !Clflags.recursive_types
    in
    let visited' = TypeMap.add ty parents !visited in
    let arg_exn =
      try
        visited := visited';
        let parents =
          if rec_ok then TypeSet.empty else TypeSet.add ty parents in
        Btype.iter_type_expr (check ty0 parents) ty;
        None
      with e ->
        visited := visited'; Some e
    in
    match ty.desc with
    | Tconstr(p, _, _) when arg_exn <> None || to_check p ->
        if to_check p then may raise arg_exn
        else Btype.iter_type_expr (check ty0 TypeSet.empty) ty;
        begin try
          let ty' = Ctype.try_expand_once_opt env ty in
          let ty0 = if TypeSet.is_empty parents then ty else ty0 in
          check ty0 (TypeSet.add ty parents) ty'
        with
          Ctype.Cannot_expand -> may raise arg_exn
        end
    | _ -> may raise arg_exn
  in
  let snap = Btype.snapshot () in
  try Ctype.wrap_trace_gadt_instances env (check ty TypeSet.empty) ty
  with Ctype.Unify _ ->
    (* Will be detected by check_recursion *)
    Btype.backtrack snap

let check_well_founded_manifest env loc path decl =
  if decl.type_manifest = None then () else
  let args = List.map (fun _ -> Ctype.newvar()) decl.type_params in
  check_well_founded env loc path (Path.same path) (Ctype.newconstr path args)

let check_well_founded_decl env loc path decl to_check =
  let open Btype in
  let it =
    {type_iterators with
     it_type_expr = (fun _ -> check_well_founded env loc path to_check)} in
  it.it_type_declaration it (Ctype.instance_declaration decl)

(* Check for ill-defined abbrevs *)

let check_recursion env loc path decl to_check =
  (* to_check is true for potentially mutually recursive paths.
     (path, decl) is the type declaration to be checked. *)

  if decl.type_params = [] then () else

  let visited = ref [] in

  let rec check_regular cpath args prev_exp ty =
    let ty = Ctype.repr ty in
    if not (List.memq ty !visited) then begin
      visited := ty :: !visited;
      match ty.desc with
      | Tconstr(path', args', _) ->
          if Path.same path path' then begin
            if not (Ctype.equal env false args args') then
              raise (Error(loc,
                     Parameters_differ(cpath, ty, Ctype.newconstr path args)))
          end
          (* Attempt to expand a type abbreviation if:
              1- [to_check path'] holds
                 (otherwise the expansion cannot involve [path]);
              2- we haven't expanded this type constructor before
                 (otherwise we could loop if [path'] is itself
                 a non-regular abbreviation). *)
          else if to_check path' && not (List.mem path' prev_exp) then begin
            try
              (* Attempt expansion *)
              let (params0, body0, _) = Env.find_type_expansion path' env in
              let (params, body) =
                Ctype.instance_parameterized_type params0 body0 in
              begin
                try List.iter2 (Ctype.unify env) params args'
                with Ctype.Unify _ ->
                  raise (Error(loc, Constraint_failed
                                 (ty, Ctype.newconstr path' params0)));
              end;
              check_regular path' args (path' :: prev_exp) body
            with Not_found -> ()
          end;
          List.iter (check_regular cpath args prev_exp) args'
      | Tpoly (ty, tl) ->
          let (_, ty) = Ctype.instance_poly ~keep_names:true false tl ty in
          check_regular cpath args prev_exp ty
      | _ ->
          Btype.iter_type_expr (check_regular cpath args prev_exp) ty
    end in

  Misc.may
    (fun body ->
      let (args, body) =
        Ctype.instance_parameterized_type
          ~keep_names:true decl.type_params body in
      check_regular path args [] body)
    decl.type_manifest

let check_abbrev_recursion env id_loc_list to_check tdecl =
  let decl = tdecl.typ_type in
  let id = tdecl.typ_id in
  check_recursion env (List.assoc id id_loc_list) (Path.Pident id) decl to_check

(* Compute variance *)

let get_variance ty visited =
  try TypeMap.find ty !visited with Not_found -> Variance.null

let compute_variance env visited vari ty =
  let rec compute_variance_rec vari ty =
    (* Format.eprintf "%a: %x@." Printtyp.type_expr ty (Obj.magic vari); *)
    let ty = Ctype.repr ty in
    let vari' = get_variance ty visited in
    if Variance.subset vari vari' then () else
    let vari = Variance.union vari vari' in
    visited := TypeMap.add ty vari !visited;
    let compute_same = compute_variance_rec vari in
    match ty.desc with
      Tarrow (_, ty1, ty2, _) ->
        let open Variance in
        let v = conjugate vari in
        let v1 =
          if mem May_pos v || mem May_neg v
          then set May_weak true v else v
        in
        compute_variance_rec v1 ty1;
        compute_same ty2
    | Ttuple tl ->
        List.iter compute_same tl
    | Tconstr (path, tl, _) ->
        let open Variance in
        if tl = [] then () else begin
          try
            let decl = Env.find_type path env in
            let cvari f = mem f vari in
            List.iter2
              (fun ty v ->
                let cv f = mem f v in
                let strict =
                  cvari Inv && cv Inj || (cvari Pos || cvari Neg) && cv Inv
                in
                if strict then compute_variance_rec full ty else
                let p1 = inter v vari
                and n1 = inter v (conjugate vari) in
                let v1 =
                  union (inter covariant (union p1 (conjugate p1)))
                    (inter (conjugate covariant) (union n1 (conjugate n1)))
                and weak =
                  cvari May_weak && (cv May_pos || cv May_neg) ||
                  (cvari May_pos || cvari May_neg) && cv May_weak
                in
                let v2 = set May_weak weak v1 in
                compute_variance_rec v2 ty)
              tl decl.type_variance
          with Not_found ->
            List.iter (compute_variance_rec may_inv) tl
        end
    | Tobject (ty, _) ->
        compute_same ty
    | Tfield (_, _, ty1, ty2) ->
        compute_same ty1;
        compute_same ty2
    | Tsubst ty ->
        compute_same ty
    | Tvariant row ->
        let row = Btype.row_repr row in
        List.iter
          (fun (_,f) ->
            match Btype.row_field_repr f with
              Rpresent (Some ty) ->
                compute_same ty
            | Reither (_, tyl, _, _) ->
                let open Variance in
                let upper =
                  List.fold_left (fun s f -> set f true s)
                    null [May_pos; May_neg; May_weak]
                in
                let v = inter vari upper in
                (* cf PR#7269:
                   if List.length tyl > 1 then upper else inter vari upper *)
                List.iter (compute_variance_rec v) tyl
            | _ -> ())
          row.row_fields;
        compute_same row.row_more
    | Tpoly (ty, _) ->
        compute_same ty
    | Tvar _ | Tnil | Tlink _ | Tunivar _ -> ()
    | Tpackage (_, _, tyl) ->
        let v =
          Variance.(if mem Pos vari || mem Neg vari then full else may_inv)
        in
        List.iter (compute_variance_rec v) tyl
  in
  compute_variance_rec vari ty

let make p n i =
  let open Variance in
  set May_pos p (set May_neg n (set May_weak n (set Inj i null)))

let compute_variance_type env check (required, loc) decl tyl =
  (* Requirements *)
  let required =
    List.map (fun (c,n,i) -> if c || n then (c,n,i) else (true,true,i))
      required
  in
  (* Prepare *)
  let params = List.map Btype.repr decl.type_params in
  let tvl = ref TypeMap.empty in
  (* Compute occurrences in the body *)
  let open Variance in
  List.iter
    (fun (cn,ty) ->
      compute_variance env tvl (if cn then full else covariant) ty)
    tyl;
  if check then begin
    (* Check variance of parameters *)
    let pos = ref 0 in
    List.iter2
      (fun ty (c, n, i) ->
        incr pos;
        let var = get_variance ty tvl in
        let (co,cn) = get_upper var and ij = mem Inj var in
        if Btype.is_Tvar ty && (co && not c || cn && not n || not ij && i)
        then raise (Error(loc, Bad_variance (!pos, (co,cn,ij), (c,n,i)))))
      params required;
    (* Check propagation from constrained parameters *)
    let args = Btype.newgenty (Ttuple params) in
    let fvl = Ctype.free_variables args in
    let fvl = List.filter (fun v -> not (List.memq v params)) fvl in
    (* If there are no extra variables there is nothing to do *)
    if fvl = [] then () else
    let tvl2 = ref TypeMap.empty in
    List.iter2
      (fun ty (p,n,_) ->
        if Btype.is_Tvar ty then () else
        let v =
          if p then if n then full else covariant else conjugate covariant in
        compute_variance env tvl2 v ty)
      params required;
    let visited = ref TypeSet.empty in
    let rec check ty =
      let ty = Ctype.repr ty in
      if TypeSet.mem ty !visited then () else
      let visited' = TypeSet.add ty !visited in
      visited := visited';
      let v1 = get_variance ty tvl in
      let snap = Btype.snapshot () in
      let v2 =
        TypeMap.fold
          (fun t vt v ->
            if Ctype.equal env false [ty] [t] then union vt v else v)
          !tvl2 null in
      Btype.backtrack snap;
      let (c1,n1) = get_upper v1 and (c2,n2,_,i2) = get_lower v2 in
      if c1 && not c2 || n1 && not n2 then
        if List.memq ty fvl then
          let code = if not i2 then -2 else if c2 || n2 then -1 else -3 in
          raise (Error (loc, Bad_variance (code, (c1,n1,false), (c2,n2,false))))
        else
          Btype.iter_type_expr check ty
    in
    List.iter (fun (_,ty) -> check ty) tyl;
  end;
  List.map2
    (fun ty (p, n, i) ->
      let v = get_variance ty tvl in
      let tr = decl.type_private in
      (* Use required variance where relevant *)
      let concr = decl.type_kind <> Type_abstract (*|| tr = Type_new*) in
      let (p, n) =
        if tr = Private || not (Btype.is_Tvar ty) then (p, n) (* set *)
        else (false, false) (* only check *)
      and i = concr  || i && tr = Private in
      let v = union v (make p n i) in
      let v =
        if not concr then v else
        if mem Pos v && mem Neg v then full else
        if Btype.is_Tvar ty then v else
        union v
          (if p then if n then full else covariant else conjugate covariant)
      in
      if decl.type_kind = Type_abstract && tr = Public then v else
      set May_weak (mem May_neg v) v)
    params required

let add_false = List.map (fun ty -> false, ty)

(* A parameter is constrained if it is either instantiated,
   or it is a variable appearing in another parameter *)
let constrained vars ty =
  match ty.desc with
  | Tvar _ -> List.exists (fun tl -> List.memq ty tl) vars
  | _ -> true

let for_constr = function
  | Types.Cstr_tuple l -> add_false l
  | Types.Cstr_record l ->
      List.map
        (fun {Types.ld_mutable; ld_type} -> (ld_mutable = Mutable, ld_type))
        l

let compute_variance_gadt env check (required, loc as rloc) decl
    (tl, ret_type_opt) =
  match ret_type_opt with
  | None ->
      compute_variance_type env check rloc {decl with type_private = Private}
        (for_constr tl)
  | Some ret_type ->
      match Ctype.repr ret_type with
      | {desc=Tconstr (_, tyl, _)} ->
          (* let tyl = List.map (Ctype.expand_head env) tyl in *)
          let tyl = List.map Ctype.repr tyl in
          let fvl = List.map (Ctype.free_variables ?env:None) tyl in
          let _ =
            List.fold_left2
              (fun (fv1,fv2) ty (c,n,_) ->
                match fv2 with [] -> assert false
                | fv :: fv2 ->
                    (* fv1 @ fv2 = free_variables of other parameters *)
                    if (c||n) && constrained (fv1 @ fv2) ty then
                      raise (Error(loc, Varying_anonymous));
                    (fv :: fv1, fv2))
              ([], fvl) tyl required
          in
          compute_variance_type env check rloc
            {decl with type_params = tyl; type_private = Private}
            (for_constr tl)
      | _ -> assert false

let compute_variance_extension env check decl ext rloc =
  compute_variance_gadt env check rloc
    {decl with type_params = ext.ext_type_params}
    (ext.ext_args, ext.ext_ret_type)

let compute_variance_decl env check decl (required, _ as rloc) =
  if (decl.type_kind = Type_abstract || decl.type_kind = Type_open)
       && decl.type_manifest = None then
    List.map
      (fun (c, n, i) ->
        make (not n) (not c) (decl.type_kind <> Type_abstract || i))
      required
  else
  let mn =
    match decl.type_manifest with
      None -> []
    | Some ty -> [false, ty]
  in
  match decl.type_kind with
    Type_abstract | Type_open ->
      compute_variance_type env check rloc decl mn
  | Type_variant tll ->
      if List.for_all (fun c -> c.Types.cd_res = None) tll then
        compute_variance_type env check rloc decl
          (mn @ List.flatten (List.map (fun c -> for_constr c.Types.cd_args)
                                tll))
      else begin
        let mn =
          List.map (fun (_,ty) -> (Types.Cstr_tuple [ty],None)) mn in
        let tll =
          mn @ List.map (fun c -> c.Types.cd_args, c.Types.cd_res) tll in
        match List.map (compute_variance_gadt env check rloc decl) tll with
        | vari :: rem ->
            let varl = List.fold_left (List.map2 Variance.union) vari rem in
            List.map
              Variance.(fun v -> if mem Pos v && mem Neg v then full else v)
              varl
        | _ -> assert false
      end
  | Type_record (ftl, _) ->
      compute_variance_type env check rloc decl
        (mn @ List.map (fun {Types.ld_mutable; ld_type} ->
             (ld_mutable = Mutable, ld_type)) ftl)

let is_hash id =
  let s = Ident.name id in
  String.length s > 0 && s.[0] = '#'

let marked_as_immediate decl =
  Builtin_attributes.immediate decl.type_attributes

let compute_immediacy env tdecl =
  match (tdecl.type_kind, tdecl.type_manifest) with
  | (Type_variant [{cd_args = Cstr_tuple [arg]; _}], _)
    | (Type_variant [{cd_args = Cstr_record [{ld_type = arg; _}]; _}], _)
    | (Type_record ([{ld_type = arg; _}], _), _)
  when tdecl.type_unboxed.unboxed ->
    begin match get_unboxed_type_representation env arg with
    | Some argrepr -> not (Ctype.maybe_pointer_type env argrepr)
    | None -> false
    end
  | (Type_variant (_ :: _ as cstrs), _) ->
    not (List.exists (fun c -> c.Types.cd_args <> Types.Cstr_tuple []) cstrs)
  | (Type_abstract, Some(typ)) ->
    not (Ctype.maybe_pointer_type env typ)
  | (Type_abstract, None) -> marked_as_immediate tdecl
  | _ -> false

(* Computes the fixpoint for the variance and immediacy of type declarations *)

let rec compute_properties_fixpoint env decls required variances immediacies =
  let new_decls =
    List.map2
      (fun (id, decl) (variance, immediacy) ->
         id, {decl with type_variance = variance; type_immediate = immediacy})
      decls (List.combine variances immediacies)
  in
  let new_env =
    List.fold_right
      (fun (id, decl) env -> Env.add_type ~check:true id decl env)
      new_decls env
  in
  let new_variances =
    List.map2
      (fun (_id, decl) -> compute_variance_decl new_env false decl)
      new_decls required
  in
  let new_variances =
    List.map2 (List.map2 Variance.union) new_variances variances in
  let new_immediacies =
    List.map
      (fun (_id, decl) -> compute_immediacy new_env decl)
      new_decls
  in
  if new_variances <> variances || new_immediacies <> immediacies then
    compute_properties_fixpoint env decls required new_variances new_immediacies
  else begin
    (* List.iter (fun (id, decl) ->
      Printf.eprintf "%s:" (Ident.name id);
      List.iter (fun (v : Variance.t) ->
        Printf.eprintf " %x" (Obj.magic v : int))
        decl.type_variance;
      prerr_endline "")
      new_decls; *)
    List.iter (fun (_, decl) ->
      if (marked_as_immediate decl) && (not decl.type_immediate) then
        raise (Error (decl.type_loc, Bad_immediate_attribute))
      else ())
      new_decls;
    List.iter2
      (fun (id, decl) req -> if not (is_hash id) then
        ignore (compute_variance_decl new_env true decl req))
      new_decls required;
    new_decls, new_env
  end

let init_variance (_id, decl) =
  List.map (fun _ -> Variance.null) decl.type_params

let add_injectivity =
  List.map
    (function
      | Covariant -> (true, false, false)
      | Contravariant -> (false, true, false)
      | Invariant -> (false, false, false)
    )

(* for typeclass.ml *)
let compute_variance_decls env cldecls =
  let decls, required =
    List.fold_right
      (fun (obj_id, obj_abbr, _cl_abbr, _clty, _cltydef, ci) (decls, req) ->
        let variance = List.map snd ci.ci_params in
        (obj_id, obj_abbr) :: decls,
        (add_injectivity variance, ci.ci_loc) :: req)
      cldecls ([],[])
  in
  let (decls, _) =
    compute_properties_fixpoint env decls required
      (List.map init_variance decls)
      (List.map (fun _ -> false) decls)
  in
  List.map2
    (fun (_,decl) (_, _, cl_abbr, clty, cltydef, _) ->
      let variance = decl.type_variance in
      (decl, {cl_abbr with type_variance = variance},
       {clty with cty_variance = variance},
       {cltydef with clty_variance = variance}))
    decls cldecls

(* Check multiple declarations of labels/constructors *)

let check_duplicates sdecl_list =
  let labels = Hashtbl.create 7 and constrs = Hashtbl.create 7 in
  List.iter
    (fun sdecl -> match sdecl.ptype_kind with
      Ptype_variant cl ->
        List.iter
          (fun pcd ->
            try
              let name' = Hashtbl.find constrs pcd.pcd_name.txt in
              Location.prerr_warning pcd.pcd_loc
                (Warnings.Duplicate_definitions
                   ("constructor", pcd.pcd_name.txt, name',
                    sdecl.ptype_name.txt))
            with Not_found ->
              Hashtbl.add constrs pcd.pcd_name.txt sdecl.ptype_name.txt)
          cl
    | Ptype_record fl ->
        List.iter
          (fun {pld_name=cname;pld_loc=loc} ->
            try
              let name' = Hashtbl.find labels cname.txt in
              Location.prerr_warning loc
                (Warnings.Duplicate_definitions
                   ("label", cname.txt, name', sdecl.ptype_name.txt))
            with Not_found -> Hashtbl.add labels cname.txt sdecl.ptype_name.txt)
          fl
    | Ptype_abstract -> ()
    | Ptype_open -> ())
    sdecl_list

(* Force recursion to go through id for private types*)
let name_recursion sdecl id decl =
  match decl with
  | { type_kind = Type_abstract;
      type_manifest = Some ty;
      type_private = Private; } when is_fixed_type sdecl ->
    let ty = Ctype.repr ty in
    let ty' = Btype.newty2 ty.level ty.desc in
    if Ctype.deep_occur ty ty' then
      let td = Tconstr(Path.Pident id, decl.type_params, ref Mnil) in
      Btype.link_type ty (Btype.newty2 ty.level td);
      {decl with type_manifest = Some ty'}
    else decl
  | _ -> decl

(* Translate a set of type declarations, mutually recursive or not *)
let transl_type_decl env rec_flag sdecl_list =
  (* Add dummy types for fixed rows *)
  let fixed_types = List.filter is_fixed_type sdecl_list in
  let sdecl_list =
    List.map
      (fun sdecl ->
        let ptype_name =
          mkloc (sdecl.ptype_name.txt ^"#row") sdecl.ptype_name.loc in
        {sdecl with
         ptype_name; ptype_kind = Ptype_abstract; ptype_manifest = None})
      fixed_types
    @ sdecl_list
  in

  (* Create identifiers. *)
  let id_list =
    List.map (fun sdecl -> Ident.create sdecl.ptype_name.txt) sdecl_list
  in
  (*
     Since we've introduced fresh idents, make sure the definition
     level is at least the binding time of these events. Otherwise,
     passing one of the recursively-defined type constrs as argument
     to an abbreviation may fail.
  *)
  Ctype.init_def(Ident.current_time());
  Ctype.begin_def();
  (* Enter types. *)
  let temp_env =
    List.fold_left2 (enter_type rec_flag) env sdecl_list id_list in
  (* Translate each declaration. *)
  let current_slot = ref None in
  let warn_unused = Warnings.is_active (Warnings.Unused_type_declaration "") in
  let id_slots id =
    match rec_flag with
    | Asttypes.Recursive when warn_unused ->
        (* See typecore.ml for a description of the algorithm used
             to detect unused declarations in a set of recursive definitions. *)
        let slot = ref [] in
        let td = Env.find_type (Path.Pident id) temp_env in
        let name = Ident.name id in
        Env.set_type_used_callback
          name td
          (fun old_callback ->
             match !current_slot with
             | Some slot -> slot := (name, td) :: !slot
             | None ->
                 List.iter (fun (name, d) -> Env.mark_type_used env name d)
                   (get_ref slot);
                 old_callback ()
          );
        id, Some slot
    | Asttypes.Recursive | Asttypes.Nonrecursive ->
        id, None
  in
  let transl_declaration name_sdecl (id, slot) =
    current_slot := slot;
    Builtin_attributes.warning_scope
      name_sdecl.ptype_attributes
      (fun () -> transl_declaration temp_env name_sdecl id)
  in
  let tdecls =
    List.map2 transl_declaration sdecl_list (List.map id_slots id_list) in
  let decls =
    List.map (fun tdecl -> (tdecl.typ_id, tdecl.typ_type)) tdecls in
  current_slot := None;
  (* Check for duplicates *)
  check_duplicates sdecl_list;
  (* Build the final env. *)
  let newenv =
    List.fold_right
      (fun (id, decl) env -> Env.add_type ~check:true id decl env)
      decls env
  in
  (* Update stubs *)
  begin match rec_flag with
    | Asttypes.Nonrecursive -> ()
    | Asttypes.Recursive ->
      List.iter2
        (fun id sdecl -> update_type temp_env newenv id sdecl.ptype_loc)
        id_list sdecl_list
  end;
  (* Generalize type declarations. *)
  Ctype.end_def();
  List.iter (fun (_, decl) -> generalize_decl decl) decls;
  (* Check for ill-formed abbrevs *)
  let id_loc_list =
    List.map2 (fun id sdecl -> (id, sdecl.ptype_loc))
      id_list sdecl_list
  in
  List.iter (fun (id, decl) ->
    check_well_founded_manifest newenv (List.assoc id id_loc_list)
      (Path.Pident id) decl)
    decls;
  let to_check =
    function Path.Pident id -> List.mem_assoc id id_loc_list | _ -> false in
  List.iter (fun (id, decl) ->
    check_well_founded_decl newenv (List.assoc id id_loc_list) (Path.Pident id)
      decl to_check)
    decls;
  List.iter (check_abbrev_recursion newenv id_loc_list to_check) tdecls;
  (* Check that all type variables are closed *)
  List.iter2
    (fun sdecl tdecl ->
      let decl = tdecl.typ_type in
       match Ctype.closed_type_decl decl with
         Some ty -> raise(Error(sdecl.ptype_loc, Unbound_type_var(ty,decl)))
       | None   -> ())
    sdecl_list tdecls;
  (* Check that constraints are enforced *)
  List.iter2 (check_constraints newenv) sdecl_list decls;
  (* Name recursion *)
  let decls =
    List.map2 (fun sdecl (id, decl) -> id, name_recursion sdecl id decl)
      sdecl_list decls
  in
  (* Add variances to the environment *)
  let required =
    List.map
      (fun sdecl ->
         add_injectivity (List.map snd sdecl.ptype_params),
         sdecl.ptype_loc
      )
      sdecl_list
  in
  let final_decls, final_env =
    compute_properties_fixpoint env decls required
      (List.map init_variance decls)
      (List.map (fun _ -> false) decls)
  in
  (* Check re-exportation *)
  List.iter2 (check_abbrev final_env) sdecl_list final_decls;
  (* Keep original declaration *)
  let final_decls =
    List.map2
      (fun tdecl (_id2, decl) ->
        { tdecl with typ_type = decl }
      ) tdecls final_decls
  in
  (* Done *)
  (final_decls, final_env)

(* Translating type extensions *)

let transl_extension_constructor env type_path type_params
                                 typext_params priv sext =
  let id = Ident.create sext.pext_name.txt in
  let args, ret_type, kind =
    match sext.pext_kind with
      Pext_decl(sargs, sret_type) ->
        let targs, tret_type, args, ret_type, _ =
          make_constructor env type_path typext_params
            sargs sret_type
        in
          args, ret_type, Text_decl(targs, tret_type)
    | Pext_rebind lid ->
        let cdescr = Typetexp.find_constructor env lid.loc lid.txt in
        let usage =
          if cdescr.cstr_private = Private || priv = Public
          then Env.Positive else Env.Privatize
        in
        Env.mark_constructor usage env (Longident.last lid.txt) cdescr;
        let (args, cstr_res) = Ctype.instance_constructor cdescr in
        let res, ret_type =
          if cdescr.cstr_generalized then
            let params = Ctype.instance_list env type_params in
            let res = Ctype.newconstr type_path params in
            let ret_type = Some (Ctype.newconstr type_path params) in
              res, ret_type
          else (Ctype.newconstr type_path typext_params), None
        in
        begin
          try
            Ctype.unify env cstr_res res
          with Ctype.Unify trace ->
            raise (Error(lid.loc,
                     Rebind_wrong_type(lid.txt, env, trace)))
        end;
        (* Remove "_" names from parameters used in the constructor *)
        if not cdescr.cstr_generalized then begin
          let vars =
            Ctype.free_variables (Btype.newgenty (Ttuple args))
          in
            List.iter
              (function {desc = Tvar (Some "_")} as ty ->
                          if List.memq ty vars then ty.desc <- Tvar None
                        | _ -> ())
              typext_params
        end;
        (* Ensure that constructor's type matches the type being extended *)
        let cstr_type_path, cstr_type_params =
          match cdescr.cstr_res.desc with
            Tconstr (p, _, _) ->
              let decl = Env.find_type p env in
                p, decl.type_params
          | _ -> assert false
        in
        let cstr_types =
          (Btype.newgenty
             (Tconstr(cstr_type_path, cstr_type_params, ref Mnil)))
          :: cstr_type_params
        in
        let ext_types =
          (Btype.newgenty
             (Tconstr(type_path, type_params, ref Mnil)))
          :: type_params
        in
        if not (Ctype.equal env true cstr_types ext_types) then
          raise (Error(lid.loc,
                       Rebind_mismatch(lid.txt, cstr_type_path, type_path)));
        (* Disallow rebinding private constructors to non-private *)
        begin
          match cdescr.cstr_private, priv with
            Private, Public ->
              raise (Error(lid.loc, Rebind_private lid.txt))
          | _ -> ()
        end;
        let path =
          match cdescr.cstr_tag with
            Cstr_extension(path, _) -> path
          | _ -> assert false
        in
        let args =
          match cdescr.cstr_inlined with
          | None ->
              Types.Cstr_tuple args
          | Some decl ->
              let tl =
                match args with
                | [ {desc=Tconstr(_, tl, _)} ] -> tl
                | _ -> assert false
              in
              let decl = Ctype.instance_declaration decl in
              assert (List.length decl.type_params = List.length tl);
              List.iter2 (Ctype.unify env) decl.type_params tl;
              let lbls =
                match decl.type_kind with
                | Type_record (lbls, Record_extension) -> lbls
                | _ -> assert false
              in
              Types.Cstr_record lbls
        in
        args, ret_type, Text_rebind(path, lid)
  in
  let ext =
    { ext_type_path = type_path;
      ext_type_params = typext_params;
      ext_args = args;
      ext_ret_type = ret_type;
      ext_private = priv;
      Types.ext_loc = sext.pext_loc;
      Types.ext_attributes = sext.pext_attributes; }
  in
    { ext_id = id;
      ext_name = sext.pext_name;
      ext_type = ext;
      ext_kind = kind;
      Typedtree.ext_loc = sext.pext_loc;
      Typedtree.ext_attributes = sext.pext_attributes; }

let transl_extension_constructor env type_path type_params
    typext_params priv sext =
  Builtin_attributes.warning_scope sext.pext_attributes
    (fun () -> transl_extension_constructor env type_path type_params
        typext_params priv sext)

let transl_type_extension extend env loc styext =
  reset_type_variables();
  Ctype.begin_def();
  let (type_path, type_decl) =
    let lid = styext.ptyext_path in
    Typetexp.find_type env lid.loc lid.txt
  in
  begin
    match type_decl.type_kind with
    | Type_open -> begin
        match type_decl.type_private with
        | Private when extend -> begin
            match
              List.find
                (function {pext_kind = Pext_decl _} -> true
                        | {pext_kind = Pext_rebind _} -> false)
                styext.ptyext_constructors
            with
            | {pext_loc} ->
                raise (Error(pext_loc, Cannot_extend_private_type type_path))
            | exception Not_found -> ()
          end
        | _ -> ()
      end
    | _ ->
        raise (Error(loc, Not_extensible_type type_path))
  end;
  let type_variance =
    List.map (fun v ->
                let (co, cn) = Variance.get_upper v in
                  (not cn, not co, false))
             type_decl.type_variance
  in
  let err =
    if type_decl.type_arity <> List.length styext.ptyext_params then
      [Includecore.Arity]
    else
      if List.for_all2
           (fun (c1, n1, _) (c2, n2, _) -> (not c2 || c1) && (not n2 || n1))
           type_variance
           (add_injectivity (List.map snd styext.ptyext_params))
      then [] else [Includecore.Variance]
  in
  if err <> [] then
    raise (Error(loc, Extension_mismatch (type_path, err)));
  let ttype_params = make_params env styext.ptyext_params in
  let type_params = List.map (fun (cty, _) -> cty.ctyp_type) ttype_params in
  List.iter2 (Ctype.unify_var env)
    (Ctype.instance_list env type_decl.type_params)
    type_params;
  let constructors =
    List.map (transl_extension_constructor env type_path
               type_decl.type_params type_params styext.ptyext_private)
      styext.ptyext_constructors
  in
  Ctype.end_def();
  (* Generalize types *)
  List.iter Ctype.generalize type_params;
  List.iter
    (fun ext ->
       Btype.iter_type_expr_cstr_args Ctype.generalize ext.ext_type.ext_args;
       may Ctype.generalize ext.ext_type.ext_ret_type)
    constructors;
  (* Check that all type variables are closed *)
  List.iter
    (fun ext ->
       match Ctype.closed_extension_constructor ext.ext_type with
         Some ty ->
           raise(Error(ext.ext_loc, Unbound_type_var_ext(ty, ext.ext_type)))
       | None -> ())
    constructors;
  (* Check variances are correct *)
  List.iter
    (fun ext->
      ignore (compute_variance_extension env true type_decl
                ext.ext_type (type_variance, loc)))
    constructors;
  (* Add extension constructors to the environment *)
  let newenv =
    List.fold_left
      (fun env ext ->
         Env.add_extension ~check:true ext.ext_id ext.ext_type env)
      env constructors
  in
  let tyext =
    { tyext_path = type_path;
      tyext_txt = styext.ptyext_path;
      tyext_params = ttype_params;
      tyext_constructors = constructors;
      tyext_private = styext.ptyext_private;
      tyext_attributes = styext.ptyext_attributes; }
  in
    (tyext, newenv)

let transl_type_extension extend env loc styext =
  Builtin_attributes.warning_scope styext.ptyext_attributes
    (fun () -> transl_type_extension extend env loc styext)

let transl_exception env sext =
  reset_type_variables();
  Ctype.begin_def();
  let ext =
    transl_extension_constructor env
      Predef.path_exn [] [] Asttypes.Public sext
  in
  Ctype.end_def();
  (* Generalize types *)
  Btype.iter_type_expr_cstr_args Ctype.generalize ext.ext_type.ext_args;
  may Ctype.generalize ext.ext_type.ext_ret_type;
  (* Check that all type variables are closed *)
  begin match Ctype.closed_extension_constructor ext.ext_type with
    Some ty ->
      raise (Error(ext.ext_loc, Unbound_type_var_ext(ty, ext.ext_type)))
  | None -> ()
  end;
  let newenv = Env.add_extension ~check:true ext.ext_id ext.ext_type env in
    ext, newenv

type native_repr_attribute =
  | Native_repr_attr_absent
  | Native_repr_attr_present of native_repr_kind

let get_native_repr_attribute attrs ~global_repr =
  match
    Attr_helper.get_no_payload_attribute ["unboxed"; "ocaml.unboxed"]  attrs,
    Attr_helper.get_no_payload_attribute ["untagged"; "ocaml.untagged"] attrs,
    global_repr
  with
  | None, None, None -> Native_repr_attr_absent
  | None, None, Some repr -> Native_repr_attr_present repr
  | Some _, None, None -> Native_repr_attr_present Unboxed
  | None, Some _, None -> Native_repr_attr_present Untagged
  | Some { Location.loc }, _, _
  | _, Some { Location.loc }, _ ->
    raise (Error (loc, Multiple_native_repr_attributes))

let native_repr_of_type env kind ty =
  match kind, (Ctype.expand_head_opt env ty).desc with
  | Untagged, Tconstr (path, _, _) when Path.same path Predef.path_int ->
    Some Untagged_int
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float ->
    Some Unboxed_float
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int32 ->
    Some (Unboxed_integer Pint32)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int64 ->
    Some (Unboxed_integer Pint64)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_nativeint ->
    Some (Unboxed_integer Pnativeint)
  | _ ->
    None

(* Raises an error when [core_type] contains an [@unboxed] or [@untagged]
   attribute in a strict sub-term. *)
let error_if_has_deep_native_repr_attributes core_type =
  let open Ast_iterator in
  let this_iterator =
    { default_iterator with typ = fun iterator core_type ->
      begin
        match
          get_native_repr_attribute core_type.ptyp_attributes ~global_repr:None
        with
        | Native_repr_attr_present kind ->
           raise (Error (core_type.ptyp_loc,
                         Deep_unbox_or_untag_attribute kind))
        | Native_repr_attr_absent -> ()
      end;
      default_iterator.typ iterator core_type }
  in
  default_iterator.typ this_iterator core_type

let make_native_repr env core_type ty ~global_repr =
  error_if_has_deep_native_repr_attributes core_type;
  match get_native_repr_attribute core_type.ptyp_attributes ~global_repr with
  | Native_repr_attr_absent ->
    Same_as_ocaml_repr
  | Native_repr_attr_present kind ->
    begin match native_repr_of_type env kind ty with
    | None ->
      raise (Error (core_type.ptyp_loc, Cannot_unbox_or_untag_type kind))
    | Some repr -> repr
    end

let rec parse_native_repr_attributes env core_type ty ~global_repr =
  match core_type.ptyp_desc, (Ctype.repr ty).desc,
    get_native_repr_attribute core_type.ptyp_attributes ~global_repr:None
  with
  | Ptyp_arrow _, Tarrow _, Native_repr_attr_present kind  ->
    raise (Error (core_type.ptyp_loc, Cannot_unbox_or_untag_type kind))
  | Ptyp_arrow (_, ct1, ct2), Tarrow (_, t1, t2, _), _ ->
    let repr_arg = make_native_repr env ct1 t1 ~global_repr in
    let repr_args, repr_res =
      parse_native_repr_attributes env ct2 t2 ~global_repr
    in
    (repr_arg :: repr_args, repr_res)
  | Ptyp_arrow _, _, _ | _, Tarrow _, _ -> assert false
  | _ -> ([], make_native_repr env core_type ty ~global_repr)


let check_unboxable env loc ty =
  let ty = Ctype.repr (Ctype.expand_head_opt env ty) in
  try match ty.desc with
  | Tconstr (p, _, _) ->
    let tydecl = Env.find_type p env in
    if tydecl.type_unboxed.unboxed then
      Location.prerr_warning loc
        (Warnings.Unboxable_type_in_prim_decl (Path.name p))
  | _ -> ()
  with Not_found -> ()

(* Translate a value declaration *)
let transl_value_decl env loc valdecl =
  let cty = Typetexp.transl_type_scheme env valdecl.pval_type in
  let ty = cty.ctyp_type in
  let v =
  match valdecl.pval_prim with
    [] when Env.is_in_signature env ->
      { val_type = ty; val_kind = Val_reg; Types.val_loc = loc;
        val_attributes = valdecl.pval_attributes }
  | [] ->
      raise (Error(valdecl.pval_loc, Val_in_structure))
  | _ ->
      let global_repr =
        match
          get_native_repr_attribute valdecl.pval_attributes ~global_repr:None
        with
        | Native_repr_attr_present repr -> Some repr
        | Native_repr_attr_absent -> None
      in
      let native_repr_args, native_repr_res =
        parse_native_repr_attributes env valdecl.pval_type ty ~global_repr
      in
      let prim =
        Primitive.parse_declaration valdecl
          ~native_repr_args
          ~native_repr_res
      in
      if prim.prim_arity = 0 &&
         (prim.prim_name = "" || prim.prim_name.[0] <> '%') then
        raise(Error(valdecl.pval_type.ptyp_loc, Null_arity_external));
      if !Clflags.native_code
      && prim.prim_arity > 5
      && prim.prim_native_name = ""
      then raise(Error(valdecl.pval_type.ptyp_loc, Missing_native_external));
      Btype.iter_type_expr (check_unboxable env loc) ty;
      { val_type = ty; val_kind = Val_prim prim; Types.val_loc = loc;
        val_attributes = valdecl.pval_attributes }
  in
  let (id, newenv) =
    Env.enter_value valdecl.pval_name.txt v env
      ~check:(fun s -> Warnings.Unused_value_declaration s)
  in
  let desc =
    {
     val_id = id;
     val_name = valdecl.pval_name;
     val_desc = cty; val_val = v;
     val_prim = valdecl.pval_prim;
     val_loc = valdecl.pval_loc;
     val_attributes = valdecl.pval_attributes;
    }
  in
  desc, newenv

let transl_value_decl env loc valdecl =
  Builtin_attributes.warning_scope valdecl.pval_attributes
    (fun () -> transl_value_decl env loc valdecl)

(* Translate a "with" constraint -- much simplified version of
    transl_type_decl. *)
let transl_with_constraint env id row_path orig_decl sdecl =
  Env.mark_type_used env (Ident.name id) orig_decl;
  reset_type_variables();
  Ctype.begin_def();
  let tparams = make_params env sdecl.ptype_params in
  let params = List.map (fun (cty, _) -> cty.ctyp_type) tparams in
  let orig_decl = Ctype.instance_declaration orig_decl in
  let arity_ok = List.length params = orig_decl.type_arity in
  if arity_ok then
    List.iter2 (Ctype.unify_var env) params orig_decl.type_params;
  let constraints = List.map
    (function (ty, ty', loc) ->
       try
         let cty = transl_simple_type env false ty in
         let cty' = transl_simple_type env false ty' in
         let ty = cty.ctyp_type in
         let ty' = cty'.ctyp_type in
         Ctype.unify env ty ty';
         (cty, cty', loc)
       with Ctype.Unify tr ->
         raise(Error(loc, Inconsistent_constraint (env, tr))))
    sdecl.ptype_cstrs
  in
  let no_row = not (is_fixed_type sdecl) in
  let (tman, man) =  match sdecl.ptype_manifest with
      None -> None, None
    | Some sty ->
        let cty = transl_simple_type env no_row sty in
        Some cty, Some cty.ctyp_type
  in
  let priv =
    if sdecl.ptype_private = Private then Private else
    if arity_ok && orig_decl.type_kind <> Type_abstract
    then orig_decl.type_private else sdecl.ptype_private
  in
  if arity_ok && orig_decl.type_kind <> Type_abstract
  && sdecl.ptype_private = Private then
    Location.deprecated sdecl.ptype_loc "spurious use of private";
  let type_kind, type_unboxed =
    if arity_ok && man <> None then
      orig_decl.type_kind, orig_decl.type_unboxed
    else
      Type_abstract, unboxed_false_default_false
  in
  let decl =
    { type_params = params;
      type_arity = List.length params;
      type_kind;
      type_private = priv;
      type_manifest = man;
      type_variance = [];
      type_newtype_level = None;
      type_loc = sdecl.ptype_loc;
      type_attributes = sdecl.ptype_attributes;
      type_immediate = false;
      type_unboxed;
    }
  in
  begin match row_path with None -> ()
  | Some p -> set_fixed_row env sdecl.ptype_loc p decl
  end;
  begin match Ctype.closed_type_decl decl with None -> ()
  | Some ty -> raise(Error(sdecl.ptype_loc, Unbound_type_var(ty,decl)))
  end;
  let decl = name_recursion sdecl id decl in
  let type_variance =
    compute_variance_decl env true decl
      (add_injectivity (List.map snd sdecl.ptype_params), sdecl.ptype_loc)
  in
  let type_immediate = compute_immediacy env decl in
  let decl = {decl with type_variance; type_immediate} in
  Ctype.end_def();
  generalize_decl decl;
  {
    typ_id = id;
    typ_name = sdecl.ptype_name;
    typ_params = tparams;
    typ_type = decl;
    typ_cstrs = constraints;
    typ_loc = sdecl.ptype_loc;
    typ_manifest = tman;
    typ_kind = Ttype_abstract;
    typ_private = sdecl.ptype_private;
    typ_attributes = sdecl.ptype_attributes;
  }

(* Approximate a type declaration: just make all types abstract *)

let abstract_type_decl arity =
  let rec make_params n =
    if n <= 0 then [] else Ctype.newvar() :: make_params (n-1) in
  Ctype.begin_def();
  let decl =
    { type_params = make_params arity;
      type_arity = arity;
      type_kind = Type_abstract;
      type_private = Public;
      type_manifest = None;
      type_variance = replicate_list Variance.full arity;
      type_newtype_level = None;
      type_loc = Location.none;
      type_attributes = [];
      type_immediate = false;
      type_unboxed = unboxed_false_default_false;
     } in
  Ctype.end_def();
  generalize_decl decl;
  decl

let approx_type_decl sdecl_list =
  List.map
    (fun sdecl ->
      (Ident.create sdecl.ptype_name.txt,
       abstract_type_decl (List.length sdecl.ptype_params)))
    sdecl_list

(* Variant of check_abbrev_recursion to check the well-formedness
   conditions on type abbreviations defined within recursive modules. *)

let check_recmod_typedecl env loc recmod_ids path decl =
  (* recmod_ids is the list of recursively-defined module idents.
     (path, decl) is the type declaration to be checked. *)
  let to_check path =
    List.exists (fun id -> Path.isfree id path) recmod_ids in
  check_well_founded_decl env loc path decl to_check;
  check_recursion env loc path decl to_check


(**** Error report ****)

open Format

let explain_unbound_gen ppf tv tl typ kwd pr =
  try
    let ti = List.find (fun ti -> Ctype.deep_occur tv (typ ti)) tl in
    let ty0 = (* Hack to force aliasing when needed *)
      Btype.newgenty (Tobject(tv, ref None)) in
    Printtyp.reset_and_mark_loops_list [typ ti; ty0];
    fprintf ppf
      ".@.@[<hov2>In %s@ %a@;<1 -2>the variable %a is unbound@]"
      kwd pr ti Printtyp.type_expr tv
  with Not_found -> ()

let explain_unbound ppf tv tl typ kwd lab =
  explain_unbound_gen ppf tv tl typ kwd
    (fun ppf ti -> fprintf ppf "%s%a" (lab ti) Printtyp.type_expr (typ ti))

let explain_unbound_single ppf tv ty =
  let trivial ty =
    explain_unbound ppf tv [ty] (fun t -> t) "type" (fun _ -> "") in
  match (Ctype.repr ty).desc with
    Tobject(fi,_) ->
      let (tl, rv) = Ctype.flatten_fields fi in
      if rv == tv then trivial ty else
      explain_unbound ppf tv tl (fun (_,_,t) -> t)
        "method" (fun (lab,_,_) -> lab ^ ": ")
  | Tvariant row ->
      let row = Btype.row_repr row in
      if row.row_more == tv then trivial ty else
      explain_unbound ppf tv row.row_fields
        (fun (_l,f) -> match Btype.row_field_repr f with
          Rpresent (Some t) -> t
        | Reither (_,[t],_,_) -> t
        | Reither (_,tl,_,_) -> Btype.newgenty (Ttuple tl)
        | _ -> Btype.newgenty (Ttuple[]))
        "case" (fun (lab,_) -> "`" ^ lab ^ " of ")
  | _ -> trivial ty


let tys_of_constr_args = function
  | Types.Cstr_tuple tl -> tl
  | Types.Cstr_record lbls -> List.map (fun l -> l.Types.ld_type) lbls

let report_error ppf = function
  | Repeated_parameter ->
      fprintf ppf "A type parameter occurs several times"
  | Duplicate_constructor s ->
      fprintf ppf "Two constructors are named %s" s
  | Too_many_constructors ->
      fprintf ppf
        "@[Too many non-constant constructors@ -- maximum is %i %s@]"
        (Config.max_tag + 1) "non-constant constructors"
  | Duplicate_label s ->
      fprintf ppf "Two labels are named %s" s
  | Recursive_abbrev s ->
      fprintf ppf "The type abbreviation %s is cyclic" s
  | Cycle_in_def (s, ty) ->
      Printtyp.reset_and_mark_loops ty;
      fprintf ppf "@[<v>The definition of %s contains a cycle:@ %a@]"
        s Printtyp.type_expr ty
  | Definition_mismatch (ty, errs) ->
      Printtyp.reset_and_mark_loops ty;
      fprintf ppf "@[<v>@[<hov>%s@ %s@;<1 2>%a@]%a@]"
        "This variant or record definition" "does not match that of type"
        Printtyp.type_expr ty
        (Includecore.report_type_mismatch "the original" "this" "definition")
        errs
  | Constraint_failed (ty, ty') ->
      Printtyp.reset_and_mark_loops ty;
      Printtyp.mark_loops ty';
      fprintf ppf "@[%s@ @[<hv>Type@ %a@ should be an instance of@ %a@]@]"
        "Constraints are not satisfied in this type."
        Printtyp.type_expr ty Printtyp.type_expr ty'
  | Parameters_differ (path, ty, ty') ->
      Printtyp.reset_and_mark_loops ty;
      Printtyp.mark_loops ty';
      fprintf ppf
        "@[<hv>In the definition of %s, type@ %a@ should be@ %a@]"
        (Path.name path) Printtyp.type_expr ty Printtyp.type_expr ty'
  | Inconsistent_constraint (env, trace) ->
      fprintf ppf "The type constraints are not consistent.@.";
      Printtyp.report_unification_error ppf env trace
        (fun ppf -> fprintf ppf "Type")
        (fun ppf -> fprintf ppf "is not compatible with type")
  | Type_clash (env, trace) ->
      Printtyp.report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "This type constructor expands to type")
        (function ppf ->
           fprintf ppf "but is used here with type")
  | Null_arity_external ->
      fprintf ppf "External identifiers must be functions"
  | Missing_native_external ->
      fprintf ppf "@[<hv>An external function with more than 5 arguments \
                   requires a second stub function@ \
                   for native-code compilation@]"
  | Unbound_type_var (ty, decl) ->
      fprintf ppf "A type variable is unbound in this type declaration";
      let ty = Ctype.repr ty in
      begin match decl.type_kind, decl.type_manifest with
      | Type_variant tl, _ ->
          explain_unbound_gen ppf ty tl (fun c ->
              let tl = tys_of_constr_args c.Types.cd_args in
              Btype.newgenty (Ttuple tl)
            )
            "case" (fun ppf c ->
                fprintf ppf
                  "%s of %a" (Ident.name c.Types.cd_id)
                  Printtyp.constructor_arguments c.Types.cd_args)
      | Type_record (tl, _), _ ->
          explain_unbound ppf ty tl (fun l -> l.Types.ld_type)
            "field" (fun l -> Ident.name l.Types.ld_id ^ ": ")
      | Type_abstract, Some ty' ->
          explain_unbound_single ppf ty ty'
      | _ -> ()
      end
  | Unbound_type_var_ext (ty, ext) ->
      fprintf ppf "A type variable is unbound in this extension constructor";
      let args = tys_of_constr_args ext.ext_args in
      explain_unbound ppf ty args (fun c -> c) "type" (fun _ -> "")
  | Cannot_extend_private_type path ->
      fprintf ppf "@[%s@ %a@]"
        "Cannot extend private type definition"
        Printtyp.path path
  | Not_extensible_type path ->
      fprintf ppf "@[%s@ %a@ %s@]"
        "Type definition"
        Printtyp.path path
        "is not extensible"
  | Extension_mismatch (path, errs) ->
      fprintf ppf "@[<v>@[<hov>%s@ %s@;<1 2>%s@]%a@]"
        "This extension" "does not match the definition of type"
        (Path.name path)
        (Includecore.report_type_mismatch
           "the type" "this extension" "definition")
        errs
  | Rebind_wrong_type (lid, env, trace) ->
      Printtyp.report_unification_error ppf env trace
        (function ppf ->
          fprintf ppf "The constructor %a@ has type"
            Printtyp.longident lid)
        (function ppf ->
           fprintf ppf "but was expected to be of type")
  | Rebind_mismatch (lid, p, p') ->
      fprintf ppf
        "@[%s@ %a@ %s@ %s@ %s@ %s@ %s@]"
        "The constructor" Printtyp.longident lid
        "extends type" (Path.name p)
        "whose declaration does not match"
        "the declaration of type" (Path.name p')
  | Rebind_private lid ->
      fprintf ppf "@[%s@ %a@ %s@]"
        "The constructor"
        Printtyp.longident lid
        "is private"
  | Bad_variance (n, v1, v2) ->
      let variance (p,n,i) =
        let inj = if i then "injective " else "" in
        match p, n with
          true,  true  -> inj ^ "invariant"
        | true,  false -> inj ^ "covariant"
        | false, true  -> inj ^ "contravariant"
        | false, false -> if inj = "" then "unrestricted" else inj
      in
      let suffix n =
        let teen = (n mod 100)/10 = 1 in
        match n mod 10 with
        | 1 when not teen -> "st"
        | 2 when not teen -> "nd"
        | 3 when not teen -> "rd"
        | _ -> "th"
      in
      if n = -1 then
        fprintf ppf "@[%s@ %s@ It"
          "In this definition, a type variable has a variance that"
          "is not reflected by its occurrence in type parameters."
      else if n = -2 then
        fprintf ppf "@[%s@ %s@]"
          "In this definition, a type variable cannot be deduced"
          "from the type parameters."
      else if n = -3 then
        fprintf ppf "@[%s@ %s@ It"
          "In this definition, a type variable has a variance that"
          "cannot be deduced from the type parameters."
      else
        fprintf ppf "@[%s@ %s@ The %d%s type parameter"
          "In this definition, expected parameter"
          "variances are not satisfied."
          n (suffix n);
      if n <> -2 then
        fprintf ppf " was expected to be %s,@ but it is %s.@]"
          (variance v2) (variance v1)
  | Unavailable_type_constructor p ->
      fprintf ppf "The definition of type %a@ is unavailable" Printtyp.path p
  | Bad_fixed_type r ->
      fprintf ppf "This fixed type %s" r
  | Varying_anonymous ->
      fprintf ppf "@[%s@ %s@ %s@]"
        "In this GADT definition," "the variance of some parameter"
        "cannot be checked"
  | Val_in_structure ->
      fprintf ppf "Value declarations are only allowed in signatures"
  | Multiple_native_repr_attributes ->
      fprintf ppf "Too many [@@unboxed]/[@@untagged] attributes"
  | Cannot_unbox_or_untag_type Unboxed ->
      fprintf ppf "Don't know how to unbox this type. Only float, int32, \
                   int64 and nativeint can be unboxed"
  | Cannot_unbox_or_untag_type Untagged ->
      fprintf ppf "Don't know how to untag this type. Only int \
                   can be untagged"
  | Deep_unbox_or_untag_attribute kind ->
      fprintf ppf
        "The attribute '%s' should be attached to a direct argument or \
         result of the primitive, it should not occur deeply into its type"
        (match kind with Unboxed -> "@unboxed" | Untagged -> "@untagged")
  | Bad_immediate_attribute ->
      fprintf ppf "@[%s@ %s@]"
        "Types marked with the immediate attribute must be"
        "non-pointer types like int or bool"
  | Bad_unboxed_attribute msg ->
      fprintf ppf "@[This type cannot be unboxed because@ %s.@]" msg
  | Wrong_unboxed_type_float ->
      fprintf ppf "@[This type cannot be unboxed because@ \
                   it might contain both float and non-float values.@ \
                   You should annotate it with [%@%@ocaml.boxed].@]"
  | Boxed_and_unboxed ->
      fprintf ppf "@[A type cannot be boxed and unboxed at the same time.@]"
  | Nonrec_gadt ->
      fprintf ppf
        "@[GADT case syntax cannot be used in a 'nonrec' block.@]"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
