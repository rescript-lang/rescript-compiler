(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Inclusion checks for the core language *)

open Asttypes
open Path
open Types
open Typedtree

(* Inclusion between value descriptions *)

exception Dont_match

let value_descriptions env id vd1 vd2 =
  if Ctype.moregeneral env true vd1.val_type vd2.val_type then begin
    match (vd1.val_kind, vd2.val_kind) with
        (Val_prim p1, Val_prim p2) ->
          if p1 = p2 then Tcoerce_none else raise Dont_match
      | (Val_prim p, _) -> Tcoerce_primitive (id,p)
      | (_, Val_prim p) -> raise Dont_match
      | (_, _) -> Tcoerce_none
  end else
    raise Dont_match

(* Inclusion between "private" annotations *)

let private_flags decl1 decl2 =
  match decl1.type_private, decl2.type_private with
  | Private, Public ->
      decl2.type_kind = Type_abstract &&
      (decl2.type_manifest = None || decl1.type_kind <> Type_abstract)
  | _, _ -> true

(* Inclusion between manifest types (particularly for private row types) *)

let is_absrow env ty =
  match ty.desc with
    Tconstr(Pident id, _, _) ->
      begin match Ctype.expand_head env ty with
        {desc=Tobject _|Tvariant _} -> true
      | _ -> false
      end
  | _ -> false

let type_manifest env ty1 params1 ty2 params2 priv2 =
  let ty1' = Ctype.expand_head env ty1 and ty2' = Ctype.expand_head env ty2 in
  match ty1'.desc, ty2'.desc with
    Tvariant row1, Tvariant row2 when is_absrow env (Btype.row_more row2) ->
      let row1 = Btype.row_repr row1 and row2 = Btype.row_repr row2 in
      Ctype.equal env true (ty1::params1) (row2.row_more::params2) &&
      begin match row1.row_more with
        {desc=Tvar _|Tconstr _|Tnil} -> true
      | _ -> false
      end &&
      let r1, r2, pairs =
        Ctype.merge_row_fields row1.row_fields row2.row_fields in
      (not row2.row_closed ||
       row1.row_closed && Ctype.filter_row_fields false r1 = []) &&
      List.for_all
        (fun (_,f) -> match Btype.row_field_repr f with
          Rabsent | Reither _ -> true | Rpresent _ -> false)
        r2 &&
      let to_equal = ref (List.combine params1 params2) in
      List.for_all
        (fun (_, f1, f2) ->
          match Btype.row_field_repr f1, Btype.row_field_repr f2 with
            Rpresent(Some t1),
            (Rpresent(Some t2) | Reither(false, [t2], _, _)) ->
              to_equal := (t1,t2) :: !to_equal; true
          | Rpresent None, (Rpresent None | Reither(true, [], _, _)) -> true
          | Reither(c1,tl1,_,_), Reither(c2,tl2,_,_)
            when List.length tl1 = List.length tl2 && c1 = c2 ->
              to_equal := List.combine tl1 tl2 @ !to_equal; true
          | Rabsent, (Reither _ | Rabsent) -> true
          | _ -> false)
        pairs &&
      let tl1, tl2 = List.split !to_equal in
      Ctype.equal env true tl1 tl2
  | Tobject (fi1, _), Tobject (fi2, _)
    when is_absrow env (snd(Ctype.flatten_fields fi2)) ->
      let (fields2,rest2) = Ctype.flatten_fields fi2 in
      Ctype.equal env true (ty1::params1) (rest2::params2) &&
      let (fields1,rest1) = Ctype.flatten_fields fi1 in
      (match rest1 with {desc=Tnil|Tvar _|Tconstr _} -> true | _ -> false) &&
      let pairs, miss1, miss2 = Ctype.associate_fields fields1 fields2 in
      miss2 = [] &&
      let tl1, tl2 =
        List.split (List.map (fun (_,_,t1,_,t2) -> t1, t2) pairs) in
      Ctype.equal env true (params1 @ tl1) (params2 @ tl2)
  | _ ->
      let rec check_super ty1 =
        Ctype.equal env true (ty1 :: params1) (ty2 :: params2) ||
        priv2 = Private &&
        try check_super
              (Ctype.try_expand_once_opt env (Ctype.expand_head env ty1))
        with Ctype.Cannot_expand -> false
      in check_super ty1

(* Inclusion between type declarations *)

type type_mismatch =
    Arity
  | Privacy
  | Kind
  | Constraint
  | Manifest
  | Variance
  | Field_type of Ident.t
  | Field_mutable of Ident.t
  | Field_arity of Ident.t
  | Field_names of int * Ident.t * Ident.t
  | Field_missing of bool * Ident.t
  | Record_representation of bool

let report_type_mismatch0 first second decl ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match err with
    Arity -> pr "They have different arities"
  | Privacy -> pr "A private type would be revealed"
  | Kind -> pr "Their kinds differ"
  | Constraint -> pr "Their constraints differ"
  | Manifest -> ()
  | Variance -> pr "Their variances do not agree"
  | Field_type s ->
      pr "The types for field %s are not equal" (Ident.name s)
  | Field_mutable s ->
      pr "The mutability of field %s is different" (Ident.name s)
  | Field_arity s ->
      pr "The arities for field %s differ" (Ident.name s)
  | Field_names (n, name1, name2) ->
      pr "Fields number %i have different names, %s and %s"
        n (Ident.name name1) (Ident.name name2)
  | Field_missing (b, s) ->
      pr "The field %s is only present in %s %s"
        (Ident.name s) (if b then second else first) decl
  | Record_representation b ->
      pr "Their internal representations differ:@ %s %s %s"
        (if b then second else first) decl
        "uses unboxed float representation"

let report_type_mismatch first second decl ppf =
  List.iter
    (fun err ->
      if err = Manifest then () else
      Format.fprintf ppf "@ %a." (report_type_mismatch0 first second decl) err)

let rec compare_variants env decl1 decl2 n cstrs1 cstrs2 =
  match cstrs1, cstrs2 with
    [], []           -> []
  | [], c::_ -> [Field_missing (true, c.Types.cd_id)]
  | c::_, [] -> [Field_missing (false, c.Types.cd_id)]
  | {Types.cd_id=cstr1; cd_args=arg1; cd_res=ret1}::rem1,
    {Types.cd_id=cstr2; cd_args=arg2; cd_res=ret2}::rem2 ->
      if Ident.name cstr1 <> Ident.name cstr2 then
        [Field_names (n, cstr1, cstr2)]
      else if List.length arg1 <> List.length arg2 then
        [Field_arity cstr1]
      else match ret1, ret2 with
      | Some r1, Some r2 when not (Ctype.equal env true [r1] [r2]) ->
          [Field_type cstr1]
      | Some _, None | None, Some _ ->
          [Field_type cstr1]
      | _ ->
          if Misc.for_all2
              (fun ty1 ty2 ->
                Ctype.equal env true (ty1::decl1.type_params)
                  (ty2::decl2.type_params))
              (arg1) (arg2)
          then
            compare_variants env decl1 decl2 (n+1) rem1 rem2
          else [Field_type cstr1]


let rec compare_records env decl1 decl2 n labels1 labels2 =
  match labels1, labels2 with
    [], []           -> []
  | [], l::_ -> [Field_missing (true, l.ld_id)]
  | l::_, [] -> [Field_missing (false, l.ld_id)]
  | {Types.ld_id=lab1; ld_mutable=mut1; ld_type=arg1}::rem1,
    {Types.ld_id=lab2; ld_mutable=mut2; ld_type=arg2}::rem2 ->
      if Ident.name lab1 <> Ident.name lab2
      then [Field_names (n, lab1, lab2)]
      else if mut1 <> mut2 then [Field_mutable lab1] else
      if Ctype.equal env true (arg1::decl1.type_params)
                              (arg2::decl2.type_params)
      then compare_records env decl1 decl2 (n+1) rem1 rem2
      else [Field_type lab1]

let type_declarations ?(equality = false) env name decl1 id decl2 =
  if decl1.type_arity <> decl2.type_arity then [Arity] else
  if not (private_flags decl1 decl2) then [Privacy] else
  let err = match (decl1.type_kind, decl2.type_kind) with
      (_, Type_abstract) -> []
    | (Type_variant cstrs1, Type_variant cstrs2) ->
        let mark cstrs usage name decl =
          List.iter
            (fun c ->
              Env.mark_constructor_used usage env name decl
                                        (Ident.name c.Types.cd_id))
            cstrs
        in
        let usage =
          if decl1.type_private = Private || decl2.type_private = Public
          then Env.Positive else Env.Privatize
        in
        mark cstrs1 usage name decl1;
        if equality then mark cstrs2 Env.Positive (Ident.name id) decl2;
        compare_variants env decl1 decl2 1 cstrs1 cstrs2
    | (Type_record(labels1,rep1), Type_record(labels2,rep2)) ->
        let err = compare_records env decl1 decl2 1 labels1 labels2 in
        if err <> [] || rep1 = rep2 then err else
        [Record_representation (rep2 = Record_float)]
    | (Type_open, Type_open) -> []
    | (_, _) -> [Kind]
  in
  if err <> [] then err else
  let err = match (decl1.type_manifest, decl2.type_manifest) with
      (_, None) ->
        if Ctype.equal env true decl1.type_params decl2.type_params
        then [] else [Constraint]
    | (Some ty1, Some ty2) ->
        if type_manifest env ty1 decl1.type_params ty2 decl2.type_params
            decl2.type_private
        then [] else [Manifest]
    | (None, Some ty2) ->
        let ty1 =
          Btype.newgenty (Tconstr(Pident id, decl2.type_params, ref Mnil))
        in
        if Ctype.equal env true decl1.type_params decl2.type_params then
          if Ctype.equal env false [ty1] [ty2] then []
          else [Manifest]
        else [Constraint]
  in
  if err <> [] then err else
  let abstr =
    decl2.type_private = Private ||
    decl2.type_kind = Type_abstract && decl2.type_manifest = None in
  let opn = decl2.type_kind = Type_open && decl2.type_manifest = None in
  let constrained ty = not (Btype.(is_Tvar (repr ty))) in
  if List.for_all2
      (fun ty (v1,v2) ->
        let open Variance in
        let imp a b = not a || b in
        let (co1,cn1) = get_upper v1 and (co2,cn2) = get_upper v2 in
        (if abstr then (imp co1 co2 && imp cn1 cn2)
         else if opn || constrained ty then (co1 = co2 && cn1 = cn2)
         else true) &&
        let (p1,n1,i1,j1) = get_lower v1 and (p2,n2,i2,j2) = get_lower v2 in
        imp abstr (imp p2 p1 && imp n2 n1 && imp i2 i1 && imp j2 j1))
      decl2.type_params (List.combine decl1.type_variance decl2.type_variance)
  then [] else [Variance]

(* Inclusion between extension constructors *)

let extension_constructors env id ext1 ext2 =
  let usage =
    if ext1.ext_private = Private || ext2.ext_private = Public
    then Env.Positive else Env.Privatize
  in
  Env.mark_extension_used usage env ext1 (Ident.name id);
  let ty1 =
    Btype.newgenty (Tconstr(ext1.ext_type_path, ext1.ext_type_params, ref Mnil))
  in
  let ty2 =
    Btype.newgenty (Tconstr(ext2.ext_type_path, ext2.ext_type_params, ref Mnil))
  in
  if Ctype.equal env true
       (ty1 :: ext1.ext_type_params)
       (ty2 :: ext2.ext_type_params)
  then
    if List.length ext1.ext_args = List.length ext2.ext_args then
      if match ext1.ext_ret_type, ext2.ext_ret_type with
          Some r1, Some r2 when not (Ctype.equal env true [r1] [r2]) -> false
        | Some _, None | None, Some _ -> false
        | _ ->
            Misc.for_all2
              (fun ty1 ty2 ->
                Ctype.equal env true
                  (ty1 :: ext1.ext_type_params)
                  (ty2 :: ext2.ext_type_params))
              ext1.ext_args ext2.ext_args
      then
        match ext1.ext_private, ext2.ext_private with
            Private, Public -> false
          | _, _ -> true
      else false
    else false
  else false

(* Inclusion between class types *)
let encode_val (mut, ty) rem =
  begin match mut with
    Asttypes.Mutable   -> Predef.type_unit
  | Asttypes.Immutable -> Btype.newgenvar ()
  end
  ::ty::rem

let meths meths1 meths2 =
  Meths.fold
    (fun nam t2 (ml1, ml2) ->
       (begin try
          Meths.find nam meths1 :: ml1
        with Not_found ->
          ml1
        end,
        t2 :: ml2))
    meths2 ([], [])

let vars vars1 vars2 =
  Vars.fold
    (fun lab v2 (vl1, vl2) ->
       (begin try
          encode_val (Vars.find lab vars1) vl1
        with Not_found ->
          vl1
        end,
        encode_val v2 vl2))
    vars2 ([], [])
