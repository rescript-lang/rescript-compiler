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

(* Inclusion checks for the core language *)

open Asttypes
open Path
open Types
open Typedtree

(* Inclusion between value descriptions *)

exception Dont_match

let value_descriptions ~loc env name
    (vd1 : Types.value_description)
    (vd2 : Types.value_description) =
  Builtin_attributes.check_deprecated_inclusion
    ~def:vd1.val_loc
    ~use:vd2.val_loc
    loc
    vd1.val_attributes vd2.val_attributes
    (Ident.name name);
  if Ctype.moregeneral env true vd1.val_type vd2.val_type then begin
    match (vd1.val_kind, vd2.val_kind) with
        (Val_prim p1, Val_prim p2) ->
          if !Primitive.coerce p1 p2 then Tcoerce_none else raise Dont_match
      | (Val_prim p, _) ->
          let pc = {pc_desc = p; pc_type = vd2.Types.val_type;
                  pc_env = env; pc_loc = vd1.Types.val_loc;
                      pc_id = name;
                   } in
          Tcoerce_primitive pc
      | (_, Val_prim _) -> raise Dont_match
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
    Tconstr(Pident _, _, _) ->
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
      let pairs, _miss1, miss2 = Ctype.associate_fields fields1 fields2 in
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
    Arity of {left_arity: int; right_arity: int}
  | Privacy
  | Kind
  | Constraint
  | Manifest
  | Variance
  | Field_type of {field_name: Ident.t; left_field: Types.label_declaration; right_field: Types.label_declaration}
  | Constructor_type of Ident.t
  | Field_mutable of {field_name: Ident.t; left_is_mutable: bool}
  | Field_arity of Ident.t
  | Field_names of int * string * string
  | Field_missing of bool * Ident.t
  | Record_representation of record_representation * record_representation
  | Unboxed_representation of bool  (* true means second one is unboxed *)
  | Immediate
  | Tag_name of {left_value: string option; right_value: string option} 
  | Variant_representation of Ident.t

let report_type_mismatch0 first second decl ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match err with
    Arity {left_arity; right_arity} -> pr "It has @{<error>%i@} type parameters in the implementation and @{<info>%i@} in the interface." left_arity right_arity
  | Privacy -> pr "A private type would be revealed"
  | Kind -> pr "Their kinds differ"
  | Constraint -> pr "Their constraints differ"
  | Manifest -> ()
  | Variance -> pr "Their variances do not agree"
  | Constructor_type field_name ->
      pr "The types for field %s are not equal" (Ident.name field_name)
  | Field_type {field_name; left_field; right_field} ->
      pr "Field @{<info>%s@} has type @{<error>%a@} in the implementation, but should have type @{<info>%a@} according to the interface" 
      (Ident.name field_name)
      Printtyp.type_expr left_field.ld_type
      Printtyp.type_expr right_field.ld_type
  | Field_mutable {field_name; left_is_mutable} ->
      pr "Field @{<info>%s@} is @{<error>%s@} in the implementation, but @{<info>%s@} in the interface" 
      (Ident.name field_name)
      (if left_is_mutable then "mutable" else "immutable")
      (if left_is_mutable then "immutable" else "mutable")
  | Field_arity s ->
      pr "The arities for field %s differ" (Ident.name s)
  | Field_names (n, name1, name2) ->
      pr "Field number @{<info>%i@} is named @{<error>%s@} in the implementation but @{<info>%s@} in the interface. Fields must be ordered the same in interface and implementation."
        n name1 name2
  | Field_missing (b, s) ->
      pr "Field @{<info>%s@} is only present in %s %s"
        (Ident.name s) (if b then second else first) decl
  | Record_representation (rep1, rep2) ->
      let default () = pr "Their internal representations differ" in
      ( match rep1, rep2 with
        | Record_optional_labels lbls1, Record_optional_labels lbls2 ->
          let only_in_lhs =
            Ext_list.find_first lbls1 (fun l -> not (Ext_list.mem_string lbls2 l)) in
          let only_in_rhs =
            Ext_list.find_first lbls2 (fun l -> not (Ext_list.mem_string lbls1 l)) in
          (match only_in_lhs, only_in_rhs with
            | Some l, _ ->
              pr "@optional label %s only in %s" l second
            | _, Some l ->
              pr "@optional label %s only in %s" l first
            | None, None -> default ())
        | _ ->
          default ()
      )
  | Unboxed_representation b ->
      pr "Their internal representations differ: %s %s %s"
         (if b then second else first) decl
         "uses unboxed representation"
  | Immediate -> pr "%s is not an immediate type" first
  | Tag_name {left_value; right_value} -> 
    pr "@{<info>@tag@} annotations differ. It is @{<error>%s@} in the implementation but @{<info>%s@} in the interface." 
    (match left_value with None -> "not set" | Some s -> "\"" ^ s ^ "\"")
    (match right_value with None -> "not set" | Some s -> "\"" ^ s ^ "\"")
  | Variant_representation s ->
    pr "The internal representations for case %s are not equal" (Ident.name s)

let report_type_mismatch first second decl ppf =
  List.iter
    (fun err ->
      if err = Manifest then () else
      Format.fprintf ppf "@ - %a" (report_type_mismatch0 first second decl) err)

let rec compare_constructor_arguments ~loc env cstr params1 params2 arg1 arg2 =
  match arg1, arg2 with
  | Types.Cstr_tuple arg1, Types.Cstr_tuple arg2 ->
      if List.length arg1 <> List.length arg2 then [Field_arity cstr]
      else if
        (* Ctype.equal must be called on all arguments at once, cf. PR#7378 *)
        Ctype.equal env true (params1 @ arg1) (params2 @ arg2)
      then [] else [Constructor_type cstr]
  | Types.Cstr_record l1, Types.Cstr_record l2 ->
      compare_records env ~loc params1 params2 0 l1 l2
  | _ -> [Constructor_type cstr]

and compare_variants ~loc env params1 params2 n
    (cstrs1 : Types.constructor_declaration list)
    (cstrs2 : Types.constructor_declaration list) =
  match cstrs1, cstrs2 with
    [], []           -> []
  | [], c::_ -> [Field_missing (true, c.Types.cd_id)]
  | c::_, [] -> [Field_missing (false, c.Types.cd_id)]
  | cd1::rem1, cd2::rem2 ->
      if Ident.name cd1.cd_id <> Ident.name cd2.cd_id then
        [Field_names (n, cd1.cd_id.name, cd2.cd_id.name)]
      else begin
        Builtin_attributes.check_deprecated_inclusion
          ~def:cd1.cd_loc
          ~use:cd2.cd_loc
          loc
          cd1.cd_attributes cd2.cd_attributes
          (Ident.name cd1.cd_id);
        let r =
          match cd1.cd_res, cd2.cd_res with
          | Some r1, Some r2 ->
              if Ctype.equal env true [r1] [r2] then
                compare_constructor_arguments ~loc env cd1.cd_id [r1] [r2]
                  cd1.cd_args cd2.cd_args
              else [Constructor_type cd1.cd_id]
          | Some _, None | None, Some _ ->
              [Constructor_type cd1.cd_id]
          | _ ->
              compare_constructor_arguments ~loc env cd1.cd_id
                params1 params2 cd1.cd_args cd2.cd_args
        in
        let r =
          if r <> [] then r
          else match Ast_untagged_variants.is_nullary_variant cd1.cd_args with
          | true ->
            let tag_type1 = Ast_untagged_variants.process_tag_type cd1.cd_attributes in
            let tag_type2 = Ast_untagged_variants.process_tag_type cd2.cd_attributes in
            if tag_type1 <> tag_type2 then [Variant_representation cd1.cd_id]
            else []
          | false ->
            r
        in
        if r <> [] then r
        else compare_variants ~loc env params1 params2 (n+1) rem1 rem2
      end

and compare_records ~loc env params1_ params2_ n_
    (labels1_ : Types.label_declaration list)
    (labels2_ : Types.label_declaration list) =
  (* First try a fast path that checks if all the fields at once are consistent.
     When that fails, try a slow path that blames the first inconsistent field *)
  let rec aux ~fast params1 params2 n labels1 labels2 =
    match labels1, labels2 with
      [], [] ->
        if fast then
          if Ctype.equal env true params1 params2 then
            []
          else
            aux ~fast:false params1_ params2_ n_ labels1_ labels2_
        else
          []
    | [], l::_ -> [Field_missing (true, l.Types.ld_id)]
    | l::_, [] -> [Field_missing (false, l.Types.ld_id)]
    | ld1::rem1, ld2::rem2 ->
        if Ident.name ld1.ld_id <> Ident.name ld2.ld_id
        then [Field_names (n, ld1.ld_id.name, ld2.ld_id.name)]
        else if ld1.ld_mutable <> ld2.ld_mutable then [Field_mutable {field_name=ld1.ld_id; left_is_mutable = ld1.ld_mutable = Mutable}] else begin
          Builtin_attributes.check_deprecated_mutable_inclusion
            ~def:ld1.ld_loc
            ~use:ld2.ld_loc
            loc
            ld1.ld_attributes ld2.ld_attributes
            (Ident.name ld1.ld_id);
          let field_mismatch = !Builtin_attributes.check_bs_attributes_inclusion  
            ld1.ld_attributes ld2.ld_attributes
            (Ident.name ld1.ld_id) in 
          match field_mismatch with
          | Some (a,b) -> [Field_names (n,a,b)]
          | None ->
          let current_field_consistent =
            if fast then true
            else Ctype.equal env true (ld1.ld_type::params1)(ld2.ld_type::params2) in
          if current_field_consistent
          then (* add arguments to the parameters, cf. PR#7378 *)
            aux ~fast
              (ld1.ld_type::params1) (ld2.ld_type::params2)
              (n+1)
              rem1 rem2
          else
            [Field_type {field_name=ld1.ld_id; left_field=ld1; right_field=ld2}]
      end in
  aux ~fast:true params1_ params2_ n_ labels1_ labels2_


let type_declarations ?(equality = false) ~loc env name decl1 id decl2 =
  Builtin_attributes.check_deprecated_inclusion
    ~def:decl1.type_loc
    ~use:decl2.type_loc
    loc
    decl1.type_attributes decl2.type_attributes
    name;
  if decl1.type_arity <> decl2.type_arity then [Arity {left_arity = decl1.type_arity; right_arity = decl2.type_arity}] else
  if not (private_flags decl1 decl2) then [Privacy] else
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
  let err =
    let untagged1 = Ast_untagged_variants.process_untagged decl1.type_attributes in
    let untagged2 = Ast_untagged_variants.process_untagged decl2.type_attributes in  
    match (decl2.type_kind, decl1.type_unboxed.unboxed || untagged1,
           decl2.type_unboxed.unboxed || untagged2) with
    | Type_abstract, _, _ -> []
    | _, true, false -> [Unboxed_representation false]
    | _, false, true -> [Unboxed_representation true]
    | _ -> []
  in
  if err <> [] then err else
  let err =
    let tag1 = Ast_untagged_variants.process_tag_name decl1.type_attributes in
    let tag2 = Ast_untagged_variants.process_tag_name decl2.type_attributes in
    if tag1 <> tag2 then [Tag_name {left_value = tag1; right_value = tag2}] else err in
  if err <> [] then err else
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
        compare_variants ~loc env decl1.type_params decl2.type_params 1 cstrs1 cstrs2
    | (Type_record(labels1,rep1), Type_record(labels2,rep2)) ->
        let err = compare_records ~loc env decl1.type_params decl2.type_params 1 labels1 labels2 in
        if err <> [] || rep1 = rep2 then err else
        [Record_representation (rep1, rep2)]
    | (Type_open, Type_open) -> []
    | (_, _) -> [Kind]
  in
  if err <> [] then err else
  let abstr = decl2.type_kind = Type_abstract && decl2.type_manifest = None in
  (* If attempt to assign a non-immediate type (e.g. string) to a type that
   * must be immediate, then we error *)
  let err =
    if abstr &&
       not decl1.type_immediate &&
       decl2.type_immediate then
      [Immediate]
    else []
  in
  if err <> [] then err else
  let need_variance =
    abstr || decl1.type_private = Private || decl1.type_kind = Type_open in
  if not need_variance then [] else
  let abstr = abstr || decl2.type_private = Private in
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

let extension_constructors ~loc env id ext1 ext2 =
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
    if compare_constructor_arguments ~loc env (Ident.create "")
        ext1.ext_type_params ext2.ext_type_params
        ext1.ext_args ext2.ext_args = [] then
      if match ext1.ext_ret_type, ext2.ext_ret_type with
          Some r1, Some r2 when not (Ctype.equal env true [r1] [r2]) -> false
        | Some _, None | None, Some _ -> false
        | _ -> true
      then
        match ext1.ext_private, ext2.ext_private with
            Private, Public -> false
          | _, _ -> true
      else false
    else false
  else false
