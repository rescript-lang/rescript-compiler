(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(* Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt*)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Operations on core types *)

open Misc
open Asttypes
open Types
open Btype

(*
   Type manipulation after type inference
   ======================================
   If one wants to manipulate a type after type inference (for
   instance, during code generation or in the debugger), one must
   first make sure that the type levels are correct, using the
   function [correct_levels]. Then, this type can be correctely
   manipulated by [apply], [expand_head] and [moregeneral].
*)

(*
   General notes
   =============
   - As much sharing as possible should be kept : it makes types
     smaller and better abbreviated.
     When necessary, some sharing can be lost. Types will still be
     printed correctly (+++ TO DO...), and abbreviations defined by a
     class do not depend on sharing thanks to constrained
     abbreviations. (Of course, even if some sharing is lost, typing
     will still be correct.)
   - All nodes of a type have a level : that way, one know whether a
     node need to be duplicated or not when instantiating a type.
   - Levels of a type are decreasing (generic level being considered
     as greatest).
   - The level of a type constructor is superior to the binding
     time of its path.
   - Recursive types without limitation should be handled (even if
     there is still an occur check). This avoid treating specially the
     case for objects, for instance. Furthermore, the occur check
     policy can then be easily changed.
*)

(*
   A faire
   =======
   - Revoir affichage des types.
   - Etendre la portee d'un alias [... as 'a] a tout le type englobant.
   - #-type implementes comme de vraies abreviations.
   - Niveaux plus fins pour les identificateurs :
       Champ [global] renomme en [level];
       Niveau -1 : global
               0 : module toplevel
               1 : module contenu dans module toplevel
              ...
     En fait, incrementer le niveau a chaque fois que l'on rentre dans
     un module.

       3   4 6
        \ / /
       1 2 5
        \|/
         0

     [Subst] doit ecreter les niveaux (pour qu'un variable non
     generalisable dans un module de niveau 2 ne se retrouve pas
     generalisable lorsque l'on l'utilise au niveau 0).

   - Traitement de la trace de l'unification separe de la fonction
     [unify].
*)

(**** Errors ****)

exception Unify of (type_expr * type_expr) list

exception Tags of label * label

let () =
  Location.register_error_of_exn
    (function
      | Tags (l, l') ->
          Some
            Location.
              (errorf ~loc:(in_file !input_name)
                 "In this program,@ variant constructors@ `%s and `%s@ \
                  have the same hash value.@ Change one of them." l l'
              )
      | _ -> None
    )

exception Subtype of
        (type_expr * type_expr) list * (type_expr * type_expr) list

exception Cannot_expand

exception Cannot_apply

exception Recursive_abbrev

(* GADT: recursive abbrevs can appear as a result of local constraints *)
exception Unification_recursive_abbrev of (type_expr * type_expr) list

(**** Type level management ****)

let current_level = ref 0
let nongen_level = ref 0
let global_level = ref 1
let saved_level = ref []

let get_current_level () = !current_level
let init_def level = current_level := level; nongen_level := level
let begin_def () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  incr current_level; nongen_level := !current_level
let begin_class_def () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  incr current_level
let raise_nongen_level () =
  saved_level := (!current_level, !nongen_level) :: !saved_level;
  nongen_level := !current_level
let end_def () =
  let (cl, nl) = List.hd !saved_level in
  saved_level := List.tl !saved_level;
  current_level := cl; nongen_level := nl

let reset_global_level () =
  global_level := !current_level + 1
let increase_global_level () =
  let gl = !global_level in
  global_level := !current_level;
  gl
let restore_global_level gl =
  global_level := gl

(**** Whether a path points to an object type (with hidden row variable) ****)
let is_object_type path =
  let name =
    match path with Path.Pident id -> Ident.name id
    | Path.Pdot(_, s,_) -> s
    | Path.Papply _ -> assert false
  in name.[0] = '#'

(**** Control tracing of GADT instances *)

let trace_gadt_instances = ref false
let check_trace_gadt_instances env =
  not !trace_gadt_instances && Env.has_local_constraints env &&
  (trace_gadt_instances := true; cleanup_abbrev (); true)

let reset_trace_gadt_instances b =
  if b then trace_gadt_instances := false

let wrap_trace_gadt_instances env f x =
  let b = check_trace_gadt_instances env in
  let y = f x in
  reset_trace_gadt_instances b;
  y

(**** Abbreviations without parameters ****)
(* Shall reset after generalizing *)

let simple_abbrevs = ref Mnil

let proper_abbrevs path tl abbrev =
  if tl <> [] || !trace_gadt_instances || !Clflags.principal ||
     is_object_type path
  then abbrev
  else simple_abbrevs

(**** Some type creators ****)

(* Re-export generic type creators *)

let newty2             = Btype.newty2
let newty desc         = newty2 !current_level desc
let new_global_ty desc = newty2 !global_level desc

let newvar ?name ()         = newty2 !current_level (Tvar name)
let newvar2 ?name level     = newty2 level (Tvar name)
let new_global_var ?name () = newty2 !global_level (Tvar name)

let newobj fields      = newty (Tobject (fields, ref None))

let newconstr path tyl = newty (Tconstr (path, tyl, ref Mnil))

let none = newty (Ttuple [])                (* Clearly ill-formed type *)

(**** Representative of a type ****)

(* Re-export repr *)
let repr = repr

(**** Type maps ****)

module TypePairs =
  Hashtbl.Make (struct
    type t = type_expr * type_expr
    let equal (t1, t1') (t2, t2') = (t1 == t2) && (t1' == t2')
    let hash (t, t') = t.id + 93 * t'.id
 end)


(**** unification mode ****)

type unification_mode =
  | Expression (* unification in expression *)
  | Pattern (* unification in pattern which may add local constraints *)

let umode = ref Expression
let generate_equations = ref false
let assume_injective = ref false

let set_mode_expression f =
  let old_unification_mode = !umode in
  try
    umode := Expression;
    let ret = f () in
    umode := old_unification_mode;
    ret
  with e ->
    umode := old_unification_mode;
    raise e

let set_mode_pattern ~generate ~injective f =
  let old_unification_mode = !umode
  and old_gen = !generate_equations
  and old_inj = !assume_injective in
  try
    umode := Pattern;
    generate_equations := generate;
    assume_injective := injective;
    let ret = f () in
    umode := old_unification_mode;
    generate_equations := old_gen;
    assume_injective := old_inj;
    ret
  with e ->
    umode := old_unification_mode;
    generate_equations := old_gen;
    assume_injective := old_inj;
    raise e

(*** Checks for type definitions ***)

let in_current_module = function
  | Path.Pident _ -> true
  | Path.Pdot _ | Path.Papply _ -> false

let in_pervasives p =
  in_current_module p &&
  try ignore (Env.find_type p Env.initial_safe_string); true
  with Not_found -> false

let is_datatype decl=
  match decl.type_kind with
    Type_record _ | Type_variant _ | Type_open -> true
  | Type_abstract -> false


                  (**********************************************)
                  (*  Miscellaneous operations on object types  *)
                  (**********************************************)

(* Note:
   We need to maintain some invariants:
   * cty_self must be a Tobject
   * ...
*)

(**** Object field manipulation. ****)

let object_fields ty =
  match (repr ty).desc with
    Tobject (fields, _) -> fields
  | _                   -> assert false

let flatten_fields ty =
  let rec flatten l ty =
    let ty = repr ty in
    match ty.desc with
      Tfield(s, k, ty1, ty2) ->
        flatten ((s, k, ty1)::l) ty2
    | _ ->
        (l, ty)
  in
    let (l, r) = flatten [] ty in
    (List.sort (fun (n, _, _) (n', _, _) -> compare n n') l, r)

let build_fields level =
  List.fold_right
    (fun (s, k, ty1) ty2 -> newty2 level (Tfield(s, k, ty1, ty2)))

let associate_fields fields1 fields2 =
  let rec associate p s s' =
    function
      (l, []) ->
        (List.rev p, (List.rev s) @ l, List.rev s')
    | ([], l') ->
        (List.rev p, List.rev s, (List.rev s') @ l')
    | ((n, k, t)::r, (n', k', t')::r') when n = n' ->
        associate ((n, k, t, k', t')::p) s s' (r, r')
    | ((n, k, t)::r, ((n', k', t')::_ as l')) when n < n' ->
        associate p ((n, k, t)::s) s' (r, l')
    | (((n, k, t)::r as l), (n', k', t')::r') (* when n > n' *) ->
        associate p s ((n', k', t')::s') (l, r')
  in
  associate [] [] [] (fields1, fields2)

(**** Check whether an object is open ****)

(* +++ Il faudra penser a eventuellement expanser l'abreviation *)
let rec object_row ty =
  let ty = repr ty in
  match ty.desc with
    Tobject (t, _)     -> object_row t
  | Tfield(_, _, _, t) -> object_row t
  | _ -> ty

let opened_object ty =
  match (object_row ty).desc with
  | Tvar _  | Tunivar _ | Tconstr _ -> true
  | _                               -> false

let concrete_object ty =
  match (object_row ty).desc with
  | Tvar _             -> false
  | _                  -> true

(**** Close an object ****)

let close_object ty =
  let rec close ty =
    let ty = repr ty in
    match ty.desc with
      Tvar _ ->
        link_type ty (newty2 ty.level Tnil)
    | Tfield(_, _, _, ty') -> close ty'
    | _                    -> assert false
  in
  match (repr ty).desc with
    Tobject (ty, _)   -> close ty
  | _                 -> assert false

(**** Row variable of an object type ****)

let row_variable ty =
  let rec find ty =
    let ty = repr ty in
    match ty.desc with
      Tfield (_, _, _, ty) -> find ty
    | Tvar _               -> ty
    | _                    -> assert false
  in
  match (repr ty).desc with
    Tobject (fi, _) -> find fi
  | _               -> assert false

(**** Object name manipulation ****)
(* +++ Bientot obsolete *)

let set_object_name id rv params ty =
  match (repr ty).desc with
    Tobject (fi, nm) ->
      set_name nm (Some (Path.Pident id, rv::params))
  | _ ->
      assert false

let remove_object_name ty =
  match (repr ty).desc with
    Tobject (_, nm)   -> set_name nm None
  | Tconstr (_, _, _) -> ()
  | _                 -> fatal_error "Ctype.remove_object_name"

(**** Hiding of private methods ****)

let hide_private_methods ty =
  match (repr ty).desc with
    Tobject (fi, nm) ->
      nm := None;
      let (fl, _) = flatten_fields fi in
      List.iter
        (function (_, k, _) ->
          match field_kind_repr k with
            Fvar r -> set_kind r Fabsent
          | _      -> ())
        fl
  | _ ->
      assert false


                              (*******************************)
                              (*  Operations on class types  *)
                              (*******************************)


let rec signature_of_class_type =
  function
    Cty_constr (_, _, cty) -> signature_of_class_type cty
  | Cty_signature sign     -> sign
  | Cty_arrow (_, ty, cty)   -> signature_of_class_type cty

let self_type cty =
  repr (signature_of_class_type cty).csig_self

let rec class_type_arity =
  function
    Cty_constr (_, _, cty) ->  class_type_arity cty
  | Cty_signature _        ->  0
  | Cty_arrow (_, _, cty)    ->  1 + class_type_arity cty


                  (*******************************************)
                  (*  Miscellaneous operations on row types  *)
                  (*******************************************)

let sort_row_fields = List.sort (fun (p,_) (q,_) -> compare p q)

let rec merge_rf r1 r2 pairs fi1 fi2 =
  match fi1, fi2 with
    (l1,f1 as p1)::fi1', (l2,f2 as p2)::fi2' ->
      if l1 = l2 then merge_rf r1 r2 ((l1,f1,f2)::pairs) fi1' fi2' else
      if l1 < l2 then merge_rf (p1::r1) r2 pairs fi1' fi2 else
      merge_rf r1 (p2::r2) pairs fi1 fi2'
  | [], _ -> (List.rev r1, List.rev_append r2 fi2, pairs)
  | _, [] -> (List.rev_append r1 fi1, List.rev r2, pairs)

let merge_row_fields fi1 fi2 =
  match fi1, fi2 with
    [], _ | _, [] -> (fi1, fi2, [])
  | [p1], _ when not (List.mem_assoc (fst p1) fi2) -> (fi1, fi2, [])
  | _, [p2] when not (List.mem_assoc (fst p2) fi1) -> (fi1, fi2, [])
  | _ -> merge_rf [] [] [] (sort_row_fields fi1) (sort_row_fields fi2)

let rec filter_row_fields erase = function
    [] -> []
  | (l,f as p)::fi ->
      let fi = filter_row_fields erase fi in
      match row_field_repr f with
        Rabsent -> fi
      | Reither(_,_,false,e) when erase -> set_row_field e Rabsent; fi
      | _ -> p :: fi

                    (**************************************)
                    (*  Check genericity of type schemes  *)
                    (**************************************)


exception Non_closed0

let rec closed_schema_rec ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    let level = ty.level in
    ty.level <- pivot_level - level;
    match ty.desc with
      Tvar _ when level <> generic_level ->
        raise Non_closed0
    | Tfield(_, kind, t1, t2) ->
        if field_kind_repr kind = Fpresent then
          closed_schema_rec t1;
        closed_schema_rec t2
    | Tvariant row ->
        let row = row_repr row in
        iter_row closed_schema_rec row;
        if not (static_row row) then closed_schema_rec row.row_more
    | _ ->
        iter_type_expr closed_schema_rec ty
  end

(* Return whether all variables of type [ty] are generic. *)
let closed_schema ty =
  try
    closed_schema_rec ty;
    unmark_type ty;
    true
  with Non_closed0 ->
    unmark_type ty;
    false

exception Non_closed of type_expr * bool

let free_variables = ref []
let really_closed = ref None

let rec free_vars_rec real ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
    begin match ty.desc, !really_closed with
      Tvar _, _ ->
        free_variables := (ty, real) :: !free_variables
    | Tconstr (path, tl, _), Some env ->
        begin try
          let (_, body, _) = Env.find_type_expansion path env in
          if (repr body).level <> generic_level then
            free_variables := (ty, real) :: !free_variables
        with Not_found -> ()
        end;
        List.iter (free_vars_rec true) tl
(* Do not count "virtual" free variables
    | Tobject(ty, {contents = Some (_, p)}) ->
        free_vars_rec false ty; List.iter (free_vars_rec true) p
*)
    | Tobject (ty, _), _ ->
        free_vars_rec false ty
    | Tfield (_, _, ty1, ty2), _ ->
        free_vars_rec true ty1; free_vars_rec false ty2
    | Tvariant row, _ ->
        let row = row_repr row in
        iter_row (free_vars_rec true) row;
        if not (static_row row) then free_vars_rec false row.row_more
    | _    ->
        iter_type_expr (free_vars_rec true) ty
    end;
  end

let free_vars ?env ty =
  free_variables := [];
  really_closed := env;
  free_vars_rec true ty;
  let res = !free_variables in
  free_variables := [];
  really_closed := None;
  res

let free_variables ?env ty =
  let tl = List.map fst (free_vars ?env ty) in
  unmark_type ty;
  tl

let closed_type ty =
  match free_vars ty with
      []           -> ()
  | (v, real) :: _ -> raise (Non_closed (v, real))

let closed_parameterized_type params ty =
  List.iter mark_type params;
  let ok =
    try closed_type ty; true with Non_closed _ -> false in
  List.iter unmark_type params;
  unmark_type ty;
  ok

let closed_type_decl decl =
  try
    List.iter mark_type decl.type_params;
    begin match decl.type_kind with
      Type_abstract ->
        ()
    | Type_variant v ->
        List.iter
          (fun {cd_args; cd_res; _} ->
            match cd_res with
            | Some _ -> ()
            | None -> List.iter closed_type cd_args)
          v
    | Type_record(r, rep) ->
        List.iter (fun l -> closed_type l.ld_type) r
    | Type_open -> ()
    end;
    begin match decl.type_manifest with
      None    -> ()
    | Some ty -> closed_type ty
    end;
    unmark_type_decl decl;
    None
  with Non_closed (ty, _) ->
    unmark_type_decl decl;
    Some ty

let closed_extension_constructor ext =
  try
    List.iter mark_type ext.ext_type_params;
    begin match ext.ext_ret_type with
    | Some _ -> ()
    | None -> List.iter closed_type ext.ext_args
    end;
    unmark_extension_constructor ext;
    None
  with Non_closed (ty, _) ->
    unmark_extension_constructor ext;
    Some ty

type closed_class_failure =
    CC_Method of type_expr * bool * string * type_expr
  | CC_Value of type_expr * bool * string * type_expr

exception CCFailure of closed_class_failure

let closed_class params sign =
  let ty = object_fields (repr sign.csig_self) in
  let (fields, rest) = flatten_fields ty in
  List.iter mark_type params;
  mark_type rest;
  List.iter
    (fun (lab, _, ty) -> if lab = dummy_method then mark_type ty)
    fields;
  try
    mark_type_node (repr sign.csig_self);
    List.iter
      (fun (lab, kind, ty) ->
        if field_kind_repr kind = Fpresent then
        try closed_type ty with Non_closed (ty0, real) ->
          raise (CCFailure (CC_Method (ty0, real, lab, ty))))
      fields;
    mark_type_params (repr sign.csig_self);
    List.iter unmark_type params;
    unmark_class_signature sign;
    None
  with CCFailure reason ->
    mark_type_params (repr sign.csig_self);
    List.iter unmark_type params;
    unmark_class_signature sign;
    Some reason


                            (**********************)
                            (*  Type duplication  *)
                            (**********************)


(* Duplicate a type, preserving only type variables *)
let duplicate_type ty =
  Subst.type_expr Subst.identity ty

(* Same, for class types *)
let duplicate_class_type ty =
  Subst.class_type Subst.identity ty


                         (*****************************)
                         (*  Type level manipulation  *)
                         (*****************************)

(*
   It would be a bit more efficient to remove abbreviation expansions
   rather than generalizing them: these expansions will usually not be
   used anymore. However, this is not possible in the general case, as
   [expand_abbrev] (via [subst]) requires these expansions to be
   preserved. Does it worth duplicating this code ?
*)
let rec iter_generalize tyl ty =
  let ty = repr ty in
  if (ty.level > !current_level) && (ty.level <> generic_level) then begin
    set_level ty generic_level;
    begin match ty.desc with
      Tconstr (_, _, abbrev) ->
        iter_abbrev (iter_generalize tyl) !abbrev
    | _ -> ()
    end;
    iter_type_expr (iter_generalize tyl) ty
  end else
    tyl := ty :: !tyl

let iter_generalize tyl ty =
  simple_abbrevs := Mnil;
  iter_generalize tyl ty

let generalize ty =
  iter_generalize (ref []) ty

(* Efficient repeated generalisation of the same type *)
let iterative_generalization min_level tyl =
  let tyl' = ref [] in
  List.iter (iter_generalize tyl') tyl;
  List.fold_right (fun ty l -> if ty.level <= min_level then l else ty::l)
    !tyl' []

(* Generalize the structure and lower the variables *)

let rec generalize_structure var_level ty =
  let ty = repr ty in
  if ty.level <> generic_level then begin
    if is_Tvar ty && ty.level > var_level then
      set_level ty var_level
    else if
      ty.level > !current_level &&
      match ty.desc with
        Tconstr (p, _, abbrev) ->
          not (is_object_type p) && (abbrev := Mnil; true)
      | _ -> true
    then begin
      set_level ty generic_level;
      iter_type_expr (generalize_structure var_level) ty
    end
  end

let generalize_structure var_level ty =
  simple_abbrevs := Mnil;
  generalize_structure var_level ty

(* Generalize the spine of a function, if the level >= !current_level *)

let rec generalize_spine ty =
  let ty = repr ty in
  if ty.level < !current_level || ty.level = generic_level then () else
  match ty.desc with
    Tarrow (_, ty1, ty2, _) ->
      set_level ty generic_level;
      generalize_spine ty1;
      generalize_spine ty2;
  | Tpoly (ty', _) ->
      set_level ty generic_level;
      generalize_spine ty'
  | Ttuple tyl
  | Tpackage (_, _, tyl) ->
      set_level ty generic_level;
      List.iter generalize_spine tyl
  | Tconstr (p, tyl, memo) when not (is_object_type p) ->
      set_level ty generic_level;
      memo := Mnil;
      List.iter generalize_spine tyl
  | _ -> ()

let forward_try_expand_once = (* Forward declaration *)
  ref (fun env ty -> raise Cannot_expand)

(*
   Lower the levels of a type (assume [level] is not
   [generic_level]).
*)
(*
    The level of a type constructor must be greater than its binding
    time. That way, a type constructor cannot escape the scope of its
    definition, as would be the case in
      let x = ref []
      module M = struct type t let _ = (x : t list ref) end
    (without this constraint, the type system would actually be unsound.)
*)
let get_level env p =
  try
    match (Env.find_type p env).type_newtype_level with
      | None -> Path.binding_time p
      | Some (x, _) -> x
  with
    | Not_found ->
      (* no newtypes in predef *)
      Path.binding_time p

let rec normalize_package_path env p =
  let t =
    try (Env.find_modtype p env).mtd_type
    with Not_found -> None
  in
  match t with
  | Some (Mty_ident p) -> normalize_package_path env p
  | Some (Mty_signature _ | Mty_functor _ | Mty_alias _) | None -> p

let rec update_level env level ty =
  let ty = repr ty in
  if ty.level > level then begin
    begin match Env.gadt_instance_level env ty with
      Some lv -> if level < lv then raise (Unify [(ty, newvar2 level)])
    | None -> ()
    end;
    match ty.desc with
      Tconstr(p, tl, abbrev) when level < get_level env p ->
        (* Try first to replace an abbreviation by its expansion. *)
        begin try
          (* if is_newtype env p then raise Cannot_expand; *)
          link_type ty (!forward_try_expand_once env ty);
          update_level env level ty
        with Cannot_expand ->
          (* +++ Levels should be restored... *)
          (* Format.printf "update_level: %i < %i@." level (get_level env p); *)
          if level < get_level env p then raise (Unify [(ty, newvar2 level)]);
          iter_type_expr (update_level env level) ty
        end
    | Tpackage (p, nl, tl) when level < get_level env p ->
        let p' = normalize_package_path env p in
        if Path.same p p' then raise (Unify [(ty, newvar2 level)]);
        log_type ty; ty.desc <- Tpackage (p', nl, tl);
        update_level env level ty
    | Tobject(_, ({contents=Some(p, tl)} as nm))
      when level < get_level env p ->
        set_name nm None;
        update_level env level ty
    | Tvariant row ->
        let row = row_repr row in
        begin match row.row_name with
        | Some (p, tl) when level < get_level env p ->
            log_type ty;
            ty.desc <- Tvariant {row with row_name = None}
        | _ -> ()
        end;
        set_level ty level;
        iter_type_expr (update_level env level) ty
    | Tfield(lab, _, ty1, _)
      when lab = dummy_method && (repr ty1).level > level ->
        raise (Unify [(ty1, newvar2 level)])
    | _ ->
        set_level ty level;
        (* XXX what about abbreviations in Tconstr ? *)
        iter_type_expr (update_level env level) ty
  end

(* Generalize and lower levels of contravariant branches simultaneously *)

let generalize_contravariant env =
  if !Clflags.principal then generalize_structure else update_level env

let rec generalize_expansive env var_level ty =
  let ty = repr ty in
  if ty.level <> generic_level then begin
    if ty.level > var_level then begin
      set_level ty generic_level;
      match ty.desc with
        Tconstr (path, tyl, abbrev) ->
          let variance =
            try (Env.find_type path env).type_variance
            with Not_found -> List.map (fun _ -> Variance.may_inv) tyl in
          abbrev := Mnil;
          List.iter2
            (fun v t ->
              if Variance.(mem May_weak v)
              then generalize_contravariant env var_level t
              else generalize_expansive env var_level t)
            variance tyl
      | Tpackage (_, _, tyl) ->
          List.iter (generalize_contravariant env var_level) tyl
      | Tarrow (_, t1, t2, _) ->
          generalize_contravariant env var_level t1;
          generalize_expansive env var_level t2
      | _ ->
          iter_type_expr (generalize_expansive env var_level) ty
    end
  end

let generalize_expansive env ty =
  simple_abbrevs := Mnil;
  try
    generalize_expansive env !nongen_level ty
  with Unify ([_, ty'] as tr) ->
    raise (Unify ((ty, ty') :: tr))

let generalize_global ty = generalize_structure !global_level ty
let generalize_structure ty = generalize_structure !current_level ty

(* Correct the levels of type [ty]. *)
let correct_levels ty =
  duplicate_type ty

(* Only generalize the type ty0 in ty *)
let limited_generalize ty0 ty =
  let ty0 = repr ty0 in

  let graph = Hashtbl.create 17 in
  let idx = ref lowest_level in
  let roots = ref [] in

  let rec inverse pty ty =
    let ty = repr ty in
    if (ty.level > !current_level) || (ty.level = generic_level) then begin
      decr idx;
      Hashtbl.add graph !idx (ty, ref pty);
      if (ty.level = generic_level) || (ty == ty0) then
        roots := ty :: !roots;
      set_level ty !idx;
      iter_type_expr (inverse [ty]) ty
    end else if ty.level < lowest_level then begin
      let (_, parents) = Hashtbl.find graph ty.level in
      parents := pty @ !parents
    end

  and generalize_parents ty =
    let idx = ty.level in
    if idx <> generic_level then begin
      set_level ty generic_level;
      List.iter generalize_parents !(snd (Hashtbl.find graph idx));
      (* Special case for rows: must generalize the row variable *)
      match ty.desc with
        Tvariant row ->
          let more = row_more row in
          let lv = more.level in
          if (lv < lowest_level || lv > !current_level)
          && lv <> generic_level then set_level more generic_level
      | _ -> ()
    end
  in

  inverse [] ty;
  if ty0.level < lowest_level then
    iter_type_expr (inverse []) ty0;
  List.iter generalize_parents !roots;
  Hashtbl.iter
    (fun _ (ty, _) ->
       if ty.level <> generic_level then set_level ty !current_level)
    graph


(* Compute statically the free univars of all nodes in a type *)
(* This avoids doing it repeatedly during instantiation *)

type inv_type_expr =
    { inv_type : type_expr;
      mutable inv_parents : inv_type_expr list }

let rec inv_type hash pty ty =
  let ty = repr ty in
  try
    let inv = TypeHash.find hash ty in
    inv.inv_parents <- pty @ inv.inv_parents
  with Not_found ->
    let inv = { inv_type = ty; inv_parents = pty } in
    TypeHash.add hash ty inv;
    iter_type_expr (inv_type hash [inv]) ty

let compute_univars ty =
  let inverted = TypeHash.create 17 in
  inv_type inverted [] ty;
  let node_univars = TypeHash.create 17 in
  let rec add_univar univ inv =
    match inv.inv_type.desc with
      Tpoly (ty, tl) when List.memq univ (List.map repr tl) -> ()
    | _ ->
        try
          let univs = TypeHash.find node_univars inv.inv_type in
          if not (TypeSet.mem univ !univs) then begin
            univs := TypeSet.add univ !univs;
            List.iter (add_univar univ) inv.inv_parents
          end
        with Not_found ->
          TypeHash.add node_univars inv.inv_type (ref(TypeSet.singleton univ));
          List.iter (add_univar univ) inv.inv_parents
  in
  TypeHash.iter (fun ty inv -> if is_Tunivar ty then add_univar ty inv)
    inverted;
  fun ty ->
    try !(TypeHash.find node_univars ty) with Not_found -> TypeSet.empty


                              (*******************)
                              (*  Instantiation  *)
                              (*******************)


let rec find_repr p1 =
  function
    Mnil ->
      None
  | Mcons (Public, p2, ty, _, _) when Path.same p1 p2 ->
      Some ty
  | Mcons (_, _, _, _, rem) ->
      find_repr p1 rem
  | Mlink {contents = rem} ->
      find_repr p1 rem

(*
   Generic nodes are duplicated, while non-generic nodes are left
   as-is.
   During instantiation, the description of a generic node is first
   replaced by a link to a stub ([Tsubst (newvar ())]). Once the
   copy is made, it replaces the stub.
   After instantiation, the description of generic node, which was
   stored by [save_desc], must be put back, using [cleanup_types].
*)

let abbreviations = ref (ref Mnil)
  (* Abbreviation memorized. *)

(* partial: we may not wish to copy the non generic types
   before we call type_pat *)
let rec copy ?env ?partial ?keep_names ty =
  let copy = copy ?env ?partial ?keep_names in
  let ty = repr ty in
  match ty.desc with
    Tsubst ty -> ty
  | _ ->
    if ty.level <> generic_level && partial = None then ty else
    (* We only forget types that are non generic and do not contain
       free univars *)
    let forget =
      if ty.level = generic_level then generic_level else
      match partial with
        None -> assert false
      | Some (free_univars, keep) ->
          if TypeSet.is_empty (free_univars ty) then
            if keep then ty.level else !current_level
          else generic_level
    in
    if forget <> generic_level then newty2 forget (Tvar None) else
    let desc = ty.desc in
    save_desc ty desc;
    let t = newvar() in          (* Stub *)
    begin match env with
      Some env when Env.has_local_constraints env ->
        begin match Env.gadt_instance_level env ty with
          Some lv -> Env.add_gadt_instances env lv [t]
        | None -> ()
        end
    | _ -> ()
    end;
    ty.desc <- Tsubst t;
    t.desc <-
      begin match desc with
      | Tconstr (p, tl, _) ->
          let abbrevs = proper_abbrevs p tl !abbreviations in
          begin match find_repr p !abbrevs with
            Some ty when repr ty != t -> (* XXX Commentaire... *)
              Tlink ty
          | _ ->
          (*
             One must allocate a new reference, so that abbrevia-
             tions belonging to different branches of a type are
             independent.
             Moreover, a reference containing a [Mcons] must be
             shared, so that the memorized expansion of an abbrevi-
             ation can be released by changing the content of just
             one reference.
          *)
              Tconstr (p, List.map copy tl,
                       ref (match !(!abbreviations) with
                              Mcons _ -> Mlink !abbreviations
                            | abbrev  -> abbrev))
          end
      | Tvariant row0 ->
          let row = row_repr row0 in
          let more = repr row.row_more in
          (* We must substitute in a subtle way *)
          (* Tsubst takes a tuple containing the row var and the variant *)
          begin match more.desc with
            Tsubst {desc = Ttuple [_;ty2]} ->
              (* This variant type has been already copied *)
              ty.desc <- Tsubst ty2; (* avoid Tlink in the new type *)
              Tlink ty2
          | _ ->
              (* If the row variable is not generic, we must keep it *)
              let keep = more.level <> generic_level in
              let more' =
                match more.desc with
                  Tsubst ty -> ty
                | Tconstr _ | Tnil ->
                    if keep then save_desc more more.desc;
                    copy more
                | Tvar _ | Tunivar _ ->
                    save_desc more more.desc;
                    if keep then more else newty more.desc
                |  _ -> assert false
              in
              let row =
                match repr more' with (* PR#6163 *)
                  {desc=Tconstr _} when not row.row_fixed ->
                    {row with row_fixed = true}
                | _ -> row
              in
              (* Open row if partial for pattern and contains Reither *)
              let more', row =
                match partial with
                  Some (free_univars, false) ->
                    let more' =
                      if more.id != more'.id then more' else
                      let lv = if keep then more.level else !current_level in
                      newty2 lv (Tvar None)
                    in
                    let not_reither (_, f) =
                      match row_field_repr f with
                        Reither _ -> false
                      | _ -> true
                    in
                    if row.row_closed && not row.row_fixed
                    && TypeSet.is_empty (free_univars ty)
                    && not (List.for_all not_reither row.row_fields) then
                      (more',
                       {row_fields = List.filter not_reither row.row_fields;
                        row_more = more'; row_bound = ();
                        row_closed = false; row_fixed = false; row_name = None})
                    else (more', row)
                | _ -> (more', row)
              in
              (* Register new type first for recursion *)
              more.desc <- Tsubst(newgenty(Ttuple[more';t]));
              (* Return a new copy *)
              Tvariant (copy_row copy true row keep more')
          end
      | Tfield (p, k, ty1, ty2) ->
          begin match field_kind_repr k with
            Fabsent  -> Tlink (copy ty2)
          | Fpresent -> copy_type_desc copy desc
          | Fvar r ->
              dup_kind r;
              copy_type_desc copy desc
          end
      | Tobject (ty1, _) when partial <> None ->
          Tobject (copy ty1, ref None)
      | _ -> copy_type_desc ?keep_names copy desc
      end;
    t

let simple_copy t = copy t

(**** Variants of instantiations ****)

let gadt_env env =
  if Env.has_local_constraints env
  then Some env
  else None

let instance ?partial env sch =
  let env = gadt_env env in
  let partial =
    match partial with
      None -> None
    | Some keep -> Some (compute_univars sch, keep)
  in
  let ty = copy ?env ?partial sch in
  cleanup_types ();
  ty

let instance_def sch =
  let ty = copy sch in
  cleanup_types ();
  ty

let instance_list env schl =
  let env = gadt_env env in
  let tyl = List.map (fun t -> copy ?env t) schl in
  cleanup_types ();
  tyl

let reified_var_counter = ref Vars.empty

(* names given to new type constructors.
   Used for existential types and
   local constraints *)
let get_new_abstract_name s =
  let index =
    try Vars.find s !reified_var_counter + 1
    with Not_found -> 0 in
  reified_var_counter := Vars.add s index !reified_var_counter;
  Printf.sprintf "%s#%d" s index

let new_declaration newtype manifest =
  {
    type_params = [];
    type_arity = 0;
    type_kind = Type_abstract;
    type_private = Public;
    type_manifest = manifest;
    type_variance = [];
    type_newtype_level = newtype;
    type_loc = Location.none;
    type_attributes = [];
  }

let instance_constructor ?in_pattern cstr =
  begin match in_pattern with
  | None -> ()
  | Some (env, newtype_lev) ->
      let process existential =
        let decl = new_declaration (Some (newtype_lev, newtype_lev)) None in
        let name =
          match repr existential with
            {desc = Tvar (Some name)} -> name
          | _ -> "ex"
        in
        let (id, new_env) =
          Env.enter_type (get_new_abstract_name name) decl !env in
        env := new_env;
        let to_unify = newty (Tconstr (Path.Pident id,[],ref Mnil)) in
        let tv = copy existential in
        assert (is_Tvar tv);
        link_type tv to_unify
      in
      List.iter process cstr.cstr_existentials
  end;
  let ty_res = copy cstr.cstr_res in
  let ty_args = List.map simple_copy  cstr.cstr_args in
  cleanup_types ();
  (ty_args, ty_res)

let instance_parameterized_type ?keep_names sch_args sch =
  let ty_args = List.map (fun t -> copy ?keep_names t) sch_args in
  let ty = copy sch in
  cleanup_types ();
  (ty_args, ty)

let instance_parameterized_type_2 sch_args sch_lst sch =
  let ty_args = List.map simple_copy sch_args in
  let ty_lst = List.map simple_copy sch_lst in
  let ty = copy sch in
  cleanup_types ();
  (ty_args, ty_lst, ty)

let instance_declaration decl =
  let decl =
    {decl with type_params = List.map simple_copy decl.type_params;
     type_manifest = may_map simple_copy decl.type_manifest;
     type_kind = match decl.type_kind with
     | Type_abstract -> Type_abstract
     | Type_variant cl ->
         Type_variant (
           List.map
             (fun c ->
                {c with cd_args=List.map simple_copy c.cd_args;
                        cd_res=may_map simple_copy c.cd_res})
             cl)
     | Type_record (fl, rr) ->
         Type_record (
           List.map
             (fun l ->
                {l with ld_type = copy l.ld_type}
             ) fl, rr)
     | Type_open -> Type_open
    }
  in
  cleanup_types ();
  decl

let instance_class params cty =
  let rec copy_class_type =
    function
      Cty_constr (path, tyl, cty) ->
        Cty_constr (path, List.map simple_copy tyl, copy_class_type cty)
    | Cty_signature sign ->
        Cty_signature
          {csig_self = copy sign.csig_self;
           csig_vars =
             Vars.map (function (m, v, ty) -> (m, v, copy ty)) sign.csig_vars;
           csig_concr = sign.csig_concr;
           csig_inher =
             List.map (fun (p,tl) -> (p, List.map simple_copy tl))
               sign.csig_inher}
    | Cty_arrow (l, ty, cty) ->
        Cty_arrow (l, copy ty, copy_class_type cty)
  in
  let params' = List.map simple_copy params in
  let cty' = copy_class_type cty in
  cleanup_types ();
  (params', cty')

(**** Instanciation for types with free universal variables ****)

let rec diff_list l1 l2 =
  if l1 == l2 then [] else
  match l1 with [] -> invalid_arg "Ctype.diff_list"
  | a :: l1 -> a :: diff_list l1 l2

let conflicts free bound =
  let bound = List.map repr bound in
  TypeSet.exists (fun t -> List.memq (repr t) bound) free

let delayed_copy = ref []
    (* copying to do later *)

(* Copy without sharing until there are no free univars left *)
(* all free univars must be included in [visited]            *)
let rec copy_sep fixed free bound visited ty =
  let ty = repr ty in
  let univars = free ty in
  if TypeSet.is_empty univars then
    if ty.level <> generic_level then ty else
    let t = newvar () in
    delayed_copy :=
      lazy (t.desc <- Tlink (copy ty))
      :: !delayed_copy;
    t
  else try
    let t, bound_t = List.assq ty visited in
    let dl = if is_Tunivar ty then [] else diff_list bound bound_t in
    if dl <> [] && conflicts univars dl then raise Not_found;
    t
  with Not_found -> begin
    let t = newvar() in          (* Stub *)
    let visited =
      match ty.desc with
        Tarrow _ | Ttuple _ | Tvariant _ | Tconstr _ | Tobject _ | Tpackage _ ->
          (ty,(t,bound)) :: visited
      | _ -> visited in
    let copy_rec = copy_sep fixed free bound visited in
    t.desc <-
      begin match ty.desc with
      | Tvariant row0 ->
          let row = row_repr row0 in
          let more = repr row.row_more in
          (* We shall really check the level on the row variable *)
          let keep = is_Tvar more && more.level <> generic_level in
          let more' = copy_rec more in
          let fixed' = fixed && is_Tvar (repr more') in
          let row = copy_row copy_rec fixed' row keep more' in
          Tvariant row
      | Tpoly (t1, tl) ->
          let tl = List.map repr tl in
          let tl' = List.map (fun t -> newty t.desc) tl in
          let bound = tl @ bound in
          let visited =
            List.map2 (fun ty t -> ty,(t,bound)) tl tl' @ visited in
          Tpoly (copy_sep fixed free bound visited t1, tl')
      | _ -> copy_type_desc copy_rec ty.desc
      end;
    t
  end

let instance_poly ?(keep_names=false) fixed univars sch =
  let univars = List.map repr univars in
  let copy_var ty =
    match ty.desc with
      Tunivar name -> if keep_names then newty (Tvar name) else newvar ()
    | _ -> assert false
  in
  let vars = List.map copy_var univars in
  let pairs = List.map2 (fun u v -> u, (v, [])) univars vars in
  delayed_copy := [];
  let ty = copy_sep fixed (compute_univars sch) [] pairs sch in
  List.iter Lazy.force !delayed_copy;
  delayed_copy := [];
  cleanup_types ();
  vars, ty

let instance_label fixed lbl =
  let ty_res = copy lbl.lbl_res in
  let vars, ty_arg =
    match repr lbl.lbl_arg with
      {desc = Tpoly (ty, tl)} ->
        instance_poly fixed tl ty
    | ty ->
        [], copy lbl.lbl_arg
  in
  cleanup_types ();
  (vars, ty_arg, ty_res)

(**** Instantiation with parameter substitution ****)

let unify' = (* Forward declaration *)
  ref (fun env ty1 ty2 -> raise (Unify []))

let subst env level priv abbrev ty params args body =
  if List.length params <> List.length args then raise (Unify []);
  let old_level = !current_level in
  current_level := level;
  try
    let body0 = newvar () in          (* Stub *)
    begin match ty with
      None      -> ()
    | Some ({desc = Tconstr (path, tl, _)} as ty) ->
        let abbrev = proper_abbrevs path tl abbrev in
        memorize_abbrev abbrev priv path ty body0
    | _ ->
        assert false
    end;
    abbreviations := abbrev;
    let (params', body') = instance_parameterized_type params body in
    abbreviations := ref Mnil;
    !unify' env body0 body';
    List.iter2 (!unify' env) params' args;
    current_level := old_level;
    body'
  with Unify _ as exn ->
    current_level := old_level;
    raise exn

(*
   Only the shape of the type matters, not whether is is generic or
   not. [generic_level] might be somewhat slower, but it ensures
   invariants on types are enforced (decreasing levels.), and we don't
   care about efficiency here.
*)
let apply env params body args =
  try
    subst env generic_level Public (ref Mnil) None params args body
  with
    Unify _ -> raise Cannot_apply


                              (****************************)
                              (*  Abbreviation expansion  *)
                              (****************************)

(*
   If the environnement has changed, memorized expansions might not
   be correct anymore, and so we flush the cache. This is safe but
   quite pessimistic: it would be enough to flush the cache when a
   type or module definition is overridden in the environnement.
*)
let previous_env = ref Env.empty
let string_of_kind = function Public -> "public" | Private -> "private"
let check_abbrev_env env =
  if env != !previous_env then begin
    (* prerr_endline "cleanup expansion cache"; *)
    cleanup_abbrev ();
    previous_env := env
  end


(* Expand an abbreviation. The expansion is memorized. *)
(*
   Assume the level is greater than the path binding time of the
   expanded abbreviation.
*)
(*
   An abbreviation expansion will fail in either of these cases:
   1. The type constructor does not correspond to a manifest type.
   2. The type constructor is defined in an external file, and this
      file is not in the path (missing -I options).
   3. The type constructor is not in the "local" environment. This can
      happens when a non-generic type variable has been instantiated
      afterwards to the not yet defined type constructor. (Actually,
      this cannot happen at the moment due to the strong constraints
      between type levels and constructor binding time.)
   4. The expansion requires the expansion of another abbreviation,
      and this other expansion fails.
*)
let expand_abbrev_gen kind find_type_expansion env ty =
  check_abbrev_env env;
  match ty with
    {desc = Tconstr (path, args, abbrev); level = level} ->
      let lookup_abbrev = proper_abbrevs path args abbrev in
      begin match find_expans kind path !lookup_abbrev with
        Some ty ->
          (* prerr_endline
            ("found a "^string_of_kind kind^" expansion for "^Path.name path);*)
          if level <> generic_level then
            begin try
              update_level env level ty
            with Unify _ ->
              (* XXX This should not happen.
                 However, levels are not correctly restored after a
                 typing error *)
              ()
            end;
          ty
      | None ->
          let (params, body, lv) =
            try find_type_expansion path env with Not_found ->
              raise Cannot_expand
          in
          (* prerr_endline
            ("add a "^string_of_kind kind^" expansion for "^Path.name path);*)
          let ty' = subst env level kind abbrev (Some ty) params args body in
          (* Hack to name the variant type *)
          begin match repr ty' with
            {desc=Tvariant row} as ty when static_row row ->
              ty.desc <- Tvariant { row with row_name = Some (path, args) }
          | _ -> ()
          end;
          (* For gadts, remember type as non exportable *)
          (* The ambiguous level registered for ty' should be the highest *)
          if !trace_gadt_instances then begin
            match max lv (Env.gadt_instance_level env ty) with
              None -> ()
            | Some lv ->
                if level < lv then raise (Unify [(ty, newvar2 level)]);
                Env.add_gadt_instances env lv [ty; ty']
          end;
          ty'
      end
  | _ ->
      assert false

(* Expand respecting privacy *)
let expand_abbrev ty =
  expand_abbrev_gen Public Env.find_type_expansion ty

(* Expand once the head of a type *)
let expand_head_once env ty =
  try expand_abbrev env (repr ty) with Cannot_expand -> assert false

(* Check whether a type can be expanded *)
let safe_abbrev env ty =
  let snap = Btype.snapshot () in
  try ignore (expand_abbrev env ty); true
  with Cannot_expand | Unify _ ->
    Btype.backtrack snap;
    false

(* Expand the head of a type once.
   Raise Cannot_expand if the type cannot be expanded.
   May raise Unify, if a recursion was hidden in the type. *)
let try_expand_once env ty =
  let ty = repr ty in
  match ty.desc with
    Tconstr (p, _, _) -> repr (expand_abbrev env ty)
  | _ -> raise Cannot_expand

(* This one only raises Cannot_expand *)
let try_expand_safe env ty =
  let snap = Btype.snapshot () in
  try try_expand_once env ty
  with Unify _ ->
    Btype.backtrack snap; raise Cannot_expand

(* Fully expand the head of a type. *)
let rec try_expand_head try_once env ty =
  let ty' = try_once env ty in
  try try_expand_head try_once env ty'
  with Cannot_expand -> ty'

let try_expand_head try_once env ty =
  let ty' = try_expand_head try_once env ty in
  begin match Env.gadt_instance_level env ty' with
    None -> ()
  | Some lv -> Env.add_gadt_instance_chain env lv ty
  end;
  ty'

(* Unsafe full expansion, may raise Unify. *)
let expand_head_unif env ty =
  try try_expand_head try_expand_once env ty with Cannot_expand -> repr ty

(* Safe version of expand_head, never fails *)
let expand_head env ty =
  try try_expand_head try_expand_safe env ty with Cannot_expand -> repr ty

let _ = forward_try_expand_once := try_expand_safe


(* Expand until we find a non-abstract type declaration *)

let rec extract_concrete_typedecl env ty =
  let ty = repr ty in
  match ty.desc with
    Tconstr (p, _, _) ->
      let decl = Env.find_type p env in
      if decl.type_kind <> Type_abstract then (p, p, decl) else
      let ty =
        try try_expand_once env ty with Cannot_expand -> raise Not_found
      in
      let (_, p', decl) = extract_concrete_typedecl env ty in
        (p, p', decl)
  | _ -> raise Not_found

(* Implementing function [expand_head_opt], the compiler's own version of
   [expand_head] used for type-based optimisations.
   [expand_head_opt] uses [Env.find_type_expansion_opt] to access the
   manifest type information of private abstract data types which is
   normally hidden to the type-checker out of the implementation module of
   the private abbreviation. *)

let expand_abbrev_opt =
  expand_abbrev_gen Private Env.find_type_expansion_opt

let try_expand_once_opt env ty =
  let ty = repr ty in
  match ty.desc with
    Tconstr _ -> repr (expand_abbrev_opt env ty)
  | _ -> raise Cannot_expand

let rec try_expand_head_opt env ty =
  let ty' = try_expand_once_opt env ty in
  begin try
    try_expand_head_opt env ty'
  with Cannot_expand ->
    ty'
  end

let expand_head_opt env ty =
  let snap = Btype.snapshot () in
  try try_expand_head_opt env ty
  with Cannot_expand | Unify _ -> (* expand_head shall never fail *)
    Btype.backtrack snap;
    repr ty

(* Make sure that the type parameters of the type constructor [ty]
   respect the type constraints *)
let enforce_constraints env ty =
  match ty with
    {desc = Tconstr (path, args, abbrev); level = level} ->
      begin try
        let decl = Env.find_type path env in
        ignore
          (subst env level Public (ref Mnil) None decl.type_params args
             (newvar2 level))
      with Not_found -> ()
      end
  | _ ->
      assert false

(* Recursively expand the head of a type.
   Also expand #-types. *)
let full_expand env ty =
  let ty = repr (expand_head env ty) in
  match ty.desc with
    Tobject (fi, {contents = Some (_, v::_)}) when is_Tvar (repr v) ->
      newty2 ty.level (Tobject (fi, ref None))
  | _ ->
      ty

(*
   Check whether the abbreviation expands to a well-defined type.
   During the typing of a class, abbreviations for correspondings
   types expand to non-generic types.
*)
let generic_abbrev env path =
  try
    let (_, body, _) = Env.find_type_expansion path env in
    (repr body).level = generic_level
  with
    Not_found ->
      false

let generic_private_abbrev env path =
  try
    match Env.find_type path env with
      {type_kind = Type_abstract;
       type_private = Private;
       type_manifest = Some body} ->
         (repr body).level = generic_level
    | _ -> false
  with Not_found -> false

let is_contractive env ty =
  match (repr ty).desc with
    Tconstr (p, _, _) ->
      in_pervasives p ||
      (try is_datatype (Env.find_type p env) with Not_found -> false)
  | _ -> true

(* Code moved to Typedecl

(* The marks are already used by [expand_abbrev]... *)
let visited = ref []

let rec non_recursive_abbrev env ty0 ty =
  let ty = repr ty in
  if ty == repr ty0 then raise Recursive_abbrev;
  if not (List.memq ty !visited) then begin
    visited := ty :: !visited;
    match ty.desc with
      Tconstr(p, args, abbrev) ->
        begin try
          non_recursive_abbrev env ty0 (try_expand_once_opt env ty)
        with Cannot_expand ->
          if !Clflags.recursive_types &&
            (in_pervasives p ||
             try is_datatype (Env.find_type p env) with Not_found -> false)
          then ()
          else iter_type_expr (non_recursive_abbrev env ty0) ty
        end
    | Tobject _ | Tvariant _ ->
        ()
    | _ ->
        if !Clflags.recursive_types then () else
        iter_type_expr (non_recursive_abbrev env ty0) ty
  end

let correct_abbrev env path params ty =
  check_abbrev_env env;
  let ty0 = newgenvar () in
  visited := [];
  let abbrev = Mcons (Public, path, ty0, ty0, Mnil) in
  simple_abbrevs := abbrev;
  try
    non_recursive_abbrev env ty0
      (subst env generic_level Public (ref abbrev) None [] [] ty);
    simple_abbrevs := Mnil;
    visited := []
  with exn ->
    simple_abbrevs := Mnil;
    visited := [];
    raise exn
*)

                              (*****************)
                              (*  Occur check  *)
                              (*****************)


exception Occur

let rec occur_rec env visited ty0 ty =
  if ty == ty0  then raise Occur;
  let occur_ok = !Clflags.recursive_types && is_contractive env ty in
  match ty.desc with
    Tconstr(p, tl, abbrev) ->
      begin try
        if occur_ok || List.memq ty visited then raise Occur;
        iter_type_expr (occur_rec env (ty::visited) ty0) ty
      with Occur -> try
        let ty' = try_expand_head try_expand_once env ty in
        (* Maybe we could simply make a recursive call here,
           but it seems it could make the occur check loop
           (see change in rev. 1.58) *)
        if ty' == ty0 || List.memq ty' visited then raise Occur;
        match ty'.desc with
          Tobject _ | Tvariant _ -> ()
        | _ ->
            if not (!Clflags.recursive_types && is_contractive env ty') then
              iter_type_expr (occur_rec env (ty'::visited) ty0) ty'
      with Cannot_expand ->
        if not occur_ok then raise Occur
      end
  | Tobject _ | Tvariant _ ->
      ()
  | _ ->
      if not occur_ok then
        iter_type_expr (occur_rec env visited ty0) ty

let type_changed = ref false (* trace possible changes to the studied type *)

let merge r b = if b then r := true

let occur env ty0 ty =
  let old = !type_changed in
  try
    while type_changed := false; occur_rec env [] ty0 ty; !type_changed
    do () (* prerr_endline "changed" *) done;
    merge type_changed old
  with exn ->
    merge type_changed old;
    raise (match exn with Occur -> Unify [] | _ -> exn)

let occur_in env ty0 t =
  try occur env ty0 t; false with Unify _ -> true

(* Check that a local constraint is well-founded *)
(* PR#6405: not needed since we allow recursion and work on normalized types *)
(*
let rec local_non_recursive_abbrev visited env p ty =
  let ty = repr ty in
  if not (List.memq ty !visited) then begin
    visited := ty :: !visited;
    match ty.desc with
      Tconstr(p', args, abbrev) ->
        if Path.same p p' then raise Recursive_abbrev;
        begin try
          local_non_recursive_abbrev visited env p (try_expand_once_opt env ty)
        with Cannot_expand -> ()
        end
    | _ -> ()
  end

let local_non_recursive_abbrev env p =
  local_non_recursive_abbrev (ref []) env p
*)

                   (*****************************)
                   (*  Polymorphic Unification  *)
                   (*****************************)

(* Since we cannot duplicate universal variables, unification must
   be done at meta-level, using bindings in univar_pairs *)
let rec unify_univar t1 t2 = function
    (cl1, cl2) :: rem ->
      let find_univ t cl =
        try
          let (_, r) = List.find (fun (t',_) -> t == repr t') cl in
          Some r
        with Not_found -> None
      in
      begin match find_univ t1 cl1, find_univ t2 cl2 with
        Some {contents=Some t'2}, Some _ when t2 == repr t'2 ->
          ()
      | Some({contents=None} as r1), Some({contents=None} as r2) ->
          set_univar r1 t2; set_univar r2 t1
      | None, None ->
          unify_univar t1 t2 rem
      | _ ->
          raise (Unify [])
      end
  | [] -> raise (Unify [])

(* Test the occurence of free univars in a type *)
(* that's way too expansive. Must do some kind of cacheing *)
let occur_univar env ty =
  let visited = ref TypeMap.empty in
  let rec occur_rec bound ty =
    let ty = repr ty in
    if ty.level >= lowest_level &&
      if TypeSet.is_empty bound then
        (ty.level <- pivot_level - ty.level; true)
      else try
        let bound' = TypeMap.find ty !visited in
        if TypeSet.exists (fun x -> not (TypeSet.mem x bound)) bound' then
          (visited := TypeMap.add ty (TypeSet.inter bound bound') !visited;
           true)
        else false
      with Not_found ->
        visited := TypeMap.add ty bound !visited;
        true
    then
      match ty.desc with
        Tunivar _ ->
          if not (TypeSet.mem ty bound) then raise (Unify [ty, newgenvar ()])
      | Tpoly (ty, tyl) ->
          let bound = List.fold_right TypeSet.add (List.map repr tyl) bound in
          occur_rec bound  ty
      | Tconstr (_, [], _) -> ()
      | Tconstr (p, tl, _) ->
          begin try
            let td = Env.find_type p env in
            List.iter2
              (fun t v ->
                if Variance.(mem May_pos v || mem May_neg v)
                then occur_rec bound t)
              tl td.type_variance
          with Not_found ->
            List.iter (occur_rec bound) tl
          end
      | _ -> iter_type_expr (occur_rec bound) ty
  in
  try
    occur_rec TypeSet.empty ty; unmark_type ty
  with exn ->
    unmark_type ty; raise exn

(* Grouping univars by families according to their binders *)
let add_univars =
  List.fold_left (fun s (t,_) -> TypeSet.add (repr t) s)

let get_univar_family univar_pairs univars =
  if univars = [] then TypeSet.empty else
  let insert s = function
      cl1, (_::_ as cl2) ->
        if List.exists (fun (t1,_) -> TypeSet.mem (repr t1) s) cl1 then
          add_univars s cl2
        else s
    | _ -> s
  in
  let s = List.fold_right TypeSet.add univars TypeSet.empty in
  List.fold_left insert s univar_pairs

(* Whether a family of univars escapes from a type *)
let univars_escape env univar_pairs vl ty =
  let family = get_univar_family univar_pairs vl in
  let visited = ref TypeSet.empty in
  let rec occur t =
    let t = repr t in
    if TypeSet.mem t !visited then () else begin
      visited := TypeSet.add t !visited;
      match t.desc with
        Tpoly (t, tl) ->
          if List.exists (fun t -> TypeSet.mem (repr t) family) tl then ()
          else occur t
      | Tunivar _ ->
          if TypeSet.mem t family then raise Occur
      | Tconstr (_, [], _) -> ()
      | Tconstr (p, tl, _) ->
          begin try
            let td = Env.find_type p env in
            List.iter2
              (fun t v ->
                if Variance.(mem May_pos v || mem May_neg v) then occur t)
              tl td.type_variance
          with Not_found ->
            List.iter occur tl
          end
      | _ ->
          iter_type_expr occur t
    end
  in
  try occur ty; false with Occur -> true

(* Wrapper checking that no variable escapes and updating univar_pairs *)
let enter_poly env univar_pairs t1 tl1 t2 tl2 f =
  let old_univars = !univar_pairs in
  let known_univars =
    List.fold_left (fun s (cl,_) -> add_univars s cl)
      TypeSet.empty old_univars
  in
  let tl1 = List.map repr tl1 and tl2 = List.map repr tl2 in
  if List.exists (fun t -> TypeSet.mem t known_univars) tl1 &&
    univars_escape env old_univars tl1 (newty(Tpoly(t2,tl2)))
  || List.exists (fun t -> TypeSet.mem t known_univars) tl2 &&
    univars_escape env old_univars tl2 (newty(Tpoly(t1,tl1)))
  then raise (Unify []);
  let cl1 = List.map (fun t -> t, ref None) tl1
  and cl2 = List.map (fun t -> t, ref None) tl2 in
  univar_pairs := (cl1,cl2) :: (cl2,cl1) :: old_univars;
  try let res = f t1 t2 in univar_pairs := old_univars; res
  with exn -> univar_pairs := old_univars; raise exn

let univar_pairs = ref []


                              (*****************)
                              (*  Unification  *)
                              (*****************)



let rec has_cached_expansion p abbrev =
  match abbrev with
    Mnil                   -> false
  | Mcons(_, p', _, _, rem)   -> Path.same p p' || has_cached_expansion p rem
  | Mlink rem              -> has_cached_expansion p !rem

(**** Transform error trace ****)
(* +++ Move it to some other place ? *)

let expand_trace env trace =
  List.fold_right
    (fun (t1, t2) rem ->
       (repr t1, full_expand env t1)::(repr t2, full_expand env t2)::rem)
    trace []

(* build a dummy variant type *)
let mkvariant fields closed =
  newgenty
    (Tvariant
       {row_fields = fields; row_closed = closed; row_more = newvar();
        row_bound = (); row_fixed = false; row_name = None })

(* force unification in Reither when one side has as non-conjunctive type *)
let rigid_variants = ref false

(**** Unification ****)

(* Return whether [t0] occurs in [ty]. Objects are also traversed. *)
let deep_occur t0 ty =
  let rec occur_rec ty =
    let ty = repr ty in
    if ty.level >= lowest_level then begin
      if ty == t0 then raise Occur;
      ty.level <- pivot_level - ty.level;
      iter_type_expr occur_rec ty
    end
  in
  try
    occur_rec ty; unmark_type ty; false
  with Occur ->
    unmark_type ty; true

(*
   1. When unifying two non-abbreviated types, one type is made a link
      to the other. When unifying an abbreviated type with a
      non-abbreviated type, the non-abbreviated type is made a link to
      the other one. When unifying to abbreviated types, these two
      types are kept distincts, but they are made to (temporally)
      expand to the same type.
   2. Abbreviations with at least one parameter are systematically
      expanded. The overhead does not seem to high, and that way
      abbreviations where some parameters does not appear in the
      expansion, such as ['a t = int], are correctly handled. In
      particular, for this example, unifying ['a t] with ['b t] keeps
      ['a] and ['b] distincts. (Is it really important ?)
   3. Unifying an abbreviation ['a t = 'a] with ['a] should not yield
      ['a t as 'a]. Indeed, the type variable would otherwise be lost.
      This problem occurs for abbreviations expanding to a type
      variable, but also to many other constrained abbreviations (for
      instance, [(< x : 'a > -> unit) t = <x : 'a>]). The solution is
      that, if an abbreviation is unified with some subpart of its
      parameters, then the parameter actually does not get
      abbreviated.  It would be possible to check whether some
      information is indeed lost, but it probably does not worth it.
*)

let newtype_level = ref None

let get_newtype_level () =
  match !newtype_level with
  | None -> assert false
  | Some x -> x

(* a local constraint can be added only if the rhs
   of the constraint does not contain any Tvars.
   They need to be removed using this function *)
let reify env t =
  let newtype_level = get_newtype_level () in
  let create_fresh_constr lev name =
    let decl = new_declaration (Some (newtype_level, newtype_level)) None in
    let name = get_new_abstract_name name in
    let (id, new_env) = Env.enter_type name decl !env in
    let t = newty2 lev (Tconstr (Path.Pident id,[],ref Mnil))  in
    env := new_env;
    t
  in
  let visited = ref TypeSet.empty in
  let rec iterator ty =
    let ty = repr ty in
    if TypeSet.mem ty !visited then () else begin
      visited := TypeSet.add ty !visited;
      match ty.desc with
        Tvar o ->
          let name = match o with Some s -> s | _ -> "ex" in
          let t = create_fresh_constr ty.level name in
          link_type ty t
      | Tvariant r ->
          let r = row_repr r in
          if not (static_row r) then begin
            if r.row_fixed then iterator (row_more r) else
            let m = r.row_more in
            match m.desc with
              Tvar o ->
                let name = match o with Some s -> s | _ -> "ex" in
                let t = create_fresh_constr m.level name in
                let row =
                  {r with row_fields=[]; row_fixed=true; row_more = t} in
                link_type m (newty2 m.level (Tvariant row))
            | _ -> assert false
          end;
          iter_row iterator r
      | Tconstr (p, _, _) when is_object_type p ->
          iter_type_expr iterator (full_expand !env ty)
      | _ ->
          iter_type_expr iterator ty
    end
  in
  iterator t

let is_newtype env p =
  try
    let decl = Env.find_type p env in
    decl.type_newtype_level <> None &&
    decl.type_kind = Type_abstract &&
    decl.type_private = Public
  with Not_found -> false

let non_aliasable p decl =
  (* in_pervasives p ||  (subsumed by in_current_module) *)
  in_current_module p && decl.type_newtype_level = None

(* Check for datatypes carefully; see PR#6348 *)
let rec expands_to_datatype env ty =
  let ty = repr ty in
  match ty.desc with
    Tconstr (p, _, _) ->
      begin try
        is_datatype (Env.find_type p env) ||
        expands_to_datatype env (try_expand_once env ty)
      with Not_found | Cannot_expand -> false
      end
  | _ -> false

(* mcomp type_pairs subst env t1 t2 does not raise an
   exception if it is possible that t1 and t2 are actually
   equal, assuming the types in type_pairs are equal and
   that the mapping subst holds.
   Assumes that both t1 and t2 do not contain any tvars
   and that both their objects and variants are closed
 *)

let rec mcomp type_pairs env t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else
  match (t1.desc, t2.desc) with
  | (Tvar _, _)
  | (_, Tvar _)  ->
      ()
  | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
      ()
  | _ ->
      let t1' = expand_head_opt env t1 in
      let t2' = expand_head_opt env t2 in
      (* Expansion may have changed the representative of the types... *)
      let t1' = repr t1' and t2' = repr t2' in
      if t1' == t2' then () else
      begin try TypePairs.find type_pairs (t1', t2')
      with Not_found ->
        TypePairs.add type_pairs (t1', t2') ();
        match (t1'.desc, t2'.desc) with
          (Tvar _, Tvar _) -> assert false
        | (Tarrow (l1, t1, u1, _), Tarrow (l2, t2, u2, _))
          when l1 = l2 || not (is_optional l1 || is_optional l2) ->
            mcomp type_pairs env t1 t2;
            mcomp type_pairs env u1 u2;
        | (Ttuple tl1, Ttuple tl2) ->
            mcomp_list type_pairs env tl1 tl2
        | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _)) ->
            mcomp_type_decl type_pairs env p1 p2 tl1 tl2
        | (Tconstr (p, _, _), _) | (_, Tconstr (p, _, _)) ->
            begin try
              let decl = Env.find_type p env in
              if non_aliasable p decl || is_datatype decl then raise (Unify [])
            with Not_found -> ()
            end
        (*
        | (Tpackage (p1, n1, tl1), Tpackage (p2, n2, tl2)) when n1 = n2 ->
            mcomp_list type_pairs env tl1 tl2
        *)
        | (Tpackage _, Tpackage _) -> ()
        | (Tvariant row1, Tvariant row2) ->
            mcomp_row type_pairs env row1 row2
        | (Tobject (fi1, _), Tobject (fi2, _)) ->
            mcomp_fields type_pairs env fi1 fi2
        | (Tfield _, Tfield _) ->       (* Actually unused *)
            mcomp_fields type_pairs env t1' t2'
        | (Tnil, Tnil) ->
            ()
        | (Tpoly (t1, []), Tpoly (t2, [])) ->
            mcomp type_pairs env t1 t2
        | (Tpoly (t1, tl1), Tpoly (t2, tl2)) ->
            enter_poly env univar_pairs t1 tl1 t2 tl2
              (mcomp type_pairs env)
        | (Tunivar _, Tunivar _) ->
            unify_univar t1' t2' !univar_pairs
        | (_, _) ->
            raise (Unify [])
      end

and mcomp_list type_pairs env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (mcomp type_pairs env) tl1 tl2

and mcomp_fields type_pairs env ty1 ty2 =
  if not (concrete_object ty1 && concrete_object ty2) then assert false;
  let (fields2, rest2) = flatten_fields ty2 in
  let (fields1, rest1) = flatten_fields ty1 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  mcomp type_pairs env rest1 rest2;
  if miss1 <> []  && (object_row ty1).desc = Tnil
  || miss2 <> []  && (object_row ty2).desc = Tnil then raise (Unify []);
  List.iter
    (function (n, k1, t1, k2, t2) ->
       mcomp_kind k1 k2;
       mcomp type_pairs env t1 t2)
    pairs

and mcomp_kind k1 k2 =
  let k1 = field_kind_repr k1 in
  let k2 = field_kind_repr k2 in
  match k1, k2 with
    (Fvar _, Fvar _)
  | (Fpresent, Fpresent) -> ()
  | _                    -> raise (Unify [])

and mcomp_row type_pairs env row1 row2 =
  let row1 = row_repr row1 and row2 = row_repr row2 in
  let r1, r2, pairs = merge_row_fields row1.row_fields row2.row_fields in
  let cannot_erase (_,f) =
    match row_field_repr f with
      Rpresent _ -> true
    | Rabsent | Reither _ -> false
  in
  if row1.row_closed && List.exists cannot_erase r2
  || row2.row_closed && List.exists cannot_erase r1 then raise (Unify []);
  List.iter
    (fun (_,f1,f2) ->
      match row_field_repr f1, row_field_repr f2 with
      | Rpresent None, (Rpresent (Some _) | Reither (_, _::_, _, _) | Rabsent)
      | Rpresent (Some _), (Rpresent None | Reither (true, _, _, _) | Rabsent)
      | (Reither (_, _::_, _, _) | Rabsent), Rpresent None
      | (Reither (true, _, _, _) | Rabsent), Rpresent (Some _) ->
          raise (Unify [])
      | Rpresent(Some t1), Rpresent(Some t2) ->
          mcomp type_pairs env t1 t2
      | Rpresent(Some t1), Reither(false, tl2, _, _) ->
          List.iter (mcomp type_pairs env t1) tl2
      | Reither(false, tl1, _, _), Rpresent(Some t2) ->
          List.iter (mcomp type_pairs env t2) tl1
      | _ -> ())
    pairs

and mcomp_type_decl type_pairs env p1 p2 tl1 tl2 =
  try
    let decl = Env.find_type p1 env in
    let decl' = Env.find_type p2 env in
    if Path.same p1 p2 then begin
      let inj =
        try List.map Variance.(mem Inj) (Env.find_type p1 env).type_variance
        with Not_found -> List.map (fun _ -> false) tl1
      in
      List.iter2
        (fun i (t1,t2) -> if i then mcomp type_pairs env t1 t2)
        inj (List.combine tl1 tl2)
    end else if non_aliasable p1 decl && non_aliasable p2 decl' then
      raise (Unify [])
    else
      match decl.type_kind, decl'.type_kind with
      | Type_record (lst,r), Type_record (lst',r') when r = r' ->
          mcomp_list type_pairs env tl1 tl2;
          mcomp_record_description type_pairs env lst lst'
      | Type_variant v1, Type_variant v2 ->
          mcomp_list type_pairs env tl1 tl2;
          mcomp_variant_description type_pairs env v1 v2
      | Type_open, Type_open ->
          mcomp_list type_pairs env tl1 tl2
      | Type_abstract, Type_abstract -> ()
      | Type_abstract, _ when not (non_aliasable p1 decl)-> ()
      | _, Type_abstract when not (non_aliasable p2 decl') -> ()
      | _ -> raise (Unify [])
  with Not_found -> ()

and mcomp_type_option type_pairs env t t' =
  match t, t' with
    None, None -> ()
  | Some t, Some t' -> mcomp type_pairs env t t'
  | _ -> raise (Unify [])

and mcomp_variant_description type_pairs env xs ys =
  let rec iter = fun x y ->
    match x, y with
    | c1 :: xs, c2 :: ys   ->
      mcomp_type_option type_pairs env c1.cd_res c2.cd_res;
      mcomp_list type_pairs env c1.cd_args c2.cd_args;
     if Ident.name c1.cd_id = Ident.name c2.cd_id
      then iter xs ys
      else raise (Unify [])
    | [],[] -> ()
    | _ -> raise (Unify [])
  in
  iter xs ys

and mcomp_record_description type_pairs env =
  let rec iter x y =
    match x, y with
    | l1 :: xs, l2 :: ys ->
        mcomp type_pairs env l1.ld_type l2.ld_type;
        if Ident.name l1.ld_id = Ident.name l2.ld_id &&
           l1.ld_mutable = l2.ld_mutable
        then iter xs ys
        else raise (Unify [])
    | [], [] -> ()
    | _ -> raise (Unify [])
  in
  iter

let mcomp env t1 t2 =
  mcomp (TypePairs.create 4) env t1 t2

(* Real unification *)

let find_lowest_level ty =
  let lowest = ref generic_level in
  let rec find ty =
    let ty = repr ty in
    if ty.level >= lowest_level then begin
      if ty.level < !lowest then lowest := ty.level;
      ty.level <- pivot_level - ty.level;
      iter_type_expr find ty
    end
  in find ty; unmark_type ty; !lowest

let find_newtype_level env path =
  try match (Env.find_type path env).type_newtype_level with
    Some x -> x
  | None -> assert false
  with Not_found -> assert false

let add_gadt_equation env source destination =
  let destination = duplicate_type destination in
  let source_lev = find_newtype_level !env (Path.Pident source) in
  let decl = new_declaration (Some source_lev) (Some destination) in
  let newtype_level = get_newtype_level () in
  env := Env.add_local_constraint source decl newtype_level !env;
  cleanup_abbrev ()

let unify_eq_set = TypePairs.create 11

let order_type_pair t1 t2 =
  if t1.id <= t2.id then (t1, t2) else (t2, t1)

let add_type_equality t1 t2 =
  TypePairs.add unify_eq_set (order_type_pair t1 t2) ()

let eq_package_path env p1 p2 =
  Path.same p1 p2 ||
  Path.same (normalize_package_path env p1) (normalize_package_path env p2)

let nondep_type' = ref (fun _ _ _ -> assert false)
let package_subtype = ref (fun _ _ _ _ _ _ _ -> assert false)

let rec concat_longident lid1 =
  let open Longident in
  function
    Lident s -> Ldot (lid1, s)
  | Ldot (lid2, s) -> Ldot (concat_longident lid1 lid2, s)
  | Lapply (lid2, lid) -> Lapply (concat_longident lid1 lid2, lid)

let nondep_instance env level id ty =
  let ty = !nondep_type' env id ty in
  if level = generic_level then duplicate_type ty else
  let old = !current_level in
  current_level := level;
  let ty = instance env ty in
  current_level := old;
  ty

(* Find the type paths nl1 in the module type mty2, and add them to the
   list (nl2, tl2). raise Not_found if impossible *)
let complete_type_list ?(allow_absent=false) env nl1 lv2 mty2 nl2 tl2 =
  let id2 = Ident.create "Pkg" in
  let env' = Env.add_module id2 mty2 env in
  let rec complete nl1 ntl2 =
    match nl1, ntl2 with
      [], _ -> ntl2
    | n :: nl, (n2, _ as nt2) :: ntl' when n >= n2 ->
        nt2 :: complete (if n = n2 then nl else nl1) ntl'
    | n :: nl, _ ->
        try
          let (_, decl) =
            Env.lookup_type (concat_longident (Longident.Lident "Pkg") n) env'
          in
          match decl with
            {type_arity = 0; type_kind = Type_abstract;
             type_private = Public; type_manifest = Some t2} ->
               (n, nondep_instance env' lv2 id2 t2) :: complete nl ntl2
          | {type_arity = 0; type_kind = Type_abstract;
             type_private = Public; type_manifest = None} when allow_absent ->
               complete nl ntl2
          | _ -> raise Exit
        with
        | Not_found when allow_absent -> complete nl ntl2
        | Exit -> raise Not_found
  in
  complete nl1 (List.combine nl2 tl2)

(* raise Not_found rather than Unify if the module types are incompatible *)
let unify_package env unify_list lv1 p1 n1 tl1 lv2 p2 n2 tl2 =
  let ntl2 = complete_type_list env n1 lv2 (Mty_ident p2) n2 tl2
  and ntl1 = complete_type_list env n2 lv2 (Mty_ident p1) n1 tl1 in
  unify_list (List.map snd ntl1) (List.map snd ntl2);
  if eq_package_path env p1 p2
  || !package_subtype env p1 n1 tl1 p2 n2 tl2
  && !package_subtype env p2 n2 tl2 p1 n1 tl1 then () else raise Not_found


let unify_eq env t1 t2 =
  t1 == t2 ||
  match !umode with
  | Expression -> false
  | Pattern ->
      try TypePairs.find unify_eq_set (order_type_pair t1 t2); true
      with Not_found -> false

let rec unify (env:Env.t ref) t1 t2 =
  (* First step: special cases (optimizations) *)
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if unify_eq !env t1 t2 then () else
  let reset_tracing = check_trace_gadt_instances !env in

  try
    type_changed := true;
    begin match (t1.desc, t2.desc) with
      (Tvar _, Tconstr _) when deep_occur t1 t2 ->
        unify2 env t1 t2
    | (Tconstr _, Tvar _) when deep_occur t2 t1 ->
        unify2 env t1 t2
    | (Tvar _, _) ->
        occur !env t1 t2;
        occur_univar !env t2;
        link_type t1 t2;
        update_level !env t1.level t2
    | (_, Tvar _) ->
        occur !env t2 t1;
        occur_univar !env t1;
        link_type t2 t1;
        update_level !env t2.level t1
    | (Tunivar _, Tunivar _) ->
        unify_univar t1 t2 !univar_pairs;
        update_level !env t1.level t2;
        link_type t1 t2
    | (Tconstr (p1, [], a1), Tconstr (p2, [], a2))
          when Path.same p1 p2 (* && actual_mode !env = Old *)
            (* This optimization assumes that t1 does not expand to t2
               (and conversely), so we fall back to the general case
               when any of the types has a cached expansion. *)
            && not (has_cached_expansion p1 !a1
                 || has_cached_expansion p2 !a2) ->
        update_level !env t1.level t2;
        link_type t1 t2
    | (Tconstr (p1, [], _), Tconstr (p2, [], _))
      when Env.has_local_constraints !env
      && is_newtype !env p1 && is_newtype !env p2 ->
        (* Do not use local constraints more than necessary *)
        begin try
          if find_newtype_level !env p1 < find_newtype_level !env p2 then
            unify env t1 (try_expand_once !env t2)
          else
            unify env (try_expand_once !env t1) t2
        with Cannot_expand ->
          unify2 env t1 t2
        end
    | _ ->
        unify2 env t1 t2
    end;
    reset_trace_gadt_instances reset_tracing;
  with Unify trace ->
    reset_trace_gadt_instances reset_tracing;
    raise (Unify ((t1, t2)::trace))

and unify2 env t1 t2 =
  (* Second step: expansion of abbreviations *)
  let rec expand_both t1'' t2'' =
    let t1' = expand_head_unif !env t1 in
    let t2' = expand_head_unif !env t2 in
    (* Expansion may have changed the representative of the types... *)
    if unify_eq !env t1' t1'' && unify_eq !env t2' t2'' then (t1',t2') else
    expand_both t1' t2'
  in
  let t1', t2' = expand_both t1 t2 in
  let lv = min t1'.level t2'.level in
  update_level !env lv t2;
  update_level !env lv t1;
  if unify_eq !env t1' t2' then () else

  let t1 = repr t1 and t2 = repr t2 in
  if !trace_gadt_instances then begin
    (* All types in chains already have the same ambiguity levels *)
    let ilevel t =
      match Env.gadt_instance_level !env t with None -> 0 | Some lv -> lv in
    let lv1 = ilevel t1 and lv2 = ilevel t2 in
    if lv1 > lv2 then Env.add_gadt_instance_chain !env lv1 t2 else
    if lv2 > lv1 then Env.add_gadt_instance_chain !env lv2 t1
  end;
  let t1, t2 =
    if !Clflags.principal
    && (find_lowest_level t1' < lv || find_lowest_level t2' < lv) then
      (* Expand abbreviations hiding a lower level *)
      (* Should also do it for parameterized types, after unification... *)
      (match t1.desc with Tconstr (_, [], _) -> t1' | _ -> t1),
      (match t2.desc with Tconstr (_, [], _) -> t2' | _ -> t2)
    else (t1, t2)
  in
  if unify_eq !env t1 t1' || not (unify_eq !env t2 t2') then
    unify3 env t1 t1' t2 t2'
  else
    try unify3 env t2 t2' t1 t1' with Unify trace ->
      raise (Unify (List.map (fun (x, y) -> (y, x)) trace))

and unify3 env t1 t1' t2 t2' =
  (* Third step: truly unification *)
  (* Assumes either [t1 == t1'] or [t2 != t2'] *)
  let d1 = t1'.desc and d2 = t2'.desc in
  let create_recursion = (t2 != t2') && (deep_occur t1' t2) in

  begin match (d1, d2) with (* handle vars and univars specially *)
    (Tunivar _, Tunivar _) ->
      unify_univar t1' t2' !univar_pairs;
      link_type t1' t2'
  | (Tvar _, _) ->
      occur !env t1' t2;
      occur_univar !env t2;
      link_type t1' t2;
  | (_, Tvar _) ->
      occur !env t2' t1;
      occur_univar !env t1;
      link_type t2' t1;
  | (Tfield _, Tfield _) -> (* special case for GADTs *)
      unify_fields env t1' t2'
  | _ ->
    begin match !umode with
    | Expression ->
        occur !env t1' t2';
        link_type t1' t2
    | Pattern ->
        add_type_equality t1' t2'
    end;
    try
      begin match (d1, d2) with
        (Tarrow (l1, t1, u1, c1), Tarrow (l2, t2, u2, c2)) when l1 = l2 ||
        !Clflags.classic && not (is_optional l1 || is_optional l2) ->
          unify  env t1 t2; unify env  u1 u2;
          begin match commu_repr c1, commu_repr c2 with
            Clink r, c2 -> set_commu r c2
          | c1, Clink r -> set_commu r c1
          | _ -> ()
          end
      | (Ttuple tl1, Ttuple tl2) ->
          unify_list env tl1 tl2
      | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _)) when Path.same p1 p2 ->
          if !umode = Expression || not !generate_equations then
            unify_list env tl1 tl2
          else if !assume_injective then
            set_mode_pattern ~generate:true ~injective:false
                             (fun () -> unify_list env tl1 tl2)
          else if in_current_module p1 (* || in_pervasives p1 *)
                  || List.exists (expands_to_datatype !env) [t1'; t1; t2] then
            unify_list env tl1 tl2
          else
            let inj =
              try List.map Variance.(mem Inj)
                    (Env.find_type p1 !env).type_variance
              with Not_found -> List.map (fun _ -> false) tl1
            in
            List.iter2
              (fun i (t1, t2) ->
                if i then unify env t1 t2 else
                set_mode_pattern ~generate:false ~injective:false
                  begin fun () ->
                    let snap = snapshot () in
                    try unify env t1 t2 with Unify _ ->
                      backtrack snap;
                      reify env t1; reify env t2
                  end)
              inj (List.combine tl1 tl2)
      | (Tconstr ((Path.Pident p) as path,[],_),
         Tconstr ((Path.Pident p') as path',[],_))
        when is_newtype !env path && is_newtype !env path'
        && !generate_equations ->
          let source,destination =
            if find_newtype_level !env path > find_newtype_level !env path'
            then  p,t2'
            else  p',t1'
          in add_gadt_equation env source destination
      | (Tconstr ((Path.Pident p) as path,[],_), _)
        when is_newtype !env path && !generate_equations ->
          reify env t2';
          (* local_non_recursive_abbrev !env (Path.Pident p) t2'; *)
          add_gadt_equation env p t2'
      | (_, Tconstr ((Path.Pident p) as path,[],_))
        when is_newtype !env path && !generate_equations ->
          reify env t1' ;
          (* local_non_recursive_abbrev !env (Path.Pident p) t1'; *)
          add_gadt_equation env p t1'
      | (Tconstr (_,_,_), _) | (_, Tconstr (_,_,_)) when !umode = Pattern ->
          reify env t1';
          reify env t2';
          if !generate_equations then mcomp !env t1' t2'
      | (Tobject (fi1, nm1), Tobject (fi2, _)) ->
          unify_fields env fi1 fi2;
          (* Type [t2'] may have been instantiated by [unify_fields] *)
          (* XXX One should do some kind of unification... *)
          begin match (repr t2').desc with
            Tobject (_, {contents = Some (_, va::_)}) when
              (match (repr va).desc with
                Tvar _|Tunivar _|Tnil -> true | _ -> false) -> ()
          | Tobject (_, nm2) -> set_name nm2 !nm1
          | _ -> ()
          end
      | (Tvariant row1, Tvariant row2) ->
          if !umode = Expression then
            unify_row env row1 row2
          else begin
            let snap = snapshot () in
            try unify_row env row1 row2
            with Unify _ ->
              backtrack snap;
              reify env t1';
              reify env t2';
              if !generate_equations then mcomp !env t1' t2'
          end
      | (Tfield(f,kind,_,rem), Tnil) | (Tnil, Tfield(f,kind,_,rem)) ->
          begin match field_kind_repr kind with
            Fvar r when f <> dummy_method ->
              set_kind r Fabsent;
              if d2 = Tnil then unify env rem t2'
              else unify env (newty2 rem.level Tnil) rem
          | _      -> raise (Unify [])
          end
      | (Tnil, Tnil) ->
          ()
      | (Tpoly (t1, []), Tpoly (t2, [])) ->
          unify env t1 t2
      | (Tpoly (t1, tl1), Tpoly (t2, tl2)) ->
          enter_poly !env univar_pairs t1 tl1 t2 tl2 (unify env)
      | (Tpackage (p1, n1, tl1), Tpackage (p2, n2, tl2)) ->
          begin try
            unify_package !env (unify_list env)
              t1.level p1 n1 tl1 t2.level p2 n2 tl2
          with Not_found ->
            if !umode = Expression then raise (Unify []);
            List.iter (reify env) (tl1 @ tl2);
            (* if !generate_equations then List.iter2 (mcomp !env) tl1 tl2 *)
          end
      | (_, _) ->
          raise (Unify [])
      end;
      (* XXX Commentaires + changer "create_recursion" *)
      if create_recursion then
        match t2.desc with
          Tconstr (p, tl, abbrev) ->
            forget_abbrev abbrev p;
            let t2'' = expand_head_unif !env t2 in
            if not (closed_parameterized_type tl t2'') then
              link_type (repr t2) (repr t2')
        | _ ->
            () (* t2 has already been expanded by update_level *)
    with Unify trace ->
      t1'.desc <- d1;
      raise (Unify trace)
  end

and unify_list env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (unify env) tl1 tl2

(* Build a fresh row variable for unification *)
and make_rowvar level use1 rest1 use2 rest2  =
  let set_name ty name =
    match ty.desc with
      Tvar None -> log_type ty; ty.desc <- Tvar name
    | _ -> ()
  in
  let name =
    match rest1.desc, rest2.desc with
      Tvar (Some _ as name1), Tvar (Some _ as name2) ->
        if rest1.level <= rest2.level then name1 else name2
    | Tvar (Some _ as name), _ ->
        if use2 then set_name rest2 name; name
    | _, Tvar (Some _ as name) ->
        if use1 then set_name rest2 name; name
    | _ -> None
  in
  if use1 then rest1 else
  if use2 then rest2 else newvar2 ?name level

and unify_fields env ty1 ty2 =          (* Optimization *)
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  let l1 = (repr ty1).level and l2 = (repr ty2).level in
  let va = make_rowvar (min l1 l2) (miss2=[]) rest1 (miss1=[]) rest2 in
  let d1 = rest1.desc and d2 = rest2.desc in
  try
    unify env (build_fields l1 miss1 va) rest2;
    unify env rest1 (build_fields l2 miss2 va);
    List.iter
      (fun (n, k1, t1, k2, t2) ->
        unify_kind k1 k2;
        try
          if !trace_gadt_instances then update_level !env va.level t1;
          unify env t1 t2
        with Unify trace ->
          raise (Unify ((newty (Tfield(n, k1, t1, newty Tnil)),
                         newty (Tfield(n, k2, t2, newty Tnil)))::trace)))
      pairs
  with exn ->
    log_type rest1; rest1.desc <- d1;
    log_type rest2; rest2.desc <- d2;
    raise exn

and unify_kind k1 k2 =
  let k1 = field_kind_repr k1 in
  let k2 = field_kind_repr k2 in
  if k1 == k2 then () else
  match k1, k2 with
    (Fvar r, (Fvar _ | Fpresent)) -> set_kind r k2
  | (Fpresent, Fvar r)            -> set_kind r k1
  | (Fpresent, Fpresent)          -> ()
  | _                             -> assert false

and unify_pairs mode env tpl =
  List.iter (fun (t1, t2) -> unify env t1 t2) tpl

and unify_row env row1 row2 =
  let row1 = row_repr row1 and row2 = row_repr row2 in
  let rm1 = row_more row1 and rm2 = row_more row2 in
  if unify_eq !env rm1 rm2 then () else
  let r1, r2, pairs = merge_row_fields row1.row_fields row2.row_fields in
  if r1 <> [] && r2 <> [] then begin
    let ht = Hashtbl.create (List.length r1) in
    List.iter (fun (l,_) -> Hashtbl.add ht (hash_variant l) l) r1;
    List.iter
      (fun (l,_) ->
        try raise (Tags(l, Hashtbl.find ht (hash_variant l)))
        with Not_found -> ())
      r2
  end;
  let fixed1 = row_fixed row1 and fixed2 = row_fixed row2 in
  let more =
    if fixed1 then rm1 else
    if fixed2 then rm2 else
    newty2 (min rm1.level rm2.level) (Tvar None) in
  let fixed = fixed1 || fixed2
  and closed = row1.row_closed || row2.row_closed in
  let keep switch =
    List.for_all
      (fun (_,f1,f2) ->
        let f1, f2 = switch f1 f2 in
        row_field_repr f1 = Rabsent || row_field_repr f2 <> Rabsent)
      pairs
  in
  let empty fields =
    List.for_all (fun (_,f) -> row_field_repr f = Rabsent) fields in
  (* Check whether we are going to build an empty type *)
  if closed && (empty r1 || row2.row_closed) && (empty r2 || row1.row_closed)
  && List.for_all
      (fun (_,f1,f2) ->
        row_field_repr f1 = Rabsent || row_field_repr f2 = Rabsent)
      pairs
  then raise (Unify [mkvariant [] true, mkvariant [] true]);
  let name =
    if row1.row_name <> None && (row1.row_closed || empty r2) &&
      (not row2.row_closed || keep (fun f1 f2 -> f1, f2) && empty r1)
    then row1.row_name
    else if row2.row_name <> None && (row2.row_closed || empty r1) &&
      (not row1.row_closed || keep (fun f1 f2 -> f2, f1) && empty r2)
    then row2.row_name
    else None
  in
  let row0 = {row_fields = []; row_more = more; row_bound = ();
              row_closed = closed; row_fixed = fixed; row_name = name} in
  let set_more row rest =
    let rest =
      if closed then
        filter_row_fields row.row_closed rest
      else rest in
    if rest <> [] && (row.row_closed || row_fixed row)
    || closed && row_fixed row && not row.row_closed then begin
      let t1 = mkvariant [] true and t2 = mkvariant rest false in
      raise (Unify [if row == row1 then (t1,t2) else (t2,t1)])
    end;
    (* The following test is not principal... should rather use Tnil *)
    let rm = row_more row in
    if !trace_gadt_instances && rm.desc = Tnil then () else
    if !trace_gadt_instances then
      update_level !env rm.level (newgenty (Tvariant row));
    if row_fixed row then
      if more == rm then () else
      if is_Tvar rm then link_type rm more else unify env rm more
    else
      let ty = newgenty (Tvariant {row0 with row_fields = rest}) in
      update_level !env rm.level ty;
      link_type rm ty
  in
  let md1 = rm1.desc and md2 = rm2.desc in
  begin try
    set_more row2 r1;
    set_more row1 r2;
    List.iter
      (fun (l,f1,f2) ->
        try unify_row_field env fixed1 fixed2 more l f1 f2
        with Unify trace ->
          raise (Unify ((mkvariant [l,f1] true,
                         mkvariant [l,f2] true) :: trace)))
      pairs;
  with exn ->
    log_type rm1; rm1.desc <- md1; log_type rm2; rm2.desc <- md2; raise exn
  end

and unify_row_field env fixed1 fixed2 more l f1 f2 =
  let f1 = row_field_repr f1 and f2 = row_field_repr f2 in
  if f1 == f2 then () else
  match f1, f2 with
    Rpresent(Some t1), Rpresent(Some t2) -> unify env t1 t2
  | Rpresent None, Rpresent None -> ()
  | Reither(c1, tl1, m1, e1), Reither(c2, tl2, m2, e2) ->
      if e1 == e2 then () else
      let redo =
        (m1 || m2 || fixed1 || fixed2 ||
         !rigid_variants && (List.length tl1 = 1 || List.length tl2 = 1)) &&
        begin match tl1 @ tl2 with [] -> false
        | t1 :: tl ->
            if c1 || c2 then raise (Unify []);
            List.iter (unify env t1) tl;
            !e1 <> None || !e2 <> None
        end in
      if redo then unify_row_field env fixed1 fixed2 more l f1 f2 else
      let tl1 = List.map repr tl1 and tl2 = List.map repr tl2 in
      let rec remq tl = function [] -> []
        | ty :: tl' ->
            if List.memq ty tl then remq tl tl' else ty :: remq tl tl'
      in
      let tl2' = remq tl2 tl1 and tl1' = remq tl1 tl2 in
      (* Is this handling of levels really principal? *)
      List.iter (update_level !env (repr more).level) (tl1' @ tl2');
      let e = ref None in
      let f1' = Reither(c1 || c2, tl1', m1 || m2, e)
      and f2' = Reither(c1 || c2, tl2', m1 || m2, e) in
      set_row_field e1 f1'; set_row_field e2 f2';
  | Reither(_, _, false, e1), Rabsent when not fixed1 -> set_row_field e1 f2
  | Rabsent, Reither(_, _, false, e2) when not fixed2 -> set_row_field e2 f1
  | Rabsent, Rabsent -> ()
  | Reither(false, tl, _, e1), Rpresent(Some t2) when not fixed1 ->
      set_row_field e1 f2;
      update_level !env (repr more).level t2;
      (try List.iter (fun t1 -> unify env t1 t2) tl
      with exn -> e1 := None; raise exn)
  | Rpresent(Some t1), Reither(false, tl, _, e2) when not fixed2 ->
      set_row_field e2 f1;
      update_level !env (repr more).level t1;
      (try List.iter (unify env t1) tl
      with exn -> e2 := None; raise exn)
  | Reither(true, [], _, e1), Rpresent None when not fixed1 ->
      set_row_field e1 f2
  | Rpresent None, Reither(true, [], _, e2) when not fixed2 ->
      set_row_field e2 f1
  | _ -> raise (Unify [])


let unify env ty1 ty2 =
  try
    unify env ty1 ty2
  with
    Unify trace ->
      raise (Unify (expand_trace !env trace))
  | Recursive_abbrev ->
      raise (Unification_recursive_abbrev (expand_trace !env [(ty1,ty2)]))

let unify_gadt ~newtype_level:lev (env:Env.t ref) ty1 ty2 =
  try
    univar_pairs := [];
    newtype_level := Some lev;
    set_mode_pattern ~generate:true ~injective:true
                     (fun () -> unify env ty1 ty2);
    newtype_level := None;
    TypePairs.clear unify_eq_set;
  with e ->
    TypePairs.clear unify_eq_set;
    match e with
      Unify e -> raise (Unify e)
    | e -> newtype_level := None; raise e

let unify_var env t1 t2 =
  let t1 = repr t1 and t2 = repr t2 in
  if t1 == t2 then () else
  match t1.desc with
    Tvar _ ->
      let reset_tracing = check_trace_gadt_instances env in
      begin try
        occur env t1 t2;
        update_level env t1.level t2;
        link_type t1 t2;
        reset_trace_gadt_instances reset_tracing;
      with Unify trace ->
        reset_trace_gadt_instances reset_tracing;
        let expanded_trace = expand_trace env ((t1,t2)::trace) in
        raise (Unify expanded_trace)
      end
  | _ ->
      unify (ref env) t1 t2

let _ = unify' := unify_var

let unify_pairs env ty1 ty2 pairs =
  univar_pairs := pairs;
  unify env ty1 ty2

let unify env ty1 ty2 =
  unify_pairs (ref env) ty1 ty2 []



(**** Special cases of unification ****)

let expand_head_trace env t =
  let reset_tracing = check_trace_gadt_instances env in
  let t = expand_head_unif env t in
  reset_trace_gadt_instances reset_tracing;
  t

(*
   Unify [t] and [l:'a -> 'b]. Return ['a] and ['b].
   In label mode, label mismatch is accepted when
   (1) the requested label is ""
   (2) the original label is not optional
*)

let filter_arrow env t l =
  let t = expand_head_trace env t in
  match t.desc with
    Tvar _ ->
      let lv = t.level in
      let t1 = newvar2 lv and t2 = newvar2 lv in
      let t' = newty2 lv (Tarrow (l, t1, t2, Cok)) in
      link_type t t';
      (t1, t2)
  | Tarrow(l', t1, t2, _)
    when l = l' || !Clflags.classic && l = "" && not (is_optional l') ->
      (t1, t2)
  | _ ->
      raise (Unify [])

(* Used by [filter_method]. *)
let rec filter_method_field env name priv ty =
  let ty = expand_head_trace env ty in
  match ty.desc with
    Tvar _ ->
      let level = ty.level in
      let ty1 = newvar2 level and ty2 = newvar2 level in
      let ty' = newty2 level (Tfield (name,
                                      begin match priv with
                                        Private -> Fvar (ref None)
                                      | Public  -> Fpresent
                                      end,
                                      ty1, ty2))
      in
      link_type ty ty';
      ty1
  | Tfield(n, kind, ty1, ty2) ->
      let kind = field_kind_repr kind in
      if (n = name) && (kind <> Fabsent) then begin
        if priv = Public then
          unify_kind kind Fpresent;
        ty1
      end else
        filter_method_field env name priv ty2
  | _ ->
      raise (Unify [])

(* Unify [ty] and [< name : 'a; .. >]. Return ['a]. *)
let filter_method env name priv ty =
  let ty = expand_head_trace env ty in
  match ty.desc with
    Tvar _ ->
      let ty1 = newvar () in
      let ty' = newobj ty1 in
      update_level env ty.level ty';
      link_type ty ty';
      filter_method_field env name priv ty1
  | Tobject(f, _) ->
      filter_method_field env name priv f
  | _ ->
      raise (Unify [])

let check_filter_method env name priv ty =
  ignore(filter_method env name priv ty)

let filter_self_method env lab priv meths ty =
  let ty' = filter_method env lab priv ty in
  try
    Meths.find lab !meths
  with Not_found ->
    let pair = (Ident.create lab, ty') in
    meths := Meths.add lab pair !meths;
    pair


                        (***********************************)
                        (*  Matching between type schemes  *)
                        (***********************************)

(*
   Update the level of [ty]. First check that the levels of generic
   variables from the subject are not lowered.
*)
let moregen_occur env level ty =
  let rec occur ty =
    let ty = repr ty in
    if ty.level > level then begin
      if is_Tvar ty && ty.level >= generic_level - 1 then raise Occur;
      ty.level <- pivot_level - ty.level;
      match ty.desc with
        Tvariant row when static_row row ->
          iter_row occur row
      | _ ->
          iter_type_expr occur ty
    end
  in
  begin try
    occur ty; unmark_type ty
  with Occur ->
    unmark_type ty; raise (Unify [])
  end;
  (* also check for free univars *)
  occur_univar env ty;
  update_level env level ty

let may_instantiate inst_nongen t1 =
  if inst_nongen then t1.level <> generic_level - 1
                 else t1.level =  generic_level

let rec moregen inst_nongen type_pairs env t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else

  try
    match (t1.desc, t2.desc) with
      (Tvar _, _) when may_instantiate inst_nongen t1 ->
        moregen_occur env t1.level t2;
        occur env t1 t2;
        link_type t1 t2
    | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
        ()
    | _ ->
        let t1' = expand_head env t1 in
        let t2' = expand_head env t2 in
        (* Expansion may have changed the representative of the types... *)
        let t1' = repr t1' and t2' = repr t2' in
        if t1' == t2' then () else
        begin try
          TypePairs.find type_pairs (t1', t2')
        with Not_found ->
          TypePairs.add type_pairs (t1', t2') ();
          match (t1'.desc, t2'.desc) with
            (Tvar _, _) when may_instantiate inst_nongen t1' ->
              moregen_occur env t1'.level t2;
              link_type t1' t2
          | (Tarrow (l1, t1, u1, _), Tarrow (l2, t2, u2, _)) when l1 = l2
            || !Clflags.classic && not (is_optional l1 || is_optional l2) ->
              moregen inst_nongen type_pairs env t1 t2;
              moregen inst_nongen type_pairs env u1 u2
          | (Ttuple tl1, Ttuple tl2) ->
              moregen_list inst_nongen type_pairs env tl1 tl2
          | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _))
                when Path.same p1 p2 ->
              moregen_list inst_nongen type_pairs env tl1 tl2
          | (Tpackage (p1, n1, tl1), Tpackage (p2, n2, tl2)) ->
              begin try
                unify_package env (moregen_list inst_nongen type_pairs env)
                  t1'.level p1 n1 tl1 t2'.level p2 n2 tl2
              with Not_found -> raise (Unify [])
              end
          | (Tvariant row1, Tvariant row2) ->
              moregen_row inst_nongen type_pairs env row1 row2
          | (Tobject (fi1, nm1), Tobject (fi2, nm2)) ->
              moregen_fields inst_nongen type_pairs env fi1 fi2
          | (Tfield _, Tfield _) ->           (* Actually unused *)
              moregen_fields inst_nongen type_pairs env t1' t2'
          | (Tnil, Tnil) ->
              ()
          | (Tpoly (t1, []), Tpoly (t2, [])) ->
              moregen inst_nongen type_pairs env t1 t2
          | (Tpoly (t1, tl1), Tpoly (t2, tl2)) ->
              enter_poly env univar_pairs t1 tl1 t2 tl2
                (moregen inst_nongen type_pairs env)
          | (Tunivar _, Tunivar _) ->
              unify_univar t1' t2' !univar_pairs
          | (_, _) ->
              raise (Unify [])
        end
  with Unify trace ->
    raise (Unify ((t1, t2)::trace))

and moregen_list inst_nongen type_pairs env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (moregen inst_nongen type_pairs env) tl1 tl2

and moregen_fields inst_nongen type_pairs env ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1
  and (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  if miss1 <> [] then raise (Unify []);
  moregen inst_nongen type_pairs env rest1
    (build_fields (repr ty2).level miss2 rest2);
  List.iter
    (fun (n, k1, t1, k2, t2) ->
       moregen_kind k1 k2;
       try moregen inst_nongen type_pairs env t1 t2 with Unify trace ->
         raise (Unify ((newty (Tfield(n, k1, t1, rest2)),
                        newty (Tfield(n, k2, t2, rest2)))::trace)))
    pairs

and moregen_kind k1 k2 =
  let k1 = field_kind_repr k1 in
  let k2 = field_kind_repr k2 in
  if k1 == k2 then () else
  match k1, k2 with
    (Fvar r, (Fvar _ | Fpresent))  -> set_kind r k2
  | (Fpresent, Fpresent)           -> ()
  | _                              -> raise (Unify [])

and moregen_row inst_nongen type_pairs env row1 row2 =
  let row1 = row_repr row1 and row2 = row_repr row2 in
  let rm1 = repr row1.row_more and rm2 = repr row2.row_more in
  if rm1 == rm2 then () else
  let may_inst =
    is_Tvar rm1 && may_instantiate inst_nongen rm1 || rm1.desc = Tnil in
  let r1, r2, pairs = merge_row_fields row1.row_fields row2.row_fields in
  let r1, r2 =
    if row2.row_closed then
      filter_row_fields may_inst r1, filter_row_fields false r2
    else r1, r2
  in
  if r1 <> [] || row1.row_closed && (not row2.row_closed || r2 <> [])
  then raise (Unify []);
  begin match rm1.desc, rm2.desc with
    Tunivar _, Tunivar _ ->
      unify_univar rm1 rm2 !univar_pairs
  | Tunivar _, _ | _, Tunivar _ ->
      raise (Unify [])
  | _ when static_row row1 -> ()
  | _ when may_inst ->
      let ext = newgenty (Tvariant {row2 with row_fields = r2}) in
      moregen_occur env rm1.level ext;
      link_type rm1 ext
  | Tconstr _, Tconstr _ ->
      moregen inst_nongen type_pairs env rm1 rm2
  | _ -> raise (Unify [])
  end;
  List.iter
    (fun (l,f1,f2) ->
      let f1 = row_field_repr f1 and f2 = row_field_repr f2 in
      if f1 == f2 then () else
      match f1, f2 with
        Rpresent(Some t1), Rpresent(Some t2) ->
          moregen inst_nongen type_pairs env t1 t2
      | Rpresent None, Rpresent None -> ()
      | Reither(false, tl1, _, e1), Rpresent(Some t2) when may_inst ->
          set_row_field e1 f2;
          List.iter (fun t1 -> moregen inst_nongen type_pairs env t1 t2) tl1
      | Reither(c1, tl1, _, e1), Reither(c2, tl2, m2, e2) ->
          if e1 != e2 then begin
            if c1 && not c2 then raise(Unify []);
            set_row_field e1 (Reither (c2, [], m2, e2));
            if List.length tl1 = List.length tl2 then
              List.iter2 (moregen inst_nongen type_pairs env) tl1 tl2
            else match tl2 with
              t2 :: _ ->
                List.iter (fun t1 -> moregen inst_nongen type_pairs env t1 t2)
                  tl1
            | [] ->
                if tl1 <> [] then raise (Unify [])
          end
      | Reither(true, [], _, e1), Rpresent None when may_inst ->
          set_row_field e1 f2
      | Reither(_, _, _, e1), Rabsent when may_inst ->
          set_row_field e1 f2
      | Rabsent, Rabsent -> ()
      | _ -> raise (Unify []))
    pairs

(* Must empty univar_pairs first *)
let moregen inst_nongen type_pairs env patt subj =
  univar_pairs := [];
  moregen inst_nongen type_pairs env patt subj

(*
   Non-generic variable can be instanciated only if [inst_nongen] is
   true. So, [inst_nongen] should be set to false if the subject might
   contain non-generic variables (and we do not want them to be
   instanciated).
   Usually, the subject is given by the user, and the pattern
   is unimportant.  So, no need to propagate abbreviations.
*)
let moregeneral env inst_nongen pat_sch subj_sch =
  let old_level = !current_level in
  current_level := generic_level - 1;
  (*
     Generic variables are first duplicated with [instance].  So,
     their levels are lowered to [generic_level - 1].  The subject is
     then copied with [duplicate_type].  That way, its levels won't be
     changed.
  *)
  let subj = duplicate_type (instance env subj_sch) in
  current_level := generic_level;
  (* Duplicate generic variables *)
  let patt = instance env pat_sch in
  let res =
    try moregen inst_nongen (TypePairs.create 13) env patt subj; true with
      Unify _ -> false
  in
  current_level := old_level;
  res


(* Alternative approach: "rigidify" a type scheme,
   and check validity after unification *)
(* Simpler, no? *)

let rec rigidify_rec vars ty =
  let ty = repr ty in
  if ty.level >= lowest_level then begin
    ty.level <- pivot_level - ty.level;
    match ty.desc with
    | Tvar _ ->
        if not (List.memq ty !vars) then vars := ty :: !vars
    | Tvariant row ->
        let row = row_repr row in
        let more = repr row.row_more in
        if is_Tvar more && not (row_fixed row) then begin
          let more' = newty2 more.level more.desc in
          let row' = {row with row_fixed=true; row_fields=[]; row_more=more'}
          in link_type more (newty2 ty.level (Tvariant row'))
        end;
        iter_row (rigidify_rec vars) row;
        (* only consider the row variable if the variant is not static *)
        if not (static_row row) then rigidify_rec vars (row_more row)
    | _ ->
        iter_type_expr (rigidify_rec vars) ty
  end

let rigidify ty =
  let vars = ref [] in
  rigidify_rec vars ty;
  unmark_type ty;
  !vars

let all_distinct_vars env vars =
  let tyl = ref [] in
  List.for_all
    (fun ty ->
      let ty = expand_head env ty in
      if List.memq ty !tyl then false else
      (tyl := ty :: !tyl; is_Tvar ty))
    vars

let matches env ty ty' =
  let snap = snapshot () in
  let vars = rigidify ty in
  cleanup_abbrev ();
  let ok =
    try unify env ty ty'; all_distinct_vars env vars
    with Unify _ -> false
  in
  backtrack snap;
  ok


                 (*********************************************)
                 (*  Equivalence between parameterized types  *)
                 (*********************************************)

let rec get_object_row ty =
  match repr ty with
  | {desc=Tfield (_, _, _, tl)} -> get_object_row tl
  | ty -> ty

let expand_head_rigid env ty =
  let old = !rigid_variants in
  rigid_variants := true;
  let ty' = expand_head env ty in
  rigid_variants := old; ty'

let normalize_subst subst =
  if List.exists
      (function {desc=Tlink _}, _ | _, {desc=Tlink _} -> true | _ -> false)
      !subst
  then subst := List.map (fun (t1,t2) -> repr t1, repr t2) !subst

let rec eqtype rename type_pairs subst env t1 t2 =
  if t1 == t2 then () else
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else

  try
    match (t1.desc, t2.desc) with
      (Tvar _, Tvar _) when rename ->
        begin try
          normalize_subst subst;
          if List.assq t1 !subst != t2 then raise (Unify [])
        with Not_found ->
          if List.exists (fun (_, t) -> t == t2) !subst then raise (Unify []);
          subst := (t1, t2) :: !subst
        end
    | (Tconstr (p1, [], _), Tconstr (p2, [], _)) when Path.same p1 p2 ->
        ()
    | _ ->
        let t1' = expand_head_rigid env t1 in
        let t2' = expand_head_rigid env t2 in
        (* Expansion may have changed the representative of the types... *)
        let t1' = repr t1' and t2' = repr t2' in
        if t1' == t2' then () else
        begin try
          TypePairs.find type_pairs (t1', t2')
        with Not_found ->
          TypePairs.add type_pairs (t1', t2') ();
          match (t1'.desc, t2'.desc) with
            (Tvar _, Tvar _) when rename ->
              begin try
                normalize_subst subst;
                if List.assq t1' !subst != t2' then raise (Unify [])
              with Not_found ->
                if List.exists (fun (_, t) -> t == t2') !subst
                then raise (Unify []);
                subst := (t1', t2') :: !subst
              end
          | (Tarrow (l1, t1, u1, _), Tarrow (l2, t2, u2, _)) when l1 = l2
            || !Clflags.classic && not (is_optional l1 || is_optional l2) ->
              eqtype rename type_pairs subst env t1 t2;
              eqtype rename type_pairs subst env u1 u2;
          | (Ttuple tl1, Ttuple tl2) ->
              eqtype_list rename type_pairs subst env tl1 tl2
          | (Tconstr (p1, tl1, _), Tconstr (p2, tl2, _))
                when Path.same p1 p2 ->
              eqtype_list rename type_pairs subst env tl1 tl2
          | (Tpackage (p1, n1, tl1), Tpackage (p2, n2, tl2)) ->
              begin try
                unify_package env (eqtype_list rename type_pairs subst env)
                  t1'.level p1 n1 tl1 t2'.level p2 n2 tl2
              with Not_found -> raise (Unify [])
              end
          | (Tvariant row1, Tvariant row2) ->
              eqtype_row rename type_pairs subst env row1 row2
          | (Tobject (fi1, nm1), Tobject (fi2, nm2)) ->
              eqtype_fields rename type_pairs subst env fi1 fi2
          | (Tfield _, Tfield _) ->       (* Actually unused *)
              eqtype_fields rename type_pairs subst env t1' t2'
          | (Tnil, Tnil) ->
              ()
          | (Tpoly (t1, []), Tpoly (t2, [])) ->
              eqtype rename type_pairs subst env t1 t2
          | (Tpoly (t1, tl1), Tpoly (t2, tl2)) ->
              enter_poly env univar_pairs t1 tl1 t2 tl2
                (eqtype rename type_pairs subst env)
          | (Tunivar _, Tunivar _) ->
              unify_univar t1' t2' !univar_pairs
          | (_, _) ->
              raise (Unify [])
        end
  with Unify trace ->
    raise (Unify ((t1, t2)::trace))

and eqtype_list rename type_pairs subst env tl1 tl2 =
  if List.length tl1 <> List.length tl2 then
    raise (Unify []);
  List.iter2 (eqtype rename type_pairs subst env) tl1 tl2

and eqtype_fields rename type_pairs subst env ty1 ty2 =
  let (fields1, rest1) = flatten_fields ty1 in
  let (fields2, rest2) = flatten_fields ty2 in
  (* First check if same row => already equal *)
  let same_row =
    rest1 == rest2 || TypePairs.mem type_pairs (rest1,rest2) ||
    (rename && List.mem (rest1, rest2) !subst)
  in
  if same_row then () else
  (* Try expansion, needed when called from Includecore.type_manifest *)
  match expand_head_rigid env rest2 with
    {desc=Tobject(ty2,_)} -> eqtype_fields rename type_pairs subst env ty1 ty2
  | _ ->
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  eqtype rename type_pairs subst env rest1 rest2;
  if (miss1 <> []) || (miss2 <> []) then raise (Unify []);
  List.iter
    (function (n, k1, t1, k2, t2) ->
       eqtype_kind k1 k2;
       try eqtype rename type_pairs subst env t1 t2 with Unify trace ->
         raise (Unify ((newty (Tfield(n, k1, t1, rest2)),
                        newty (Tfield(n, k2, t2, rest2)))::trace)))
    pairs

and eqtype_kind k1 k2 =
  let k1 = field_kind_repr k1 in
  let k2 = field_kind_repr k2 in
  match k1, k2 with
    (Fvar _, Fvar _)
  | (Fpresent, Fpresent) -> ()
  | _                    -> raise (Unify [])

and eqtype_row rename type_pairs subst env row1 row2 =
  (* Try expansion, needed when called from Includecore.type_manifest *)
  match expand_head_rigid env (row_more row2) with
    {desc=Tvariant row2} -> eqtype_row rename type_pairs subst env row1 row2
  | _ ->
  let row1 = row_repr row1 and row2 = row_repr row2 in
  let r1, r2, pairs = merge_row_fields row1.row_fields row2.row_fields in
  if row1.row_closed <> row2.row_closed
  || not row1.row_closed && (r1 <> [] || r2 <> [])
  || filter_row_fields false (r1 @ r2) <> []
  then raise (Unify []);
  if not (static_row row1) then
    eqtype rename type_pairs subst env row1.row_more row2.row_more;
  List.iter
    (fun (_,f1,f2) ->
      match row_field_repr f1, row_field_repr f2 with
        Rpresent(Some t1), Rpresent(Some t2) ->
          eqtype rename type_pairs subst env t1 t2
      | Reither(true, [], _, _), Reither(true, [], _, _) ->
          ()
      | Reither(false, t1::tl1, _, _), Reither(false, t2::tl2, _, _) ->
          eqtype rename type_pairs subst env t1 t2;
          if List.length tl1 = List.length tl2 then
            (* if same length allow different types (meaning?) *)
            List.iter2 (eqtype rename type_pairs subst env) tl1 tl2
          else begin
            (* otherwise everything must be equal *)
            List.iter (eqtype rename type_pairs subst env t1) tl2;
            List.iter (fun t1 -> eqtype rename type_pairs subst env t1 t2) tl1
          end
      | Rpresent None, Rpresent None -> ()
      | Rabsent, Rabsent -> ()
      | _ -> raise (Unify []))
    pairs

(* Two modes: with or without renaming of variables *)
let equal env rename tyl1 tyl2 =
  try
    univar_pairs := [];
    eqtype_list rename (TypePairs.create 11) (ref []) env tyl1 tyl2; true
  with
    Unify _ -> false

(* Must empty univar_pairs first *)
let eqtype rename type_pairs subst env t1 t2 =
  univar_pairs := [];
  eqtype rename type_pairs subst env t1 t2


                          (*************************)
                          (*  Class type matching  *)
                          (*************************)


type class_match_failure =
    CM_Virtual_class
  | CM_Parameter_arity_mismatch of int * int
  | CM_Type_parameter_mismatch of Env.t * (type_expr * type_expr) list
  | CM_Class_type_mismatch of Env.t * class_type * class_type
  | CM_Parameter_mismatch of Env.t * (type_expr * type_expr) list
  | CM_Val_type_mismatch of string * Env.t * (type_expr * type_expr) list
  | CM_Meth_type_mismatch of string * Env.t * (type_expr * type_expr) list
  | CM_Non_mutable_value of string
  | CM_Non_concrete_value of string
  | CM_Missing_value of string
  | CM_Missing_method of string
  | CM_Hide_public of string
  | CM_Hide_virtual of string * string
  | CM_Public_method of string
  | CM_Private_method of string
  | CM_Virtual_method of string

exception Failure of class_match_failure list

let rec moregen_clty trace type_pairs env cty1 cty2 =
  try
    match cty1, cty2 with
      Cty_constr (_, _, cty1), _ ->
        moregen_clty true type_pairs env cty1 cty2
    | _, Cty_constr (_, _, cty2) ->
        moregen_clty true type_pairs env cty1 cty2
    | Cty_arrow (l1, ty1, cty1'), Cty_arrow (l2, ty2, cty2') when l1 = l2 ->
        begin try moregen true type_pairs env ty1 ty2 with Unify trace ->
          raise (Failure [CM_Parameter_mismatch (env, expand_trace env trace)])
        end;
        moregen_clty false type_pairs env cty1' cty2'
    | Cty_signature sign1, Cty_signature sign2 ->
        let ty1 = object_fields (repr sign1.csig_self) in
        let ty2 = object_fields (repr sign2.csig_self) in
        let (fields1, rest1) = flatten_fields ty1
        and (fields2, rest2) = flatten_fields ty2 in
        let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
        List.iter
          (fun (lab, k1, t1, k2, t2) ->
            begin try moregen true type_pairs env t1 t2 with Unify trace ->
              raise (Failure [CM_Meth_type_mismatch
                                 (lab, env, expand_trace env trace)])
           end)
        pairs;
      Vars.iter
        (fun lab (mut, v, ty) ->
           let (mut', v', ty') = Vars.find lab sign1.csig_vars in
           try moregen true type_pairs env ty' ty with Unify trace ->
             raise (Failure [CM_Val_type_mismatch
                                (lab, env, expand_trace env trace)]))
        sign2.csig_vars
  | _ ->
      raise (Failure [])
  with
    Failure error when trace || error = [] ->
      raise (Failure (CM_Class_type_mismatch (env, cty1, cty2)::error))

let match_class_types ?(trace=true) env pat_sch subj_sch =
  let type_pairs = TypePairs.create 53 in
  let old_level = !current_level in
  current_level := generic_level - 1;
  (*
     Generic variables are first duplicated with [instance].  So,
     their levels are lowered to [generic_level - 1].  The subject is
     then copied with [duplicate_type].  That way, its levels won't be
     changed.
  *)
  let (_, subj_inst) = instance_class [] subj_sch in
  let subj = duplicate_class_type subj_inst in
  current_level := generic_level;
  (* Duplicate generic variables *)
  let (_, patt) = instance_class [] pat_sch in
  let res =
    let sign1 = signature_of_class_type patt in
    let sign2 = signature_of_class_type subj in
    let t1 = repr sign1.csig_self in
    let t2 = repr sign2.csig_self in
    TypePairs.add type_pairs (t1, t2) ();
    let (fields1, rest1) = flatten_fields (object_fields t1)
    and (fields2, rest2) = flatten_fields (object_fields t2) in
    let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
    let error =
      List.fold_right
        (fun (lab, k, _) err ->
           let err =
             let k = field_kind_repr k in
             begin match k with
               Fvar r -> set_kind r Fabsent; err
             | _      -> CM_Hide_public lab::err
             end
           in
           if Concr.mem lab sign1.csig_concr then err
           else CM_Hide_virtual ("method", lab) :: err)
        miss1 []
    in
    let missing_method = List.map (fun (m, _, _) -> m) miss2 in
    let error =
      (List.map (fun m -> CM_Missing_method m) missing_method) @ error
    in
    (* Always succeeds *)
    moregen true type_pairs env rest1 rest2;
    let error =
      List.fold_right
        (fun (lab, k1, t1, k2, t2) err ->
           try moregen_kind k1 k2; err with
             Unify _ -> CM_Public_method lab::err)
        pairs error
    in
    let error =
      Vars.fold
        (fun lab (mut, vr, ty) err ->
          try
            let (mut', vr', ty') = Vars.find lab sign1.csig_vars in
            if mut = Mutable && mut' <> Mutable then
              CM_Non_mutable_value lab::err
            else if vr = Concrete && vr' <> Concrete then
              CM_Non_concrete_value lab::err
            else
              err
          with Not_found ->
            CM_Missing_value lab::err)
        sign2.csig_vars error
    in
    let error =
      Vars.fold
        (fun lab (_,vr,_) err ->
          if vr = Virtual && not (Vars.mem lab sign2.csig_vars) then
            CM_Hide_virtual ("instance variable", lab) :: err
          else err)
        sign1.csig_vars error
    in
    let error =
      List.fold_right
        (fun e l ->
           if List.mem e missing_method then l else CM_Virtual_method e::l)
        (Concr.elements (Concr.diff sign2.csig_concr sign1.csig_concr))
        error
    in
    match error with
      [] ->
        begin try
          moregen_clty trace type_pairs env patt subj;
          []
        with
          Failure r -> r
        end
    | error ->
        CM_Class_type_mismatch (env, patt, subj)::error
  in
  current_level := old_level;
  res

let rec equal_clty trace type_pairs subst env cty1 cty2 =
  try
    match cty1, cty2 with
      Cty_constr (_, _, cty1), Cty_constr (_, _, cty2) ->
        equal_clty true type_pairs subst env cty1 cty2
    | Cty_constr (_, _, cty1), _ ->
        equal_clty true type_pairs subst env cty1 cty2
    | _, Cty_constr (_, _, cty2) ->
        equal_clty true type_pairs subst env cty1 cty2
    | Cty_arrow (l1, ty1, cty1'), Cty_arrow (l2, ty2, cty2') when l1 = l2 ->
        begin try eqtype true type_pairs subst env ty1 ty2 with Unify trace ->
          raise (Failure [CM_Parameter_mismatch (env, expand_trace env trace)])
        end;
        equal_clty false type_pairs subst env cty1' cty2'
    | Cty_signature sign1, Cty_signature sign2 ->
        let ty1 = object_fields (repr sign1.csig_self) in
        let ty2 = object_fields (repr sign2.csig_self) in
        let (fields1, rest1) = flatten_fields ty1
        and (fields2, rest2) = flatten_fields ty2 in
        let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
        List.iter
          (fun (lab, k1, t1, k2, t2) ->
             begin try eqtype true type_pairs subst env t1 t2 with
               Unify trace ->
                 raise (Failure [CM_Meth_type_mismatch
                                    (lab, env, expand_trace env trace)])
             end)
          pairs;
        Vars.iter
          (fun lab (_, _, ty) ->
             let (_, _, ty') = Vars.find lab sign1.csig_vars in
             try eqtype true type_pairs subst env ty' ty with Unify trace ->
               raise (Failure [CM_Val_type_mismatch
                                  (lab, env, expand_trace env trace)]))
          sign2.csig_vars
    | _ ->
        raise
          (Failure (if trace then []
                    else [CM_Class_type_mismatch (env, cty1, cty2)]))
  with
    Failure error when trace ->
      raise (Failure (CM_Class_type_mismatch (env, cty1, cty2)::error))

let match_class_declarations env patt_params patt_type subj_params subj_type =
  let type_pairs = TypePairs.create 53 in
  let subst = ref [] in
  let sign1 = signature_of_class_type patt_type in
  let sign2 = signature_of_class_type subj_type in
  let t1 = repr sign1.csig_self in
  let t2 = repr sign2.csig_self in
  TypePairs.add type_pairs (t1, t2) ();
  let (fields1, rest1) = flatten_fields (object_fields t1)
  and (fields2, rest2) = flatten_fields (object_fields t2) in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  let error =
    List.fold_right
      (fun (lab, k, _) err ->
        let err =
          let k = field_kind_repr k in
          begin match k with
            Fvar r -> err
          | _      -> CM_Hide_public lab::err
          end
        in
        if Concr.mem lab sign1.csig_concr then err
        else CM_Hide_virtual ("method", lab) :: err)
      miss1 []
  in
  let missing_method = List.map (fun (m, _, _) -> m) miss2 in
  let error =
    (List.map (fun m -> CM_Missing_method m) missing_method) @ error
  in
  (* Always succeeds *)
  eqtype true type_pairs subst env rest1 rest2;
  let error =
    List.fold_right
      (fun (lab, k1, t1, k2, t2) err ->
        let k1 = field_kind_repr k1 in
        let k2 = field_kind_repr k2 in
        match k1, k2 with
          (Fvar _, Fvar _)
        | (Fpresent, Fpresent) -> err
        | (Fvar _, Fpresent)   -> CM_Private_method lab::err
        | (Fpresent, Fvar _)  -> CM_Public_method lab::err
        | _                    -> assert false)
      pairs error
  in
  let error =
    Vars.fold
      (fun lab (mut, vr, ty) err ->
         try
           let (mut', vr', ty') = Vars.find lab sign1.csig_vars in
           if mut = Mutable && mut' <> Mutable then
             CM_Non_mutable_value lab::err
           else if vr = Concrete && vr' <> Concrete then
             CM_Non_concrete_value lab::err
           else
             err
         with Not_found ->
           CM_Missing_value lab::err)
      sign2.csig_vars error
  in
  let error =
    Vars.fold
      (fun lab (_,vr,_) err ->
        if vr = Virtual && not (Vars.mem lab sign2.csig_vars) then
          CM_Hide_virtual ("instance variable", lab) :: err
        else err)
      sign1.csig_vars error
  in
  let error =
    List.fold_right
      (fun e l ->
        if List.mem e missing_method then l else CM_Virtual_method e::l)
      (Concr.elements (Concr.diff sign2.csig_concr sign1.csig_concr))
      error
  in
  match error with
    [] ->
      begin try
        let lp = List.length patt_params in
        let ls = List.length subj_params in
        if lp  <> ls then
          raise (Failure [CM_Parameter_arity_mismatch (lp, ls)]);
        List.iter2 (fun p s ->
          try eqtype true type_pairs subst env p s with Unify trace ->
            raise (Failure [CM_Type_parameter_mismatch
                               (env, expand_trace env trace)]))
          patt_params subj_params;
     (* old code: equal_clty false type_pairs subst env patt_type subj_type; *)
        equal_clty false type_pairs subst env
          (Cty_signature sign1) (Cty_signature sign2);
        (* Use moregeneral for class parameters, need to recheck everything to
           keeps relationships (PR#4824) *)
        let clty_params =
          List.fold_right (fun ty cty -> Cty_arrow ("*",ty,cty)) in
        match_class_types ~trace:false env
          (clty_params patt_params patt_type)
          (clty_params subj_params subj_type)
      with
        Failure r -> r
      end
  | error ->
      error


                              (***************)
                              (*  Subtyping  *)
                              (***************)


(**** Build a subtype of a given type. ****)

(* build_subtype:
   [visited] traces traversed object and variant types
   [loops] is a mapping from variables to variables, to reproduce
     positive loops in a class type
   [posi] true if the current variance is positive
   [level] number of expansions/enlargement allowed on this branch *)

let warn = ref false  (* whether double coercion might do better *)
let pred_expand n = if n mod 2 = 0 && n > 0 then pred n else n
let pred_enlarge n = if n mod 2 = 1 then pred n else n

type change = Unchanged | Equiv | Changed
let collect l = List.fold_left (fun c1 (_, c2) -> max c1 c2) Unchanged l

let rec filter_visited = function
    [] -> []
  | {desc=Tobject _|Tvariant _} :: _ as l -> l
  | _ :: l -> filter_visited l

let memq_warn t visited =
  if List.memq t visited then (warn := true; true) else false

let rec lid_of_path ?(sharp="") = function
    Path.Pident id ->
      Longident.Lident (sharp ^ Ident.name id)
  | Path.Pdot (p1, s, _) ->
      Longident.Ldot (lid_of_path p1, sharp ^ s)
  | Path.Papply (p1, p2) ->
      Longident.Lapply (lid_of_path ~sharp p1, lid_of_path p2)

let find_cltype_for_path env p =
  let path, cl_abbr = Env.lookup_type (lid_of_path ~sharp:"#" p) env in
  match cl_abbr.type_manifest with
    Some ty ->
      begin match (repr ty).desc with
        Tobject(_,{contents=Some(p',_)}) when Path.same p p' -> cl_abbr, ty
      | _ -> raise Not_found
      end
  | None -> assert false

let has_constr_row' env t =
  has_constr_row (expand_abbrev env t)

let rec build_subtype env visited loops posi level t =
  let t = repr t in
  match t.desc with
    Tvar _ ->
      if posi then
        try
          let t' = List.assq t loops in
          warn := true;
          (t', Equiv)
        with Not_found ->
          (t, Unchanged)
      else
        (t, Unchanged)
  | Tarrow(l, t1, t2, _) ->
      if memq_warn t visited then (t, Unchanged) else
      let visited = t :: visited in
      let (t1', c1) = build_subtype env visited loops (not posi) level t1 in
      let (t2', c2) = build_subtype env visited loops posi level t2 in
      let c = max c1 c2 in
      if c > Unchanged then (newty (Tarrow(l, t1', t2', Cok)), c)
      else (t, Unchanged)
  | Ttuple tlist ->
      if memq_warn t visited then (t, Unchanged) else
      let visited = t :: visited in
      let tlist' =
        List.map (build_subtype env visited loops posi level) tlist
      in
      let c = collect tlist' in
      if c > Unchanged then (newty (Ttuple (List.map fst tlist')), c)
      else (t, Unchanged)
  | Tconstr(p, tl, abbrev)
    when level > 0 && generic_abbrev env p && safe_abbrev env t
    && not (has_constr_row' env t) ->
      let t' = repr (expand_abbrev env t) in
      let level' = pred_expand level in
      begin try match t'.desc with
        Tobject _ when posi && not (opened_object t') ->
          let cl_abbr, body = find_cltype_for_path env p in
          let ty =
            subst env !current_level Public abbrev None
              cl_abbr.type_params tl body in
          let ty = repr ty in
          let ty1, tl1 =
            match ty.desc with
              Tobject(ty1,{contents=Some(p',tl1)}) when Path.same p p' ->
                ty1, tl1
            | _ -> raise Not_found
          in
          (* Fix PR4505: do not set ty to Tvar when it appears in tl1,
             as this occurence might break the occur check.
             XXX not clear whether this correct anyway... *)
          if List.exists (deep_occur ty) tl1 then raise Not_found;
          ty.desc <- Tvar None;
          let t'' = newvar () in
          let loops = (ty, t'') :: loops in
          (* May discard [visited] as level is going down *)
          let (ty1', c) =
            build_subtype env [t'] loops posi (pred_enlarge level') ty1 in
          assert (is_Tvar t'');
          let nm =
            if c > Equiv || deep_occur ty ty1' then None else Some(p,tl1) in
          t''.desc <- Tobject (ty1', ref nm);
          (try unify_var env ty t with Unify _ -> assert false);
          (t'', Changed)
      | _ -> raise Not_found
      with Not_found ->
        let (t'',c) = build_subtype env visited loops posi level' t' in
        if c > Unchanged then (t'',c)
        else (t, Unchanged)
      end
  | Tconstr(p, tl, abbrev) ->
      (* Must check recursion on constructors, since we do not always
         expand them *)
      if memq_warn t visited then (t, Unchanged) else
      let visited = t :: visited in
      begin try
        let decl = Env.find_type p env in
        if level = 0 && generic_abbrev env p && safe_abbrev env t
        && not (has_constr_row' env t)
        then warn := true;
        let tl' =
          List.map2
            (fun v t ->
              let (co,cn) = Variance.get_upper v in
              if cn then
                if co then (t, Unchanged)
                else build_subtype env visited loops (not posi) level t
              else
                if co then build_subtype env visited loops posi level t
                else (newvar(), Changed))
            decl.type_variance tl
        in
        let c = collect tl' in
        if c > Unchanged then (newconstr p (List.map fst tl'), c)
        else (t, Unchanged)
      with Not_found ->
        (t, Unchanged)
      end
  | Tvariant row ->
      let row = row_repr row in
      if memq_warn t visited || not (static_row row) then (t, Unchanged) else
      let level' = pred_enlarge level in
      let visited =
        t :: if level' < level then [] else filter_visited visited in
      let fields = filter_row_fields false row.row_fields in
      let fields =
        List.map
          (fun (l,f as orig) -> match row_field_repr f with
            Rpresent None ->
              if posi then
                (l, Reither(true, [], false, ref None)), Unchanged
              else
                orig, Unchanged
          | Rpresent(Some t) ->
              let (t', c) = build_subtype env visited loops posi level' t in
              let f =
                if posi && level > 0
                then Reither(false, [t'], false, ref None)
                else Rpresent(Some t')
              in (l, f), c
          | _ -> assert false)
          fields
      in
      let c = collect fields in
      let row =
        { row_fields = List.map fst fields; row_more = newvar();
          row_bound = (); row_closed = posi; row_fixed = false;
          row_name = if c > Unchanged then None else row.row_name }
      in
      (newty (Tvariant row), Changed)
  | Tobject (t1, _) ->
      if memq_warn t visited || opened_object t1 then (t, Unchanged) else
      let level' = pred_enlarge level in
      let visited =
        t :: if level' < level then [] else filter_visited visited in
      let (t1', c) = build_subtype env visited loops posi level' t1 in
      if c > Unchanged then (newty (Tobject (t1', ref None)), c)
      else (t, Unchanged)
  | Tfield(s, _, t1, t2) (* Always present *) ->
      let (t1', c1) = build_subtype env visited loops posi level t1 in
      let (t2', c2) = build_subtype env visited loops posi level t2 in
      let c = max c1 c2 in
      if c > Unchanged then (newty (Tfield(s, Fpresent, t1', t2')), c)
      else (t, Unchanged)
  | Tnil ->
      if posi then
        let v = newvar () in
        (v, Changed)
      else begin
        warn := true;
        (t, Unchanged)
      end
  | Tsubst _ | Tlink _ ->
      assert false
  | Tpoly(t1, tl) ->
      let (t1', c) = build_subtype env visited loops posi level t1 in
      if c > Unchanged then (newty (Tpoly(t1', tl)), c)
      else (t, Unchanged)
  | Tunivar _ | Tpackage _ ->
      (t, Unchanged)

let enlarge_type env ty =
  warn := false;
  (* [level = 4] allows 2 expansions involving objects/variants *)
  let (ty', _) = build_subtype env [] [] true 4 ty in
  (ty', !warn)

(**** Check whether a type is a subtype of another type. ****)

(*
    During the traversal, a trace of visited types is maintained. It
    is printed in case of error.
    Constraints (pairs of types that must be equals) are accumulated
    rather than being enforced straight. Indeed, the result would
    otherwise depend on the order in which these constraints are
    enforced.
    A function enforcing these constraints is returned. That way, type
    variables can be bound to their actual values before this function
    is called (see Typecore).
    Only well-defined abbreviations are expanded (hence the tests
    [generic_abbrev ...]).
*)

let subtypes = TypePairs.create 17

let subtype_error env trace =
  raise (Subtype (expand_trace env (List.rev trace), []))

(* check list inclusion, assuming lists are ordered *)
let rec included nl1 nl2 =
  match nl1, nl2 with
    (a::nl1', b::nl2') ->
      if a = b then included nl1' nl2' else
      a > b && included nl1 nl2'
  | ([], _) -> true
  | (_, []) -> false

let rec extract_assoc nl1 nl2 tl2 =
  match (nl1, nl2, tl2) with
    (a::nl1', b::nl2, t::tl2) ->
      if a = b then t :: extract_assoc nl1' nl2 tl2
      else extract_assoc nl1 nl2 tl2
  | ([], _, _) -> []
  | _ -> assert false

let rec subtype_rec env trace t1 t2 cstrs =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then cstrs else

  begin try
    TypePairs.find subtypes (t1, t2);
    cstrs
  with Not_found ->
    TypePairs.add subtypes (t1, t2) ();
    match (t1.desc, t2.desc) with
      (Tvar _, _) | (_, Tvar _) ->
        (trace, t1, t2, !univar_pairs)::cstrs
    | (Tarrow(l1, t1, u1, _), Tarrow(l2, t2, u2, _)) when l1 = l2
      || !Clflags.classic && not (is_optional l1 || is_optional l2) ->
        let cstrs = subtype_rec env ((t2, t1)::trace) t2 t1 cstrs in
        subtype_rec env ((u1, u2)::trace) u1 u2 cstrs
    | (Ttuple tl1, Ttuple tl2) ->
        subtype_list env trace tl1 tl2 cstrs
    | (Tconstr(p1, [], _), Tconstr(p2, [], _)) when Path.same p1 p2 ->
        cstrs
    | (Tconstr(p1, tl1, abbrev1), _)
      when generic_abbrev env p1 && safe_abbrev env t1 ->
        subtype_rec env trace (expand_abbrev env t1) t2 cstrs
    | (_, Tconstr(p2, tl2, abbrev2))
      when generic_abbrev env p2 && safe_abbrev env t2 ->
        subtype_rec env trace t1 (expand_abbrev env t2) cstrs
    | (Tconstr(p1, tl1, _), Tconstr(p2, tl2, _)) when Path.same p1 p2 ->
        begin try
          let decl = Env.find_type p1 env in
          List.fold_left2
            (fun cstrs v (t1, t2) ->
              let (co, cn) = Variance.get_upper v in
              if co then
                if cn then
                  (trace, newty2 t1.level (Ttuple[t1]),
                   newty2 t2.level (Ttuple[t2]), !univar_pairs) :: cstrs
                else subtype_rec env ((t1, t2)::trace) t1 t2 cstrs
              else
                if cn then subtype_rec env ((t2, t1)::trace) t2 t1 cstrs
                else cstrs)
            cstrs decl.type_variance (List.combine tl1 tl2)
        with Not_found ->
          (trace, t1, t2, !univar_pairs)::cstrs
        end
    | (Tconstr(p1, _, _), _) when generic_private_abbrev env p1 ->
        subtype_rec env trace (expand_abbrev_opt env t1) t2 cstrs
(*  | (_, Tconstr(p2, _, _)) when generic_private_abbrev false env p2 ->
        subtype_rec env trace t1 (expand_abbrev_opt env t2) cstrs *)
    | (Tobject (f1, _), Tobject (f2, _))
      when is_Tvar (object_row f1) && is_Tvar (object_row f2) ->
        (* Same row variable implies same object. *)
        (trace, t1, t2, !univar_pairs)::cstrs
    | (Tobject (f1, _), Tobject (f2, _)) ->
        subtype_fields env trace f1 f2 cstrs
    | (Tvariant row1, Tvariant row2) ->
        begin try
          subtype_row env trace row1 row2 cstrs
        with Exit ->
          (trace, t1, t2, !univar_pairs)::cstrs
        end
    | (Tpoly (u1, []), Tpoly (u2, [])) ->
        subtype_rec env trace u1 u2 cstrs
    | (Tpoly (u1, tl1), Tpoly (u2, [])) ->
        let _, u1' = instance_poly false tl1 u1 in
        subtype_rec env trace u1' u2 cstrs
    | (Tpoly (u1, tl1), Tpoly (u2,tl2)) ->
        begin try
          enter_poly env univar_pairs u1 tl1 u2 tl2
            (fun t1 t2 -> subtype_rec env trace t1 t2 cstrs)
        with Unify _ ->
          (trace, t1, t2, !univar_pairs)::cstrs
        end
(*    | (Tpackage (p1, nl1, tl1), Tpackage (p2, nl2, tl2))
      when eq_package_path env p1 p2 && included nl2 nl1 ->
        List.map2 (fun t1 t2 -> (trace, t1, t2, !univar_pairs))
          (extract_assoc nl2 nl1 tl1) tl2
        @ cstrs *)
    | (Tpackage (p1, nl1, tl1), Tpackage (p2, nl2, tl2)) ->
        begin try
          let ntl1 = complete_type_list env nl2 t1.level (Mty_ident p1) nl1 tl1
          and ntl2 = complete_type_list env nl1 t2.level (Mty_ident p2) nl2 tl2
              ~allow_absent:true in
          let cstrs' =
            List.map
              (fun (n2,t2) -> (trace, List.assoc n2 ntl1, t2, !univar_pairs))
              ntl2
          in
          if eq_package_path env p1 p2 then cstrs' @ cstrs
          else begin
            (* need to check module subtyping *)
            let snap = Btype.snapshot () in
            try
              List.iter (fun (_, t1, t2, _) -> unify env t1 t2) cstrs';
              if !package_subtype env p1 nl1 tl1 p2 nl2 tl2
              then (Btype.backtrack snap; cstrs' @ cstrs)
              else raise (Unify [])
            with Unify _ ->
              Btype.backtrack snap; raise Not_found
          end
        with Not_found ->
          (trace, t1, t2, !univar_pairs)::cstrs
        end
    | (_, _) ->
        (trace, t1, t2, !univar_pairs)::cstrs
  end

and subtype_list env trace tl1 tl2 cstrs =
  if List.length tl1 <> List.length tl2 then
    subtype_error env trace;
  List.fold_left2
    (fun cstrs t1 t2 -> subtype_rec env ((t1, t2)::trace) t1 t2 cstrs)
    cstrs tl1 tl2

and subtype_fields env trace ty1 ty2 cstrs =
  (* Assume that either rest1 or rest2 is not Tvar *)
  let (fields1, rest1) = flatten_fields ty1 in
  let (fields2, rest2) = flatten_fields ty2 in
  let (pairs, miss1, miss2) = associate_fields fields1 fields2 in
  let cstrs =
    if rest2.desc = Tnil then cstrs else
    if miss1 = [] then
      subtype_rec env ((rest1, rest2)::trace) rest1 rest2 cstrs
    else
      (trace, build_fields (repr ty1).level miss1 rest1, rest2,
       !univar_pairs) :: cstrs
  in
  let cstrs =
    if miss2 = [] then cstrs else
    (trace, rest1, build_fields (repr ty2).level miss2 (newvar ()),
     !univar_pairs) :: cstrs
  in
  List.fold_left
    (fun cstrs (_, k1, t1, k2, t2) ->
      (* Theses fields are always present *)
      subtype_rec env ((t1, t2)::trace) t1 t2 cstrs)
    cstrs pairs

and subtype_row env trace row1 row2 cstrs =
  let row1 = row_repr row1 and row2 = row_repr row2 in
  let r1, r2, pairs =
    merge_row_fields row1.row_fields row2.row_fields in
  let more1 = repr row1.row_more
  and more2 = repr row2.row_more in
  match more1.desc, more2.desc with
    Tconstr(p1,_,_), Tconstr(p2,_,_) when Path.same p1 p2 ->
      subtype_rec env ((more1,more2)::trace) more1 more2 cstrs
  | (Tvar _|Tconstr _|Tnil), (Tvar _|Tconstr _|Tnil)
    when row1.row_closed && r1 = [] ->
      List.fold_left
        (fun cstrs (_,f1,f2) ->
          match row_field_repr f1, row_field_repr f2 with
            (Rpresent None|Reither(true,_,_,_)), Rpresent None ->
              cstrs
          | Rpresent(Some t1), Rpresent(Some t2) ->
              subtype_rec env ((t1, t2)::trace) t1 t2 cstrs
          | Reither(false, t1::_, _, _), Rpresent(Some t2) ->
              subtype_rec env ((t1, t2)::trace) t1 t2 cstrs
          | Rabsent, _ -> cstrs
          | _ -> raise Exit)
        cstrs pairs
  | Tunivar _, Tunivar _
    when row1.row_closed = row2.row_closed && r1 = [] && r2 = [] ->
      let cstrs =
        subtype_rec env ((more1,more2)::trace) more1 more2 cstrs in
      List.fold_left
        (fun cstrs (_,f1,f2) ->
          match row_field_repr f1, row_field_repr f2 with
            Rpresent None, Rpresent None
          | Reither(true,[],_,_), Reither(true,[],_,_)
          | Rabsent, Rabsent ->
              cstrs
          | Rpresent(Some t1), Rpresent(Some t2)
          | Reither(false,[t1],_,_), Reither(false,[t2],_,_) ->
              subtype_rec env ((t1, t2)::trace) t1 t2 cstrs
          | _ -> raise Exit)
        cstrs pairs
  | _ ->
      raise Exit

let subtype env ty1 ty2 =
  TypePairs.clear subtypes;
  univar_pairs := [];
  (* Build constraint set. *)
  let cstrs = subtype_rec env [(ty1, ty2)] ty1 ty2 [] in
  TypePairs.clear subtypes;
  (* Enforce constraints. *)
  function () ->
    List.iter
      (function (trace0, t1, t2, pairs) ->
         try unify_pairs (ref env) t1 t2 pairs with Unify trace ->
           raise (Subtype (expand_trace env (List.rev trace0),
                           List.tl (List.tl trace))))
      (List.rev cstrs)

                              (*******************)
                              (*  Miscellaneous  *)
                              (*******************)

(* Utility for printing. The resulting type is not used in computation. *)
let rec unalias_object ty =
  let ty = repr ty in
  match ty.desc with
    Tfield (s, k, t1, t2) ->
      newty2 ty.level (Tfield (s, k, t1, unalias_object t2))
  | Tvar _ | Tnil ->
      newty2 ty.level ty.desc
  | Tunivar _ ->
      ty
  | Tconstr _ ->
      newvar2 ty.level
  | _ ->
      assert false

let unalias ty =
  let ty = repr ty in
  match ty.desc with
    Tvar _ | Tunivar _ ->
      ty
  | Tvariant row ->
      let row = row_repr row in
      let more = row.row_more in
      newty2 ty.level
        (Tvariant {row with row_more = newty2 more.level more.desc})
  | Tobject (ty, nm) ->
      newty2 ty.level (Tobject (unalias_object ty, nm))
  | _ ->
      newty2 ty.level ty.desc

(* Return the arity (as for curried functions) of the given type. *)
let rec arity ty =
  match (repr ty).desc with
    Tarrow(_, t1, t2, _) -> 1 + arity t2
  | _ -> 0

(* Check whether an abbreviation expands to itself. *)
let cyclic_abbrev env id ty =
  let rec check_cycle seen ty =
    let ty = repr ty in
    match ty.desc with
      Tconstr (p, tl, abbrev) ->
        p = Path.Pident id || List.memq ty seen ||
        begin try
          check_cycle (ty :: seen) (expand_abbrev_opt env ty)
        with
          Cannot_expand -> false
        | Unify _ -> true
        end
    | _ ->
        false
  in check_cycle [] ty

(* Normalize a type before printing, saving... *)
(* Cannot use mark_type because deep_occur uses it too *)
let rec normalize_type_rec env visited ty =
  let ty = repr ty in
  if not (TypeSet.mem ty !visited) then begin
    visited := TypeSet.add ty !visited;
    begin match ty.desc with
    | Tvariant row ->
      let row = row_repr row in
      let fields = List.map
          (fun (l,f0) ->
            let f = row_field_repr f0 in l,
            match f with Reither(b, ty::(_::_ as tyl), m, e) ->
              let tyl' =
                List.fold_left
                  (fun tyl ty ->
                    if List.exists (fun ty' -> equal env false [ty] [ty']) tyl
                    then tyl else ty::tyl)
                  [ty] tyl
              in
              if f != f0 || List.length tyl' < List.length tyl then
                Reither(b, List.rev tyl', m, e)
              else f
            | _ -> f)
          row.row_fields in
      let fields =
        List.sort (fun (p,_) (q,_) -> compare p q)
          (List.filter (fun (_,fi) -> fi <> Rabsent) fields) in
      log_type ty;
      ty.desc <- Tvariant {row with row_fields = fields}
    | Tobject (fi, nm) ->
        begin match !nm with
        | None -> ()
        | Some (n, v :: l) ->
            if deep_occur ty (newgenty (Ttuple l)) then
              (* The abbreviation may be hiding something, so remove it *)
              set_name nm None
            else let v' = repr v in
            begin match v'.desc with
            | Tvar _ | Tunivar _ ->
                if v' != v then set_name nm (Some (n, v' :: l))
            | Tnil ->
                log_type ty; ty.desc <- Tconstr (n, l, ref Mnil)
            | _ -> set_name nm None
            end
        | _ ->
            fatal_error "Ctype.normalize_type_rec"
        end;
        let fi = repr fi in
        if fi.level < lowest_level then () else
        let fields, row = flatten_fields fi in
        let fi' = build_fields fi.level fields row in
        log_type ty; fi.desc <- fi'.desc
    | _ -> ()
    end;
    iter_type_expr (normalize_type_rec env visited) ty
  end

let normalize_type env ty =
  normalize_type_rec env (ref TypeSet.empty) ty


                              (*************************)
                              (*  Remove dependencies  *)
                              (*************************)


(*
   Variables are left unchanged. Other type nodes are duplicated, with
   levels set to generic level.
   We cannot use Tsubst here, because unification may be called by
   expand_abbrev.
*)

let nondep_hash     = TypeHash.create 47
let nondep_variants = TypeHash.create 17
let clear_hash ()   =
  TypeHash.clear nondep_hash; TypeHash.clear nondep_variants

let rec nondep_type_rec env id ty =
  match ty.desc with
    Tvar _ | Tunivar _ -> ty
  | Tlink ty -> nondep_type_rec env id ty
  | _ -> try TypeHash.find nondep_hash ty
  with Not_found ->
    let ty' = newgenvar () in        (* Stub *)
    TypeHash.add nondep_hash ty ty';
    ty'.desc <-
      begin match ty.desc with
      | Tconstr(p, tl, abbrev) ->
          if Path.isfree id p then
            begin try
              Tlink (nondep_type_rec env id
                       (expand_abbrev env (newty2 ty.level ty.desc)))
              (*
                 The [Tlink] is important. The expanded type may be a
                 variable, or may not be completely copied yet
                 (recursive type), so one cannot just take its
                 description.
               *)
            with Cannot_expand | Unify _ ->
              raise Not_found
            end
          else
            Tconstr(p, List.map (nondep_type_rec env id) tl, ref Mnil)
      | Tpackage(p, nl, tl) when Path.isfree id p ->
          let p' = normalize_package_path env p in
          if Path.isfree id p' then raise Not_found;
          Tpackage (p', nl, List.map (nondep_type_rec env id) tl)
      | Tobject (t1, name) ->
          Tobject (nondep_type_rec env id t1,
                 ref (match !name with
                        None -> None
                      | Some (p, tl) ->
                          if Path.isfree id p then None
                          else Some (p, List.map (nondep_type_rec env id) tl)))
      | Tvariant row ->
          let row = row_repr row in
          let more = repr row.row_more in
          (* We must keep sharing according to the row variable *)
          begin try
            let ty2 = TypeHash.find nondep_variants more in
            (* This variant type has been already copied *)
            TypeHash.add nondep_hash ty ty2;
            Tlink ty2
          with Not_found ->
            (* Register new type first for recursion *)
            TypeHash.add nondep_variants more ty';
            let static = static_row row in
            let more' = if static then newgenty Tnil else more in
            (* Return a new copy *)
            let row =
              copy_row (nondep_type_rec env id) true row true more' in
            match row.row_name with
              Some (p, tl) when Path.isfree id p ->
                Tvariant {row with row_name = None}
            | _ -> Tvariant row
          end
      | _ -> copy_type_desc (nondep_type_rec env id) ty.desc
      end;
    ty'

let nondep_type env id ty =
  try
    let ty' = nondep_type_rec env id ty in
    clear_hash ();
    ty'
  with Not_found ->
    clear_hash ();
    raise Not_found

let () = nondep_type' := nondep_type

let unroll_abbrev id tl ty =
  let ty = repr ty and path = Path.Pident id in
  if is_Tvar ty || (List.exists (deep_occur ty) tl)
  || is_object_type path then
    ty
  else
    let ty' = newty2 ty.level ty.desc in
    link_type ty (newty2 ty.level (Tconstr (path, tl, ref Mnil)));
    ty'

(* Preserve sharing inside type declarations. *)
let nondep_type_decl env mid id is_covariant decl =
  try
    let params = List.map (nondep_type_rec env mid) decl.type_params in
    let tk =
      try match decl.type_kind with
        Type_abstract ->
          Type_abstract
      | Type_variant cstrs ->
          Type_variant
            (List.map
               (fun c ->
                 {c with
                  cd_args = List.map (nondep_type_rec env mid) c.cd_args;
                  cd_res = may_map (nondep_type_rec env mid) c.cd_res;
                 }
               )
               cstrs)
      | Type_record(lbls, rep) ->
          Type_record
            (List.map
               (fun l ->
                  {l with ld_type = nondep_type_rec env mid l.ld_type}
               )
               lbls,
             rep)
      | Type_open ->
          Type_open
      with Not_found when is_covariant -> Type_abstract
    and tm =
      try match decl.type_manifest with
        None -> None
      | Some ty ->
          Some (unroll_abbrev id params (nondep_type_rec env mid ty))
      with Not_found when is_covariant ->
        None
    in
    clear_hash ();
    let priv =
      match tm with
      | Some ty when Btype.has_constr_row ty -> Private
      | _ -> decl.type_private
    in
    { type_params = params;
      type_arity = decl.type_arity;
      type_kind = tk;
      type_manifest = tm;
      type_private = priv;
      type_variance = decl.type_variance;
      type_newtype_level = None;
      type_loc = decl.type_loc;
      type_attributes = decl.type_attributes;
    }
  with Not_found ->
    clear_hash ();
    raise Not_found

(* Preserve sharing inside extension constructors. *)
let nondep_extension_constructor env mid ext =
  try
    let type_path, type_params =
      if Path.isfree mid ext.ext_type_path then
        begin
          let ty =
            newgenty (Tconstr(ext.ext_type_path, ext.ext_type_params, ref Mnil))
          in
          let ty' = nondep_type_rec env mid ty in
            match (repr ty').desc with
                Tconstr(p, tl, _) -> p, tl
              | _ -> raise Not_found
        end
      else
        let type_params =
          List.map (nondep_type_rec env mid) ext.ext_type_params
        in
          ext.ext_type_path, type_params
    in
    let args = List.map (nondep_type_rec env mid) ext.ext_args in
    let ret_type = may_map (nondep_type_rec env mid) ext.ext_ret_type in
      clear_hash ();
      { ext_type_path = type_path;
        ext_type_params = type_params;
        ext_args = args;
        ext_ret_type = ret_type;
        ext_private = ext.ext_private;
        ext_attributes = ext.ext_attributes;
        ext_loc = ext.ext_loc;
      }
  with Not_found ->
    clear_hash ();
    raise Not_found


(* Preserve sharing inside class types. *)
let nondep_class_signature env id sign =
  { csig_self = nondep_type_rec env id sign.csig_self;
    csig_vars =
      Vars.map (function (m, v, t) -> (m, v, nondep_type_rec env id t))
        sign.csig_vars;
    csig_concr = sign.csig_concr;
    csig_inher =
      List.map (fun (p,tl) -> (p, List.map (nondep_type_rec env id) tl))
        sign.csig_inher }

let rec nondep_class_type env id =
  function
    Cty_constr (p, _, cty) when Path.isfree id p ->
      nondep_class_type env id cty
  | Cty_constr (p, tyl, cty) ->
      Cty_constr (p, List.map (nondep_type_rec env id) tyl,
                   nondep_class_type env id cty)
  | Cty_signature sign ->
      Cty_signature (nondep_class_signature env id sign)
  | Cty_arrow (l, ty, cty) ->
      Cty_arrow (l, nondep_type_rec env id ty, nondep_class_type env id cty)

let nondep_class_declaration env id decl =
  assert (not (Path.isfree id decl.cty_path));
  let decl =
    { cty_params = List.map (nondep_type_rec env id) decl.cty_params;
      cty_variance = decl.cty_variance;
      cty_type = nondep_class_type env id decl.cty_type;
      cty_path = decl.cty_path;
      cty_new =
        begin match decl.cty_new with
          None    -> None
        | Some ty -> Some (nondep_type_rec env id ty)
        end;
      cty_loc = decl.cty_loc;
      cty_attributes = decl.cty_attributes;
    }
  in
  clear_hash ();
  decl

let nondep_cltype_declaration env id decl =
  assert (not (Path.isfree id decl.clty_path));
  let decl =
    { clty_params = List.map (nondep_type_rec env id) decl.clty_params;
      clty_variance = decl.clty_variance;
      clty_type = nondep_class_type env id decl.clty_type;
      clty_path = decl.clty_path;
      clty_loc = decl.clty_loc;
      clty_attributes = decl.clty_attributes;
    }
  in
  clear_hash ();
  decl

(* collapse conjonctive types in class parameters *)
let rec collapse_conj env visited ty =
  let ty = repr ty in
  if List.memq ty visited then () else
  let visited = ty :: visited in
  match ty.desc with
    Tvariant row ->
      let row = row_repr row in
      List.iter
        (fun (l,fi) ->
          match row_field_repr fi with
            Reither (c, t1::(_::_ as tl), m, e) ->
              List.iter (unify env t1) tl;
              set_row_field e (Reither (c, [t1], m, ref None))
          | _ ->
              ())
        row.row_fields;
      iter_row (collapse_conj env visited) row
  | _ ->
      iter_type_expr (collapse_conj env visited) ty

let collapse_conj_params env params =
  List.iter (collapse_conj env []) params
