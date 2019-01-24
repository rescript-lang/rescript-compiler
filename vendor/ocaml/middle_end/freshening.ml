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

type tbl = {
  sb_var : Variable.t Variable.Map.t;
  sb_mutable_var : Mutable_variable.t Mutable_variable.Map.t;
  sb_exn : Static_exception.t Static_exception.Map.t;
  (* Used to handle substitution sequences: we cannot call the substitution
     recursively because there can be name clashes. *)
  back_var : Variable.t list Variable.Map.t;
  back_mutable_var : Mutable_variable.t list Mutable_variable.Map.t;
}

type t =
  | Inactive
  | Active of tbl

type subst = t

let empty_tbl = {
  sb_var = Variable.Map.empty;
  sb_mutable_var = Mutable_variable.Map.empty;
  sb_exn = Static_exception.Map.empty;
  back_var = Variable.Map.empty;
  back_mutable_var = Mutable_variable.Map.empty;
}

let print ppf = function
  | Inactive -> Format.fprintf ppf "Inactive"
  | Active tbl ->
    Format.fprintf ppf "Active:@ ";
    Variable.Map.iter (fun var1 var2 ->
        Format.fprintf ppf "%a -> %a@ "
          Variable.print var1
          Variable.print var2)
      tbl.sb_var;
    Mutable_variable.Map.iter (fun mut_var1 mut_var2 ->
        Format.fprintf ppf "(mutable) %a -> %a@ "
          Mutable_variable.print mut_var1
          Mutable_variable.print mut_var2)
      tbl.sb_mutable_var;
    Variable.Map.iter (fun var vars ->
        Format.fprintf ppf "%a -> %a@ "
          Variable.print var
          Variable.Set.print (Variable.Set.of_list vars))
      tbl.back_var;
    Mutable_variable.Map.iter (fun mut_var mut_vars ->
        Format.fprintf ppf "(mutable) %a -> %a@ "
          Mutable_variable.print mut_var
          Mutable_variable.Set.print (Mutable_variable.Set.of_list mut_vars))
      tbl.back_mutable_var

let empty = Inactive

let empty_preserving_activation_state = function
  | Inactive -> Inactive
  | Active _ -> Active empty_tbl

let activate = function
  | Inactive -> Active empty_tbl
  | Active _ as t -> t

let rec add_sb_var sb id id' =
  let sb = { sb with sb_var = Variable.Map.add id id' sb.sb_var } in
  let sb =
    try let pre_vars = Variable.Map.find id sb.back_var in
      List.fold_left (fun sb pre_id -> add_sb_var sb pre_id id') sb pre_vars
    with Not_found -> sb in
  let back_var =
    let l = try Variable.Map.find id' sb.back_var with Not_found -> [] in
    Variable.Map.add id' (id :: l) sb.back_var in
  { sb with back_var }

let rec add_sb_mutable_var sb id id' =
  let sb =
    { sb with
      sb_mutable_var = Mutable_variable.Map.add id id' sb.sb_mutable_var;
    }
  in
  let sb =
    try
      let pre_vars = Mutable_variable.Map.find id sb.back_mutable_var in
      List.fold_left (fun sb pre_id -> add_sb_mutable_var sb pre_id id')
        sb pre_vars
    with Not_found -> sb in
  let back_mutable_var =
    let l =
      try Mutable_variable.Map.find id' sb.back_mutable_var
      with Not_found -> []
    in
    Mutable_variable.Map.add id' (id :: l) sb.back_mutable_var
  in
  { sb with back_mutable_var }

let apply_static_exception t i =
  match t with
  | Inactive ->
    i
  | Active t ->
    try Static_exception.Map.find i t.sb_exn
    with Not_found -> i

let add_static_exception t i =
  match t with
  | Inactive -> i, t
  | Active t ->
    let i' = Static_exception.create () in
    let sb_exn =
      Static_exception.Map.add i i' t.sb_exn
    in
    i', Active { t with sb_exn; }

let active_add_variable t id =
  let id' = Variable.rename id in
  let t = add_sb_var t id id' in
  id', t

let active_add_parameter t param =
  let param' = Parameter.rename param in
  let t = add_sb_var t (Parameter.var param) (Parameter.var param') in
  param', t

let add_variable t id =
  match t with
  | Inactive -> id, t
  | Active t ->
     let id', t = active_add_variable t id in
     id', Active t

let active_add_parameters' t (params:Parameter.t list) =
  List.fold_right (fun param (params, t) ->
      let param', t = active_add_parameter t param in
      param' :: params, t)
    params ([], t)

let add_variables t defs =
  List.fold_right (fun (id, data) (defs, t) ->
      let id', t = add_variable t id in
      (id', data) :: defs, t) defs ([], t)

let add_variables' t ids =
  List.fold_right (fun id (ids, t) ->
      let id', t = add_variable t id in
      id' :: ids, t) ids ([], t)

let active_add_mutable_variable t id =
  let id' = Mutable_variable.freshen id in
  let t = add_sb_mutable_var t id id' in
  id', t

let add_mutable_variable t id =
  match t with
  | Inactive -> id, t
  | Active t ->
     let id', t = active_add_mutable_variable t id in
     id', Active t

let active_find_var_exn t id =
  try Variable.Map.find id t.sb_var with
  | Not_found ->
      Misc.fatal_error (Format.asprintf "find_var: can't find %a@."
          Variable.print id)

let apply_variable t var =
  match t with
  | Inactive -> var
  | Active t ->
   try Variable.Map.find var t.sb_var with
   | Not_found -> var

let apply_mutable_variable t mut_var =
  match t with
  | Inactive -> mut_var
  | Active t ->
   try Mutable_variable.Map.find mut_var t.sb_mutable_var with
   | Not_found -> mut_var

let rewrite_recursive_calls_with_symbols t
      (function_declarations : Flambda.function_declarations)
      ~make_closure_symbol =
  match t with
  | Inactive -> function_declarations
  | Active _ ->
    let all_free_symbols =
      Flambda_utils.all_free_symbols function_declarations
    in
    let closure_symbols_used = ref false in
    let closure_symbols =
      Variable.Map.fold (fun var _ map ->
        let closure_id = Closure_id.wrap var in
        let sym = make_closure_symbol closure_id in
        if Symbol.Set.mem sym all_free_symbols then begin
          closure_symbols_used := true;
          Symbol.Map.add sym var map
        end else begin
          map
        end)
      function_declarations.funs Symbol.Map.empty
    in
    if not !closure_symbols_used then begin
      (* Don't waste time rewriting the function declaration(s) if there
         are no occurrences of any of the closure symbols. *)
      function_declarations
    end else begin
      let funs =
        Variable.Map.map (fun (ffun : Flambda.function_declaration) ->
          let body =
            Flambda_iterators.map_toplevel_named
              (* CR-someday pchambart: This may be worth deep substituting
                 below the closures, but that means that we need to take care
                 of functions' free variables. *)
              (function
                | Symbol sym when Symbol.Map.mem sym closure_symbols ->
                  Expr (Var (Symbol.Map.find sym closure_symbols))
                | e -> e)
              ffun.body
          in
          Flambda.create_function_declaration ~params:ffun.params
            ~body ~stub:ffun.stub ~dbg:ffun.dbg ~inline:ffun.inline
            ~specialise:ffun.specialise ~is_a_functor:ffun.is_a_functor)
          function_declarations.funs
      in
      Flambda.update_function_declarations function_declarations ~funs
    end

module Project_var = struct
  type t =
    { vars_within_closure : Var_within_closure.t Var_within_closure.Map.t;
      closure_id : Closure_id.t Closure_id.Map.t }

  let empty =
    { vars_within_closure = Var_within_closure.Map.empty;
      closure_id = Closure_id.Map.empty;
    }

  let print ppf t =
    Format.fprintf ppf "{ vars_within_closure %a, closure_id %a }"
      (Var_within_closure.Map.print Var_within_closure.print)
      t.vars_within_closure
      (Closure_id.Map.print Closure_id.print)
      t.closure_id

  let new_subst_fv t id subst =
    match subst with
    | Inactive -> id, subst, t
    | Active subst ->
      let id' = Variable.rename id in
      let subst = add_sb_var subst id id' in
      let off = Var_within_closure.wrap id in
      let off' = Var_within_closure.wrap id' in
      let off_sb = Var_within_closure.Map.add off off' t.vars_within_closure in
      id', Active subst, { t with vars_within_closure = off_sb; }

  let new_subst_fun t id subst =
    let id' = Variable.rename id in
    let subst = add_sb_var subst id id' in
    let off = Closure_id.wrap id in
    let off' = Closure_id.wrap id' in
    let off_sb = Closure_id.Map.add off off' t.closure_id in
    id', subst, { t with closure_id = off_sb; }

  (** Returns :
      * The map of new_identifiers -> expression
      * The new environment with added substitution
      * a fresh ffunction_subst with only the substitution of free variables
   *)
  let subst_free_vars fv subst ~only_freshen_parameters
      : (Flambda.specialised_to * _) Variable.Map.t * _ * _ =
    Variable.Map.fold (fun id lam (fv, subst, t) ->
        let id, subst, t =
          if only_freshen_parameters then
            id, subst, t
          else
            new_subst_fv t id subst
        in
        Variable.Map.add id lam fv, subst, t)
      fv
      (Variable.Map.empty, subst, empty)

  (** Returns :
      * The function_declaration with renamed function identifiers
      * The new environment with added substitution
      * The ffunction_subst completed with function substitution

      subst_free_vars must have been used to build off_sb
   *)
  let func_decls_subst t (subst : subst)
        (func_decls : Flambda.function_declarations)
        ~only_freshen_parameters =
    match subst with
    | Inactive -> func_decls, subst, t
    | Active subst ->
      let subst_func_decl _fun_id (func_decl : Flambda.function_declaration)
          subst =
        let params, subst = active_add_parameters' subst func_decl.params in
        (* Since all parameters are distinct, even between functions, we can
           just use a single substitution. *)
        let body =
          Flambda_utils.toplevel_substitution subst.sb_var func_decl.body
        in
        let function_decl =
          Flambda.create_function_declaration ~params
            ~body ~stub:func_decl.stub ~dbg:func_decl.dbg
            ~inline:func_decl.inline ~specialise:func_decl.specialise
            ~is_a_functor:func_decl.is_a_functor
        in
        function_decl, subst
      in
      let subst, t =
        if only_freshen_parameters then
          subst, t
        else
          Variable.Map.fold (fun orig_id _func_decl (subst, t) ->
              let _id, subst, t = new_subst_fun t orig_id subst in
              subst, t)
            func_decls.funs
            (subst, t)
      in
      let funs, subst =
        Variable.Map.fold (fun orig_id func_decl (funs, subst) ->
            let func_decl, subst = subst_func_decl orig_id func_decl subst in
            let id =
              if only_freshen_parameters then orig_id
              else active_find_var_exn subst orig_id
            in
            let funs = Variable.Map.add id func_decl funs in
            funs, subst)
          func_decls.funs
          (Variable.Map.empty, subst)
      in
      let function_decls =
        Flambda.update_function_declarations func_decls ~funs
      in
      function_decls, Active subst, t

  let apply_closure_id t closure_id =
    try Closure_id.Map.find closure_id t.closure_id
    with Not_found -> closure_id

  let apply_var_within_closure t var_in_closure =
    try Var_within_closure.Map.find var_in_closure t.vars_within_closure
    with Not_found -> var_in_closure

  module Compose (T : Identifiable.S) = struct
    let compose ~earlier ~later =
      if (T.Map.equal T.equal) earlier later
        || T.Map.cardinal later = 0
      then
        earlier
      else
        T.Map.mapi (fun src_var var ->
            if T.Map.mem src_var later then begin
              Misc.fatal_errorf "Freshening.Project_var.compose: domains \
                  of substitutions must be disjoint.  earlier=%a later=%a"
                (T.Map.print T.print) earlier
                (T.Map.print T.print) later
            end;
            match T.Map.find var later with
            | exception Not_found -> var
            | var -> var)
          earlier
  end

  module V = Compose (Var_within_closure)
  module C = Compose (Closure_id)

  let compose ~earlier ~later : t =
    { vars_within_closure =
        V.compose ~earlier:earlier.vars_within_closure
          ~later:later.vars_within_closure;
      closure_id =
        C.compose ~earlier:earlier.closure_id
          ~later:later.closure_id;
    }
end

let apply_function_decls_and_free_vars t fv func_decls
      ~only_freshen_parameters =
  let module I = Project_var in
  let fv, t, of_closures = I.subst_free_vars fv t ~only_freshen_parameters in
  let func_decls, t, of_closures =
    I.func_decls_subst of_closures t func_decls ~only_freshen_parameters
  in
  fv, func_decls, t, of_closures

let does_not_freshen t vars =
  match t with
  | Inactive -> true
  | Active subst ->
    not (List.exists (fun var -> Variable.Map.mem var subst.sb_var) vars)

let freshen_projection (projection : Projection.t) ~freshening
      ~closure_freshening : Projection.t =
  match projection with
  | Project_var { closure; closure_id; var; } ->
    Project_var {
      closure = apply_variable freshening closure;
      closure_id = Project_var.apply_closure_id closure_freshening closure_id;
      var = Project_var.apply_var_within_closure closure_freshening var;
    }
  | Project_closure { set_of_closures; closure_id; } ->
    Project_closure {
      set_of_closures = apply_variable freshening set_of_closures;
      closure_id = Project_var.apply_closure_id closure_freshening closure_id;
    }
  | Move_within_set_of_closures { closure; start_from; move_to; } ->
    Move_within_set_of_closures {
      closure = apply_variable freshening closure;
      start_from = Project_var.apply_closure_id closure_freshening start_from;
      move_to = Project_var.apply_closure_id closure_freshening move_to;
    }
  | Field (field_index, var) ->
    Field (field_index, apply_variable freshening var)

let freshen_projection_relation relation ~freshening ~closure_freshening =
  Variable.Map.map (fun (spec_to : Flambda.specialised_to) ->
      let projection =
        match spec_to.projection with
        | None -> None
        | Some projection ->
          Some (freshen_projection projection ~freshening ~closure_freshening)
      in
      { spec_to with projection; })
    relation

let freshen_projection_relation' relation ~freshening ~closure_freshening =
  Variable.Map.map (fun ((spec_to : Flambda.specialised_to), data) ->
      let projection =
        match spec_to.projection with
        | None -> None
        | Some projection ->
          Some (freshen_projection projection ~freshening ~closure_freshening)
      in
      { spec_to with projection; }, data)
    relation
