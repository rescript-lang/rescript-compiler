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

module Env : sig
  type t

  val new_descr : t -> Export_info.descr -> Export_id.t
  val record_descr : t -> Export_id.t -> Export_info.descr -> unit
  val get_descr : t -> Export_info.approx -> Export_info.descr option

  val add_approx : t -> Variable.t -> Export_info.approx -> t
  val add_approx_maps : t -> Export_info.approx Variable.Map.t list -> t
  val find_approx : t -> Variable.t -> Export_info.approx

  val get_symbol_descr : t -> Symbol.t -> Export_info.descr option

  val new_unit_descr : t -> Export_id.t

  module Global : sig
    (* "Global" as in "without local variable bindings". *)
    type t

    val create_empty : unit -> t

    val add_symbol : t -> Symbol.t -> Export_id.t -> t
    val new_symbol : t -> Symbol.t -> Export_id.t * t

    val symbol_to_export_id_map : t -> Export_id.t Symbol.Map.t
    val export_id_to_descr_map : t -> Export_info.descr Export_id.Map.t
  end

  (** Creates a new environment, sharing the mapping from export IDs to
      export descriptions with the given global environment. *)
  val empty_of_global : Global.t -> t
end = struct
  let fresh_id () = Export_id.create (Compilenv.current_unit ())

  module Global = struct
    type t =
      { sym : Export_id.t Symbol.Map.t;
        (* Note that [ex_table]s themselves are shared (hence [ref] and not
           [mutable]). *)
        ex_table : Export_info.descr Export_id.Map.t ref;
      }

    let create_empty () =
      { sym = Symbol.Map.empty;
        ex_table = ref Export_id.Map.empty;
      }

    let add_symbol t sym export_id =
      if Symbol.Map.mem sym t.sym then begin
        Misc.fatal_errorf "Build_export_info.Env.Global.add_symbol: cannot \
            rebind symbol %a in environment"
          Symbol.print sym
      end;
      { t with sym = Symbol.Map.add sym export_id t.sym }

    let new_symbol t sym =
      let export_id = fresh_id () in
      export_id, add_symbol t sym export_id

    let symbol_to_export_id_map t = t.sym
    let export_id_to_descr_map t = !(t.ex_table)
  end

  (* CR-someday mshinwell: The half-mutable nature of [t] with sharing of
     the [ex_table] is kind of nasty.  Consider making it immutable. *)
  type t =
    { var : Export_info.approx Variable.Map.t;
      sym : Export_id.t Symbol.Map.t;
      ex_table : Export_info.descr Export_id.Map.t ref;
    }

  let empty_of_global (env : Global.t) =
    { var = Variable.Map.empty;
      sym = env.sym;
      ex_table = env.ex_table;
    }

  let extern_id_descr export_id =
    let export = Compilenv.approx_env () in
    try Some (Export_info.find_description export export_id)
    with Not_found -> None

  let extern_symbol_descr sym =
    if Compilenv.is_predefined_exception sym
    then None
    else
      let export = Compilenv.approx_for_global (Symbol.compilation_unit sym) in
      try
        let id = Symbol.Map.find sym export.symbol_id in
        let descr = Export_info.find_description export id in
        Some descr
      with
      | Not_found -> None

  let get_id_descr t export_id =
    try Some (Export_id.Map.find export_id !(t.ex_table))
    with Not_found -> extern_id_descr export_id

  let get_symbol_descr t sym =
    try
      let export_id = Symbol.Map.find sym t.sym in
      Some (Export_id.Map.find export_id !(t.ex_table))
    with
    | Not_found -> extern_symbol_descr sym

  let get_descr t (approx : Export_info.approx) =
    match approx with
    | Value_unknown -> None
    | Value_id export_id -> get_id_descr t export_id
    | Value_symbol sym -> get_symbol_descr t sym

  let record_descr t id (descr : Export_info.descr) =
    if Export_id.Map.mem id !(t.ex_table) then begin
      Misc.fatal_errorf "Build_export_info.Env.record_descr: cannot rebind \
          export ID %a in environment"
        Export_id.print id
    end;
    t.ex_table := Export_id.Map.add id descr !(t.ex_table)

  let new_descr t (descr : Export_info.descr) =
    let id = fresh_id () in
    record_descr t id descr;
    id

  let new_unit_descr t =
    new_descr t (Value_constptr 0)

  let add_approx t var approx =
    if Variable.Map.mem var t.var then begin
      Misc.fatal_errorf "Build_export_info.Env.add_approx: cannot rebind \
          variable %a in environment"
        Variable.print var
    end;
    { t with var = Variable.Map.add var approx t.var; }

  let add_approx_map t vars_to_approxs =
    Variable.Map.fold (fun var approx t -> add_approx t var approx)
      vars_to_approxs
      t

  let add_approx_maps t vars_to_approxs_list =
    List.fold_left add_approx_map t vars_to_approxs_list

  let find_approx t var : Export_info.approx =
    try Variable.Map.find var t.var with
    | Not_found -> Value_unknown
end

let descr_of_constant (c : Flambda.const) : Export_info.descr =
  match c with
  (* [Const_pointer] is an immediate value of a type whose values may be
     boxed (typically a variant type with both constant and non-constant
     constructors). *)
  | Int i -> Value_int i
  | Char c -> Value_char c
  | Const_pointer i -> Value_constptr i

let descr_of_allocated_constant (c : Allocated_const.t) : Export_info.descr =
  match c with
  | Float f -> Value_float f
  | Int32 i -> Value_boxed_int (Int32, i)
  | Int64 i -> Value_boxed_int (Int64, i)
  | Nativeint i -> Value_boxed_int (Nativeint, i)
  | String s ->
    let v_string : Export_info.value_string =
      { size = String.length s; contents = Unknown_or_mutable; }
    in
    Value_string v_string
  | Immutable_string s ->
    let v_string : Export_info.value_string =
      { size = String.length s; contents = Contents s; }
    in
    Value_string v_string
  | Immutable_float_array fs ->
    Value_float_array {
      contents = Contents (Array.map (fun x -> Some x) (Array.of_list fs));
      size = List.length fs;
    }
  | Float_array fs ->
    Value_float_array {
      contents = Unknown_or_mutable;
      size = List.length fs;
    }

let rec approx_of_expr (env : Env.t) (flam : Flambda.t) : Export_info.approx =
  match flam with
  | Var var -> Env.find_approx env var
  | Let { var; defining_expr; body; _ } ->
    let approx = descr_of_named env defining_expr in
    let env = Env.add_approx env var approx in
    approx_of_expr env body
  | Let_mutable { body } ->
    approx_of_expr env body
  | Let_rec (defs, body) ->
    let env =
      List.fold_left (fun env (var, defining_expr) ->
          let approx = descr_of_named env defining_expr in
          Env.add_approx env var approx)
        env defs
    in
    approx_of_expr env body
  | Apply { func; kind; _ } ->
    begin match kind with
    | Indirect -> Value_unknown
    | Direct closure_id' ->
      match Env.get_descr env (Env.find_approx env func) with
      | Some (Value_closure
          { closure_id; set_of_closures = { results; _ }; }) ->
        assert (Closure_id.equal closure_id closure_id');
        assert (Closure_id.Map.mem closure_id results);
        Closure_id.Map.find closure_id results
      | _ -> Value_unknown
    end
  | Assign _ -> Value_id (Env.new_unit_descr env)
  | For _ -> Value_id (Env.new_unit_descr env)
  | While _ -> Value_id (Env.new_unit_descr env)
  | Static_raise _ | Static_catch _ | Try_with _ | If_then_else _
  | Switch _ | String_switch _ | Send _ | Proved_unreachable ->
    Value_unknown

and descr_of_named (env : Env.t) (named : Flambda.named)
      : Export_info.approx =
  match named with
  | Expr expr -> approx_of_expr env expr
  | Symbol sym -> Value_symbol sym
  | Read_mutable _ -> Value_unknown
  | Read_symbol_field (sym, i) ->
    begin match Env.get_symbol_descr env sym with
    | Some (Value_block (_, fields)) when Array.length fields > i -> fields.(i)
    | _ -> Value_unknown
    end
  | Const const ->
    Value_id (Env.new_descr env (descr_of_constant const))
  | Allocated_const const ->
    Value_id (Env.new_descr env (descr_of_allocated_constant const))
  | Prim (Pmakeblock (tag, _, Immutable, _value_kind), args, _dbg) ->
    let approxs = List.map (Env.find_approx env) args in
    let descr : Export_info.descr =
      Value_block (Tag.create_exn tag, Array.of_list approxs)
    in
    Value_id (Env.new_descr env descr)
  | Prim (Pfield (i,_), [arg], _) ->
    begin match Env.get_descr env (Env.find_approx env arg) with
    | Some (Value_block (_, fields)) when Array.length fields > i -> fields.(i)
    | _ -> Value_unknown
    end
  | Prim (Pgetglobal id, _, _) ->
    Value_symbol (Compilenv.symbol_for_global' id)
  | Prim _ -> Value_unknown
  | Set_of_closures set ->
    let descr : Export_info.descr =
      Value_set_of_closures (describe_set_of_closures env set)
    in
    Value_id (Env.new_descr env descr)
  | Project_closure { set_of_closures; closure_id; } ->
    begin match Env.get_descr env (Env.find_approx env set_of_closures) with
    | Some (Value_set_of_closures set_of_closures) ->
      if not (Closure_id.Map.mem closure_id set_of_closures.results) then begin
        Misc.fatal_errorf "Could not build export description for \
            [Project_closure]: closure ID %a not in set of closures"
          Closure_id.print closure_id
      end;
      let descr : Export_info.descr =
        Value_closure { closure_id = closure_id; set_of_closures; }
      in
      Value_id (Env.new_descr env descr)
    | _ ->
      (* It would be nice if this were [assert false], but owing to the fact
         that this pass may propagate less information than for example
         [Inline_and_simplify], we might end up here. *)
      Value_unknown
    end
  | Move_within_set_of_closures { closure; start_from; move_to; } ->
    begin match Env.get_descr env (Env.find_approx env closure) with
    | Some (Value_closure { set_of_closures; closure_id; }) ->
      assert (Closure_id.equal closure_id start_from);
      let descr : Export_info.descr =
        Value_closure { closure_id = move_to; set_of_closures; }
      in
      Value_id (Env.new_descr env descr)
    | _ -> Value_unknown
    end
  | Project_var { closure; closure_id = closure_id'; var; } ->
    begin match Env.get_descr env (Env.find_approx env closure) with
    | Some (Value_closure
        { set_of_closures = { bound_vars; _ }; closure_id; }) ->
      assert (Closure_id.equal closure_id closure_id');
      if not (Var_within_closure.Map.mem var bound_vars) then begin
        Misc.fatal_errorf "Project_var from %a (closure ID %a) of \
            variable %a that is not bound by the closure.  \
            Variables bound by the closure are: %a"
          Variable.print closure
          Closure_id.print closure_id
          Var_within_closure.print var
          (Var_within_closure.Map.print (fun _ _ -> ())) bound_vars
      end;
      Var_within_closure.Map.find var bound_vars
    | _ -> Value_unknown
    end

and describe_set_of_closures env (set : Flambda.set_of_closures)
      : Export_info.value_set_of_closures =
  let bound_vars_approx =
    Variable.Map.map (fun (external_var : Flambda.specialised_to) ->
        Env.find_approx env external_var.var)
      set.free_vars
  in
  let specialised_args_approx =
    Variable.Map.map (fun (spec_to : Flambda.specialised_to) ->
        Env.find_approx env spec_to.var)
      set.specialised_args
  in
  let closures_approx =
    (* To build an approximation of the results, we need an
       approximation of the functions. The first one we can build is
       one where every function returns something unknown.
    *)
    (* CR-someday pchambart: we could improve a bit on that by building a
       recursive approximation of the closures: The value_closure
       description contains a [value_set_of_closures]. We could replace
       this field by a [Expr_id.t] or an [approx].
       mshinwell: Deferred for now.
    *)
    let initial_value_set_of_closures =
      { Export_info.
        set_of_closures_id = set.function_decls.set_of_closures_id;
        bound_vars = Var_within_closure.wrap_map bound_vars_approx;
        results =
          Closure_id.wrap_map
            (Variable.Map.map (fun _ -> Export_info.Value_unknown)
              set.function_decls.funs);
        aliased_symbol = None;
      }
    in
    Variable.Map.mapi (fun fun_var _function_decl ->
        let descr : Export_info.descr =
          Value_closure
            { closure_id = Closure_id.wrap fun_var;
              set_of_closures = initial_value_set_of_closures;
            }
        in
        Export_info.Value_id (Env.new_descr env descr))
      set.function_decls.funs
  in
  let closure_env =
    Env.add_approx_maps env
      [closures_approx; bound_vars_approx; specialised_args_approx]
  in
  let results =
    let result_approx _var (function_decl : Flambda.function_declaration) =
      approx_of_expr closure_env function_decl.body
    in
    Variable.Map.mapi result_approx set.function_decls.funs
  in
  { set_of_closures_id = set.function_decls.set_of_closures_id;
    bound_vars = Var_within_closure.wrap_map bound_vars_approx;
    results = Closure_id.wrap_map results;
    aliased_symbol = None;
  }

let approx_of_constant_defining_value_block_field env
      (c : Flambda.constant_defining_value_block_field) : Export_info.approx =
  match c with
  | Symbol s -> Value_symbol s
  | Const c -> Value_id (Env.new_descr env (descr_of_constant c))

let describe_constant_defining_value env export_id symbol
      (const : Flambda.constant_defining_value) =
  let env =
    (* Assignments of variables to export IDs are local to each constant
       defining value. *)
    Env.empty_of_global env
  in
  match const with
  | Allocated_const alloc_const ->
    let descr = descr_of_allocated_constant alloc_const in
    Env.record_descr env export_id descr
  | Block (tag, fields) ->
    let approxs =
      List.map (approx_of_constant_defining_value_block_field env) fields
    in
    Env.record_descr env export_id (Value_block (tag, Array.of_list approxs))
  | Set_of_closures set_of_closures ->
    let descr : Export_info.descr =
      Value_set_of_closures
        { (describe_set_of_closures env set_of_closures) with
          aliased_symbol = Some symbol;
        }
    in
    Env.record_descr env export_id descr
  | Project_closure (sym, closure_id) ->
    begin match Env.get_symbol_descr env sym with
    | Some (Value_set_of_closures set_of_closures) ->
      if not (Closure_id.Map.mem closure_id set_of_closures.results) then begin
        Misc.fatal_errorf "Could not build export description for \
            [Project_closure] constant defining value: closure ID %a not in \
            set of closures"
          Closure_id.print closure_id
      end;
      let descr =
        Export_info.Value_closure
          { closure_id = closure_id; set_of_closures; }
      in
      Env.record_descr env export_id descr
    | None ->
      Misc.fatal_errorf
        "Cannot project symbol %a to closure_id %a.  \
          No available export description@."
        Symbol.print sym
        Closure_id.print closure_id
    | Some (Value_closure _) ->
      Misc.fatal_errorf
        "Cannot project symbol %a to closure_id %a.  \
          The symbol is a closure instead of a set of closures.@."
        Symbol.print sym
        Closure_id.print closure_id
    | Some _ ->
      Misc.fatal_errorf
        "Cannot project symbol %a to closure_id %a.  \
          The symbol is not a set of closures.@."
        Symbol.print sym
        Closure_id.print closure_id
    end

let describe_program (env : Env.Global.t) (program : Flambda.program) =
  let rec loop env (program : Flambda.program_body) =
    match program with
    | Let_symbol (symbol, constant_defining_value, program) ->
      let id, env = Env.Global.new_symbol env symbol in
      describe_constant_defining_value env id symbol constant_defining_value;
      loop env program
    | Let_rec_symbol (defs, program) ->
      let env, defs =
        List.fold_left (fun (env, defs) (symbol, def) ->
            let id, env = Env.Global.new_symbol env symbol in
            env, ((id, symbol, def) :: defs))
          (env, []) defs
      in
      (* [Project_closure]s are separated to be handled last.  They are the
         only values that need a description for their argument. *)
      let project_closures, other_constants =
        List.partition (function
            | _, _, Flambda.Project_closure _ -> true
            | _ -> false)
          defs
      in
      List.iter (fun (id, symbol, def) ->
          describe_constant_defining_value env id symbol def)
        other_constants;
      List.iter (fun (id, symbol, def) ->
          describe_constant_defining_value env id symbol def)
        project_closures;
      loop env program
    | Initialize_symbol (symbol, tag, fields, program) ->
      let id =
        let env =
          (* Assignments of variables to export IDs are local to each
             [Initialize_symbol] construction. *)
          Env.empty_of_global env
        in
        let field_approxs = List.map (approx_of_expr env) fields in
        let descr : Export_info.descr =
          Value_block (tag, Array.of_list field_approxs)
        in
        Env.new_descr env descr
      in
      let env = Env.Global.add_symbol env symbol id in
      loop env program
    | Effect (_expr, program) -> loop env program
    | End symbol -> symbol, env
  in
  loop env program.program_body

let build_export_info ~(backend : (module Backend_intf.S))
      (program : Flambda.program) : Export_info.t =
  if !Clflags.opaque then
    Export_info.empty
  else
    (* CR-soon pchambart: Should probably use that instead of the ident of
       the module as global identifier.
       mshinwell: Is "that" the variable "_global_symbol"?
       Yes it is.  We are just assuming that the symbol produced from
       the identifier of the module is the right one. *)
    let _global_symbol, env =
      describe_program (Env.Global.create_empty ()) program
    in
    let sets_of_closures =
      Flambda_utils.all_function_decls_indexed_by_set_of_closures_id program
    in
    let closures =
      Flambda_utils.all_function_decls_indexed_by_closure_id program
    in
    let invariant_params =
      Set_of_closures_id.Map.map
        (fun { Flambda. function_decls; _ } ->
           Invariant_params.invariant_params_in_recursion
             ~backend function_decls)
        (Flambda_utils.all_sets_of_closures_map program)
    in
    let unnested_values =
      Env.Global.export_id_to_descr_map env
    in
    let invariant_params =
      let export = Compilenv.approx_env () in
      Export_id.Map.fold (fun _eid (descr:Export_info.descr)
                           (invariant_params) ->
          match descr with
          | Value_closure { set_of_closures }
          | Value_set_of_closures set_of_closures ->
            let { Export_info.set_of_closures_id } = set_of_closures in
            begin match
              Set_of_closures_id.Map.find set_of_closures_id
                export.invariant_params
            with
            | exception Not_found ->
              invariant_params
            | (set:Variable.Set.t Variable.Map.t) ->
              Set_of_closures_id.Map.add set_of_closures_id set invariant_params
            end
          | _ ->
            invariant_params)
        unnested_values invariant_params
    in
    let values =
      Export_info.nest_eid_map unnested_values
    in
    Export_info.create ~values
      ~symbol_id:(Env.Global.symbol_to_export_id_map env)
      ~offset_fun:Closure_id.Map.empty
      ~offset_fv:Var_within_closure.Map.empty
      ~sets_of_closures ~closures
      ~constant_sets_of_closures:Set_of_closures_id.Set.empty
      ~invariant_params
