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

(* CR-someday mshinwell: move to Flambda_utils *)
let rec tail_variable : Flambda.t -> Variable.t option = function
  | Var v -> Some v
  | Let_rec (_, e)
  | Let_mutable { body = e }
  | Let { body = e; _ } -> tail_variable e
  | _ -> None

let closure_symbol ~(backend : (module Backend_intf.S)) closure_id =
  let module Backend = (val backend) in
  Backend.closure_symbol closure_id

let make_variable_symbol prefix var =
  Symbol.create (Compilation_unit.get_current_exn ())
    (Linkage_name.create
       (prefix ^ Variable.unique_name (Variable.rename var)))

(** Traverse the given expression assigning symbols to [let]- and [let rec]-
    bound constant variables.  At the same time collect the definitions of
    such variables. *)
let assign_symbols_and_collect_constant_definitions
    ~(backend : (module Backend_intf.S))
    ~(program : Flambda.program)
    ~(inconstants : Inconstant_idents.result) =
  let var_to_symbol_tbl = Variable.Tbl.create 42 in
  let var_to_definition_tbl = Variable.Tbl.create 42 in
  let module AA = Alias_analysis in
  let assign_symbol var (named : Flambda.named) =
    if not (Inconstant_idents.variable var inconstants) then begin
      let assign_symbol () =
        let symbol = make_variable_symbol "" var in
        Variable.Tbl.add var_to_symbol_tbl var symbol
      in
      let assign_existing_symbol = Variable.Tbl.add var_to_symbol_tbl var in
      let record_definition = Variable.Tbl.add var_to_definition_tbl var in
      match named with
      | Symbol symbol ->
        assign_existing_symbol symbol;
        record_definition (AA.Symbol symbol)
      | Const const -> record_definition (AA.Const const)
      | Allocated_const const ->
        assign_symbol ();
        record_definition (AA.Allocated_const (Normal const))
      | Read_mutable _ ->
        (* [Inconstant_idents] always marks these expressions as
           inconstant, so we should never get here. *)
        assert false
      | Prim (Pmakeblock (tag, _, _,  _value_kind), fields, _) ->
        assign_symbol ();
        record_definition (AA.Block (Tag.create_exn tag, fields))
      | Read_symbol_field (symbol, field) ->
        record_definition (AA.Symbol_field (symbol, field))
      | Set_of_closures (
          { function_decls = { funs; set_of_closures_id; _ };
            _ } as set) ->
        assert (not (Inconstant_idents.closure set_of_closures_id
                       inconstants));
        assign_symbol ();
        record_definition (AA.Set_of_closures set);
        Variable.Map.iter (fun fun_var _ ->
            let closure_id = Closure_id.wrap fun_var in
            let closure_symbol = closure_symbol ~backend closure_id in
            Variable.Tbl.add var_to_symbol_tbl fun_var closure_symbol;
            let project_closure =
              Alias_analysis.Project_closure
                { set_of_closures = var; closure_id }
            in
            Variable.Tbl.add var_to_definition_tbl fun_var
              project_closure)
          funs
      | Move_within_set_of_closures ({ closure = _; start_from = _; move_to; }
          as move) ->
        assign_existing_symbol (closure_symbol ~backend  move_to);
        record_definition (AA.Move_within_set_of_closures move)
      | Project_closure ({ closure_id } as project_closure) ->
        assign_existing_symbol (closure_symbol ~backend  closure_id);
        record_definition (AA.Project_closure project_closure)
      | Prim (Pfield (index,_), [block], _) ->
        record_definition (AA.Field (block, index))
      | Prim (Pfield _, _, _) ->
        Misc.fatal_errorf "[Pfield] with the wrong number of arguments"
          Flambda.print_named named
      | Prim (Pmakearray (Pfloatarray as kind, mutability), args, _) ->
        assign_symbol ();
        record_definition (AA.Allocated_const (Array (kind, mutability, args)))
      | Prim (Pduparray (kind, mutability), [arg], _) ->
        assign_symbol ();
        record_definition (AA.Allocated_const (
          Duplicate_array (kind, mutability, arg)))
      | Prim _ ->
        Misc.fatal_errorf "Primitive not expected to be constant: @.%a@."
          Flambda.print_named named
      | Project_var project_var ->
        record_definition (AA.Project_var project_var)
      | Expr e ->
        match tail_variable e with
        | None -> assert false  (* See [Inconstant_idents]. *)
        | Some v -> record_definition (AA.Variable v)
    end
  in
  let assign_symbol_program expr =
    Flambda_iterators.iter_all_immutable_let_and_let_rec_bindings expr
      ~f:assign_symbol
  in
  Flambda_iterators.iter_exprs_at_toplevel_of_program program
    ~f:assign_symbol_program;
  let let_symbol_to_definition_tbl = Symbol.Tbl.create 42 in
  let initialize_symbol_to_definition_tbl = Symbol.Tbl.create 42 in
  let rec collect_let_and_initialize_symbols (program : Flambda.program_body) =
    match program with
    | Let_symbol (symbol, decl, program) ->
      Symbol.Tbl.add let_symbol_to_definition_tbl symbol decl;
      collect_let_and_initialize_symbols program
    | Let_rec_symbol (decls, program) ->
      List.iter (fun (symbol, decl) ->
          Symbol.Tbl.add let_symbol_to_definition_tbl symbol decl)
        decls;
      collect_let_and_initialize_symbols program
    | Effect (_, program) -> collect_let_and_initialize_symbols program
    | Initialize_symbol (symbol,_tag,fields,program) ->
      collect_let_and_initialize_symbols program;
      let fields = List.map tail_variable fields in
      Symbol.Tbl.add initialize_symbol_to_definition_tbl symbol fields
    | End _ -> ()
  in
  collect_let_and_initialize_symbols program.program_body;
  let record_set_of_closure_equalities
        (set_of_closures : Flambda.set_of_closures) =
    Variable.Map.iter (fun arg (var : Flambda.specialised_to) ->
        if not (Inconstant_idents.variable arg inconstants) then
          Variable.Tbl.add var_to_definition_tbl arg (AA.Variable var.var))
      set_of_closures.free_vars;
    Variable.Map.iter (fun arg (spec_to : Flambda.specialised_to) ->
        if not (Inconstant_idents.variable arg inconstants) then
          Variable.Tbl.add var_to_definition_tbl arg
            (AA.Variable spec_to.var))
      set_of_closures.specialised_args
  in
  Flambda_iterators.iter_on_set_of_closures_of_program program
    ~f:(fun ~constant set_of_closures ->
      record_set_of_closure_equalities set_of_closures;
      if constant then begin
        Variable.Map.iter (fun fun_var _ ->
            let closure_id = Closure_id.wrap fun_var in
            let closure_symbol = closure_symbol ~backend closure_id in
            Variable.Tbl.add var_to_definition_tbl fun_var
              (AA.Symbol closure_symbol);
            Variable.Tbl.add var_to_symbol_tbl fun_var closure_symbol)
          set_of_closures.Flambda.function_decls.funs
      end);
  var_to_symbol_tbl, var_to_definition_tbl,
    let_symbol_to_definition_tbl, initialize_symbol_to_definition_tbl

let variable_field_definition
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl :
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (var : Variable.t) : Flambda.constant_defining_value_block_field =
  try
    Symbol (Variable.Tbl.find var_to_symbol_tbl var)
  with Not_found ->
    match Variable.Tbl.find var_to_definition_tbl var with
    | Const c -> Const c
    | const_defining_value ->
      Misc.fatal_errorf "Unexpected pattern for a constant: %a: %a"
        Variable.print var
        Alias_analysis.print_constant_defining_value const_defining_value
    | exception Not_found ->
      Misc.fatal_errorf "No associated symbol for the constant %a"
        Variable.print var

let resolve_variable
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl :
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (var : Variable.t) : Flambda.constant_defining_value_block_field =
  match Variable.Map.find var aliases with
  | exception Not_found ->
    variable_field_definition var_to_symbol_tbl var_to_definition_tbl var
  | Symbol s -> Symbol s
  | Variable aliased_variable ->
    variable_field_definition var_to_symbol_tbl var_to_definition_tbl
      aliased_variable

let translate_set_of_closures
    (inconstants : Inconstant_idents.result)
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (set_of_closures : Flambda.set_of_closures) =
  let f var (named : Flambda.named) : Flambda.named =
    if Inconstant_idents.variable var inconstants then
      named
    else
      let resolved =
        resolve_variable
          aliases
          var_to_symbol_tbl
          var_to_definition_tbl
          var
      in
      match resolved with
      | Symbol s -> Symbol s
      | Const c -> Const c
  in
  Flambda_iterators.map_function_bodies set_of_closures
    ~f:(Flambda_iterators.map_all_immutable_let_and_let_rec_bindings ~f)

let translate_constant_set_of_closures
    (inconstants : Inconstant_idents.result)
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (constant_defining_values : Flambda.constant_defining_value Symbol.Map.t) =
  Symbol.Map.map (fun (const : Flambda.constant_defining_value) ->
      match const with
      | Flambda.Allocated_const _
      | Flambda.Block _
      | Flambda.Project_closure _ ->
        const
      | Flambda.Set_of_closures set_of_closures ->
        let set_of_closures =
          translate_set_of_closures
            (inconstants : Inconstant_idents.result)
            (aliases : Alias_analysis.allocation_point Variable.Map.t)
            (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
            (var_to_definition_tbl:
              Alias_analysis.constant_defining_value Variable.Tbl.t)
            (set_of_closures : Flambda.set_of_closures)
        in
        Flambda.Set_of_closures set_of_closures)
    constant_defining_values

let find_original_set_of_closure
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    project_closure_map
    var =
  let rec loop var =
    match Variable.Map.find var aliases with
    | Variable var ->
      begin match Variable.Tbl.find var_to_definition_tbl var with
        | Project_closure { set_of_closures = var }
        | Move_within_set_of_closures { closure = var } ->
          loop var
        | Set_of_closures _ -> begin
            match Variable.Tbl.find var_to_symbol_tbl var with
            | s ->
              s
            | exception Not_found ->
              Format.eprintf "var: %a@." Variable.print var;
              assert false
          end
        | _ -> assert false
      end
    | Symbol s ->
      match Symbol.Map.find s project_closure_map with
      | exception Not_found ->
        Misc.fatal_errorf "find_original_set_of_closure: cannot find \
            symbol %a in the project-closure map"
          Symbol.print s
      | s -> s
  in
  loop var

let translate_definition_and_resolve_alias inconstants
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl :
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (symbol_definition_map : Flambda.constant_defining_value Symbol.Map.t)
    (project_closure_map : Symbol.t Symbol.Map.t)
    (definition : Alias_analysis.constant_defining_value)
    ~(backend : (module Backend_intf.S))
    : Flambda.constant_defining_value option =
  let resolve_float_array_involving_variables
        ~(mutability : Asttypes.mutable_flag) ~vars =
    (* Resolve an [Allocated_const] of the form:
        [Array (Pfloatarray, _, _)]
       (which references its contents via variables; it does not contain
        manifest floats). *)
    let find_float_var_definition var =
      match Variable.Tbl.find var_to_definition_tbl var with
      | Allocated_const (Normal (Float f)) -> f
      | const_defining_value ->
          Misc.fatal_errorf "Bad definition for float array member %a: %a"
            Variable.print var
            Alias_analysis.print_constant_defining_value
            const_defining_value
    in
    let find_float_symbol_definition sym =
      match Symbol.Map.find sym symbol_definition_map with
      | Allocated_const (Float f) -> f
      | const_defining_value ->
          Misc.fatal_errorf "Bad definition for float array member %a: %a"
            Symbol.print sym
            Flambda.print_constant_defining_value
            const_defining_value
    in
    let floats =
      List.map (fun var ->
          match Variable.Map.find var aliases with
          | exception Not_found -> find_float_var_definition var
          | Variable var -> find_float_var_definition var
          | Symbol sym -> find_float_symbol_definition sym)
        vars
    in
    let const : Allocated_const.t =
      match mutability with
      | Immutable -> Immutable_float_array floats
      | Mutable -> Float_array floats
    in
    Some (Flambda.Allocated_const const)
  in
  match definition with
  | Block (tag, fields) ->
    Some (Flambda.Block (tag,
      List.map (resolve_variable aliases var_to_symbol_tbl
          var_to_definition_tbl)
        fields))
  | Allocated_const (Normal const) -> Some (Flambda.Allocated_const const)
  | Allocated_const (Duplicate_array (Pfloatarray, mutability, var)) ->
    (* CR-someday mshinwell: This next section could do with cleanup.
       What happens is:
        - Duplicate contains a variable, which is resolved to
        a float array thing full of variables;
        - We send that value back through this function again so the
        individual members of that array are resolved from variables to
        floats.
        - Then we can build the Flambda.name term containing the
        Allocated_const (full of floats).
       We should maybe factor out the code from the
       Allocated_const (Array (...)) case below so this function doesn't have
       to be recursive. *)
    let (constant_defining_value : Alias_analysis.constant_defining_value) =
      match Variable.Map.find var aliases with
      | exception Not_found ->
        Variable.Tbl.find var_to_definition_tbl var
      | Variable var ->
        Variable.Tbl.find var_to_definition_tbl var
      | Symbol sym ->
        match Symbol.Map.find sym symbol_definition_map with
        | Allocated_const ((Immutable_float_array _) as const) ->
          Alias_analysis.Allocated_const (Normal const)
        | (Allocated_const _ | Block _ | Set_of_closures _
            | Project_closure _) as wrong ->
          Misc.fatal_errorf
            "Lift_constants.translate_definition_and_resolve_alias: \
              Duplicate Pfloatarray %a with symbol %a mapping to \
              wrong constant defining value %a"
            Variable.print var
            Alias_analysis.print_constant_defining_value definition
            Flambda.print_constant_defining_value wrong
        | exception Not_found ->
          let module Backend = (val backend) in
          match (Backend.import_symbol sym).descr with
          | Value_unresolved _ ->
            Misc.fatal_errorf
              "Lift_constants.translate_definition_and_resolve_alias: \
               Duplicate Pfloatarray %a with unknown symbol: %a"
              Variable.print var
              Alias_analysis.print_constant_defining_value definition
          | Value_float_array value_float_array ->
            let contents =
              Simple_value_approx.float_array_as_constant value_float_array
            in
            begin match contents with
            | None ->
              Misc.fatal_errorf
                "Lift_constants.translate_definition_and_resolve_alias: \
                 Duplicate Pfloatarray %a with not completely known float \
                 array from symbol: %a"
                Variable.print var
                Alias_analysis.print_constant_defining_value definition
            | Some l ->
              Alias_analysis.Allocated_const (Normal (Immutable_float_array l))
            end
          | wrong ->
            (* CR-someday mshinwell: we might hit this if we ever duplicate
               a mutable array across compilation units (e.g. "snapshotting"
               an array).  We do not currently generate such code. *)
            Misc.fatal_errorf
              "Lift_constants.translate_definition_and_resolve_alias: \
               Duplicate Pfloatarray %a with symbol %a that does not \
               have an export description of an immutable array"
              Variable.print var
              Alias_analysis.print_constant_defining_value definition
              Simple_value_approx.print_descr wrong
    in
    begin match constant_defining_value with
    | Allocated_const (Normal (Float_array _)) ->
      (* This example from pchambart illustrates why we do not allow
         the duplication of mutable arrays:

         {|
         let_symbol a = Allocated_const (Immutable_float_array [|0.|])
         initialize_symbol b = Duparray(Mutable, a)
         effect b.(0) <- 1.
         initialize_symbol c = Duparray(Mutable, b)
         |}

         This will be converted to:
         {|
         let_symbol a = Allocated_const (Immutable_float_array [|0.|])
         let_symbol b = Allocated_const (Float_array [|0.|])
         effect b.(0) <- 1.
         let_symbol c = Allocated_const (Float_array [|0.|])
         |}

         We can't encounter that currently, but it's scary.
      *)
      Misc.fatal_error "Pduparray is not allowed on mutable arrays"
    | Allocated_const (Normal (Immutable_float_array floats)) ->
      let const : Allocated_const.t =
        match mutability with
        | Immutable -> Immutable_float_array floats
        | Mutable -> Float_array floats
      in
      Some (Flambda.Allocated_const const)
    | Allocated_const (Array (Pfloatarray, _, vars)) ->
      (* Important: [mutability] is from the [Duplicate_array]
         construction above. *)
      resolve_float_array_involving_variables ~mutability ~vars
    | const ->
      Misc.fatal_errorf
        "Lift_constants.translate_definition_and_resolve_alias: \
          Duplicate Pfloatarray %a with wrong argument: %a"
        Variable.print var
        Alias_analysis.print_constant_defining_value const
    end
  | Allocated_const (Duplicate_array (_, _, _)) ->
    Misc.fatal_errorf "Lift_constants.translate_definition_and_resolve_alias: \
        Duplicate_array with non-Pfloatarray kind: %a"
      Alias_analysis.print_constant_defining_value definition
  | Allocated_const (Array (Pfloatarray, mutability, vars)) ->
    resolve_float_array_involving_variables ~mutability ~vars
  | Allocated_const (Array (_, _, _)) ->
    Misc.fatal_errorf "Lift_constants.translate_definition_and_resolve_alias: \
        Array with non-Pfloatarray kind: %a"
      Alias_analysis.print_constant_defining_value definition
  | Project_closure { set_of_closures; closure_id } ->
    begin match Variable.Map.find set_of_closures aliases with
    | Symbol s ->
      Some (Flambda.Project_closure (s, closure_id))
    (* If a closure projection is a constant, the set of closures must
       be assigned to a symbol. *)
    | exception Not_found ->
      assert false
    | Variable v ->
      match Variable.Tbl.find var_to_symbol_tbl v with
      | s ->
        Some (Flambda.Project_closure (s, closure_id))
      | exception Not_found ->
        Format.eprintf "var: %a@." Variable.print v;
        assert false
    end
  | Move_within_set_of_closures { closure; move_to } ->
    let set_of_closure_symbol =
      find_original_set_of_closure
        aliases
        var_to_symbol_tbl
        var_to_definition_tbl
        project_closure_map
        closure
    in
    Some (Flambda.Project_closure (set_of_closure_symbol, move_to))
  | Set_of_closures set_of_closures ->
    let set_of_closures =
      translate_set_of_closures
        inconstants
        aliases
        var_to_symbol_tbl
        var_to_definition_tbl
        set_of_closures
    in
    Some (Flambda.Set_of_closures set_of_closures)
  | Project_var _ -> None
  | Field (_,_) | Symbol_field _ -> None
  | Const _ -> None
  | Symbol _ -> None
  | Variable _ -> None

let translate_definitions_and_resolve_alias
    inconstants
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    symbol_definition_map
    project_closure_map
    ~backend =
  Variable.Tbl.fold (fun var def map ->
      match
        translate_definition_and_resolve_alias inconstants aliases ~backend
          var_to_symbol_tbl var_to_definition_tbl symbol_definition_map
          project_closure_map def
      with
      | None -> map
      | Some def ->
        let symbol = Variable.Tbl.find var_to_symbol_tbl var in
        Symbol.Map.add symbol def map)
    var_to_definition_tbl Symbol.Map.empty

(* Resorting of graph including Initialize_symbol *)
let constant_dependencies ~backend:_
        (const : Flambda.constant_defining_value) =
  match const with
  | Allocated_const _ -> Symbol.Set.empty
  | Block (_, fields) ->
    let symbol_fields =
      Misc.Stdlib.List.filter_map
        (function
          | (Symbol s : Flambda.constant_defining_value_block_field) -> Some s
          | Flambda.Const _ -> None)
        fields
    in
    Symbol.Set.of_list symbol_fields
  | Set_of_closures set_of_closures ->
    Flambda.free_symbols_named (Set_of_closures set_of_closures)
  | Project_closure (s, _) ->
    Symbol.Set.singleton s

let program_graph ~backend imported_symbols symbol_to_constant
    (initialize_symbol_tbl :
      (Tag.t * Flambda.t list * Symbol.t option) Symbol.Tbl.t)
    (effect_tbl : (Flambda.t * Symbol.t option) Symbol.Tbl.t) =
  let expression_symbol_dependencies expr = Flambda.free_symbols expr in
  let graph_with_only_constant_parts =
    Symbol.Map.map (fun const ->
        Symbol.Set.diff (constant_dependencies ~backend const)
          imported_symbols)
      symbol_to_constant
  in
  let graph_with_initialisation =
    Symbol.Tbl.fold (fun sym (_tag, fields, previous) ->
        let order_dep =
          match previous with
          | None -> Symbol.Set.empty
          | Some previous -> Symbol.Set.singleton previous
        in
        let deps = List.fold_left (fun set field ->
            Symbol.Set.union (expression_symbol_dependencies field) set)
            order_dep fields
        in
        let deps = Symbol.Set.diff deps imported_symbols in
        Symbol.Map.add sym deps)
      initialize_symbol_tbl graph_with_only_constant_parts
  in
  let graph =
    Symbol.Tbl.fold (fun sym (expr, previous) ->
        let order_dep =
          match previous with
          | None -> Symbol.Set.empty
          | Some previous -> Symbol.Set.singleton previous
        in
        let deps =
          Symbol.Set.union (expression_symbol_dependencies expr) order_dep
        in
        let deps = Symbol.Set.diff deps imported_symbols in
        Symbol.Map.add sym deps
      )
      effect_tbl graph_with_initialisation
  in
  let module Symbol_SCC = Strongly_connected_components.Make (Symbol) in
  let components =
    Symbol_SCC.connected_components_sorted_from_roots_to_leaf
      graph
  in
  components

(* rebuilding the program *)
let add_definition_of_symbol constant_definitions
    (initialize_symbol_tbl :
      (Tag.t * Flambda.t list * Symbol.t option) Symbol.Tbl.t)
    (effect_tbl : (Flambda.t * Symbol.t option) Symbol.Tbl.t)
    (program : Flambda.program_body) component : Flambda.program_body =
  let symbol_declaration sym =
    (* A symbol declared through an Initialize_symbol construct
       cannot be recursive, this is not allowed in the construction.
       This also couldn't have been introduced by this pass, so we can
       safely assert that this is not possible here *)
    assert(not (Symbol.Tbl.mem initialize_symbol_tbl sym));
    (sym, Symbol.Map.find sym constant_definitions)
  in
  let module Symbol_SCC = Strongly_connected_components.Make (Symbol) in
  match component with
  | Symbol_SCC.Has_loop l ->
    let l = List.map symbol_declaration l in
    Let_rec_symbol (l, program)
  | Symbol_SCC.No_loop sym ->
    match Symbol.Tbl.find initialize_symbol_tbl sym with
    | (tag, fields, _previous) ->
      Initialize_symbol (sym, tag, fields, program)
    | exception Not_found ->
      match Symbol.Tbl.find effect_tbl sym with
      | (expr, _previous) ->
        Effect (expr, program)
      | exception Not_found ->
        let decl = Symbol.Map.find sym constant_definitions in
        Let_symbol (sym, decl, program)

let add_definitions_of_symbols constant_definitions initialize_symbol_tbl
    effect_tbl program components =
  Array.fold_left
    (add_definition_of_symbol constant_definitions initialize_symbol_tbl
      effect_tbl)
    program components

let introduce_free_variables_in_set_of_closures
    (var_to_block_field_tbl :
      Flambda.constant_defining_value_block_field Variable.Tbl.t)
    ({ Flambda.function_decls; free_vars; specialised_args;
        direct_call_surrogates; }
      as set_of_closures) =
  let add_definition_and_make_substitution var (expr, subst) =
    let searched_var =
      match Variable.Map.find var specialised_args with
      | exception Not_found -> var
      | external_var ->
        (* specialised arguments bound to constant can be rewritten *)
        external_var.var
    in
    match Variable.Tbl.find var_to_block_field_tbl searched_var with
    | def ->
      let fresh = Variable.rename var in
      let named : Flambda.named = match def with
        | Symbol sym -> Symbol sym
        | Const c -> Const c
      in
      (Flambda.create_let fresh named expr), Variable.Map.add var fresh subst
    | exception Not_found ->
      (* The variable is bound by the closure or the arguments or not
         constant. In either case it does not need to be bound *)
      expr, subst
  in
  let done_something = ref false in
  let function_decls : Flambda.function_declarations =
    Flambda.update_function_declarations function_decls
      ~funs:(Variable.Map.map
          (fun (func_decl : Flambda.function_declaration) ->
             let variables_to_bind =
               (* Closures from the same set must not be bound. *)
               Variable.Set.diff func_decl.free_variables
                 (Variable.Map.keys function_decls.funs)
             in
             let body, subst =
               Variable.Set.fold add_definition_and_make_substitution
                 variables_to_bind
                 (func_decl.body, Variable.Map.empty)
             in
             if Variable.Map.is_empty subst then begin
               func_decl
             end else begin
               done_something := true;
               let body = Flambda_utils.toplevel_substitution subst body in
               Flambda.create_function_declaration
                 ~params:func_decl.params
                 ~body
                 ~stub:func_decl.stub
                 ~dbg:func_decl.dbg
                 ~inline:func_decl.inline
                 ~specialise:func_decl.specialise
                 ~is_a_functor:func_decl.is_a_functor
             end)
          function_decls.funs)
  in
  let free_vars =
    (* Keep only those that are not rewritten to constants. *)
    Variable.Map.filter (fun v _ ->
        let keep = not (Variable.Tbl.mem var_to_block_field_tbl v) in
        if not keep then done_something := true;
        keep)
      free_vars
  in
  let free_vars =
    Flambda_utils.clean_projections ~which_variables:free_vars
  in
  let specialised_args =
    (* Keep only those that are not rewritten to constants. *)
    Variable.Map.filter (fun _ (spec_to : Flambda.specialised_to) ->
        let keep =
          not (Variable.Tbl.mem var_to_block_field_tbl spec_to.var)
        in
        if not keep then begin
          done_something := true
        end;
        keep)
      specialised_args
  in
  let specialised_args =
    Flambda_utils.clean_projections ~which_variables:specialised_args
  in
  if not !done_something then
    set_of_closures
  else
    Flambda.create_set_of_closures ~function_decls ~free_vars
      ~specialised_args ~direct_call_surrogates

let rewrite_project_var
      (var_to_block_field_tbl
        : Flambda.constant_defining_value_block_field Variable.Tbl.t)
      (project_var : Flambda.project_var) ~original : Flambda.named =
  let var = Var_within_closure.unwrap project_var.var in
  match Variable.Tbl.find var_to_block_field_tbl var with
  | exception Not_found -> original
  | Symbol sym -> Symbol sym
  | Const const -> Const const

let introduce_free_variables_in_sets_of_closures
    (var_to_block_field_tbl:
      Flambda.constant_defining_value_block_field Variable.Tbl.t)
    (translate_definition : Flambda.constant_defining_value Symbol.Map.t) =
  Symbol.Map.map (fun (def : Flambda.constant_defining_value) ->
      match def with
      | Allocated_const _
      | Block _
      | Project_closure _ -> def
      | Set_of_closures set_of_closures ->
        Flambda.Set_of_closures
          (introduce_free_variables_in_set_of_closures
             var_to_block_field_tbl
             set_of_closures))
    translate_definition

let var_to_block_field
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl :
      Alias_analysis.constant_defining_value Variable.Tbl.t) =
  let var_to_block_field_tbl = Variable.Tbl.create 42 in
  Variable.Tbl.iter (fun var _ ->
      let def =
        resolve_variable aliases var_to_symbol_tbl var_to_definition_tbl var
      in
      Variable.Tbl.add var_to_block_field_tbl var def)
    var_to_definition_tbl;
  var_to_block_field_tbl

let program_symbols ~backend (program : Flambda.program) =
  let new_fake_symbol =
    let r = ref 0 in
    fun () ->
      incr r;
      Symbol.create (Compilation_unit.get_current_exn ())
        (Linkage_name.create ("fake_effect_symbol_" ^ string_of_int !r))
  in
  let initialize_symbol_tbl = Symbol.Tbl.create 42 in
  let effect_tbl = Symbol.Tbl.create 42 in
  let symbol_definition_tbl = Symbol.Tbl.create 42 in
  let add_project_closure_definitions def_symbol
        (const : Flambda.constant_defining_value) =
    match const with
    | Set_of_closures { function_decls = { funs } } ->
        Variable.Map.iter (fun fun_var _ ->
            let closure_id = Closure_id.wrap fun_var in
            let closure_symbol = closure_symbol ~backend closure_id in
            let project_closure =
              Flambda.Project_closure (def_symbol, closure_id)
            in
            Symbol.Tbl.add symbol_definition_tbl closure_symbol
              project_closure)
          funs
    | Project_closure _
    | Allocated_const _
    | Block _ -> ()
  in
  let rec loop (program : Flambda.program_body) previous_effect =
    match program with
    | Flambda.Let_symbol (symbol, def, program) ->
      add_project_closure_definitions symbol def;
      Symbol.Tbl.add symbol_definition_tbl symbol def;
      loop program previous_effect
    | Flambda.Let_rec_symbol (defs, program) ->
      List.iter (fun (symbol, def) ->
          add_project_closure_definitions symbol def;
          Symbol.Tbl.add symbol_definition_tbl symbol def)
        defs;
      loop program previous_effect
    | Flambda.Initialize_symbol (symbol, tag, fields, program) ->
      (* previous_effect is used to keep the order of initialize and effect
         values. Their effects order must be kept ordered.
         it is used as an extra dependency when sorting the symbols. *)
      (* CR-someday pchambart: if the fields expressions are pure, we could
         drop this dependency
         mshinwell: deferred CR *)
      Symbol.Tbl.add initialize_symbol_tbl symbol
        (tag, fields, previous_effect);
      loop program (Some symbol)
    | Flambda.Effect (expr, program) ->
      (* Used to ensure that effects are correctly ordered *)
      let fake_effect_symbol = new_fake_symbol () in
      Symbol.Tbl.add effect_tbl fake_effect_symbol (expr, previous_effect);
      loop program (Some fake_effect_symbol)
    | Flambda.End _ -> ()
  in
  loop program.program_body None;
  initialize_symbol_tbl, symbol_definition_tbl, effect_tbl

let replace_definitions_in_initialize_symbol_and_effects
    (inconstants : Inconstant_idents.result)
    (aliases : Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl :
      Alias_analysis.constant_defining_value Variable.Tbl.t)
    (initialize_symbol_tbl :
      (Tag.t * Flambda.t list * Symbol.t option) Symbol.Tbl.t)
    (effect_tbl : (Flambda.t * Symbol.t option) Symbol.Tbl.t) =
  let rewrite_expr expr =
    Flambda_iterators.map_all_immutable_let_and_let_rec_bindings expr
      ~f:(fun var (named : Flambda.named) : Flambda.named ->
        if Inconstant_idents.variable var inconstants then
          named
        else
          let resolved =
            resolve_variable
              aliases
              var_to_symbol_tbl
              var_to_definition_tbl
              var
          in
          match named, resolved with
          | Symbol s1, Symbol s2 ->
            assert (s1 == s2);  (* physical equality for speed *)
            named;
          | Const c1, Const c2 ->
            assert (c1 == c2);
            named
          | _, Symbol s -> Symbol s
          | _, Const c -> Const c)
  in
  (* This is safe because we only [replace] the current key during
     iteration (cf. https://github.com/ocaml/ocaml/pull/337) *)
  Symbol.Tbl.iter
    (fun symbol (tag, fields, previous) ->
      let fields = List.map rewrite_expr fields in
      Symbol.Tbl.replace initialize_symbol_tbl symbol (tag, fields, previous))
    initialize_symbol_tbl;
  Symbol.Tbl.iter
    (fun symbol (expr, previous) ->
      Symbol.Tbl.replace effect_tbl symbol (rewrite_expr expr, previous))
    effect_tbl

(* CR-soon mshinwell: Update the name of [project_closure_map]. *)
let project_closure_map symbol_definition_map =
  Symbol.Map.fold (fun sym (const : Flambda.constant_defining_value) acc ->
      match const with
      | Project_closure (set_of_closures, _) ->
        Symbol.Map.add sym set_of_closures acc
      | Set_of_closures _ ->
        Symbol.Map.add sym sym acc
      | Allocated_const _
      | Block _ -> acc)
    symbol_definition_map
    Symbol.Map.empty

let the_dead_constant_index = ref 0

let lift_constants (program : Flambda.program) ~backend =
  let the_dead_constant =
    let index = !the_dead_constant_index in
    incr the_dead_constant_index;
    let name = Printf.sprintf "the_dead_constant_%d" index in
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create name)
  in
  let program_body : Flambda.program_body =
    Let_symbol (the_dead_constant, Allocated_const (Nativeint 0n),
      program.program_body)
  in
  let program : Flambda.program =
    { program with program_body; }
  in
  let inconstants =
    Inconstant_idents.inconstants_on_program program ~backend
      ~compilation_unit:(Compilation_unit.get_current_exn ())
  in
  let initialize_symbol_tbl, symbol_definition_tbl, effect_tbl =
    program_symbols ~backend program
  in
  let var_to_symbol_tbl, var_to_definition_tbl, let_symbol_to_definition_tbl,
      initialize_symbol_to_definition_tbl =
    assign_symbols_and_collect_constant_definitions ~backend ~program
      ~inconstants
  in
  let aliases =
    Alias_analysis.run var_to_definition_tbl
      initialize_symbol_to_definition_tbl
      let_symbol_to_definition_tbl
      ~the_dead_constant
  in
  replace_definitions_in_initialize_symbol_and_effects
      (inconstants : Inconstant_idents.result)
      (aliases : Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl
        : Alias_analysis.constant_defining_value Variable.Tbl.t)
      initialize_symbol_tbl
      effect_tbl;
  let symbol_definition_map =
    translate_constant_set_of_closures
      (inconstants : Inconstant_idents.result)
      (aliases : Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl
        : Alias_analysis.constant_defining_value Variable.Tbl.t)
      (Symbol.Tbl.to_map symbol_definition_tbl)
  in
  let project_closure_map = project_closure_map symbol_definition_map in
  let translated_definitions =
    translate_definitions_and_resolve_alias
      inconstants
      (aliases : Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl
        : Alias_analysis.constant_defining_value Variable.Tbl.t)
      symbol_definition_map
      project_closure_map
      ~backend
  in
  let var_to_block_field_tbl =
    var_to_block_field
      (aliases : Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl : Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl
        : Alias_analysis.constant_defining_value Variable.Tbl.t)
  in
  let translated_definitions =
    introduce_free_variables_in_sets_of_closures var_to_block_field_tbl
      translated_definitions
  in
  let constant_definitions =
    (* Add previous Let_symbol to the newly discovered ones *)
    Symbol.Map.union
      (fun _sym
        (c1:Flambda.constant_defining_value)
        (c2:Flambda.constant_defining_value) ->
        match c1, c2 with
        | Project_closure (s1, closure_id1),
          Project_closure (s2, closure_id2) when
            Symbol.equal s1 s2 &&
            Closure_id.equal closure_id1 closure_id2 ->
          Some c1
        | Project_closure (s1, closure_id1),
          Project_closure (s2, closure_id2) ->
          Format.eprintf "not equal project closure@. s %a %a@. cid %a %a@."
            Symbol.print s1 Symbol.print s2
            Closure_id.print closure_id1 Closure_id.print closure_id2;
          assert false
        | _ ->
          assert false
      )
      symbol_definition_map
      translated_definitions
  in
  (* Upon the [Initialize_symbol]s, the [Effect]s and the constant definitions,
     do the following:
     1. Introduce [Let]s to bind variables that are going to be replaced
     by constants.
     2. If a variable bound by a closure gets replaced by a symbol and
     thus eliminated from the [free_vars] set of the closure, we need to
     rewrite any subsequent [Project_var] expressions that project that
     variable. *)
  let rewrite_expr expr =
    Flambda_iterators.map_named (function
        | (Set_of_closures set_of_closures) as named ->
          let new_set_of_closures =
            introduce_free_variables_in_set_of_closures
              var_to_block_field_tbl set_of_closures
          in
          if new_set_of_closures == set_of_closures then
            named
          else
            Set_of_closures new_set_of_closures
        | (Project_var project_var) as original ->
          rewrite_project_var var_to_block_field_tbl project_var ~original
        | (Symbol _ | Const _ | Allocated_const _ | Project_closure _
        | Move_within_set_of_closures _ | Prim _ | Expr _
        | Read_mutable _ | Read_symbol_field _) as named -> named)
      expr
  in
  let constant_definitions =
    Symbol.Map.map (fun (const : Flambda.constant_defining_value) ->
        match const with
        | Allocated_const _ | Block _ | Project_closure _ -> const
        | Set_of_closures set_of_closures ->
          let set_of_closures =
            Flambda_iterators.map_function_bodies set_of_closures
              ~f:rewrite_expr
          in
          Flambda.Set_of_closures
            (introduce_free_variables_in_set_of_closures
              var_to_block_field_tbl set_of_closures))
    constant_definitions
  in
  let effect_tbl =
    Symbol.Tbl.map effect_tbl (fun (effect, dep) -> rewrite_expr effect, dep)
  in
  let initialize_symbol_tbl =
    Symbol.Tbl.map initialize_symbol_tbl (fun (tag, fields, dep) ->
      let fields = List.map rewrite_expr fields in
      tag, fields, dep)
  in
  let imported_symbols = Flambda_utils.imported_symbols program in
  let components =
    program_graph ~backend imported_symbols constant_definitions
      initialize_symbol_tbl effect_tbl
  in
  let program_body =
    add_definitions_of_symbols constant_definitions
      initialize_symbol_tbl
      effect_tbl
      (End (Flambda_utils.root_symbol program))
      components
  in
  Flambda_utils.introduce_needed_import_symbols { program with program_body; }
