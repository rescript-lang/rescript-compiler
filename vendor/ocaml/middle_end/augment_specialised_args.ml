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

module E = Inline_and_simplify_aux.Env
module B = Inlining_cost.Benefit

module Definition = struct
  type t =
    | Existing_inner_free_var of Variable.t
    | Projection_from_existing_specialised_arg of Projection.t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Existing_inner_free_var var1, Existing_inner_free_var var2 ->
        Variable.compare var1 var2
      | Projection_from_existing_specialised_arg proj1,
          Projection_from_existing_specialised_arg proj2 ->
        Projection.compare proj1 proj2
      | Existing_inner_free_var _, _ -> -1
      | _, Existing_inner_free_var _ -> 1

    let equal t1 t2 =
      (compare t1 t2) = 0

    let hash = Hashtbl.hash

    let print ppf t =
      match t with
      | Existing_inner_free_var var ->
        Format.fprintf ppf "Existing_inner_free_var %a"
          Variable.print var
      | Projection_from_existing_specialised_arg projection ->
        Format.fprintf ppf "Projection_from_existing_specialised_arg %a"
          Projection.print projection

    let output _ _ = failwith "Definition.output not yet implemented"
  end)
end

module What_to_specialise = struct
  type t = {
    (* [definitions] is indexed by (fun_var, group) *)
    definitions : Definition.t list Variable.Pair.Map.t;
    set_of_closures : Flambda.set_of_closures;
    make_direct_call_surrogates_for : Variable.Set.t;
  }

  let create ~set_of_closures =
    { definitions = Variable.Pair.Map.empty;
      set_of_closures;
      make_direct_call_surrogates_for = Variable.Set.empty;
    }

  let new_specialised_arg t ~fun_var ~group ~definition =
    let key = fun_var, group in
    let definitions =
      match Variable.Pair.Map.find key t.definitions with
      | exception Not_found -> []
      | definitions -> definitions
    in
    let definitions =
      Variable.Pair.Map.add (fun_var, group) (definition :: definitions)
        t.definitions
    in
    { t with definitions; }

  let make_direct_call_surrogate_for t ~fun_var =
    match Variable.Map.find fun_var t.set_of_closures.function_decls.funs with
    | exception Not_found ->
      Misc.fatal_errorf "use_direct_call_surrogate_for: %a is not a fun_var \
          from the given set of closures"
        Variable.print fun_var
    | _ ->
      { t with
        make_direct_call_surrogates_for =
          Variable.Set.add fun_var t.make_direct_call_surrogates_for;
      }
end

module W = What_to_specialise

module type S = sig
  val pass_name : string
  val variable_suffix : string

  val what_to_specialise
     : env:Inline_and_simplify_aux.Env.t
    -> set_of_closures:Flambda.set_of_closures
    -> What_to_specialise.t
end

module Processed_what_to_specialise = struct
  type for_one_function = {
    fun_var : Variable.t;
    function_decl : Flambda.function_declaration;
    make_direct_call_surrogates : bool;
    new_definitions_indexed_by_new_inner_vars : Definition.t Variable.Map.t;
    all_new_definitions : Definition.Set.t;
    new_inner_to_new_outer_vars : Variable.t Variable.Map.t;
    total_number_of_args : int;
    existing_specialised_args : Flambda.specialised_to Variable.Map.t;
  }

  type t = {
    variable_suffix : string;
    set_of_closures : Flambda.set_of_closures;
    existing_definitions_via_spec_args_indexed_by_fun_var
      : Definition.Set.t Variable.Map.t;
    (* The following two maps' definitions have already been rewritten
       into their lifted form (i.e. they reference outer rather than inner
       variables). *)
    new_lifted_defns_indexed_by_new_outer_vars : Projection.t Variable.Map.t;
    new_outer_vars_indexed_by_new_lifted_defns : Variable.t Projection.Map.t;
    functions : for_one_function Variable.Map.t;
    make_direct_call_surrogates_for : Variable.Set.t;
  }

  let lift_projection t ~(projection : Projection.t) =
    (* The lifted definition must be in terms of outer variables,
       not inner variables. *)
    let find_outer_var inner_var =
      match Variable.Map.find inner_var t.set_of_closures.specialised_args with
      | (outer_var : Flambda.specialised_to) -> outer_var.var
      | exception Not_found ->
        Misc.fatal_errorf "find_outer_var: expected %a \
            to be in [specialised_args], but it is \
            not.  The projection was: %a.  Set of closures: %a"
          Variable.print inner_var
          Projection.print projection
          Flambda.print_set_of_closures t.set_of_closures
    in
    Projection.map_projecting_from projection ~f:find_outer_var

  let really_add_new_specialised_arg t ~group ~(definition : Definition.t)
        ~(for_one_function : for_one_function) =
    let fun_var = for_one_function.fun_var in
    (* We know here that a new specialised argument must be added.  This
       needs a "new inner var" and a "new outer var".  However if there
       is already a lifted projection being introduced around the set
       of closures (corresponding to another new specialised argument),
       we should re-use its "new outer var" to avoid duplication of
       projection definitions.  Likewise if the definition is just
       [Existing_inner_free_var], in which case we can use the
       corresponding existing outer free variable. *)
    let new_outer_var, t =
      let existing_outer_var =
        match definition with
        | Existing_inner_free_var _ -> None
        | Projection_from_existing_specialised_arg projection ->
          let projection = lift_projection t ~projection in
          match
            Projection.Map.find projection
              t.new_outer_vars_indexed_by_new_lifted_defns
          with
          | new_outer_var -> Some new_outer_var
          | exception Not_found -> None
      in
      match existing_outer_var with
      | Some existing_outer_var -> existing_outer_var, t
      | None ->
        match definition with
        | Existing_inner_free_var existing_inner_var ->
          begin match
            Variable.Map.find existing_inner_var
              t.set_of_closures.free_vars
          with
          | exception Not_found ->
            Misc.fatal_errorf "really_add_new_specialised_arg: \
                Existing_inner_free_var %a is not an inner free variable \
                of %a in %a"
              Variable.print existing_inner_var
              Variable.print fun_var
              Flambda.print_set_of_closures t.set_of_closures
          | existing_outer_var -> existing_outer_var.var, t
          end
        | Projection_from_existing_specialised_arg projection ->
          let new_outer_var =
            Variable.rename group ~append:t.variable_suffix
          in
          let projection = lift_projection t ~projection in
          let new_outer_vars_indexed_by_new_lifted_defns =
            Projection.Map.add
              projection new_outer_var
              t.new_outer_vars_indexed_by_new_lifted_defns
          in
          let new_lifted_defns_indexed_by_new_outer_vars =
            Variable.Map.add
              new_outer_var projection
              t.new_lifted_defns_indexed_by_new_outer_vars
          in
          let t =
            { t with
              new_outer_vars_indexed_by_new_lifted_defns;
              new_lifted_defns_indexed_by_new_outer_vars;
            }
          in
          new_outer_var, t
    in
    let new_inner_var =
      Variable.rename group ~append:t.variable_suffix
    in
    let new_inner_to_new_outer_vars =
      Variable.Map.add new_inner_var new_outer_var
        for_one_function.new_inner_to_new_outer_vars
    in
    let for_one_function : for_one_function =
      { for_one_function with
        new_definitions_indexed_by_new_inner_vars =
          Variable.Map.add new_inner_var definition
            for_one_function.new_definitions_indexed_by_new_inner_vars;
        all_new_definitions =
          Definition.Set.add definition
            for_one_function.all_new_definitions;
        new_inner_to_new_outer_vars;
        total_number_of_args = for_one_function.total_number_of_args + 1;
      }
    in
    { t with
      functions = Variable.Map.add fun_var for_one_function t.functions;
    }

  let new_specialised_arg t ~fun_var ~group ~definition =
    let for_one_function : for_one_function =
      match Variable.Map.find fun_var t.functions with
      | exception Not_found ->
        begin
          match Variable.Map.find fun_var t.set_of_closures.function_decls.funs
        with
        | exception Not_found -> assert false
        | (function_decl : Flambda.function_declaration) ->
          let params = Parameter.Set.vars function_decl.params in
          let existing_specialised_args =
            Variable.Map.filter (fun inner_var _spec_to ->
                Variable.Set.mem inner_var params)
              t.set_of_closures.specialised_args
          in
          let make_direct_call_surrogates =
            Variable.Set.mem fun_var t.make_direct_call_surrogates_for
          in
          { fun_var;
            function_decl;
            make_direct_call_surrogates;
            new_definitions_indexed_by_new_inner_vars = Variable.Map.empty;
            all_new_definitions = Definition.Set.empty;
            new_inner_to_new_outer_vars = Variable.Map.empty;
            (* The "+ 1" is just in case there is a closure environment
               parameter added later. *)
            total_number_of_args = List.length function_decl.params + 1;
            existing_specialised_args;
          }
        end
      | for_one_function -> for_one_function
    in
    (* Determine whether there already exists an existing specialised argument
       that is known to be equal to the one proposed to this function.  If so,
       use that instead.  (Note that we also desire to dedup against any
       new specialised arguments added to the current function; but that
       happens automatically since [Extract_projections] returns a set.) *)
    let exists_already =
      match
        Variable.Map.find fun_var
          t.existing_definitions_via_spec_args_indexed_by_fun_var
      with
      | exception Not_found -> false
      | definitions -> Definition.Set.mem definition definitions
    in
    if exists_already then t
    else really_add_new_specialised_arg t ~group ~definition ~for_one_function

  let create ~env ~(what_to_specialise : W.t) ~variable_suffix =
    let existing_definitions_via_spec_args_indexed_by_fun_var =
      Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
          if function_decl.stub then
            Definition.Set.empty
          else
            let params = Parameter.Set.vars function_decl.params in
            Variable.Map.fold (fun inner_var
                      (spec_to : Flambda.specialised_to) definitions ->
                if not (Variable.Set.mem inner_var params) then
                  definitions
                else
                  let definition : Definition.t =
                    match spec_to.projection with
                    | None -> Existing_inner_free_var inner_var
                    | Some projection ->
                      Projection_from_existing_specialised_arg projection
                  in
                  Definition.Set.add definition definitions)
              what_to_specialise.set_of_closures.specialised_args
              Definition.Set.empty)
          what_to_specialise.set_of_closures.function_decls.funs
    in
    let t : t =
      { variable_suffix;
        set_of_closures = what_to_specialise.set_of_closures;
        existing_definitions_via_spec_args_indexed_by_fun_var;
        new_lifted_defns_indexed_by_new_outer_vars = Variable.Map.empty;
        new_outer_vars_indexed_by_new_lifted_defns = Projection.Map.empty;
        functions = Variable.Map.empty;
        make_direct_call_surrogates_for =
          what_to_specialise.make_direct_call_surrogates_for;
      }
    in
    (* It is important to limit the number of arguments added: if arguments
       end up being passed on the stack, tail call optimization will be
       disabled (see asmcomp/selectgen.ml).
       For each group of new specialised args provided by [T], either all or
       none of them will be added.  (This is to avoid the situation where we
       add extra arguments but yet fail to eliminate an original one by
       stopping part-way through the specialised args addition.) *)
    let by_group =
      Variable.Pair.Map.fold (fun (fun_var, group) definitions by_group ->
          let fun_vars_and_definitions =
            match Variable.Map.find group by_group with
            | exception Not_found -> []
            | fun_vars_and_definitions -> fun_vars_and_definitions
          in
          Variable.Map.add group
            ((fun_var, definitions)::fun_vars_and_definitions)
            by_group)
        what_to_specialise.definitions
        Variable.Map.empty
    in
    let module Backend = (val (E.backend env) : Backend_intf.S) in
    Variable.Map.fold (fun group fun_vars_and_definitions t ->
        let original_t = t in
        let t =
          (* Try adding all specialised args in the current group. *)
          List.fold_left (fun t (fun_var, definitions) ->
              List.fold_left (fun t definition ->
                  new_specialised_arg t ~fun_var ~group ~definition)
                t
                definitions)
            t
            fun_vars_and_definitions
        in
        let some_function_has_too_many_args =
          Variable.Map.exists (fun _ (for_one_function : for_one_function) ->
              for_one_function.total_number_of_args
                > Backend.max_sensible_number_of_arguments)
            t.functions
        in
        if some_function_has_too_many_args then
          original_t  (* drop this group *)
        else
          t)
      by_group
      t
end

module P = Processed_what_to_specialise

let check_invariants ~pass_name ~(set_of_closures : Flambda.set_of_closures)
      ~original_set_of_closures =
  if !Clflags.flambda_invariant_checks then begin
    Variable.Map.iter (fun fun_var
              (function_decl : Flambda.function_declaration) ->
        let params = Parameter.Set.vars function_decl.params in
        Variable.Map.iter (fun inner_var
                    (outer_var : Flambda.specialised_to) ->
              if Variable.Set.mem inner_var params then begin
                assert (not (Variable.Set.mem outer_var.var
                  function_decl.free_variables));
                match outer_var.projection with
                | None -> ()
                | Some projection ->
                  let from = Projection.projecting_from projection in
                  if not (Variable.Set.mem from params) then begin
                    Misc.fatal_errorf "Augment_specialised_args (%s): \
                        specialised argument (%a -> %a) references a \
                        projection variable that is not a specialised \
                        argument of the function %a. @ The set of closures \
                        before the transformation was:@  %a. @ The set of \
                        closures after the transformation was:@ %a."
                      pass_name
                      Variable.print inner_var
                      Flambda.print_specialised_to outer_var
                      Variable.print fun_var
                      Flambda.print_set_of_closures original_set_of_closures
                      Flambda.print_set_of_closures set_of_closures
                  end
              end)
          set_of_closures.specialised_args)
      set_of_closures.function_decls.funs
  end

module Make (T : S) = struct
  let () = Pass_wrapper.register ~pass_name:T.pass_name

  let rename_function_and_parameters ~fun_var
        ~(function_decl : Flambda.function_declaration) =
    let new_fun_var = Variable.rename fun_var ~append:T.variable_suffix in
    let params_renaming_list =
      List.map (fun param ->
          let new_param = Parameter.rename param ~append:T.variable_suffix in
          param, new_param)
        function_decl.params
    in
    let renamed_params = List.map snd params_renaming_list in
    let params_renaming =
      Variable.Map.of_list
        (List.map (fun (param, new_param) ->
             Parameter.var param, Parameter.var new_param)
           params_renaming_list)
    in
    new_fun_var, params_renaming, renamed_params

  let create_wrapper ~(for_one_function : P.for_one_function) ~benefit =
    let fun_var = for_one_function.fun_var in
    let function_decl = for_one_function.function_decl in
    (* To avoid increasing the free variables of the wrapper, for
       general cleanliness, we restate the definitions of the
       newly-specialised arguments in the wrapper itself in terms of the
       original specialised arguments.  The variables bound to these
       definitions are called the "specialised args bound in the wrapper".
       Note that the domain of [params_renaming] is a (non-strict) superset
       of the "inner vars" of the original specialised args. *)
    let params = Parameter.Set.vars function_decl.params in
    let new_fun_var, params_renaming, wrapper_params =
      rename_function_and_parameters ~fun_var ~function_decl
    in
    let find_wrapper_param param =
      assert (Variable.Set.mem param params);
      match Variable.Map.find param params_renaming with
      | wrapper_param -> wrapper_param
      | exception Not_found ->
        Misc.fatal_errorf "find_wrapper_param: expected %a \
            to be in [params_renaming], but it is not."
          Variable.print param
    in
    let new_inner_vars_to_spec_args_bound_in_the_wrapper_renaming =
      Variable.Map.mapi (fun new_inner_var _ ->
          Variable.rename new_inner_var)
        for_one_function.new_definitions_indexed_by_new_inner_vars
    in
    let spec_args_bound_in_the_wrapper =
      (* N.B.: in the order matching the new specialised argument parameters
         to the main function. *)
      Variable.Map.data
        new_inner_vars_to_spec_args_bound_in_the_wrapper_renaming
    in
    (* New definitions that project from existing specialised args need
       to be rewritten to use the corresponding specialised args of
       the wrapper.  Definitions that are just equality to existing
       inner free variables do not need to be changed.  Once this has
       been done the wrapper body can be constructed.
       We also need to rewrite definitions for any existing specialised
       args; these now have corresponding wrapper parameters that must
       also be specialised. *)
    let wrapper_body, benefit =
      let apply : Flambda.expr =
        Apply {
          func = new_fun_var;
          args =
            (Parameter.List.vars wrapper_params) @
            spec_args_bound_in_the_wrapper;
          kind = Direct (Closure_id.wrap new_fun_var);
          dbg = Debuginfo.none;
          inline = Default_inline;
          specialise = Default_specialise;
        }
      in
      Variable.Map.fold (fun new_inner_var definition (wrapper_body, benefit) ->
          let definition : Definition.t =
            match (definition : Definition.t) with
            | Existing_inner_free_var _ -> definition
            | Projection_from_existing_specialised_arg projection ->
              Projection_from_existing_specialised_arg
                (Projection.map_projecting_from projection
                  ~f:find_wrapper_param)
          in
          let benefit =
            match (definition : Definition.t) with
            | Existing_inner_free_var _ -> benefit
            | Projection_from_existing_specialised_arg projection ->
              B.add_projection projection benefit
          in
          match
            Variable.Map.find new_inner_var
              new_inner_vars_to_spec_args_bound_in_the_wrapper_renaming
          with
          | exception Not_found -> assert false
          | new_inner_var_of_wrapper ->
            let named : Flambda.named =
              match definition with
              | Existing_inner_free_var existing_inner_var ->
                Expr (Var existing_inner_var)
              | Projection_from_existing_specialised_arg projection ->
                Flambda_utils.projection_to_named projection
            in
            let wrapper_body =
              Flambda.create_let new_inner_var_of_wrapper named wrapper_body
            in
            (wrapper_body, benefit))
      for_one_function.new_definitions_indexed_by_new_inner_vars
      (apply, benefit)
    in
    let rewritten_existing_specialised_args =
      Variable.Map.fold (fun inner_var (spec_to : Flambda.specialised_to)
                result ->
          let inner_var = find_wrapper_param inner_var in
          let projection =
            match spec_to.projection with
            | None -> None
            | Some projection ->
              Some (Projection.map_projecting_from projection
                ~f:find_wrapper_param)
          in
          let spec_to : Flambda.specialised_to =
            { var = spec_to.var;
              projection;
            }
          in
          Variable.Map.add inner_var spec_to result)
        for_one_function.existing_specialised_args
        Variable.Map.empty
    in
    let new_function_decl =
      Flambda.create_function_declaration
        ~params:wrapper_params
        ~body:wrapper_body
        ~stub:true
        ~dbg:Debuginfo.none
        ~inline:Default_inline
        ~specialise:Default_specialise
        ~is_a_functor:false
    in
    new_fun_var, new_function_decl, rewritten_existing_specialised_args,
      benefit

  let rewrite_function_decl (t : P.t) ~env ~duplicate_function
      ~(for_one_function : P.for_one_function) ~benefit =
    let set_of_closures = t.set_of_closures in
    let fun_var = for_one_function.fun_var in
    let function_decl = for_one_function.function_decl in
    let num_definitions =
      Variable.Map.cardinal for_one_function.
        new_definitions_indexed_by_new_inner_vars
    in
    if function_decl.stub
      || num_definitions < 1
      || Variable.Map.mem fun_var set_of_closures.direct_call_surrogates
    then
      None
    else
      let new_fun_var, wrapper, rewritten_existing_specialised_args, benefit =
        create_wrapper ~for_one_function ~benefit
      in
      let new_specialised_args =
        Variable.Map.mapi (fun new_inner_var (definition : Definition.t)
                : Flambda.specialised_to ->
            assert (not (Variable.Map.mem new_inner_var
              set_of_closures.specialised_args));
            match
              Variable.Map.find new_inner_var
                for_one_function.new_inner_to_new_outer_vars
            with
            | exception Not_found -> assert false
            | new_outer_var ->
              match definition with
              | Existing_inner_free_var _ ->
                { var = new_outer_var;
                  projection = None;
                }
              | Projection_from_existing_specialised_arg projection ->
                let projecting_from = Projection.projecting_from projection in
                assert (Variable.Map.mem projecting_from
                  set_of_closures.specialised_args);
                assert (Variable.Set.mem projecting_from
                  (Parameter.Set.vars function_decl.params));
                { var = new_outer_var;
                  projection = Some projection;
                })
          for_one_function.new_definitions_indexed_by_new_inner_vars
      in
      let specialised_args =
        Variable.Map.disjoint_union rewritten_existing_specialised_args
          new_specialised_args
      in
      let specialised_args, existing_function_decl =
        if not for_one_function.make_direct_call_surrogates then
          specialised_args, None
        else
          let function_decl, new_specialised_args =
            duplicate_function ~env ~set_of_closures ~fun_var
          in
          let specialised_args =
            Variable.Map.disjoint_union specialised_args new_specialised_args
          in
          specialised_args, Some function_decl
      in
      let all_params =
        let new_params =
          Variable.Set.elements (Variable.Map.keys
            for_one_function.new_inner_to_new_outer_vars)
        in
        let new_params =
          List.map Parameter.wrap new_params
        in
        function_decl.params @ new_params
      in
      let rewritten_function_decl =
        Flambda.create_function_declaration
          ~params:all_params
          ~body:function_decl.body
          ~stub:function_decl.stub
          ~dbg:function_decl.dbg
          ~inline:function_decl.inline
          ~specialise:function_decl.specialise
          ~is_a_functor:function_decl.is_a_functor
      in
      let funs, direct_call_surrogates =
        if for_one_function.make_direct_call_surrogates then
          let surrogate = Variable.rename fun_var ~append:"_surrogate" in
          let funs =
            (* In this case, the original function declaration remains
               untouched up to alpha-equivalence.  Direct calls to it
               (including inside the rewritten original function) will be
               replaced by calls to the surrogate (i.e. the wrapper) which
               will then be inlined. *)
            let existing_function_decl =
              match existing_function_decl with
              | Some decl -> decl
              | None -> assert false
            in
            Variable.Map.add new_fun_var rewritten_function_decl
              (Variable.Map.add surrogate wrapper
                (Variable.Map.add fun_var existing_function_decl
                  Variable.Map.empty))
          in
          let direct_call_surrogates =
            Variable.Map.add fun_var surrogate Variable.Map.empty
          in
          funs, direct_call_surrogates
        else
          let funs =
            Variable.Map.add new_fun_var rewritten_function_decl
              (Variable.Map.add fun_var wrapper Variable.Map.empty)
          in
          funs, Variable.Map.empty
      in
      let free_vars = Variable.Map.empty in
      Some (funs, free_vars, specialised_args, direct_call_surrogates, benefit)

  let add_lifted_projections_around_set_of_closures
        ~(set_of_closures : Flambda.set_of_closures) ~benefit
        ~new_lifted_defns_indexed_by_new_outer_vars =
    let body =
      Flambda_utils.name_expr (Set_of_closures set_of_closures)
        ~name:("set_of_closures" ^ T.variable_suffix)
    in
    Variable.Map.fold (fun new_outer_var (projection : Projection.t)
          (expr, benefit) ->
        let named = Flambda_utils.projection_to_named projection in
        let benefit = B.add_projection projection benefit in
        let expr = Flambda.create_let new_outer_var named expr in
        expr, benefit)
      new_lifted_defns_indexed_by_new_outer_vars
      (body, benefit)

  let rewrite_set_of_closures_core ~env ~duplicate_function ~benefit
        ~(set_of_closures : Flambda.set_of_closures) =
    let what_to_specialise =
      P.create ~env ~variable_suffix:T.variable_suffix
        ~what_to_specialise:(T.what_to_specialise ~env ~set_of_closures)
    in
    let original_set_of_closures = set_of_closures in
    let funs, free_vars, specialised_args, direct_call_surrogates,
        done_something, benefit =
      Variable.Map.fold (fun fun_var function_decl
                (funs, free_vars, specialised_args, direct_call_surrogates,
                  done_something, benefit) ->
          match Variable.Map.find fun_var what_to_specialise.functions with
          | exception Not_found ->
            let funs = Variable.Map.add fun_var function_decl funs in
            funs, free_vars, specialised_args, direct_call_surrogates,
              done_something, benefit
          | (for_one_function : P.for_one_function) ->
            assert (Variable.equal fun_var for_one_function.fun_var);
            match
              rewrite_function_decl what_to_specialise ~env
                ~duplicate_function ~for_one_function ~benefit
            with
            | None ->
              let function_decl = for_one_function.function_decl in
              let funs = Variable.Map.add fun_var function_decl funs in
              funs, free_vars, specialised_args, direct_call_surrogates,
                done_something, benefit
            | Some (funs', free_vars', specialised_args',
                direct_call_surrogates', benefit) ->
              let funs = Variable.Map.disjoint_union funs funs' in
              let direct_call_surrogates =
                Variable.Map.disjoint_union direct_call_surrogates
                  direct_call_surrogates'
              in
              let free_vars =
                Variable.Map.disjoint_union free_vars free_vars'
              in
              let specialised_args =
                Variable.Map.disjoint_union specialised_args specialised_args'
              in
              funs, free_vars, specialised_args, direct_call_surrogates, true,
                benefit)
        set_of_closures.function_decls.funs
        (Variable.Map.empty, set_of_closures.free_vars,
          set_of_closures.specialised_args,
          set_of_closures.direct_call_surrogates, false, benefit)
    in
    if not done_something then
      None
    else
      let function_decls =
        Flambda.update_function_declarations set_of_closures.function_decls
          ~funs
      in
      assert (Variable.Map.cardinal specialised_args
        >= Variable.Map.cardinal original_set_of_closures.specialised_args);
      let set_of_closures =
        Flambda.create_set_of_closures
          ~function_decls
          ~free_vars
          ~specialised_args
          ~direct_call_surrogates
      in
      if !Clflags.flambda_invariant_checks then begin
        check_invariants ~set_of_closures ~original_set_of_closures
          ~pass_name:T.pass_name
      end;
      let expr, benefit =
        add_lifted_projections_around_set_of_closures ~set_of_closures ~benefit
          ~new_lifted_defns_indexed_by_new_outer_vars:
            what_to_specialise.new_lifted_defns_indexed_by_new_outer_vars
      in
      Some (expr, benefit)

  let rewrite_set_of_closures ~env ~duplicate_function ~set_of_closures =
    Pass_wrapper.with_dump ~pass_name:T.pass_name ~input:set_of_closures
      ~print_input:Flambda.print_set_of_closures
      ~print_output:(fun ppf (expr, _) -> Flambda.print ppf expr)
      ~f:(fun () ->
        rewrite_set_of_closures_core ~env ~duplicate_function
          ~benefit:B.zero ~set_of_closures)
end
