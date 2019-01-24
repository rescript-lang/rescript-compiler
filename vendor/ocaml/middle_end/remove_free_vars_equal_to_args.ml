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

let pass_name = "remove-free-vars-equal-to-args"
let () = Pass_wrapper.register ~pass_name

let rewrite_one_function_decl ~(function_decl : Flambda.function_declaration)
      ~back_free_vars ~specialised_args =
  let params_for_equal_free_vars =
    List.fold_left (fun subst param ->
        match Variable.Map.find param specialised_args with
        | exception Not_found ->
          (* param is not specialised *)
          subst
        | (spec_to : Flambda.specialised_to) ->
          let outside_var = spec_to.var in
          match Variable.Map.find outside_var back_free_vars with
          | exception Not_found ->
            (* No free variables equal to the param *)
            subst
          | set ->
            (* Replace the free variables equal to a parameter *)
            Variable.Set.fold (fun free_var subst ->
                Variable.Map.add free_var param subst)
              set subst)
      Variable.Map.empty (Parameter.List.vars function_decl.params)
  in
  if Variable.Map.is_empty params_for_equal_free_vars then
    function_decl
  else
    let body =
      Flambda_utils.toplevel_substitution
        params_for_equal_free_vars
        function_decl.body
    in
    Flambda.create_function_declaration
      ~params:function_decl.params
      ~body:body
      ~stub:function_decl.stub
      ~dbg:function_decl.dbg
      ~inline:function_decl.inline
      ~specialise:function_decl.specialise
      ~is_a_functor:function_decl.is_a_functor

let rewrite_one_set_of_closures (set_of_closures : Flambda.set_of_closures) =
  let back_free_vars =
    Variable.Map.fold (fun var (outside_var : Flambda.specialised_to) map ->
        let set =
          match Variable.Map.find outside_var.var map with
          | exception Not_found -> Variable.Set.singleton var
          | set -> Variable.Set.add var set
        in
        Variable.Map.add outside_var.var set map)
      set_of_closures.free_vars Variable.Map.empty
  in
  let done_something = ref false in
  let funs =
    Variable.Map.map (fun function_decl ->
        let new_function_decl =
          rewrite_one_function_decl ~function_decl ~back_free_vars
            ~specialised_args:set_of_closures.specialised_args
        in
        if not (new_function_decl == function_decl) then begin
          done_something := true
        end;
        new_function_decl)
      set_of_closures.function_decls.funs
  in
  if not !done_something then
    None
  else
    let function_decls =
      Flambda.update_function_declarations
        set_of_closures.function_decls ~funs
    in
    let set_of_closures =
      Flambda.create_set_of_closures
        ~function_decls
        ~free_vars:set_of_closures.free_vars
        ~specialised_args:set_of_closures.specialised_args
        ~direct_call_surrogates:set_of_closures.direct_call_surrogates
    in
    Some set_of_closures

let run set_of_closures =
  Pass_wrapper.with_dump ~pass_name ~input:set_of_closures
    ~print_input:Flambda.print_set_of_closures
    ~print_output:Flambda.print_set_of_closures
    ~f:(fun () -> rewrite_one_set_of_closures set_of_closures)
