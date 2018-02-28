open Misc
open Asttypes
open Parsetree
open Types
open Typedtree
open Btype
open Ctype

open Format
open Printtyp

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typetexp.ml#L869 *)
let spellcheck ppf fold env lid =
  let cutoff =
    match String.length (Longident.last lid) with
      | 1 | 2 -> 0
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | _ -> 3
  in
  let compare target head acc =
    let (best_choice, best_dist) = acc in
    match Misc.edit_distance target head cutoff with
      | None -> (best_choice, best_dist)
      | Some dist ->
        let choice =
          if dist < best_dist then [head]
          else if dist = best_dist then head :: best_choice
          else best_choice in
        (choice, min dist best_dist)
  in
  let init = ([], max_int) in
  let handle (choice, _dist) =
    match List.rev choice with
      | [] -> ()
      | last :: rev_rest ->
        (* the modified part *)
        fprintf ppf "@[<v 2>@,@,@{<info>Hint: Did you mean %s%s%s?@}@]"
          (String.concat ", " (List.rev rev_rest))
          (if rev_rest = [] then "" else " or ")
          last
  in
  (* flush now to get the error report early, in the (unheard of) case
     where the linear search would take a bit of time; in the worst
     case, the user has seen the error, she can interrupt the process
     before the spell-checking terminates. *)
  fprintf ppf "@?";
  match lid with
    | Longident.Lapply _ -> ()
    | Longident.Lident s ->
      handle (fold (compare s) None env init)
    | Longident.Ldot (r, s) ->
      handle (fold (compare s) (Some r) env init)

let spellcheck ppf fold =
  spellcheck ppf (fun f -> fold (fun s _ _ x -> f s x))

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typetexp.ml#L918 *)
(* modified branches are commented *)
let report_error env ppf = function
  | Typetexp.Unbound_type_variable name ->
    fprintf ppf "Unbound type parameter %s@." name
  | Unbound_type_constructor lid ->
    (* modified *)
    fprintf ppf "This type constructor's parameter, `%a`, can't be found. Is it a typo?" longident lid;
    spellcheck ppf Env.fold_types env lid;
  | Unbound_type_constructor_2 p ->
    fprintf ppf "The type constructor@ %a@ is not yet completely defined"
      path p
  | Type_arity_mismatch(lid, expected, provided) ->
    fprintf ppf
      "@[The type constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
      longident lid expected provided
  | Bound_type_variable name ->
    fprintf ppf "Already bound type parameter '%s" name
  | Recursive_type ->
    fprintf ppf "This type is recursive"
  | Unbound_row_variable lid ->
      (* we don't use "spellcheck" here: this error is not raised
         anywhere so it's unclear how it should be handled *)
      fprintf ppf "Unbound row variable in #%a" longident lid
  | Type_mismatch trace ->
      Printtyp.super_report_unification_error ppf Env.empty trace
        (function ppf ->
           fprintf ppf "This type")
        (function ppf ->
           fprintf ppf "should be an instance of type")
  | Alias_type_mismatch trace ->
      Printtyp.super_report_unification_error ppf Env.empty trace
        (function ppf ->
           fprintf ppf "This alias is bound to type")
        (function ppf ->
           fprintf ppf "but is used as an instance of type")
  | Present_has_conjunction l ->
      fprintf ppf "The present constructor %s has a conjunctive type" l
  | Present_has_no_type l ->
      fprintf ppf "The present constructor %s has no type" l
  | Constructor_mismatch (ty, ty') ->
      wrap_printing_env env (fun ()  ->
        Printtyp.reset_and_mark_loops_list [ty; ty'];
        fprintf ppf "@[<hov>%s %a@ %s@ %a@]"
          "This variant type contains a constructor"
          Printtyp.type_expr ty
          "which should be"
          Printtyp.type_expr ty')
  | Not_a_variant ty ->
      Printtyp.reset_and_mark_loops ty;
      fprintf ppf "@[The type %a@ is not a polymorphic variant type@]"
        Printtyp.type_expr ty
  | Variant_tags (lab1, lab2) ->
      fprintf ppf
        "@[Variant tags `%s@ and `%s have the same hash value.@ %s@]"
        lab1 lab2 "Change one of them."
  | Invalid_variable_name name ->
      fprintf ppf "The type variable name %s is not allowed in programs" name
  | Cannot_quantify (name, v) ->
      fprintf ppf
        "@[<hov>The universal type variable '%s cannot be generalized:@ %s.@]"
        name
        (if Btype.is_Tvar v then "it escapes its scope" else
         if Btype.is_Tunivar v then "it is already bound to another variable"
         else "it is not a variable")
  | Multiple_constraints_on_type s ->
      fprintf ppf "Multiple constraints for type %a" longident s
  | Repeated_method_label s ->
      fprintf ppf "@[This is the second method `%s' of this object type.@ %s@]"
        s "Multiple occurences are not allowed."
  | Unbound_value lid ->
      (* modified *)
      begin
        match lid with
        | Ldot (outer, inner) ->
          fprintf ppf "The value %s can't be found in %a"
            inner
            Printtyp.longident outer;
        | other_ident -> fprintf ppf "The value %a can't be found" Printtyp.longident other_ident
      end;
      spellcheck ppf Env.fold_values env lid;
  | Unbound_module lid ->
      (* modified *)
      begin match lid with
      | Lident "Str" -> 
        begin
          fprintf ppf "@[\
              @{<info>The module or file %a can't be found.@}@,@,\
              Are you trying to use the standard library's Str?@ \
              If you're compiling to JavaScript,@ use @{<info>Js.Re@} instead.@ \
              Otherwise, add str.cma to your ocamlc/ocamlopt command.\
            @]"
            longident lid 
        end
      | lid -> 
        begin
          fprintf ppf "@[<v>\
              @{<info>The module or file %a can't be found.@}@,\
              @[<v 2>- If it's a third-party dependency:@,\
                - Did you list it in bsconfig.json?@,\
                - @[Did you run `bsb` instead of `bsb -make-world`@ (latter builds third-parties)@]?\
              @]@,\
              - Did you include the file's directory in bsconfig.json?@]\
            @]"
            longident lid
        end
      end;
      spellcheck ppf Env.fold_modules env lid
  | Unbound_constructor lid ->
      (* modified *)
      fprintf ppf "@[<v>\
      @{<info>The variant constructor %a can't be found.@}@,@,\
      @[<v 2>- If it's defined in another module or file, bring it into scope by:@,\
        @[- Annotating it with said module name:@ @{<info>let food = MyModule.Apple@}@]@,\
        @[- Or specifying its type:@ @{<info>let food: MyModule.fruit = Apple@}@]\
      @]@,\
      - @[Constructors and modules are both capitalized.@ Did you want the latter?@ Then instead of @{<dim>let foo = Bar@}, try @{<info>module Foo = Bar@}.@]\
      @]"
      longident lid;
      Typetexp.spellcheck_simple ppf Env.fold_constructors (fun d -> d.cstr_name)
        env lid;
  | Unbound_label lid ->
      (* modified *)
      fprintf ppf "@[<v>\
      @{<info>The record field %a can't be found.@}@,@,\
      If it's defined in another module or file, bring it into scope by:@,\
      @[- Annotating it with said module name:@ @{<info>let baby = {MyModule.age: 3}@}@]@,\
      @[- Or specifying its type:@ @{<info>let baby: MyModule.person = {age: 3}@}@]\
      @]"
      longident lid;
      Typetexp.spellcheck_simple ppf Env.fold_labels (fun d -> d.lbl_name) env lid;
  | Unbound_class lid ->
      fprintf ppf "Unbound class %a" longident lid;
      spellcheck ppf Env.fold_classs env lid;
  | Unbound_modtype lid ->
      fprintf ppf "Unbound module type %a" longident lid;
      spellcheck ppf Env.fold_modtypes env lid;
  | Unbound_cltype lid ->
      fprintf ppf "Unbound class type %a" longident lid;
      spellcheck ppf Env.fold_cltypes env lid;
  | Ill_typed_functor_application lid ->
      fprintf ppf "Ill-typed functor application %a" longident lid
  | Illegal_reference_to_recursive_module ->
      fprintf ppf "Illegal recursive module reference"
  | Access_functor_as_structure lid ->
      fprintf ppf "The module %a is a functor, not a structure" longident lid

(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Typetexp.Error (loc, env, err) ->
        Some (Super_location.error_of_printer loc (report_error env) err)
      (* typetexp doesn't expose Error_forward  *)
      (* | Error_forward err ->
        Some err *)
      | _ ->
        None
    )
