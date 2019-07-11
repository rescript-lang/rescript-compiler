open Printtyp

let fprintf = Format.fprintf

let non_generalizable_msg ppf ~result ~is_module print_fallback_msg =
  let contain_vs_be =
    if is_module then "This module seems to contain" else "This seems to be"
  in
  match result with
  | (_, true, _) :: _ ->
      fprintf ppf
        "@[<v>@{<info>%s a ReasonReact reducerComponentWithRetainedProps?@}@ \
         The retained props feature is deprecated.@ Please use a regular \
         @{<info>reducerComponent@} and keep the props you want to retain in \
         state.@]"
        contain_vs_be
  | (true, _, _) :: _ ->
      fprintf ppf
        "@[<v>@[@{<info>%s a ReasonReact reducerComponent?@}@ We don't have@ \
         all@ the@ type@ info@ for@ its@ @{<info>state@}.@ Make sure you've \
         done the following: @]@,@,@[- Define the component `make` \
         function@]@,@[- Define `reducer` in that `make` body@]@,@[- Annotate \
         reducer's second parameter (state) with the desired state type@]@]"
        contain_vs_be
  | (_, _, true) :: _ ->
      fprintf ppf
        "@[<v>@[@{<info>%s a ReasonReact reducerComponent?@}@ We don't have@ \
         all@ the@ type@ info@ for@ its@ @{<info>action@}.@ Make sure you've \
         done the following: @]@,@,@[- Define the component `make` \
         function@]@,@[- Define `reducer` in that `make` body@]@,@[- Annotate \
         reducer's first parameter (action) with the desired action type@]@]"
        contain_vs_be
  | _ ->
      fprintf ppf
        "%a@,@,@[This happens when the type system senses there's a \
         mutation/side-effect,@ in combination with a polymorphic \
         value.@,@{<info>Using or annotating that value usually solves it.@}@ \
         More info:@ \
         https://realworldocaml.org/v1/en/html/imperative-programming-1.html#side-effects-and-weak-polymorphism@]"
        print_fallback_msg ()

(* taken from
   https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typemod.ml#L1754 *)
(* modified branches are commented *)
let report_error ppf = function
  | Typemod.Non_generalizable typ ->
      (* modified *)
      fprintf ppf "@[<v>" ;
      non_generalizable_msg ppf
        ~result:[Super_reason_react.component_spec_weak_type_variables typ]
        ~is_module:false (fun ppf () ->
          fprintf ppf
            "@[This expression's type contains type variables that can't be \
             generalized:@,@{<error>%a@}@]"
            type_scheme typ) ;
      fprintf ppf "@]"
  | Non_generalizable_module mty ->
      (* modified *)
      fprintf ppf "@[<v>" ;
      non_generalizable_msg ppf
        ~result:
          (Super_reason_react.component_spec_weak_type_variables_in_module_type
             mty) ~is_module:true (fun ppf () ->
          fprintf ppf
            "@[The type of this module contains type variables that cannot be \
             generalized:@,@{<error>%a@}@]"
            modtype mty) ;
      fprintf ppf "@]"
  | anythingElse ->
      Typemod.super_report_error_no_wrap_printing_env ppf anythingElse

let report_error env ppf err =
  Printtyp.wrap_printing_env env (fun () -> report_error ppf err)

(* This will be called in super_main. This is how you'd override the default
   error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn (function
    | Typemod.Error (loc, env, err) ->
        Some (Super_location.error_of_printer loc (report_error env) err)
    | Typemod.Error_forward err -> Some err
    | _ -> None)
