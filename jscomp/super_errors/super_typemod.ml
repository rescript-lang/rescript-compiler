open Printtyp

let fprintf = Format.fprintf


let pp_component_type_not_generalizable_pre ppf =
  fprintf ppf "@[<v>\
    @[@{<info>Is this a ReasonReact reducerComponent or component with retained props?@}@ \
    If so, you can fix it by doing one of these:@]@,\
    @[- Defining the component's `make` function@]@,\
    @[- Annotating the state once with a type, wherever it's used (e.g. render)@]@,\
    @[- Annotating actions (in e.g. reducer)@]@,\
    @[- Annotating retained props, if any@]@,@,\
    @[@{<info>Here's the original error message@}@]\
  @]@,"

let pp_component_type_not_generalizable_post ppf () =
  fprintf ppf
  "@[This happens when the type system senses there's a mutation/side-effect,@ in combination with a polymorphic value.@,\
  Using or annotating that value usually solves it.@ \
  More info:@ https://realworldocaml.org/v1/en/html/imperative-programming-1.html#side-effects-and-weak-polymorphism@]"

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typemod.ml#L1754 *)
(* modified branches are commented *)
let report_error ppf = Typemod.(function
    Cannot_apply mty ->
      fprintf ppf
        "@[This module is not a functor; it has type@ %a@]" modtype mty
  | Not_included errs ->
      fprintf ppf
        "@[<v>Signature mismatch:@ %a@]" Includemod.report_error errs
  | Cannot_eliminate_dependency mty ->
      fprintf ppf
        "@[This functor has type@ %a@ \
           The parameter cannot be eliminated in the result type.@  \
           Please bind the argument to a module identifier.@]" modtype mty
  | Signature_expected -> fprintf ppf "This module type is not a signature"
  | Structure_expected mty ->
      fprintf ppf
        "@[This module is not a structure; it has type@ %a" modtype mty
  | With_no_component lid ->
      fprintf ppf
        "@[The signature constrained by `with' has no component named %a@]"
        longident lid
  | With_mismatch(lid, explanation) ->
      fprintf ppf
        "@[<v>\
           @[In this `with' constraint, the new definition of %a@ \
             does not match its original definition@ \
             in the constrained signature:@]@ \
           %a@]"
        longident lid Includemod.report_error explanation
  | Repeated_name(kind, name) ->
      fprintf ppf
        "@[Multiple definition of the %s name %s.@ \
           Names must be unique in a given structure or signature.@]" kind name
  | Non_generalizable typ ->
      (* modified *)
      fprintf ppf "@[<v>";
      if Super_reason_react.type_is_component_spec typ then begin
        pp_component_type_not_generalizable_pre ppf
      end;
      fprintf ppf
        "@[This expression's type contains type variables that can't be generalized:@,@{<error>%a@}@]@,@,\
         %a"
        type_scheme typ
        pp_component_type_not_generalizable_post ();
      fprintf ppf "@]"
  | Non_generalizable_class (id, desc) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains type variables that cannot be generalized@]"
        (class_declaration id) desc
  | Non_generalizable_module mty ->
      (* modified *)
      fprintf ppf "@[<v>";
      if Super_reason_react.module_type_is_component_spec mty then begin
        pp_component_type_not_generalizable_pre ppf
      end;
      fprintf ppf
        "@[The type of this module contains type variables that cannot be generalized:@,@{<error>%a@}@]@,@,\
         %a"
        modtype mty
        pp_component_type_not_generalizable_post ();
      fprintf ppf "@]"
  | Implementation_is_required intf_name ->
      fprintf ppf
        "@[The interface %a@ declares values, not just types.@ \
           An implementation must be provided.@]"
        Location.print_filename intf_name
  | Interface_not_compiled intf_name ->
      fprintf ppf
        "@[Could not find the .cmi file for interface@ %a.@]"
        Location.print_filename intf_name
  | Not_allowed_in_functor_body ->
      fprintf ppf
        "@[This expression creates fresh types.@ %s@]"
        "It is not allowed inside applicative functors."
  | With_need_typeconstr ->
      fprintf ppf
        "Only type constructors with identical parameters can be substituted."
  | Not_a_packed_module ty ->
      fprintf ppf
        "This expression is not a packed module. It has type@ %a"
        type_expr ty
  | Incomplete_packed_module ty ->
      fprintf ppf
        "The type of this packed module contains variables:@ %a"
        type_expr ty
  | Scoping_pack (lid, ty) ->
      fprintf ppf
        "The type %a in this module cannot be exported.@ " longident lid;
      fprintf ppf
        "Its type contains local dependencies:@ %a" type_expr ty
  | Recursive_module_require_explicit_type ->
      fprintf ppf "Recursive modules require an explicit module type."
  | Apply_generative ->
      fprintf ppf "This is a generative functor. It can only be applied to ()"
)

let report_error env ppf err =
  Printtyp.wrap_printing_env env (fun () -> report_error ppf err)

(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Typemod.Error (loc, env, err) ->
        Some (Super_location.error_of_printer loc (report_error env) err)
      | Typemod.Error_forward err ->
        Some err
      | _ ->
        None
    )
