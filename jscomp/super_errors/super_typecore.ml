open Misc
open Asttypes
open Parsetree
open Types
open Typedtree
open Btype
open Ctype

let fprintf = Format.fprintf
let sprintf = Format.sprintf
let longident = Printtyp.longident
let super_report_unification_error = Printtyp.super_report_unification_error
let reset_and_mark_loops = Printtyp.reset_and_mark_loops
let type_expr = Printtyp.type_expr

let tagged tag fn ppf arg =
    Format.pp_open_tag ppf tag;
    fn ppf arg;
    Format.pp_close_tag ppf ()

let rec bottom_aliases = function
  | (_, one) :: (_, two) :: rest -> begin match bottom_aliases rest with
    | Some types -> Some types
    | None -> Some (one, two)
  end
  | _ -> None

let simple_conversions = [
  (("float", "int"), "int_of_float");
  (("int", "float"), "float_of_int");
  (("int", "string"), "string_of_int");
  (("float", "string"), "string_of_float");
]

let print_simple_conversion ppf (actual, expected) =
  try (
    let converter = List.assoc (actual, expected) simple_conversions in
    Format.pp_print_newline ppf ();
    Format.pp_print_newline ppf ();
    fprintf ppf "You can convert a @{<info>%s@} to a @{<info>%s@} with @{<info>%s@}." actual expected converter
  ) with | Not_found -> ()

let print_simple_message ppf = function
  | ("float", "int") -> fprintf ppf "@[If this is a literal, you want a number without a trailing dot (e.g. @{<info>20@}).@]"
  | ("int", "float") -> fprintf ppf "@[If this is a literal, you want a number with a trailing dot (e.g. @{<info>20.@}).@]"
  | _ -> ()

let show_extra_help ppf env trace = begin
  match bottom_aliases trace with
  | Some ({desc = Tconstr (actualPath, actualArgs, _)}, {desc = Tconstr (expectedPath, expextedArgs, _)}) -> begin
    match (actualPath, actualArgs, expectedPath, expextedArgs) with
    | (Pident {name = actualName}, [], Pident {name = expectedName}, []) -> begin
      print_simple_conversion ppf (actualName, expectedName);
      print_simple_message ppf (actualName, expectedName);
    end
    | _ -> ()
  end;
  | _ -> ();
end

(* given type1 is foo => bar => baz(qux) and type 2 is bar => baz(qux), return Some(foo) *)
let rec collect_missing_arguments env type1 type2 = match type1 with
  (* why do we use Ctype.matches here? Please see https://github.com/BuckleScript/bucklescript/pull/2554 *)
  | {desc=Tarrow (label, argtype, typ, _)} when Ctype.matches env typ type2 ->
    Some [(label, argtype)]
  | {desc=Tarrow (label, argtype, typ, _)} -> begin
    match collect_missing_arguments env typ type2 with
    | Some res -> Some ((label, argtype) :: res)
    | None -> None
    end
  | _ -> None

let check_bs_arity_mismatch ppf trace =
  let arity t = match t.desc with
    | Tvariant { row_fields = [(label,_)] } ->
        let label_len = String.length label in
        let arity_str = "Arity_" in
        let arity_len = String.length arity_str in
        if arity_len < label_len &&
          String.sub label 0 arity_len = arity_str
        then
          try
            Some (int_of_string (String.sub label arity_len (label_len-arity_len)))
          with _ -> None
        else None
    | _ ->
        None in
  let check_mismatch t1 t2 = match (arity t1, arity t2) with
    | Some n1, Some n2 ->
        fprintf ppf "@[@{<info>Found uncurried application [@bs] with arity %d, where arity %d was expected.@}@]" n1 n2;
        true
    | None, _
    | _, None ->
        false in
  let rec traverse = function
    | (_arity1, type1) :: (_arity2, type2) :: rest ->
        if traverse rest
        then true
        else check_mismatch type1 type2
    | _ ->
        false in
  ignore (traverse trace)

let print_expr_type_clash env trace ppf =
  (* this is the most frequent error. Do whatever we can to provide specific
    guidance to this generic error before giving up *)
  if Super_reason_react.state_escape_scope trace && Super_reason_react.trace_both_component_spec trace then
    fprintf ppf "@[<v>\
      @[@{<info>Is this a ReasonReact reducerComponent or component with retained props?@}@ \
      If so, is the type for state, retained props or action declared _after_@ the component declaration?@ \
      @{<info>Moving these types above the component declaration@} should resolve this!@]\
    @]"
    (* This one above shouldn't catch any false positives, so we can safely not display the original type clash error. *)
  else if Super_reason_react.is_component_spec_wanted_react_element trace then
    fprintf ppf "@[<v>\
      @[@{<info>Did you want to create a ReasonReact element without using JSX?@}@ If not, disregard this.@ \
      If so, don't forget to wrap this value in `ReasonReact.element` yourself:@ https://reasonml.github.io/reason-react/docs/en/jsx.html#capitalized@]@,@,\
      @[@{<info>Here's the original error message@}@]@,\
    @]";
    begin
    let bottom_aliases_result = bottom_aliases trace in
    let missing_arguments = match bottom_aliases_result with
    | Some (actual, expected) -> collect_missing_arguments env actual expected
    | None -> assert false
    in
    let print_arguments =
      Format.pp_print_list
        ~pp_sep:(fun ppf _ -> fprintf ppf ",@ ")
        (fun ppf (label, argtype) ->
          match label with
          | Nolabel -> fprintf ppf "@[%a@]" type_expr argtype
          | Labelled label ->
            fprintf ppf "@[(~%s: %a)@]" label type_expr argtype
          | Optional label ->
            fprintf ppf "@[(?%s: %a)@]" label type_expr argtype
        )
    in
    match missing_arguments with
    | Some [singleArgument] ->
      (* btw, you can't say "final arguments". Intermediate labeled
        arguments might be the ones missing *)
      fprintf ppf "@[@{<info>This call is missing an argument@} of type@ %a@]"
        print_arguments [singleArgument]
    | Some arguments ->
      fprintf ppf "@[<hv>@{<info>This call is missing arguments@} of type:@ %a@]"
        print_arguments arguments
    | None ->
      let missing_parameters = match bottom_aliases_result with
      | Some (actual, expected) -> collect_missing_arguments env expected actual
      | None -> assert false
      in
      begin match missing_parameters with
      | Some [singleParameter] ->
        fprintf ppf "@[This value might need to be @{<info>wrapped in a function@ that@ takes@ an@ extra@ parameter@}@ of@ type@ %a@]@,@,"
          print_arguments [singleParameter];
        fprintf ppf "@[@{<info>Here's the original error message@}@]@,"
      | Some arguments ->
        fprintf ppf "@[This value seems to @{<info>need to be wrapped in a function that takes extra@ arguments@}@ of@ type:@ @[<hv>%a@]@]@,@,"
          print_arguments arguments;
        fprintf ppf "@[@{<info>Here's the original error message@}@]@,"
      | None -> ()
      end;
      (* final fallback: show the generic type mismatch error *)
      check_bs_arity_mismatch ppf trace;
      super_report_unification_error ppf env trace
        (function ppf ->
            fprintf ppf "This has type:")
        (function ppf ->
            fprintf ppf "But somewhere wanted:");
      show_extra_help ppf env trace;
    end

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/typecore.ml#L3769 *)
(* modified branches are commented *)
let report_error env ppf = function
  | Typecore.Constructor_arity_mismatch(lid, expected, provided) ->
      (* modified *)
      fprintf ppf
       "@[This variant constructor, %a, expects %i %s; here, we've %sfound %i.@]"
       longident lid expected (if expected == 1 then "argument" else "arguments") (if provided < expected then "only " else "") provided
  | Label_mismatch(lid, trace) ->
      (* modified *)
      super_report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "The record field %a@ belongs to the type"
                   longident lid)
        (function ppf ->
           fprintf ppf "but is mixed here with fields of type")
  | Pattern_type_clash trace ->
      (* modified *)
      super_report_unification_error ppf env trace
        (function ppf ->
          fprintf ppf "This pattern matches values of type")
        (function ppf ->
          fprintf ppf "but a pattern was expected which matches values of type")
  | Or_pattern_type_clash (id, trace) ->
      (* modified *)
      super_report_unification_error ppf env trace
        (function ppf ->
          fprintf ppf "The variable %s on the left-hand side of this or-pattern has type" (Ident.name id))
        (function ppf ->
          fprintf ppf "but on the right-hand side it has type")
  | Expr_type_clash trace ->
      (* modified *)
      fprintf ppf "@[<v>";
      print_expr_type_clash env trace ppf;
      fprintf ppf "@]"
  | Apply_non_function typ ->
      (* modified *)
      reset_and_mark_loops typ;
      begin match (repr typ).desc with
        Tarrow (_, _inputType, returnType, _) ->
          let rec countNumberOfArgs count {desc} = match desc with
          | Tarrow (_, _inputType, returnType, _) -> countNumberOfArgs (count + 1) returnType
          | _ -> count
          in
          let countNumberOfArgs = countNumberOfArgs 1 in
          let acceptsCount = countNumberOfArgs returnType in
          fprintf ppf "@[<v>@[<2>This function has type@ @{<info>%a@}@]"
            type_expr typ;
          fprintf ppf "@ @[It only accepts %i %s; here, it's called with more.@]@]"
                      acceptsCount (if acceptsCount == 1 then "argument" else "arguments")
      | Tconstr (
          (Path.Pdot (((Pdot (Path.Pident {name="Js"}, "Internal", _)) | (Pident {name="Js_internal"})), ("fn" | "meth"), _)),
          _,
          _
        )
        ->
          fprintf
            ppf
            "@[<v>This is an uncurried BuckleScript function. @{<info>It must be applied with a dot@}.@,@,\
            Like this: @{<info>foo(. a, b)@}@,\
            Not like this: @{<dim>foo(a, b)@}@,@,\
            This guarantees that your function is fully applied. More info here:@,\
            https://bucklescript.github.io/docs/en/function.html#solution-guaranteed-uncurrying@]"
      | _ ->
          fprintf ppf "@[<v>@[<2>This expression has type@ %a@]@ %s@]"
            type_expr typ
            "It is not a function."
      end
  | Coercion_failure (ty, ty', trace, b) ->
      (* modified *)
      super_report_unification_error ppf env trace
        (function ppf ->
           let ty, ty' = Printtyp.prepare_expansion (ty, ty') in
           fprintf ppf
             "This expression cannot be coerced to type@;<1 2>%a;@ it has type"
           (Printtyp.type_expansion ty) ty')
        (function ppf ->
           fprintf ppf "but is here used with type");
      if b then
        fprintf ppf ".@.@[<hov>%s@ %s@]"
          "This simple coercion was not fully general."
          "Consider using a double coercion."
  | Too_many_arguments (in_function, ty) ->
      (* modified *)
      reset_and_mark_loops ty;
      if in_function then begin
        fprintf ppf "@[This function expects too many arguments,@ ";
        fprintf ppf "it should have type@ %a@]"
          type_expr ty
      end else begin
        fprintf ppf "@[This expression should not be a function,@ ";
        fprintf ppf "the expected type is@ %a@]"
          type_expr ty
      end
  | Less_general (kind, trace) ->
      (* modified *)
      super_report_unification_error ppf env trace
        (fun ppf -> fprintf ppf "This %s has type" kind)
        (fun ppf -> fprintf ppf "which is less general than")
  | Recursive_local_constraint trace ->
      (* modified *)
      super_report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "Recursive local constraint when unifying")
        (function ppf ->
           fprintf ppf "with")
  | anythingElse ->
      Typecore.super_report_error_no_wrap_printing_env env ppf anythingElse

let report_error env ppf err =
  Printtyp.wrap_printing_env env (fun () -> report_error env ppf err)

(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Typecore.Error (loc, env, err) ->
        Some (Super_location.error_of_printer loc (report_error env) err)
      | Typecore.Error_forward err ->
        Some err
      | _ ->
        None
    )
