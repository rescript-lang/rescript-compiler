type type_clash_statement = FunctionCall
type type_clash_context =
  | SetRecordField
  | ArrayValue
  | FunctionReturn
  | MaybeUnwrapOption
  | IfCondition
  | IfReturn
  | Switch
  | StringConcat
  | ComparisonOperator
  | MathOperator of {
      for_float: bool;
      operator: string;
      is_constant: string option;
    }
  | FunctionArgument
  | Statement of type_clash_statement

let fprintf = Format.fprintf

let error_type_text ppf type_clash_context =
  let text =
    match type_clash_context with
    | Some (Statement FunctionCall) -> "This function call returns:"
    | Some (MathOperator {is_constant = Some _}) -> "This value has type:"
    | Some ArrayValue -> "This array item has type:"
    | Some SetRecordField ->
      "You're assigning something to this field that has type:"
    | _ -> "This has type:"
  in
  fprintf ppf "%s" text

let error_expected_type_text ppf type_clash_context =
  match type_clash_context with
  | Some FunctionArgument ->
    fprintf ppf "But this function argument is expecting:"
  | Some ComparisonOperator ->
    fprintf ppf "But it's being compared to something of type:"
  | Some Switch -> fprintf ppf "But this switch is expected to return:"
  | Some IfCondition ->
    fprintf ppf "But @{<info>if@} conditions must always be of type:"
  | Some IfReturn ->
    fprintf ppf "But this @{<info>if@} statement is expected to return:"
  | Some ArrayValue ->
    fprintf ppf "But this array is expected to have items of type:"
  | Some SetRecordField -> fprintf ppf "But this record field is of type:"
  | Some (Statement FunctionCall) -> fprintf ppf "But it's expected to return:"
  | Some (MathOperator {operator}) ->
    fprintf ppf
      "But it's being used with the @{<info>%s@} operator, which works on:"
      operator
  | Some FunctionReturn ->
    fprintf ppf "But this function is expecting you to return:"
  | _ -> fprintf ppf "But it's expected to have type:"

let print_extra_type_clash_help ppf trace type_clash_context =
  match (type_clash_context, trace) with
  | Some (MathOperator {for_float; operator; is_constant}), _ -> (
    let operator_for_other_type =
      match operator with
      | "+" -> "+."
      | "+." -> "+"
      | "/" -> "/."
      | "/." -> "/"
      | "-" -> "-."
      | "*" -> "*."
      | "*." -> "*"
      | v -> v
    in
    let operator_text =
      match operator.[0] with
      | '+' -> "add"
      | '-' -> "subtract"
      | '/' -> "divide"
      | '*' -> "multiply"
      | _ -> "compute"
    in
    (* TODO check int vs float explicitly before showing this *)
    (match (operator, trace) with
    | ( "+",
        [
          ({Types.desc = Tconstr (p1, _, _)}, _);
          ({desc = Tconstr (p2, _, _)}, _);
        ] )
      when Path.same Predef.path_string p1 || Path.same Predef.path_string p2 ->
      fprintf ppf
        "\n\n\
        \  Are you looking to concatenate strings? Use the operator \
         @{<info>++@}, which concatenates strings.\n\n\
        \  Possible solutions:\n\
        \  - Change the @{<info>+@} operator to @{<info>++@} to concatenate \
         strings instead."
    | _ ->
      fprintf ppf
        "\n\n\
        \  Floats and ints have their own mathematical operators. This means \
         you cannot %s a float and an int without converting between the two.\n\n\
        \  Possible solutions:\n\
        \  - Ensure all values in this calculation has the type @{<info>%s@}. \
         You can convert between floats and ints via \
         @{<info>Belt.Float.toInt@} and @{<info>Belt.Int.fromFloat@}."
        operator_text
        (if for_float then "float" else "int"));
    match (is_constant, trace) with
    | Some constant, _ ->
      if for_float then
        fprintf ppf
          "\n\
          \  - Make @{<info>%s@} a @{<info>float@} by adding a trailing dot: \
           @{<info>%s.@}"
          constant constant
      else
        fprintf ppf
          "\n\
          \  - Make @{<info>%s@} an @{<info>int@} by removing the dot or \
           explicitly converting to int"
          constant
    | ( _,
        [
          ({Types.desc = Tconstr (p1, _, _)}, _);
          ({desc = Tconstr (p2, _, _)}, _);
        ] ) -> (
      match (Path.name p1, Path.name p2) with
      | "float", "int" | "int", "float" ->
        fprintf ppf
          "\n\
          \  - Change the operator to @{<info>%s@}, which works on @{<info>%s@}"
          operator_for_other_type
          (if for_float then "int" else "float")
      | _ -> ())
    | _ -> ())
  | Some Switch, _ ->
    fprintf ppf
      "\n\n\
      \  All branches in a @{<info>switch@} must return the same type. To fix \
       this, change your branch to return the expected type."
  | Some IfCondition, _ ->
    fprintf ppf
      "\n\n\
      \  To fix this, change the highlighted code so it evaluates to a \
       @{<info>bool@}."
  | Some IfReturn, _ ->
    fprintf ppf
      "\n\n\
      \  @{<info>if@} expressions must return the same type in all branches \
       (@{<info>if@}, @{<info>else if@}, @{<info>else@})."
  | Some MaybeUnwrapOption, _ ->
    fprintf ppf
      "\n\n\
      \  Possible solutions:\n\
      \  - Unwrap the option to its underlying value using \
       `yourValue->Belt.Option.getWithDefault(someDefaultValue)`"
  | Some ComparisonOperator, _ ->
    fprintf ppf "\n\n  You can only compare things of the same type."
  | Some ArrayValue, _ ->
    fprintf ppf
      "\n\n\
      \  Arrays can only contain items of the same type.\n\n\
      \  Possible solutions:\n\
      \  - Convert all values in the array to the same type.\n\
      \  - Use a tuple, if your array is of fixed length. Tuples can mix types \
       freely, and compiles to a JavaScript array. Example of a tuple: `let \
       myTuple = (10, \"hello\", 15.5, true)"
  | _ -> ()

let type_clash_context_from_function sexp sfunct =
  let is_constant =
    match sexp.Parsetree.pexp_desc with
    | Pexp_constant (Pconst_integer (txt, _) | Pconst_float (txt, _)) ->
      Some txt
    | _ -> None
  in
  match sfunct.Parsetree.pexp_desc with
  | Pexp_ident
      {txt = Lident ("=" | "==" | "<>" | "!=" | ">" | ">=" | "<" | "<=")} ->
    Some ComparisonOperator
  | Pexp_ident {txt = Lident "++"} -> Some StringConcat
  | Pexp_ident {txt = Lident (("/." | "*." | "+." | "-.") as operator)} ->
    Some (MathOperator {for_float = true; operator; is_constant})
  | Pexp_ident {txt = Lident (("/" | "*" | "+" | "-") as operator)} ->
    Some (MathOperator {for_float = false; operator; is_constant})
  | _ -> Some FunctionArgument

let type_clash_context_for_function_argument type_clash_context sarg0 =
  match type_clash_context with
  | Some (MathOperator {for_float; operator}) ->
    Some
      (MathOperator
         {
           for_float;
           operator;
           is_constant =
             (match sarg0.Parsetree.pexp_desc with
             | Pexp_constant (Pconst_integer (txt, _) | Pconst_float (txt, _))
               ->
               Some txt
             | _ -> None);
         })
  | type_clash_context -> type_clash_context

let type_clash_context_maybe_option ty_expected ty_res =
  match (ty_expected, ty_res) with
  | ( {Types.desc = Tconstr (expected_path, _, _)},
      {Types.desc = Tconstr (type_path, _, _)} )
    when Path.same Predef.path_option type_path
         && Path.same expected_path Predef.path_option = false 
         && Path.same expected_path Predef.path_uncurried = false ->
    Some MaybeUnwrapOption
  | _ -> None

let type_clash_context_in_statement sexp =
  match sexp.Parsetree.pexp_desc with
  | Pexp_apply _ -> Some (Statement FunctionCall)
  | _ -> None
