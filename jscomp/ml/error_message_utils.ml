type typeClashStatement = FunctionCall
type typeClashContext =
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
      forFloat: bool;
      operator: string;
      isConstant: string option;
    }
  | FunctionArgument
  | Statement of typeClashStatement

let fprintf = Format.fprintf

let errorTypeText ppf typeClashContext =
  let text =
    match typeClashContext with
    | Some (Statement FunctionCall) -> "This function call returns:"
    | Some (MathOperator {isConstant = Some _}) -> "This value has type:"
    | Some ArrayValue -> "This array item has type:"
    | Some SetRecordField ->
      "You're assigning something to this field that has type:"
    | _ -> "This has type:"
  in
  fprintf ppf "%s" text

let errorExpectedTypeText ppf typeClashContext =
  match typeClashContext with
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

let printExtraTypeClashHelp ppf trace typeClashContext =
  match (typeClashContext, trace) with
  | Some (MathOperator {forFloat; operator; isConstant}), _ -> (
    let operatorForOtherType =
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
    let operatorText =
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
        operatorText
        (if forFloat then "float" else "int"));
    match (isConstant, trace) with
    | Some constant, _ ->
      if forFloat then
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
          operatorForOtherType
          (if forFloat then "int" else "float")
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

let typeClashContextFromFunction sexp sfunct =
  let isConstant =
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
    Some (MathOperator {forFloat = true; operator; isConstant})
  | Pexp_ident {txt = Lident (("/" | "*" | "+" | "-") as operator)} ->
    Some (MathOperator {forFloat = false; operator; isConstant})
  | _ -> Some FunctionArgument

let typeClashContextForFunctionArgument typeClashContext sarg0 =
  match typeClashContext with
  | Some (MathOperator {forFloat; operator}) ->
    Some
      (MathOperator
         {
           forFloat;
           operator;
           isConstant =
             (match sarg0.Parsetree.pexp_desc with
             | Pexp_constant (Pconst_integer (txt, _) | Pconst_float (txt, _))
               ->
               Some txt
             | _ -> None);
         })
  | typeClashContext -> typeClashContext

let typeClashContextMaybeOption ty_expected ty_res =
  match (ty_expected, ty_res) with
  | ( {Types.desc = Tconstr (expectedPath, _, _)},
      {Types.desc = Tconstr (typePath, _, _)} )
    when Path.same Predef.path_option typePath
         && Path.same expectedPath Predef.path_option = false ->
    Some MaybeUnwrapOption
  | _ -> None

let typeClashContextInStatement sexp =
  match sexp.Parsetree.pexp_desc with
  | Pexp_apply _ -> Some (Statement FunctionCall)
  | _ -> None
