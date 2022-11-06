module Grammar = Res_grammar
module Token = Res_token

type category =
  | Unexpected of {token: Token.t; context: (Grammar.t * Lexing.position) list}
  | Expected of {
      context: Grammar.t option;
      pos: Lexing.position; (* prev token end*)
      token: Token.t;
    }
  | Message of string
  | Uident of Token.t
  | Lident of Token.t
  | UnclosedString
  | UnclosedTemplate
  | UnclosedComment
  | UnknownUchar of Char.t

type t = {
  startPos: Lexing.position;
  endPos: Lexing.position;
  category: category;
}

type report = t list

let getStartPos t = t.startPos
let getEndPos t = t.endPos

let defaultUnexpected token =
  "I'm not sure what to parse here when looking at \"" ^ Token.toString token
  ^ "\"."

let reservedKeyword token =
  let tokenTxt = Token.toString token in
  "`" ^ tokenTxt ^ "` is a reserved keyword. Keywords need to be escaped: \\\""
  ^ tokenTxt ^ "\""

let explain t =
  match t.category with
  | Uident currentToken -> (
    match currentToken with
    | Lident lident ->
      let guess = String.capitalize_ascii lident in
      "Did you mean `" ^ guess ^ "` instead of `" ^ lident ^ "`?"
    | t when Token.isKeyword t ->
      let token = Token.toString t in
      "`" ^ token ^ "` is a reserved keyword."
    | _ ->
      "At this point, I'm looking for an uppercased name like `Belt` or `Array`"
    )
  | Lident currentToken -> (
    match currentToken with
    | Uident uident ->
      let guess = String.uncapitalize_ascii uident in
      "Did you mean `" ^ guess ^ "` instead of `" ^ uident ^ "`?"
    | t when Token.isKeyword t ->
      let token = Token.toString t in
      "`" ^ token ^ "` is a reserved keyword. Keywords need to be escaped: \\\""
      ^ token ^ "\""
    | Underscore -> "`_` isn't a valid name."
    | _ -> "I'm expecting a lowercase name like `user or `age`")
  | Message txt -> txt
  | UnclosedString -> "This string is missing a double quote at the end"
  | UnclosedTemplate ->
    "Did you forget to close this template expression with a backtick?"
  | UnclosedComment -> "This comment seems to be missing a closing `*/`"
  | UnknownUchar uchar -> (
    match uchar with
    | '^' ->
      "Not sure what to do with this character.\n"
      ^ "  If you're trying to dereference a mutable value, use \
         `myValue.contents` instead.\n"
      ^ "  To concatenate strings, use `\"a\" ++ \"b\"` instead."
    | _ -> "Not sure what to do with this character.")
  | Expected {context; token = t} ->
    let hint =
      match context with
      | Some grammar -> " It signals the start of " ^ Grammar.toString grammar
      | None -> ""
    in
    "Did you forget a `" ^ Token.toString t ^ "` here?" ^ hint
  | Unexpected {token = t; context = breadcrumbs} -> (
    let name = Token.toString t in
    match breadcrumbs with
    | (AtomicTypExpr, _) :: breadcrumbs -> (
      match (breadcrumbs, t) with
      | ( ((StringFieldDeclarations | FieldDeclarations), _) :: _,
          (String _ | At | Rbrace | Comma | Eof) ) ->
        "I'm missing a type here"
      | _, t when Grammar.isStructureItemStart t || t = Eof ->
        "Missing a type here"
      | _ -> defaultUnexpected t)
    | (ExprOperand, _) :: breadcrumbs -> (
      match (breadcrumbs, t) with
      | (ExprBlock, _) :: _, Rbrace ->
        "It seems that this expression block is empty"
      | (ExprBlock, _) :: _, Bar ->
        (* Pattern matching *)
        "Looks like there might be an expression missing here"
      | (ExprSetField, _) :: _, _ ->
        "It seems that this record field mutation misses an expression"
      | (ExprArrayMutation, _) :: _, _ ->
        "Seems that an expression is missing, with what do I mutate the array?"
      | ((ExprBinaryAfterOp _ | ExprUnary), _) :: _, _ ->
        "Did you forget to write an expression here?"
      | (Grammar.LetBinding, _) :: _, _ ->
        "This let-binding misses an expression"
      | _ :: _, (Rbracket | Rbrace | Eof) -> "Missing expression"
      | _ -> "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
      )
    | (TypeParam, _) :: _ -> (
      match t with
      | Lident ident ->
        "Did you mean '" ^ ident ^ "? A Type parameter starts with a quote."
      | _ -> "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
      )
    | (Pattern, _) :: breadcrumbs -> (
      match (t, breadcrumbs) with
      | Equal, (LetBinding, _) :: _ ->
        "I was expecting a name for this let-binding. Example: `let message = \
         \"hello\"`"
      | In, (ExprFor, _) :: _ ->
        "A for-loop has the following form: `for i in 0 to 10`. Did you forget \
         to supply a name before `in`?"
      | EqualGreater, (PatternMatchCase, _) :: _ ->
        "I was expecting a pattern to match on before the `=>`"
      | token, _ when Token.isKeyword t -> reservedKeyword token
      | token, _ -> defaultUnexpected token)
    | _ ->
      (* TODO: match on circumstance to verify Lident needed ? *)
      if Token.isKeyword t then
        "`" ^ name
        ^ "` is a reserved keyword. Keywords need to be escaped: \\\""
        ^ Token.toString t ^ "\""
      else "I'm not sure what to parse here when looking at \"" ^ name ^ "\".")

let make ~startPos ~endPos category = {startPos; endPos; category}

let printReport diagnostics src =
  let rec print diagnostics src =
    match diagnostics with
    | [] -> ()
    | d :: rest ->
      Res_diagnostics_printing_utils.Super_location.super_error_reporter
        Format.err_formatter src
        Location.
          {
            loc = {loc_start = d.startPos; loc_end = d.endPos; loc_ghost = false};
            msg = explain d;
            sub = [];
            if_highlight = "";
          };
      (match rest with
      | [] -> ()
      | _ -> Format.fprintf Format.err_formatter "@.");
      print rest src
  in
  Format.fprintf Format.err_formatter "@[<v>";
  print (List.rev diagnostics) src;
  Format.fprintf Format.err_formatter "@]@."

let unexpected token context = Unexpected {token; context}

let expected ?grammar pos token = Expected {context = grammar; pos; token}

let uident currentToken = Uident currentToken
let lident currentToken = Lident currentToken
let unclosedString = UnclosedString
let unclosedComment = UnclosedComment
let unclosedTemplate = UnclosedTemplate
let unknownUchar code = UnknownUchar code
let message txt = Message txt
