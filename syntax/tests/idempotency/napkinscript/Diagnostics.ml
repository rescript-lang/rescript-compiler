module Diagnostics: sig
  type t
  type category

  val unexpected: Token.t -> (Grammar.t * Lexing.position) list -> category
  val expected:  ?grammar:Grammar.t -> Lexing.position -> Token.t -> category
  val uident: Token.t -> category
  val lident: Token.t -> category
  val unclosedString: category
  val unclosedTemplate: category
  val unclosedComment: category
  val unknownUchar: int -> category
  val message: string -> category
  val unbalanced: Token.t -> category

  val make:
    filename: string
    -> startPos: Lexing.position
    -> endPos: Lexing.position
    -> category
    -> t

  val makeReport: t list -> string -> string
end = struct
  type category =
    | Unexpected of (Token.t * ((Grammar.t * Lexing.position) list))
    | Expected of (Grammar.t option * Lexing.position (* prev token end*) * Token.t)
    | Message of string
    | Uident of Token.t
    | Lident of Token.t
    | UnclosedString
    | UnclosedTemplate
    | UnclosedComment
    | UnknownUchar of int
    | Unbalanced of Token.t

  let stringOfCategory = function
    | Unexpected _ -> "unexpected"
    | Expected _ -> "expected"
    | Message txt -> txt
    | Uident _ -> "uident"
    | Lident _ -> "lident"
    | UnclosedString -> "unclosed string"
    | UnclosedTemplate -> "unclosed template"
    | UnclosedComment -> "unclosed comment"
    | Unbalanced _ -> "unbalanced"
    | UnknownUchar _ -> "unknown rune"

  type t = {
    filename: string;
    startPos: Lexing.position;
    endPos: Lexing.position;
    category: category;
  }

  let defaultUnexpected token =
    "I'm not sure what to parse here when looking at \"" ^ (Token.toString token) ^ "\"."

  let toString t src =
    let open Lexing in
    let  startchar = t.startPos.pos_cnum - t.startPos.pos_bol in
    let endchar = t.endPos.pos_cnum - t.startPos.pos_cnum + startchar in
    let locationInfo =
      Printf.sprintf (* ReasonLanguageServer requires the following format *)
        "File \"%s\", line %d, characters %d-%d:"
        t.filename
        t.startPos.pos_lnum
        startchar
        endchar
    in
    let code =
      let missing = match t.category with
      | Expected (_, _, t) ->
        Some (String.length (Token.toString t))
      | _ -> None
      in
      Reporting.renderCodeContext ~missing src t.startPos t.endPos
    in
    let explanation = match t.category with
    | Uident currentToken ->
      begin match currentToken with
      | Lident lident ->
        let guess = String.capitalize_ascii lident in
        "Did you mean `" ^ guess ^"` instead of `" ^ lident ^ "`?"
      | t when Token.isKeyword t ->
        let token = Token.toString t in
        "`" ^ token ^ "` is a reserved keyword. Try `" ^ token ^ "_` or `_" ^ token ^ "` instead"
      | _ ->
        "At this point, I'm looking for an uppercased identifier like `Belt` or `Array`"
      end
    | Lident currentToken ->
      begin match currentToken with
      | Uident uident ->
        let guess = String.uncapitalize_ascii uident in
        "Did you mean `" ^ guess ^"` instead of `" ^ uident ^ "`?"
      | t when Token.isKeyword t ->
        let token = Token.toString t in
        "`" ^ token ^ "` is a reserved keyword. Try `" ^ token ^ "_` or `_" ^ token ^ "` instead"
      | _ ->
        "I'm expecting an lowercased identifier like `name` or `age`"
      end
    | Message txt -> txt
    | UnclosedString ->
      "This string is missing a double quote at the end"
    | UnclosedTemplate ->
      "Did you forget to close this template expression with a backtick?"
    | UnclosedComment ->
      "This comment seems to be missing a closing `*/`"
    | UnknownUchar uchar ->
      begin match uchar with
      | 94 (* ^ *) ->
        "Hmm, not sure what I should do here with this character.\nIf you're trying to deref an expression, use `foo.contents` instead."
      | _ ->
        "Hmm, I have no idea what this character means‚Ä¶"
      end
    | Unbalanced t ->
      "Closing \"" ^ (Token.toString t) ^ "\" seems to be missing."
    | Expected (context, _, t) ->
      let hint = match context with
      | Some grammar -> "It signals the start of " ^ (Grammar.toString grammar)
      | None -> ""
      in
      "Did you forget a `" ^ (Token.toString t) ^ "` here? " ^ hint
    | Unexpected (t, breadcrumbs) ->
      let name = (Token.toString t) in
      begin match breadcrumbs with
      | (AtomicTypExpr, _)::breadcrumbs ->
          begin match breadcrumbs, t with
          | (StringFieldDeclarations, _) :: _, (String _ | At | Rbrace | Comma | Eof) ->
              "I'm missing a type here"
          | _, t when Grammar.isStructureItemStart t || t = Eof ->
              "Missing a type here"
          | _ ->
            defaultUnexpected t
          end
      | (ExprOperand, _)::breadcrumbs ->
          begin match breadcrumbs, t with
          | (ExprBlock, _) :: _, Rbrace ->
            "It seems that this expression block is empty"
          | (ExprBlock, _) :: _, Bar -> (* Pattern matching *)
            "Looks like there might be an expression missing here"
          | (ExprSetField, _) :: _, _ ->
            "It seems that this record field mutation misses an expression"
          | (ExprArrayMutation, _) :: _, _ ->
            "Seems that an expression is missing, with what do I mutate the array?"
          | ((ExprBinaryAfterOp _ | ExprUnary), _) ::_, _ ->
            "Did you forget to write an expression here?"
          | (Grammar.LetBinding, _)::_, _ ->
            "This let-binding misses an expression"
          | _::_, Rbracket ->
            "Missing expression"
          | _ ->
            "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
          end
      | (TypeParam, _)::_ ->
          begin match t with
          | Lident ident ->
            "Did you mean '" ^ ident ^"? A Type parameter starts with a quote."
          | _ ->
            "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
          end
      | _ ->
        (* TODO: match on circumstance to verify Lident needed ? *)
        if Token.isKeyword t then
          name ^ " is a reserved keyword, Try `" ^ name ^ "_` instead"
        else
        "I'm not sure what to parse here when looking at \"" ^ name ^ "\"."
      end
    in
    Printf.sprintf "%s\n\n%s\n\n%s\n\n" locationInfo code explanation

  let make ~filename ~startPos ~endPos category = {
    filename;
    startPos;
    endPos;
    category
  }

  let makeReport diagnostics src =
    List.fold_left (fun report diagnostic ->
      report ^ (toString diagnostic src) ^ "\n"
    ) "\n" (List.rev diagnostics)

  let print {category} =
    prerr_endline (stringOfCategory category)

  let unexpected token context =
    Unexpected(token, context)

  let expected ?grammar pos token =
    Expected(grammar, pos, token)

  let uident currentToken = Uident currentToken
  let lident currentToken = Lident currentToken
  let unclosedString = UnclosedString
  let unclosedComment = UnclosedComment
  let unclosedTemplate = UnclosedTemplate
  let unknownUchar code = UnknownUchar code
  let message txt = Message txt
  let unbalanced token = Unbalanced token
end
