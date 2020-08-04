module Doc = Napkin_doc
module Grammar = Napkin_grammar
module Token = Napkin_token
module Diagnostics = Napkin_diagnostics
module CommentTable = Napkin_comments_table
module NapkinscriptPrinter =  Napkin_printer
module Scanner = Napkin_scanner
module JsFfi = Napkin_js_ffi
module Parser = Napkin_parser

let mkLoc startLoc endLoc = Location.{
  loc_start = startLoc;
  loc_end = endLoc;
  loc_ghost = false;
}

module Recover = struct
  (* type action = unit option None is abort, Some () is retry *)

  let defaultExpr () =
    let id = Location.mknoloc "napkinscript.exprhole" in
    Ast_helper.Exp.mk (Pexp_extension (id, PStr []))

  let defaultType () =
    let id = Location.mknoloc "napkinscript.typehole" in
    Ast_helper.Typ.extension (id, PStr [])

  let defaultPattern () =
    let id = Location.mknoloc "napkinscript.patternhole" in
    Ast_helper.Pat.extension (id, PStr [])
    (* Ast_helper.Pat.any  () *)

  let defaultModuleExpr () = Ast_helper.Mod.structure []
  let defaultModuleType () = Ast_helper.Mty.signature []

  let recoverEqualGreater p =
    Parser.expect EqualGreater p;
    match p.Parser.token with
    | MinusGreater -> Parser.next p
    | _ -> ()

  let shouldAbortListParse p =
    let rec check breadcrumbs =
      match breadcrumbs with
      | [] -> false
      | (grammar, _)::rest ->
        if Grammar.isPartOfList grammar p.Parser.token then
          true
        else
          check rest
    in
    check p.breadcrumbs
end

module ErrorMessages = struct
  let listPatternSpread = "List pattern matches only supports one `...` spread, at the end.\n\
Explanation: a list spread at the tail is efficient, but a spread in the middle would create new list[s]; out of performance concern, our pattern matching currently guarantees to never create new intermediate data."

  let recordPatternSpread = "Record's `...` spread is not supported in pattern matches.\n\
Explanation: you can't collect a subset of a record's field into its own record, since a record needs an explicit declaration and that subset wouldn't have one.\n\
Solution: you need to pull out each field you want explicitly."

  (* let recordPatternUnderscore = "Record patterns only support one `_`, at the end." *)
  [@@live]

  let arrayPatternSpread = "Array's `...` spread is not supported in pattern matches.\n\
Explanation: such spread would create a subarray; out of performance concern, our pattern matching currently guarantees to never create new intermediate data.\n\
Solution: if it's to validate the first few elements, use a `when` clause + Array size check + `get` checks on the current pattern. If it's to obtain a subarray, use `Array.sub` or `Belt.Array.slice`."

  let arrayExprSpread = "Arrays can't use the `...` spread currently. Please use `concat` or other Array helpers."

  let recordExprSpread = "Records can only have one `...` spread, at the beginning.\n\
Explanation: since records have a known, fixed shape, a spread like `{a, ...b}` wouldn't make sense, as `b` would override every field of `a` anyway."

  let listExprSpread =  "Lists can only have one `...` spread, and at the end.\n\
Explanation: lists are singly-linked list, where a node contains a value and points to the next node. `list[a, ...bc]` efficiently creates a new item and links `bc` as its next nodes. `[...bc, a]` would be expensive, as it'd need to traverse `bc` and prepend each item to `a` one by one. We therefore disallow such syntax sugar.\n\
Solution: directly use `concat`."

  let variantIdent = "A polymorphic variant (e.g. #id) must start with an alphabetical letter."

  let experimentalIfLet expr =
    let switchExpr = {expr with Parsetree.pexp_attributes = []} in
    Doc.concat [
      Doc.text "If-let is currently highly experimental.";
      Doc.line;
      Doc.text "Use a regular `switch` with pattern matching instead:";
      Doc.concat [
        Doc.hardLine;
        Doc.hardLine;
        NapkinscriptPrinter.printExpression switchExpr (CommentTable.empty);
      ]
    ] |> Doc.toString ~width:80

  let typeParam = "A type param consists of a singlequote followed by a name like `'a` or `'A`"
  let typeVar = "A type variable consists of a singlequote followed by a name like `'a` or `'A`"
end


let jsxAttr = (Location.mknoloc "JSX", Parsetree.PStr [])
let uncurryAttr = (Location.mknoloc "bs", Parsetree.PStr [])
let ternaryAttr = (Location.mknoloc "ns.ternary", Parsetree.PStr [])
let ifLetAttr = (Location.mknoloc "ns.iflet", Parsetree.PStr [])
let suppressFragileMatchWarningAttr = (Location.mknoloc "warning", Parsetree.PStr [Ast_helper.Str.eval (Ast_helper.Exp.constant (Pconst_string ("-4", None)))])
let makeBracesAttr loc = (Location.mkloc "ns.braces" loc, Parsetree.PStr [])

type typDefOrExt =
  | TypeDef of {recFlag: Asttypes.rec_flag; types: Parsetree.type_declaration list}
  | TypeExt of Parsetree.type_extension

type labelledParameter =
  | TermParameter of
      {uncurried: bool; attrs: Parsetree.attributes; label: Asttypes.arg_label; expr: Parsetree.expression option;
      pat: Parsetree.pattern; pos: Lexing.position}
  | TypeParameter of {uncurried: bool; attrs: Parsetree.attributes; locs: string Location.loc list; pos: Lexing.position}

type recordPatternItem =
  | PatUnderscore
  | PatField of (Ast_helper.lid * Parsetree.pattern)

type context =
  | OrdinaryExpr
  | TernaryTrueBranchExpr
  | WhenExpr

let getClosingToken = function
  | Token.Lparen -> Token.Rparen
  | Lbrace -> Rbrace
  | Lbracket -> Rbracket
  | List -> Rbrace
  | _ -> assert false

let rec goToClosing closingToken state =
  match (state.Parser.token, closingToken) with
  | (Rparen, Token.Rparen) | (Rbrace, Rbrace) | (Rbracket, Rbracket) ->
    Parser.next state;
    ()
  | (Token.Lbracket | Lparen | Lbrace | List) as t, _ ->
    Parser.next state;
    goToClosing (getClosingToken t) state;
    goToClosing closingToken state
  | ((Rparen | Token.Rbrace | Rbracket | Eof), _)  ->
    () (* TODO: how do report errors here? *)
  | _ ->
    Parser.next state;
    goToClosing closingToken state

(* Madness *)
let isEs6ArrowExpression ~inTernary p =
  Parser.lookahead p (fun state ->
    match state.Parser.token with
    | Lident _ | Underscore ->
      Parser.next state;
      begin match state.Parser.token with
      (* Don't think that this valid
       * Imagine: let x = (a: int)
       * This is a parenthesized expression with a type constraint, wait for
       * the arrow *)
      (* | Colon when not inTernary -> true *)
      | EqualGreater -> true
      | _ -> false
      end
    | Lparen ->
      let prevEndPos = state.prevEndPos in
      Parser.next state;
      begin match state.token with
      | Rparen ->
        Parser.next state;
        begin match state.Parser.token with
        | Colon when not inTernary -> true
        | EqualGreater -> true
        | _ -> false
        end
      | Dot (* uncurried *) -> true
      | Tilde -> true
      | Backtick -> false (* (` always indicates the start of an expr, can't be es6 parameter *)
      | _ ->
        goToClosing Rparen state;
        begin match state.Parser.token with
        | EqualGreater -> true
        (* | Lbrace TODO: detect missing =>, is this possible? *)
        | Colon when not inTernary -> true
        | Rparen ->
          (* imagine having something as :
           * switch colour {
           * | Red
           *    when l == l'
           *    || (&Clflags.classic && (l == Nolabel && !is_optional(l'))) => (t1, t2)
           * We'll arrive at the outer rparen just before the =>.
           * This is not an es6 arrow.
           * *)
          false
        | _ ->
          Parser.next state;
          (* error recovery, peek at the next token,
           * (elements, providerId] => {
           *  in the example above, we have an unbalanced ] here
           *)
          begin match state.Parser.token with
          | EqualGreater when state.startPos.pos_lnum == prevEndPos.pos_lnum -> true
          | _ -> false
          end
        end
      end
    | _ -> false)


let isEs6ArrowFunctor p =
  Parser.lookahead p (fun state ->
    match state.Parser.token with
    (* | Uident _ | Underscore -> *)
      (* Parser.next state; *)
      (* begin match state.Parser.token with *)
      (* | EqualGreater -> true *)
      (* | _ -> false *)
      (* end *)
    | Lparen ->
      Parser.next state;
      begin match state.token with
      | Rparen ->
        Parser.next state;
        begin match state.token with
        | Colon | EqualGreater -> true
        | _ -> false
        end
      | _ ->
        goToClosing Rparen state;
        begin match state.Parser.token with
        | EqualGreater | Lbrace -> true
        | Colon -> true
        | _ -> false
        end
      end
    | _ -> false
  )

let isEs6ArrowType p =
  Parser.lookahead p (fun state ->
    match state.Parser.token with
    | Lparen ->
      Parser.next state;
      begin match state.Parser.token with
      | Rparen ->
        Parser.next state;
        begin match state.Parser.token with
        | EqualGreater -> true
        | _ -> false
        end
      | Tilde | Dot -> true
      | _ ->
        goToClosing Rparen state;
        begin match state.Parser.token with
        | EqualGreater  -> true
        | _ -> false
        end
      end
    | Tilde -> true
    | _ -> false
  )

let buildLongident words = match List.rev words with
  | [] -> assert false
  | hd::tl -> List.fold_left (fun p s -> Longident.Ldot (p, s)) (Lident hd) tl

let makeInfixOperator p token startPos endPos =
  let stringifiedToken =
    if token = Token.MinusGreater then "|."
    else if token = Token.PlusPlus then "^"
    else if token = Token.BangEqual then "<>"
    else if token = Token.BangEqualEqual then "!="
    else if token = Token.Equal then (
      (* TODO: could have a totally different meaning like x->fooSet(y)*)
      Parser.err ~startPos ~endPos p (
        Diagnostics.message "Did you mean `==` here?"
      );
      "="
    ) else if token = Token.EqualEqual then "="
    else if token = Token.EqualEqualEqual then "=="
    else Token.toString token
  in
  let loc = mkLoc startPos endPos in
  let operator = Location.mkloc
    (Longident.Lident stringifiedToken) loc
  in
  Ast_helper.Exp.ident ~loc operator

let negateString s =
  if String.length s > 0 && (s.[0] [@doesNotRaise]) = '-'
  then (String.sub [@doesNotRaise]) s 1 (String.length s - 1)
  else "-" ^ s

let makeUnaryExpr startPos tokenEnd token operand =
  match token, operand.Parsetree.pexp_desc with
  | (Token.Plus | PlusDot), Pexp_constant((Pconst_integer _ | Pconst_float _)) ->
    operand
  | Minus, Pexp_constant(Pconst_integer (n,m)) ->
    {operand with pexp_desc = Pexp_constant(Pconst_integer (negateString n,m))}
  | (Minus | MinusDot), Pexp_constant(Pconst_float (n,m)) ->
    {operand with pexp_desc = Pexp_constant(Pconst_float (negateString n,m))}
  | (Token.Plus | PlusDot | Minus | MinusDot ), _ ->
    let tokenLoc = mkLoc startPos tokenEnd in
    let operator = "~" ^ Token.toString token in
    Ast_helper.Exp.apply
      ~loc:(mkLoc startPos operand.Parsetree.pexp_loc.loc_end)
      (Ast_helper.Exp.ident ~loc:tokenLoc
        (Location.mkloc (Longident.Lident operator) tokenLoc))
      [Nolabel, operand]
  | Token.Bang, _ ->
    let tokenLoc = mkLoc startPos tokenEnd in
    Ast_helper.Exp.apply
      ~loc:(mkLoc startPos operand.Parsetree.pexp_loc.loc_end)
      (Ast_helper.Exp.ident ~loc:tokenLoc
        (Location.mkloc (Longident.Lident "not") tokenLoc))
      [Nolabel, operand]
  | _ ->
    operand

let makeListExpression loc seq extOpt =
  let rec handleSeq = function
    | [] ->
      begin match extOpt with
      | Some ext -> ext
      | None ->
        let loc = {loc with Location.loc_ghost = true} in
        let nil = Location.mkloc (Longident.Lident "[]") loc in
        Ast_helper.Exp.construct ~loc nil None
      end
    | e1 :: el ->
      let exp_el = handleSeq el in
      let loc = mkLoc
        e1.Parsetree.pexp_loc.Location.loc_start
        exp_el.pexp_loc.loc_end
      in
      let arg = Ast_helper.Exp.tuple ~loc [e1; exp_el] in
      Ast_helper.Exp.construct ~loc
        (Location.mkloc (Longident.Lident "::") loc)
        (Some arg)
  in
  let expr = handleSeq seq in
  {expr with pexp_loc = loc}

let makeListPattern loc seq ext_opt =
  let rec handle_seq = function
    [] ->
      let base_case = match ext_opt with
        | Some ext ->
          ext
        | None ->
          let loc = { loc with Location.loc_ghost = true} in
          let nil = { Location.txt = Longident.Lident "[]"; loc } in
          Ast_helper.Pat.construct ~loc nil None
      in
      base_case
  | p1 :: pl ->
      let pat_pl = handle_seq pl in
      let loc =
        mkLoc p1.Parsetree.ppat_loc.loc_start pat_pl.ppat_loc.loc_end in
      let arg = Ast_helper.Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
      Ast_helper.Pat.mk ~loc (Ppat_construct(Location.mkloc (Longident.Lident "::") loc, Some arg))
  in
  handle_seq seq


(* {"foo": bar} -> Js.t({. foo: bar})
 * {.. "foo": bar} -> Js.t({.. foo: bar})
 * {..} -> Js.t({..}) *)
let makeBsObjType ~attrs ~loc ~closed rows =
  let obj = Ast_helper.Typ.object_ ~loc rows closed in
  let jsDotTCtor =
    Location.mkloc (Longident.Ldot (Longident.Lident "Js", "t")) loc
  in
  Ast_helper.Typ.constr ~loc ~attrs jsDotTCtor [obj]

(* TODO: diagnostic reporting *)
let lidentOfPath longident =
  match Longident.flatten longident |> List.rev with
  | [] -> ""
  | ident::_ -> ident

let makeNewtypes ~attrs ~loc newtypes exp =
  let expr = List.fold_right (fun newtype exp ->
    Ast_helper.Exp.mk ~loc (Pexp_newtype (newtype, exp))
  ) newtypes exp
  in {expr with pexp_attributes = attrs}

(* locally abstract types syntax sugar
 * Transforms
 *  let f: type t u v. = (foo : list</t, u, v/>) => ...
 * into
 *  let f = (type t u v. foo : list</t, u, v/>) => ...
 *)
let wrapTypeAnnotation ~loc newtypes core_type body =
  let exp = makeNewtypes ~attrs:[] ~loc newtypes
    (Ast_helper.Exp.constraint_ ~loc body core_type)
  in
  let typ = Ast_helper.Typ.poly ~loc newtypes
    (Ast_helper.Typ.varify_constructors newtypes core_type)
  in
  (exp, typ)

(**
  * process the occurrence of _ in the arguments of a function application
  * replace _ with a new variable, currently __x, in the arguments
  * return a wrapping function that wraps ((__x) => ...) around an expression
  * e.g. foo(_, 3) becomes (__x) => foo(__x, 3)
  *)
let processUnderscoreApplication args =
  let open Parsetree in
  let exp_question = ref None in
  let hidden_var = "__x" in
  let check_arg ((lab, exp) as arg) =
    match exp.pexp_desc with
    | Pexp_ident ({ txt = Lident "_"} as id) ->
      let new_id = Location.mkloc (Longident.Lident hidden_var) id.loc in
      let new_exp = Ast_helper.Exp.mk (Pexp_ident new_id) ~loc:exp.pexp_loc in
      exp_question := Some new_exp;
      (lab, new_exp)
    | _ ->
      arg
  in
  let args = List.map check_arg args in
  let wrap exp_apply =
    match !exp_question with
    | Some {pexp_loc=loc} ->
      let pattern = Ast_helper.Pat.mk (Ppat_var (Location.mkloc hidden_var loc)) ~loc in
      Ast_helper.Exp.mk (Pexp_fun (Nolabel, None, pattern, exp_apply)) ~loc
    | None ->
      exp_apply
  in
  (args, wrap)

let rec parseLident p =
  let recoverLident p =
    if (
      Token.isKeyword p.Parser.token &&
      p.Parser.prevEndPos.pos_lnum == p.startPos.pos_lnum
    )
    then (
      Parser.err p (Diagnostics.lident p.Parser.token);
      Parser.next p;
      None
    ) else (
      let rec loop p =
        if not (Recover.shouldAbortListParse p)
        then begin
          Parser.next p;
          loop p
        end
      in
      Parser.next p;
      loop p;
      match p.Parser.token with
      | Lident _ -> Some ()
      | _ -> None
    )
  in
  let startPos = p.Parser.startPos in
  match p.Parser.token with
  | Lident ident ->
    Parser.next p;
    let loc = mkLoc startPos p.prevEndPos in
    (ident, loc)
  | _ ->
    begin match recoverLident p with
    | Some () ->
      parseLident p
    | None ->
      ("_", mkLoc startPos p.prevEndPos)
    end

let parseIdent ~msg ~startPos p =
  match p.Parser.token with
  | Lident ident
  | Uident ident ->
    Parser.next p;
    let loc = mkLoc startPos p.prevEndPos in
    (ident, loc)
  | token when Token.isKeyword token &&
      p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
    let tokenTxt = Token.toString token in
    let msg =
      "`" ^ tokenTxt ^ "` is a reserved keyword. Keywords need to be escaped: \\\"" ^ tokenTxt ^ "\""
    in
    Parser.err ~startPos p (Diagnostics.message msg);
    Parser.next p;
    (tokenTxt, mkLoc startPos p.prevEndPos)
  | _token ->
    Parser.err ~startPos p (Diagnostics.message msg);
    Parser.next p;
    ("", mkLoc startPos p.prevEndPos)

let parseHashIdent ~startPos p =
  Parser.expect Hash p;
  parseIdent ~startPos ~msg:ErrorMessages.variantIdent p

(* Ldot (Ldot (Lident "Foo", "Bar"), "baz") *)
let parseValuePath p =
  let startPos = p.Parser.startPos in
  let rec aux p path =
    match p.Parser.token with
    | Lident ident -> Longident.Ldot(path, ident)
    | Uident uident ->
      Parser.next p;
      Parser.expect Dot p;
      aux p (Ldot (path, uident))
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Longident.Lident "_"
  in
  let ident = match p.Parser.token with
  | Lident ident -> Longident.Lident ident
  | Uident ident ->
    Parser.next p;
    Parser.expect Dot p;
    aux p (Lident ident)
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    Longident.Lident "_"
  in
  Parser.next p;
  Location.mkloc ident (mkLoc startPos p.prevEndPos)

let parseValuePathTail p startPos ident =
  let rec loop p path =
    match p.Parser.token with
    | Lident ident ->
      Parser.next p;
      Location.mkloc (Longident.Ldot(path, ident)) (mkLoc startPos p.prevEndPos)
    | Uident ident ->
      Parser.next p;
      Parser.expect Dot p;
      loop p (Longident.Ldot (path, ident))
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Location.mknoloc path
  in
  loop p ident

let parseModuleLongIdentTail ~lowercase p startPos ident =
  let rec loop p acc =
    match p.Parser.token with
    | Lident ident when lowercase ->
      Parser.next p;
      let lident = (Longident.Ldot (acc, ident)) in
      Location.mkloc lident (mkLoc startPos p.prevEndPos)
    | Uident ident ->
      Parser.next p;
      let endPos = p.prevEndPos in
      let lident = (Longident.Ldot (acc, ident)) in
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        loop p lident
      | _ -> Location.mkloc lident (mkLoc startPos endPos)
      end
    | t ->
      Parser.err p (Diagnostics.uident t);
      Location.mkloc acc (mkLoc startPos p.prevEndPos)
  in
  loop p ident

(* Parses module identifiers:
     Foo
     Foo.Bar *)
let parseModuleLongIdent ~lowercase p =
  (* Parser.leaveBreadcrumb p Reporting.ModuleLongIdent; *)
  let startPos = p.Parser.startPos in
  let moduleIdent = match p.Parser.token with
  | Lident ident when lowercase ->
    let loc = mkLoc startPos p.endPos in
    let lident = Longident.Lident ident in
    Parser.next p;
    Location.mkloc lident loc
  | Uident ident ->
    let lident = Longident.Lident ident in
    let endPos = p.endPos in
    Parser.next p;
    begin match p.Parser.token with
    | Dot ->
      Parser.next p;
      parseModuleLongIdentTail ~lowercase p startPos lident
    | _ -> Location.mkloc lident (mkLoc startPos endPos)
    end
  | t ->
    Parser.err p (Diagnostics.uident t);
    Location.mkloc (Longident.Lident "_") (mkLoc startPos p.prevEndPos)
  in
  (* Parser.eatBreadcrumb p; *)
  moduleIdent

(* `window.location` or `Math` or `Foo.Bar` *)
let parseIdentPath p =
  let rec loop p acc =
    match p.Parser.token with
    | Uident ident | Lident ident ->
      Parser.next p;
      let lident = (Longident.Ldot (acc, ident)) in
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        loop p lident
      | _ -> lident
      end
    | _t -> acc
  in
  match p.Parser.token with
  | Lident ident | Uident ident ->
    Parser.next p;
    begin match p.Parser.token with
    | Dot ->
      Parser.next p;
      loop p (Longident.Lident ident)
    | _ -> Longident.Lident ident
    end
  | _ ->
    Longident.Lident "_"

let verifyJsxOpeningClosingName p nameExpr =
  let closing = match p.Parser.token with
  | Lident lident -> Parser.next p; Longident.Lident lident
  | Uident _ ->
    (parseModuleLongIdent ~lowercase:false p).txt
  | _ -> Longident.Lident ""
  in
  match nameExpr.Parsetree.pexp_desc with
  | Pexp_ident openingIdent ->
    let opening =
      let withoutCreateElement =
        Longident.flatten openingIdent.txt
        |> List.filter (fun s -> s <> "createElement")
      in
      match (Longident.unflatten withoutCreateElement) with
      | Some li -> li
      | None -> Longident.Lident ""
    in
    opening = closing
  | _ -> assert false

let string_of_pexp_ident nameExpr =
  match nameExpr.Parsetree.pexp_desc with
  | Pexp_ident openingIdent ->
    Longident.flatten openingIdent.txt
    |> List.filter (fun s -> s <> "createElement")
    |> String.concat "."
  | _ -> ""

(* open-def ::=
 *   | open module-path
 *   | open! module-path *)
let parseOpenDescription ~attrs p =
  Parser.leaveBreadcrumb p Grammar.OpenDescription;
  let startPos = p.Parser.startPos in
  Parser.expect Open p;
  let override = if Parser.optional p Token.Bang then
    Asttypes.Override
  else
    Asttypes.Fresh
  in
  let modident = parseModuleLongIdent ~lowercase:false p in
  let loc = mkLoc startPos p.prevEndPos in
  Parser.eatBreadcrumb p;
  Ast_helper.Opn.mk ~loc ~attrs ~override modident

let hexValue x =
  match x with
  | '0' .. '9' ->
    (Char.code x) - 48
  | 'A' .. 'Z' ->
    (Char.code x) - 55
  | 'a' .. 'z' ->
    (Char.code x) - 97
  | _ -> 16

let parseStringLiteral s =
  let len = String.length s in
  let b = Buffer.create (String.length s) in

  let rec loop i =
    if i = len then
      ()
    else
      let c = String.unsafe_get s i in
      match c with
      | '\\' as c ->
        let nextIx = i + 1 in
        if nextIx < len then
          let nextChar = String.unsafe_get s nextIx in
          begin match nextChar with
          (* this is interesting:
           * let x = "foo\
           * bar"
           * The `\` escapes the newline, as if there was nothing here.
           * Essentialy transforming this piece of code in `let x = "foobar"`
           *
           * What is even more interesting is that any space or tabs after the
           * escaped newline are also dropped.
           * let x = "foo\
           *          bar"
           * is the same as `let x = "foobar"`
           *)
          | '\010' | '\013' ->
            let i = ref (nextIx + 1) in
            while !i < len && (
              let c = String.unsafe_get s !i in
              c = ' ' || c = '\t'
            ) do
              incr i
            done;
            loop !i
          | 'n' ->
            Buffer.add_char b '\010';
            loop (nextIx + 1)
          | 'r' ->
            Buffer.add_char b '\013';
            loop (nextIx + 1)
          | 'b' ->
            Buffer.add_char b '\008';
            loop (nextIx + 1)
          | 't' ->
            Buffer.add_char b '\009';
            loop (nextIx + 1)
          | '\\' as c ->
            Buffer.add_char b c;
            loop (nextIx + 1)
          | ' ' as c ->
              Buffer.add_char b c;
            loop (nextIx + 1)
          | '\'' as c ->
              Buffer.add_char b c;
            loop (nextIx + 1)
          | '\"' as c ->
            Buffer.add_char b c;
            loop (nextIx + 1)
          | '0' .. '9' ->
            if nextIx + 2 < len then
              let c0 = nextChar in
              let c1 = (String.unsafe_get s (nextIx + 1)) in
              let c2 = (String.unsafe_get s (nextIx + 2)) in
              let c =
                100 * (Char.code c0 - 48) +
                10 * (Char.code c1  - 48) +
                (Char.code c2 - 48)
              in
              if (c < 0 || c > 255) then (
                Buffer.add_char b '\\';
                Buffer.add_char b c0;
                Buffer.add_char b c1;
                Buffer.add_char b c2;
                loop (nextIx + 3)
              ) else (
                Buffer.add_char b (Char.unsafe_chr c);
                loop (nextIx + 3)
              )
            else (
              Buffer.add_char b '\\';
              Buffer.add_char b nextChar;
              loop (nextIx + 1)
            )
          | 'o' ->
            if nextIx + 3 < len then
              let c0 = (String.unsafe_get s (nextIx + 1)) in
              let c1 = (String.unsafe_get s (nextIx + 2)) in
              let c2 = (String.unsafe_get s (nextIx + 3)) in
              let c =
                64 * (Char.code c0 - 48) +
                8 * (Char.code c1  - 48) +
                (Char.code c2 - 48)
              in
              if (c < 0 || c > 255) then (
                Buffer.add_char b '\\';
                Buffer.add_char b '0';
                Buffer.add_char b c0;
                Buffer.add_char b c1;
                Buffer.add_char b c2;
                loop (nextIx + 4)
              ) else (
                Buffer.add_char b (Char.unsafe_chr c);
                loop (nextIx + 4)
              )
            else (
              Buffer.add_char b '\\';
              Buffer.add_char b nextChar;
              loop (nextIx + 1)
            )
          | 'x' as c ->
            if nextIx + 2 < len then
              let c0 = (String.unsafe_get s (nextIx + 1)) in
              let c1 = (String.unsafe_get s (nextIx + 2)) in
              let c = (16 * (hexValue c0)) + (hexValue c1) in
              if (c < 0 || c > 255) then (
                Buffer.add_char b '\\';
                Buffer.add_char b 'x';
                Buffer.add_char b c0;
                Buffer.add_char b c1;
                loop (nextIx + 3)
              ) else (
                Buffer.add_char b (Char.unsafe_chr c);
                loop (nextIx + 3)
              )
            else (
              Buffer.add_char b '\\';
              Buffer.add_char b c;
              loop (nextIx + 2)
            )
          | _ ->
            Buffer.add_char b c;
            Buffer.add_char b nextChar;
            loop (nextIx + 1)
          end
        else (
          Buffer.add_char b c;
          ()
        )
      | c ->
        Buffer.add_char b c;
        loop (i + 1)
    in
    loop 0;
    Buffer.contents b

let parseTemplateStringLiteral s =
  let len = String.length s in
  let b = Buffer.create len in

  let rec loop i =
    if i < len then
      let c = String.unsafe_get s i in
      match c with
      | '\\' as c ->
        if i + 1 < len then
          let nextChar = String.unsafe_get s (i + 1) in
          begin match nextChar with
          | '\\' as c ->
            Buffer.add_char b c;
            loop (i + 2)
          | '$' as c ->
            Buffer.add_char b c;
            loop (i + 2)
          | '`' as c ->
            Buffer.add_char b c;
            loop (i + 2)
          | c ->
            Buffer.add_char b '\\';
            Buffer.add_char b c;
            loop (i + 2)
          end
        else (
          Buffer.add_char b c
        )

      | c ->
        Buffer.add_char b c;
        loop (i + 1)

    else
      ()
  in
  loop 0;
  Buffer.contents b

(* constant	::=	integer-literal   *)
 (* ∣	 float-literal   *)
 (* ∣	 string-literal   *)
let parseConstant p =
  let isNegative = match p.Parser.token with
  | Token.Minus -> Parser.next p; true
  | Plus -> Parser.next p; false
  | _ -> false
  in
  let constant = match p.Parser.token with
  | Int {i; suffix} ->
    let intTxt = if isNegative then "-" ^ i else i in
    Parsetree.Pconst_integer (intTxt, suffix)
  | Float {f; suffix} ->
    let floatTxt = if isNegative then "-" ^ f else f in
    Parsetree.Pconst_float (floatTxt, suffix)
  | String s ->
    let txt = if p.mode = ParseForTypeChecker then
      parseStringLiteral s
    else
      s
    in
    Pconst_string(txt, None)
  | Character c -> Pconst_char c
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    Pconst_string("", None)
  in
  Parser.next p;
  constant

let parseCommaDelimitedRegion p ~grammar ~closing ~f =
  Parser.leaveBreadcrumb p grammar;
  let rec loop nodes =
    match f p with
    | Some node ->
      begin match p.Parser.token with
      | Comma ->
        Parser.next p;
        loop (node::nodes)
      | token when token = closing || token = Eof ->
        List.rev (node::nodes)
      | _ ->
        if not (p.token = Eof || p.token = closing || Recover.shouldAbortListParse p) then
          Parser.expect Comma p;
        if p.token = Semicolon then Parser.next p;
        loop (node::nodes)
      end
    | None ->
      if p.token = Eof || p.token = closing || Recover.shouldAbortListParse p then
        List.rev nodes
      else (
        Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
        Parser.next p;
        loop nodes
      );
  in
  let nodes = loop [] in
  Parser.eatBreadcrumb p;
  nodes

let parseCommaDelimitedReversedList p ~grammar ~closing ~f =
  Parser.leaveBreadcrumb p grammar;
  let rec loop nodes =
    match f p with
    | Some node ->
      begin match p.Parser.token with
      | Comma ->
        Parser.next p;
        loop (node::nodes)
      | token when token = closing || token = Eof ->
        (node::nodes)
      | _ ->
        if not (p.token = Eof || p.token = closing || Recover.shouldAbortListParse p) then
          Parser.expect Comma p;
        if p.token = Semicolon then Parser.next p;
        loop (node::nodes)
      end
    | None ->
      if p.token = Eof || p.token = closing || Recover.shouldAbortListParse p then
        nodes
      else (
        Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
        Parser.next p;
        loop nodes
      );
  in
  let nodes = loop [] in
  Parser.eatBreadcrumb p;
  nodes

let parseDelimitedRegion p ~grammar ~closing ~f =
  Parser.leaveBreadcrumb p grammar;
  let rec loop nodes =
    match f p with
    | Some node ->
      loop (node::nodes)
    | None ->
      if (
        p.Parser.token = Token.Eof ||
        p.token = closing ||
        Recover.shouldAbortListParse p
      ) then
        List.rev nodes
      else (
        Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
        Parser.next p;
        loop nodes
      )
    in
    let nodes = loop [] in
    Parser.eatBreadcrumb p;
    nodes

let parseRegion p ~grammar ~f =
  Parser.leaveBreadcrumb p grammar;
  let rec loop nodes =
    match f p with
    | Some node ->
      loop (node::nodes)
    | None ->
      if p.Parser.token = Token.Eof || Recover.shouldAbortListParse p then
        List.rev nodes
      else (
        Parser.err p (Diagnostics.unexpected p.token p.breadcrumbs);
        Parser.next p;
        loop nodes
      )
  in
  let nodes = loop [] in
  Parser.eatBreadcrumb p;
  nodes

(* let-binding	::=	pattern =  expr   *)
   (* ∣	 value-name  { parameter }  [: typexpr]  [:> typexpr] =  expr   *)
   (* ∣	 value-name :  poly-typexpr =  expr   *)

 (* pattern	::=	value-name   *)
   (* ∣	 _   *)
   (* ∣	 constant   *)
   (* ∣	 pattern as  value-name   *)
   (* ∣	 ( pattern )   *)
   (* ∣	 ( pattern :  typexpr )   *)
   (* ∣	 pattern |  pattern   *)
   (* ∣	 constr  pattern   *)
   (* ∣	 #variant variant-pattern *)
   (* ∣	 #...type  *)
   (* ∣	 / pattern  { , pattern }+  /   *)
   (* ∣	 { field  [: typexpr]  [= pattern] { ; field  [: typexpr]  [= pattern] }  [; _ ] [ ; ] }   *)
   (* ∣	 [ pattern  { ; pattern }  [ ; ] ]   *)
   (* ∣	 pattern ::  pattern   *)
   (* ∣	 [| pattern  { ; pattern }  [ ; ] |]   *)
   (* ∣	 char-literal ..  char-literal *)
   (*	∣	 exception pattern  *)
let rec parsePattern ?(alias=true) ?(or_=true) p =
  let startPos = p.Parser.startPos in
  let attrs = parseAttributes p in
  let pat = match p.Parser.token with
  | (True | False) as token ->
    let endPos = p.endPos in
    Parser.next p;
    let loc = mkLoc startPos endPos in
    Ast_helper.Pat.construct ~loc
      (Location.mkloc (Longident.Lident (Token.toString token)) loc) None
  | Int _ | String _ | Float _ | Character _ | Minus | Plus ->
    let c = parseConstant p in
     begin match p.token with
      | DotDot ->
        Parser.next p;
        let c2 = parseConstant p in
        Ast_helper.Pat.interval ~loc:(mkLoc startPos p.prevEndPos) c c2
      | _ ->
        Ast_helper.Pat.constant ~loc:(mkLoc startPos p.prevEndPos) c
    end
  | Lparen ->
    Parser.next p;
    begin match p.token with
    | Rparen ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      let lid = Location.mkloc (Longident.Lident "()") loc in
      Ast_helper.Pat.construct ~loc lid None
    | _ ->
      let pat = parseConstrainedPattern p in
      begin match p.token with
      | Comma ->
        Parser.next p;
        parseTuplePattern ~attrs ~first:pat ~startPos p
      | _ ->
        Parser.expect Rparen p;
        let loc = mkLoc startPos p.prevEndPos in
        {pat with ppat_loc = loc}
      end
    end
  | Lbracket ->
    parseArrayPattern ~attrs p
  | Lbrace ->
    parseRecordPattern ~attrs p
  | Underscore ->
    let endPos = p.endPos in
    let loc = mkLoc startPos endPos in
    Parser.next p;
    Ast_helper.Pat.any ~loc ~attrs ()
  | Lident ident ->
    let endPos = p.endPos in
    let loc = mkLoc startPos endPos in
    Parser.next p;
    Ast_helper.Pat.var ~loc ~attrs (Location.mkloc ident loc)
  | Uident _ ->
    let constr = parseModuleLongIdent ~lowercase:false p in
    begin match p.Parser.token with
    | Lparen ->
      parseConstructorPatternArgs p constr startPos attrs
    | _ ->
      Ast_helper.Pat.construct ~loc:constr.loc ~attrs constr None
    end
  | Hash ->
    Parser.next p;
    if p.Parser.token == DotDotDot then (
      Parser.next p;
      let ident = parseValuePath p in
      let loc = mkLoc startPos ident.loc.loc_end in
      Ast_helper.Pat.type_ ~loc ~attrs ident
    ) else (
      let (ident, loc) = parseIdent ~msg:ErrorMessages.variantIdent ~startPos p in
      begin match p.Parser.token with
      | Lparen ->
        parseVariantPatternArgs p ident startPos attrs
      | _ ->
        Ast_helper.Pat.variant ~loc ~attrs ident None
      end
    )
  | Exception ->
    Parser.next p;
    let pat = parsePattern ~alias:false ~or_:false p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.exception_ ~loc ~attrs pat
  | Lazy ->
    Parser.next p;
    let pat = parsePattern ~alias:false ~or_:false p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.lazy_ ~loc ~attrs pat
  | List ->
    Parser.next p;
    parseListPattern ~startPos ~attrs p
  | Module ->
    parseModulePattern ~attrs p
  | Percent ->
    let extension = parseExtension p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.extension ~loc ~attrs extension
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    begin match skipTokensAndMaybeRetry p ~isStartOfGrammar:Grammar.isAtomicPatternStart with
    | None ->
      Recover.defaultPattern()
    | Some () ->
      parsePattern p
    end
  in
  let pat = if alias then parseAliasPattern ~attrs pat p else pat in
  if or_ then parseOrPattern pat p else pat

and skipTokensAndMaybeRetry p ~isStartOfGrammar =
  if Token.isKeyword p.Parser.token
      && p.Parser.prevEndPos.pos_lnum == p.startPos.pos_lnum
  then (
    Parser.next p;
    None
  ) else (
    if Recover.shouldAbortListParse p then
      begin
        if isStartOfGrammar p.Parser.token then
          begin
            Parser.next p;
            Some ()
          end
        else
          None
      end
    else
      begin
        Parser.next p;
        let rec loop p =
          if not (Recover.shouldAbortListParse p)
          then begin
            Parser.next p;
            loop p
          end in
        loop p;
        if isStartOfGrammar p.Parser.token then
          Some ()
        else
          None
      end
  )

(* alias ::= pattern as lident *)
and parseAliasPattern ~attrs pattern p =
  match p.Parser.token with
  | As ->
    Parser.next p;
    let (name, loc) = parseLident p in
    let name = Location.mkloc name loc in
    Ast_helper.Pat.alias
      ~loc:({pattern.ppat_loc with loc_end = p.prevEndPos})
      ~attrs
       pattern
       name
  | _ -> pattern

(* or ::= pattern | pattern
 * precedence: Red | Blue | Green is interpreted as (Red | Blue) | Green *)
and parseOrPattern pattern1 p =
  let rec loop pattern1 =
    match p.Parser.token with
    | Bar ->
      Parser.next p;
      let pattern2 = parsePattern ~or_:false p in
      let loc = { pattern1.Parsetree.ppat_loc with
        loc_end = pattern2.ppat_loc.loc_end
      } in
      loop (Ast_helper.Pat.or_ ~loc pattern1 pattern2)
    | _ -> pattern1
  in
  loop pattern1

and parseNonSpreadPattern ~msg p =
  let () = match p.Parser.token with
  | DotDotDot ->
    Parser.err p (Diagnostics.message msg);
    Parser.next p;
  | _ -> ()
  in
  match p.Parser.token with
  | token when Grammar.isPatternStart token ->
    let pat = parsePattern p in
    begin match p.Parser.token with
    | Colon ->
      Parser.next p;
      let typ = parseTypExpr p in
      let loc = mkLoc pat.ppat_loc.loc_start typ.Parsetree.ptyp_loc.loc_end in
      Some (Ast_helper.Pat.constraint_ ~loc pat typ)
    | _ -> Some pat
    end
  | _ -> None

and parseConstrainedPattern p =
  let pat = parsePattern p in
  match p.Parser.token with
  | Colon ->
    Parser.next p;
    let typ = parseTypExpr p in
    let loc = mkLoc pat.ppat_loc.loc_start typ.Parsetree.ptyp_loc.loc_end in
    Ast_helper.Pat.constraint_ ~loc pat typ
  | _ -> pat

and parseConstrainedPatternRegion p =
  match p.Parser.token with
  | token when Grammar.isPatternStart token ->
    Some (parseConstrainedPattern p)
  | _ -> None

(* field ::=
 *   | longident
 *   | longident : pattern
 *   | longident as lident
 *
 *  row ::=
 *	 | field ,
 *	 | field , _
 *	 | field , _,
 *)
and parseRecordPatternField p =
  let label = parseValuePath p in
  let pattern = match p.Parser.token with
  | Colon ->
    Parser.next p;
    parsePattern p
  | _ ->
    Ast_helper.Pat.var
      ~loc:label.loc
      (Location.mkloc (Longident.last label.txt) label.loc)
  in
  match p.token with
  | As -> (* {bar as baz} -> {bar: baz} *)
    Parser.next p;
    let (name, loc) = parseLident p in
    let var = Location.mkloc name loc in
    let newNameOfLabel = Ast_helper.Pat.var ~loc var in
    (label, newNameOfLabel)
  | _ ->
    (label, pattern)

 (* TODO: there are better representations than PatField|Underscore ? *)
and parseRecordPatternItem p =
  match p.Parser.token with
  | DotDotDot ->
    Parser.next p;
    Some (true, PatField (parseRecordPatternField p))
  | Uident _ | Lident _ ->
    Some (false, PatField (parseRecordPatternField p))
  | Underscore ->
    Parser.next p;
    Some (false, PatUnderscore)
  | _ ->
    None

and parseRecordPattern ~attrs p =
  let startPos = p.startPos in
  Parser.expect Lbrace p;
  let rawFields =
    parseCommaDelimitedReversedList p
     ~grammar:PatternRecord
     ~closing:Rbrace
     ~f:parseRecordPatternItem
  in
  Parser.expect Rbrace p;
  let (fields, closedFlag) =
    let (rawFields, flag) = match rawFields with
    | (_hasSpread, PatUnderscore)::rest ->
      (rest, Asttypes.Open)
    | rawFields ->
      (rawFields, Asttypes.Closed)
    in
    List.fold_left (fun (fields, flag) curr ->
      let (hasSpread, field) = curr in
      match field with
      | PatField field ->
        if hasSpread then (
          let (_, pattern) = field in
          Parser.err ~startPos:pattern.Parsetree.ppat_loc.loc_start p (Diagnostics.message ErrorMessages.recordPatternSpread)
        );
        (field::fields, flag)
      | PatUnderscore ->
        (fields, flag)
    ) ([], flag) rawFields
  in
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Pat.record ~loc ~attrs fields closedFlag

and parseTuplePattern ~attrs ~first ~startPos p =
  let patterns =
    parseCommaDelimitedRegion p
      ~grammar:Grammar.PatternList
      ~closing:Rparen
      ~f:parseConstrainedPatternRegion
  in
  Parser.expect Rparen p;
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Pat.tuple ~loc ~attrs (first::patterns)

and parsePatternRegion p =
  match p.Parser.token with
  | DotDotDot ->
    Parser.next p;
    Some (true, parseConstrainedPattern p)
  | token when Grammar.isPatternStart token ->
    Some (false, parseConstrainedPattern p)
  | _ -> None

and parseModulePattern ~attrs p =
  let startPos = p.Parser.startPos in
  Parser.expect Module p;
  Parser.expect Lparen p;
  let uident = match p.token with
  | Uident uident ->
    let loc = mkLoc p.startPos p.endPos in
    Parser.next p;
    Location.mkloc uident loc
  | _ -> (* TODO: error recovery *)
    Location.mknoloc "_"
  in
  begin match p.token with
  | Colon ->
    let colonStart = p.Parser.startPos in
    Parser.next p;
    let packageTypAttrs = parseAttributes p in
    let packageType = parsePackageType ~startPos:colonStart ~attrs:packageTypAttrs p in
    Parser.expect Rparen p;
    let loc = mkLoc startPos p.prevEndPos in
    let unpack = Ast_helper.Pat.unpack ~loc:uident.loc uident in
    Ast_helper.Pat.constraint_
      ~loc
      ~attrs
      unpack
      packageType
  | _ ->
    Parser.expect Rparen p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Pat.unpack ~loc ~attrs uident
  end

and parseListPattern ~startPos ~attrs p =
  let listPatterns =
    parseCommaDelimitedReversedList p
      ~grammar:Grammar.PatternOcamlList
      ~closing:Rbrace
      ~f:parsePatternRegion
  in
  Parser.expect Rbrace p;
  let loc = mkLoc startPos p.prevEndPos in
  let filterSpread (hasSpread, pattern) =
    if hasSpread then (
      Parser.err
        ~startPos:pattern.Parsetree.ppat_loc.loc_start
        p
        (Diagnostics.message ErrorMessages.listPatternSpread);
      pattern
    ) else
      pattern
  in
  match listPatterns with
  | (true, pattern)::patterns ->
    let patterns = patterns |> List.map filterSpread |> List.rev in
    let pat = makeListPattern loc patterns (Some pattern) in
    {pat with ppat_loc = loc; ppat_attributes = attrs;}
  | patterns ->
    let patterns = patterns |> List.map filterSpread |> List.rev in
    let pat = makeListPattern loc patterns None in
    {pat with ppat_loc = loc; ppat_attributes = attrs;}

and parseArrayPattern ~attrs p =
  let startPos = p.startPos in
  Parser.expect Lbracket p;
  let patterns =
    parseCommaDelimitedRegion
      p
      ~grammar:Grammar.PatternList
      ~closing:Rbracket
      ~f:(parseNonSpreadPattern ~msg:ErrorMessages.arrayPatternSpread)
  in
  Parser.expect Rbracket p;
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Pat.array ~loc ~attrs patterns

and parseConstructorPatternArgs p constr startPos attrs =
  let lparen = p.startPos in
  Parser.expect Lparen p;
  let args = parseCommaDelimitedRegion
    p ~grammar:Grammar.PatternList ~closing:Rparen ~f:parseConstrainedPatternRegion
  in
  Parser.expect Rparen p;
  let args = match args with
  | [] ->
    let loc = mkLoc lparen p.prevEndPos in
    Some (
      Ast_helper.Pat.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
    )
  | [{ppat_desc = Ppat_tuple _} as pat] as patterns ->
    if p.mode = ParseForTypeChecker then
      (* Some(1, 2) for type-checker *)
      Some pat
    else
    (* Some((1, 2)) for printer *)
      Some (Ast_helper.Pat.tuple ~loc:(mkLoc lparen p.endPos) patterns)
  | [pattern] -> Some pattern
  | patterns ->
    Some (Ast_helper.Pat.tuple ~loc:(mkLoc lparen p.endPos) patterns)
  in
  Ast_helper.Pat.construct ~loc:(mkLoc startPos p.prevEndPos) ~attrs constr args

and parseVariantPatternArgs p ident startPos attrs =
  let lparen = p.startPos in
  Parser.expect Lparen p;
  let patterns =
    parseCommaDelimitedRegion
      p ~grammar:Grammar.PatternList ~closing:Rparen ~f:parseConstrainedPatternRegion in
  let args =
    match patterns with
    | [{ppat_desc = Ppat_tuple _} as pat] as patterns ->
      if p.mode = ParseForTypeChecker then
        (* #ident(1, 2) for type-checker *)
        Some pat
      else
        (* #ident((1, 2)) for printer *)
        Some (Ast_helper.Pat.tuple ~loc:(mkLoc lparen p.endPos) patterns)
    | [pattern] -> Some pattern
    | patterns ->
      Some (Ast_helper.Pat.tuple ~loc:(mkLoc lparen p.endPos) patterns)
  in
  Parser.expect Rparen p;
  Ast_helper.Pat.variant ~loc:(mkLoc startPos p.prevEndPos) ~attrs ident args

and parseExpr ?(context=OrdinaryExpr) p =
  let expr = parseOperandExpr ~context p in
  let expr = parseBinaryExpr ~context ~a:expr p 1 in
  parseTernaryExpr expr p

(* expr ? expr : expr *)
and parseTernaryExpr leftOperand p =
  match p.Parser.token with
  | Question ->
    Parser.leaveBreadcrumb p Grammar.Ternary;
    Parser.next p;
    let trueBranch = parseExpr ~context:TernaryTrueBranchExpr p in
    Parser.expect Colon p;
    let falseBranch = parseExpr p in
    Parser.eatBreadcrumb p;
    let loc = {leftOperand.Parsetree.pexp_loc with
      loc_start = leftOperand.pexp_loc.loc_start;
      loc_end = falseBranch.Parsetree.pexp_loc.loc_end;
    } in
    Ast_helper.Exp.ifthenelse
      ~attrs:[ternaryAttr] ~loc
      leftOperand trueBranch (Some falseBranch)
  | _ ->
    leftOperand

and parseEs6ArrowExpression ?parameters p =
  let startPos = p.Parser.startPos in
  Parser.leaveBreadcrumb p Grammar.Es6ArrowExpr;
  let parameters = match parameters with
  | Some params -> params
  | None -> parseParameters p
  in
  let returnType = match p.Parser.token with
  | Colon ->
    Parser.next p;
    Some (parseTypExpr ~es6Arrow:false p)
  | _ ->
    None
  in
  Parser.expect EqualGreater p;
  let body =
    let expr = parseExpr p in
    match returnType with
    | Some typ ->
      Ast_helper.Exp.constraint_
        ~loc:(mkLoc expr.pexp_loc.loc_start typ.Parsetree.ptyp_loc.loc_end) expr typ
    | None -> expr
  in
  Parser.eatBreadcrumb p;
  let endPos = p.prevEndPos in
  let arrowExpr =
    List.fold_right (fun parameter expr ->
      match parameter with
      | TermParameter {uncurried; attrs; label = lbl; expr = defaultExpr; pat; pos = startPos} ->
        let attrs = if uncurried then uncurryAttr::attrs else attrs in
        Ast_helper.Exp.fun_ ~loc:(mkLoc startPos endPos) ~attrs lbl defaultExpr pat expr
      | TypeParameter {uncurried; attrs; locs = newtypes; pos = startPos} ->
        let attrs = if uncurried then uncurryAttr::attrs else attrs in
        makeNewtypes ~attrs ~loc:(mkLoc startPos endPos) newtypes expr
    ) parameters body
  in
  {arrowExpr with pexp_loc = {arrowExpr.pexp_loc with loc_start = startPos}}

(*
 * uncurried_parameter ::=
 *   | . parameter
 *
 * parameter ::=
 *   | pattern
 *   | pattern : type
 *   | ~ labelName
 *   | ~ labelName as pattern
 *   | ~ labelName as pattern : type
 *   | ~ labelName = expr
 *   | ~ labelName as pattern = expr
 *   | ~ labelName as pattern : type = expr
 *   | ~ labelName = ?
 *   | ~ labelName as pattern = ?
 *   | ~ labelName as pattern : type = ?
 *
 * labelName ::= lident
 *)
and parseParameter p =
  if (
    p.Parser.token = Token.Typ ||
    p.token = Tilde ||
    p.token = Dot ||
    Grammar.isPatternStart p.token
  ) then (
    let startPos = p.Parser.startPos in
    let uncurried = Parser.optional p Token.Dot in
    (* two scenarios:
     *   attrs ~lbl ...
     *   attrs pattern
     * Attributes before a labelled arg, indicate that it's on the whole arrow expr
     * Otherwise it's part of the pattern
     *  *)
    let attrs = parseAttributes p in
    if p.Parser.token = Typ then (
      Parser.next p;
      let lidents = parseLidentList p in
      Some (TypeParameter {uncurried; attrs; locs = lidents; pos = startPos})
    ) else (
    let (attrs, lbl, pat) = match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let (lblName, loc) = parseLident p in
      let propLocAttr = (Location.mkloc "ns.namedArgLoc" loc, Parsetree.PStr []) in
      begin match p.Parser.token with
      | Comma | Equal | Rparen ->
        let loc = mkLoc startPos p.prevEndPos in
        (
          attrs,
          Asttypes.Labelled lblName,
          Ast_helper.Pat.var ~attrs:[propLocAttr] ~loc (Location.mkloc lblName loc)
        )
      | Colon ->
        let lblEnd = p.prevEndPos in
        Parser.next p;
        let typ = parseTypExpr p in
        let loc = mkLoc startPos lblEnd in
        let pat =
          let pat = Ast_helper.Pat.var ~loc (Location.mkloc lblName loc) in
          let loc = mkLoc startPos p.prevEndPos in
          Ast_helper.Pat.constraint_ ~attrs:[propLocAttr] ~loc pat typ in
        (attrs, Asttypes.Labelled lblName, pat)
      | As ->
        Parser.next p;
        let pat =
          let pat = parseConstrainedPattern p in
          {pat with ppat_attributes = propLocAttr::pat.ppat_attributes}
        in
        (attrs, Asttypes.Labelled lblName, pat)
      | t ->
        Parser.err p (Diagnostics.unexpected t p.breadcrumbs);
        let loc = mkLoc startPos p.prevEndPos in
        (
          attrs,
          Asttypes.Labelled lblName,
          Ast_helper.Pat.var ~loc (Location.mkloc lblName loc)
        )
      end
    | _ ->
      let pattern = parseConstrainedPattern p in
      let attrs = List.concat [attrs; pattern.ppat_attributes] in
      ([], Asttypes.Nolabel, {pattern with ppat_attributes = attrs})
    in
    match p.Parser.token with
    | Equal ->
      Parser.next p;
      let lbl = match lbl with
      | Asttypes.Labelled lblName -> Asttypes.Optional lblName
      | Asttypes.Optional _ as lbl -> lbl
      | Asttypes.Nolabel -> Asttypes.Nolabel
      in
      begin match p.Parser.token with
      | Question ->
        Parser.next p;
        Some (TermParameter {uncurried; attrs; label = lbl; expr = None; pat; pos = startPos})
      | _ ->
        let expr = parseConstrainedOrCoercedExpr p in
        Some (TermParameter {uncurried; attrs; label = lbl; expr = Some expr; pat; pos = startPos})
      end
    | _ ->
      Some (TermParameter {uncurried; attrs; label = lbl; expr = None; pat; pos = startPos})
  )
  ) else None

and parseParameterList p =
  let parameters =
    parseCommaDelimitedRegion
      ~grammar:Grammar.ParameterList
      ~f:parseParameter
      ~closing:Rparen
      p
  in
  Parser.expect Rparen p;
  parameters

(* parameters ::=
 *   | _
 *   | lident
 *   | ()
 *   | (.)
 *   | ( parameter {, parameter} [,] )
 *)
and parseParameters p =
  let startPos = p.Parser.startPos in
  match p.Parser.token with
  | Lident ident ->
    Parser.next p;
    let loc = mkLoc startPos p.Parser.prevEndPos in
    [TermParameter {
      uncurried = false;
      attrs = [];
      label = Asttypes.Nolabel;
      expr = None;
      pat = Ast_helper.Pat.var ~loc (Location.mkloc ident loc);
      pos = startPos;
    }]
  | Underscore ->
    Parser.next p;
    let loc = mkLoc startPos p.Parser.prevEndPos in
    [TermParameter {uncurried = false; attrs = []; label = Asttypes.Nolabel; expr = None; pat = Ast_helper.Pat.any ~loc (); pos = startPos}]
  | Lparen ->
    Parser.next p;
    begin match p.Parser.token with
    | Rparen ->
      Parser.next p;
      let loc = mkLoc startPos p.Parser.prevEndPos in
      let unitPattern = Ast_helper.Pat.construct
        ~loc (Location.mkloc (Longident.Lident "()") loc) None
      in
      [TermParameter {uncurried = false; attrs = []; label = Asttypes.Nolabel; expr = None; pat = unitPattern; pos = startPos}]
    | Dot ->
      Parser.next p;
      begin match p.token with
      | Rparen ->
        Parser.next p;
        let loc = mkLoc startPos p.Parser.prevEndPos in
        let unitPattern = Ast_helper.Pat.construct
          ~loc (Location.mkloc (Longident.Lident "()") loc) None
        in
        [TermParameter {uncurried = true; attrs = []; label = Asttypes.Nolabel; expr = None; pat = unitPattern; pos = startPos}]
      | _ ->
        begin match parseParameterList p with
        | (TermParameter {attrs; label = lbl; expr = defaultExpr; pat = pattern; pos = startPos})::rest ->
          (TermParameter {uncurried = true; attrs; label = lbl; expr = defaultExpr; pat = pattern; pos = startPos})::rest
        | parameters -> parameters
        end
      end
    | _ -> parseParameterList p
    end
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    []

and parseCoercedExpr ~(expr: Parsetree.expression) p =
  Parser.expect ColonGreaterThan p;
  let typ = parseTypExpr p in
  let loc = mkLoc expr.pexp_loc.loc_start p.prevEndPos in
  Ast_helper.Exp.coerce ~loc expr None typ

and parseConstrainedOrCoercedExpr p =
  let expr = parseExpr p in
  match p.Parser.token with
  | ColonGreaterThan ->
    parseCoercedExpr ~expr p
  | Colon ->
    Parser.next p;
    begin match p.token with
    | _ ->
      let typ = parseTypExpr p in
      let loc = mkLoc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
      let expr = Ast_helper.Exp.constraint_ ~loc expr typ in
      begin match p.token with
        | ColonGreaterThan ->
          parseCoercedExpr ~expr p
        | _ ->
          expr
      end
    end
  | _ -> expr


and parseConstrainedExprRegion p =
  match p.Parser.token with
  | token when Grammar.isExprStart token ->
    let expr = parseExpr p in
    begin match p.Parser.token with
    | Colon ->
      Parser.next p;
      let typ = parseTypExpr p in
      let loc = mkLoc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
      Some (Ast_helper.Exp.constraint_ ~loc expr typ)
    | _ -> Some expr
    end
  | _ -> None

(* Atomic expressions represent unambiguous expressions.
 * This means that regardless of the context, these expressions
 * are always interpreted correctly. *)
and parseAtomicExpr p =
  Parser.leaveBreadcrumb p Grammar.ExprOperand;
  let startPos = p.Parser.startPos in
  let expr = match p.Parser.token with
    | (True | False) as token ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.construct ~loc
        (Location.mkloc (Longident.Lident (Token.toString token)) loc) None
    | Int _ | String _ | Float _ | Character _ ->
      let c = parseConstant p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.constant ~loc c
    | Backtick ->
      let expr = parseTemplateExpr p in
      {expr with pexp_loc = mkLoc startPos p.prevEndPos}
    | Uident _ | Lident _ ->
      parseValueOrConstructor p
    | Hash ->
      parsePolyVariantExpr p
    | Lparen ->
      Parser.next p;
      begin match p.Parser.token with
      | Rparen ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Exp.construct
          ~loc (Location.mkloc (Longident.Lident "()") loc) None
      | _t ->
        let expr = parseConstrainedOrCoercedExpr p in
        begin match p.token with
        | Comma ->
          Parser.next p;
          parseTupleExpr ~startPos ~first:expr p
        | _ ->
          Parser.expect Rparen p;
          expr
          (* {expr with pexp_loc = mkLoc startPos p.prevEndPos}
           * What does this location mean here? It means that when there's
           * a parenthesized we keep the location here for whitespace interleaving.
           * Without the closing paren in the location there will always be an extra
           * line. For now we don't include it, because it does weird things
           * with for comments. *)
        end
      end
    | List ->
      Parser.next p;
      parseListExpr ~startPos  p
    | Module ->
      Parser.next p;
      parseFirstClassModuleExpr ~startPos p
    | Lbracket ->
      parseArrayExp p
    | Lbrace ->
      parseBracedOrRecordExpr  p
    | LessThan ->
      parseJsx p
    | Percent ->
      let extension = parseExtension p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.extension ~loc extension
    | Underscore as token ->
      (* This case is for error recovery. Not sure if it's the correct place *)
      Parser.err p (Diagnostics.lident token);
      Parser.next p;
      Recover.defaultExpr ()
    | token ->
      let errPos = p.prevEndPos in
      begin match skipTokensAndMaybeRetry p ~isStartOfGrammar:Grammar.isAtomicExprStart with
      | None ->
        Parser.err ~startPos:errPos p (Diagnostics.unexpected token p.breadcrumbs);
        Recover.defaultExpr ()
      | Some () -> parseAtomicExpr p
      end
  in
  Parser.eatBreadcrumb p;
  expr

(* module(module-expr)
 * module(module-expr : package-type) *)
and parseFirstClassModuleExpr ~startPos p =
  Parser.expect Lparen p;

  let modExpr = parseModuleExpr p in
  let modEndLoc = p.prevEndPos in
  begin match p.Parser.token with
  | Colon ->
    let colonStart = p.Parser.startPos in
    Parser.next p;
    let attrs = parseAttributes p in
    let packageType = parsePackageType ~startPos:colonStart ~attrs p in
    Parser.expect Rparen p;
    let loc = mkLoc startPos modEndLoc in
    let firstClassModule = Ast_helper.Exp.pack ~loc modExpr in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.constraint_ ~loc firstClassModule packageType
  | _ ->
    Parser.expect Rparen p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.pack ~loc modExpr
  end

and parseBracketAccess p expr startPos =
  Parser.leaveBreadcrumb p Grammar.ExprArrayAccess;
  let lbracket = p.startPos in
  Parser.next p;
  let stringStart = p.startPos in
  match p.Parser.token with
  | String s ->
    Parser.next p;
    let stringEnd = p.prevEndPos in
    Parser.expect Rbracket p;
    let rbracket = p.prevEndPos in
    let e =
      let identLoc = mkLoc stringStart stringEnd in
      let loc = mkLoc lbracket rbracket in
      Ast_helper.Exp.apply ~loc
      (Ast_helper.Exp.ident ~loc (Location.mkloc (Longident.Lident "##") loc))
      [Nolabel, expr; Nolabel, (Ast_helper.Exp.ident ~loc:identLoc (Location.mkloc (Longident.Lident s) identLoc))]
    in
    let e = parsePrimaryExpr ~operand:e p in
    let equalStart = p.startPos in
    begin match p.token with
    | Equal ->
      Parser.next p;
      let equalEnd = p.prevEndPos in
      let rhsExpr = parseExpr p in
      let loc = mkLoc startPos rhsExpr.pexp_loc.loc_end in
      let operatorLoc = mkLoc equalStart equalEnd in
      Ast_helper.Exp.apply ~loc
        (Ast_helper.Exp.ident ~loc:operatorLoc (Location.mkloc (Longident.Lident "#=") operatorLoc))
        [Nolabel, e; Nolabel, rhsExpr]
    | _ -> e
    end
  | _ ->
    let accessExpr = parseConstrainedOrCoercedExpr p in
    Parser.expect Rbracket p;
    let rbracket = p.prevEndPos in
    let arrayLoc = mkLoc lbracket rbracket in
    begin match p.token with
    | Equal ->
      Parser.leaveBreadcrumb p ExprArrayMutation;
      Parser.next p;
      let rhsExpr = parseExpr p in
      let arraySet = Location.mkloc
        (Longident.Ldot(Lident "Array", "set"))
        arrayLoc
      in
      let endPos = p.prevEndPos in
      let arraySet = Ast_helper.Exp.apply
        ~loc:(mkLoc startPos endPos)
        (Ast_helper.Exp.ident ~loc:arrayLoc arraySet)
        [Nolabel, expr; Nolabel, accessExpr; Nolabel, rhsExpr]
      in
      Parser.eatBreadcrumb p;
      arraySet
    | _ ->
      let endPos = p.prevEndPos in
      let e =
        Ast_helper.Exp.apply
          ~loc:(mkLoc startPos endPos)
          (Ast_helper.Exp.ident
            ~loc:arrayLoc
            (Location.mkloc (Longident.Ldot(Lident "Array", "get")) arrayLoc)
            )
          [Nolabel, expr; Nolabel, accessExpr]
      in
      Parser.eatBreadcrumb p;
      parsePrimaryExpr ~operand:e p
    end

(* * A primary expression represents
 *  - atomic-expr
 *  - john.age
 *  - array[0]
 *  - applyFunctionTo(arg1, arg2)
 *
 *  The "operand" represents the expression that is operated on
 *)
and parsePrimaryExpr ~operand ?(noCall=false) p =
  let startPos = operand.pexp_loc.loc_start in
  let rec loop p expr =
    match p.Parser.token with
    | Dot ->
      Parser.next p;
      let lident = parseValuePath p in
      begin match p.Parser.token with
      | Equal when noCall = false ->
        Parser.leaveBreadcrumb p Grammar.ExprSetField;
        Parser.next p;
        let targetExpr = parseExpr p in
        let loc = mkLoc startPos p.prevEndPos in
        let setfield = Ast_helper.Exp.setfield ~loc expr lident targetExpr  in
        Parser.eatBreadcrumb p;
        setfield
      | _ ->
        let endPos = p.prevEndPos in
        let loc = mkLoc startPos endPos in
        loop p (Ast_helper.Exp.field ~loc expr lident)
      end
    | Lbracket when noCall = false && p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
      parseBracketAccess p expr startPos
    | Lparen when noCall = false && p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
      loop p (parseCallExpr p expr)
    | Backtick when noCall = false && p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
      begin match expr.pexp_desc with
      | Pexp_ident {txt = Longident.Lident ident} ->
        parseTemplateExpr ~prefix:ident p
      | _ ->
        Parser.err
          ~startPos:expr.pexp_loc.loc_start
          ~endPos:expr.pexp_loc.loc_end
          p
          (Diagnostics.message "Tagged template literals are currently restricted to identifiers like: json`null`.");
        parseTemplateExpr p
      end
    | _ -> expr
  in
  loop p operand

(* a unary expression is an expression with only one operand and
 * unary operator. Examples:
 *   -1
 *   !condition
 *   -. 1.6
 *)
and parseUnaryExpr p =
  let startPos = p.Parser.startPos in
  match p.Parser.token with
  | (Minus | MinusDot | Plus | PlusDot | Bang) as token ->
    Parser.leaveBreadcrumb p Grammar.ExprUnary;
    let tokenEnd = p.endPos in
    Parser.next p;
    let operand = parseUnaryExpr p in
    let unaryExpr = makeUnaryExpr startPos tokenEnd token operand  in
    Parser.eatBreadcrumb p;
    unaryExpr
  | _ ->
    parsePrimaryExpr ~operand:(parseAtomicExpr p) p

(* Represents an "operand" in a binary expression.
 * If you have `a + b`, `a` and `b` both represent
 * the operands of the binary expression with opeartor `+` *)
and parseOperandExpr ~context p =
  let startPos = p.Parser.startPos in
  let attrs = parseAttributes p in
  let expr = match p.Parser.token with
  | Assert ->
    Parser.next p;
    let expr = parseUnaryExpr p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.assert_ ~loc expr
  | Lazy ->
    Parser.next p;
    let expr = parseUnaryExpr p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.lazy_ ~loc expr
  | Try ->
    parseTryExpression p
  | If ->
    parseIfOrIfLetExpression p
  | For ->
    parseForExpression p
  | While ->
    parseWhileExpression p
  | Switch ->
    parseSwitchExpression p
  | _ ->
    if (context != WhenExpr) &&
       isEs6ArrowExpression ~inTernary:(context=TernaryTrueBranchExpr) p
    then
      parseEs6ArrowExpression p
    else
      parseUnaryExpr p
  in
  (* let endPos = p.Parser.prevEndPos in *)
  {expr with
    pexp_attributes = List.concat[expr.Parsetree.pexp_attributes; attrs];
    (* pexp_loc = mkLoc startPos endPos *)
  }

(* a binary expression is an expression that combines two expressions with an
 * operator. Examples:
 *    a + b
 *    f(x) |> g(y)
 *)
and parseBinaryExpr ?(context=OrdinaryExpr) ?a p prec =
  let a = match a with
  | Some e -> e
  | None -> parseOperandExpr ~context p
  in
  let rec loop a =
    let token = p.Parser.token in
    let tokenPrec =
      match token with
      (* Can the minus be interpreted as a binary operator? Or is it a unary?
       * let w = {
       *   x
       *   -10
       * }
       * vs
       * let w = {
       *   width
       *   - gap
       * }
       *
       * First case is unary, second is a binary operator.
       * See Scanner.isBinaryOp *)
      | Minus | MinusDot | LessThan when not (
          Scanner.isBinaryOp p.scanner.src p.startPos.pos_cnum p.endPos.pos_cnum
        ) && p.startPos.pos_lnum > p.prevEndPos.pos_lnum -> -1
      | token -> Token.precedence token
    in
    if tokenPrec < prec then a
    else begin
      Parser.leaveBreadcrumb p (Grammar.ExprBinaryAfterOp token);
      let startPos = p.startPos in
      Parser.next p;
      let endPos = p.prevEndPos in
      let b = parseBinaryExpr ~context p (tokenPrec + 1) in
      let loc = mkLoc a.Parsetree.pexp_loc.loc_start b.pexp_loc.loc_end in
      let expr = Ast_helper.Exp.apply
        ~loc
        (makeInfixOperator p token startPos endPos)
        [Nolabel, a; Nolabel, b]
      in
      Parser.eatBreadcrumb p;
      loop expr
    end
  in
  loop a

(* If we even need this, determines if < might be the start of jsx. Not 100% complete *)
(* and isStartOfJsx p = *)
  (* Parser.lookahead p (fun p -> *)
    (* match p.Parser.token with *)
    (* | LessThan -> *)
      (* Parser.next p; *)
      (* begin match p.token with *)
      (* | GreaterThan (* <> *) -> true *)
      (* | Lident _ | Uident _ | List -> *)
        (* ignore (parseJsxName p); *)
        (* begin match p.token with *)
        (* | GreaterThan (* <div> *) -> true *)
        (* | Question (*<Component ? *) -> true *)
        (* | Lident _ | List -> *)
          (* Parser.next p; *)
          (* begin match p.token with *)
          (* | Equal (* <Component handleClick= *) -> true *)
          (* | _ -> false (* TODO *) *)
          (* end *)
        (* | Forwardslash (* <Component / *)-> *)
          (* Parser.next p; *)
          (* begin match p.token with *)
          (* | GreaterThan (* <Component /> *) -> true *)
          (* | _ -> false *)
          (* end *)
        (* | _ -> *)
          (* false *)
        (* end *)
      (* | _ -> false *)
      (* end *)
    (* | _ -> false *)
  (* ) *)

and parseTemplateExpr ?(prefix="js") p =
  let hiddenOperator =
    let op = Location.mknoloc (Longident.Lident "^") in
    Ast_helper.Exp.ident op
  in
  let rec parseParts acc =
    let startPos = p.Parser.startPos in
    Parser.nextTemplateLiteralToken p;
    match p.token with
    | TemplateTail txt ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      if String.length txt > 0 then
        let txt = if p.mode = ParseForTypeChecker then parseTemplateStringLiteral txt else txt in
        let str = Ast_helper.Exp.constant ~loc (Pconst_string(txt, Some prefix)) in
        Ast_helper.Exp.apply ~loc hiddenOperator
          [Nolabel, acc; Nolabel, str]
      else
        acc
    | TemplatePart txt ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      let expr = parseExprBlock p in
      let fullLoc = mkLoc startPos p.prevEndPos in
      let txt = if p.mode = ParseForTypeChecker then parseTemplateStringLiteral txt else txt in
      let str = Ast_helper.Exp.constant ~loc (Pconst_string(txt, Some prefix)) in
      let next =
        let a = if String.length txt > 0 then
            Ast_helper.Exp.apply ~loc:fullLoc hiddenOperator [Nolabel, acc; Nolabel, str]
          else acc
        in
        Ast_helper.Exp.apply ~loc:fullLoc hiddenOperator
          [Nolabel, a; Nolabel, expr]
      in
      parseParts next
   | token ->
     Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
     Ast_helper.Exp.constant (Pconst_string("", None))
  in
  let startPos = p.startPos in
  Parser.nextTemplateLiteralToken p;
  match p.token with
  | TemplateTail txt ->
    Parser.next p;
    let txt = if p.mode = ParseForTypeChecker then parseTemplateStringLiteral txt else txt in
    Ast_helper.Exp.constant ~loc:(mkLoc startPos p.prevEndPos) (Pconst_string(txt, Some prefix))
  | TemplatePart txt ->
    Parser.next p;
    let constantLoc = mkLoc startPos p.prevEndPos in
    let expr = parseExprBlock p in
    let fullLoc = mkLoc startPos p.prevEndPos in
    let txt = if p.mode = ParseForTypeChecker then parseTemplateStringLiteral txt else txt in
    let str = Ast_helper.Exp.constant ~loc:constantLoc (Pconst_string(txt, Some prefix)) in
    let next =
      if String.length txt > 0 then
        Ast_helper.Exp.apply ~loc:fullLoc hiddenOperator [Nolabel, str; Nolabel, expr]
      else
        expr
    in
    parseParts next
 | token ->
   Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
   Ast_helper.Exp.constant (Pconst_string("", None))

(* Overparse: let f = a : int => a + 1, is it (a : int) => or (a): int =>
 * Also overparse constraints:
 *  let x = {
 *    let a = 1
 *    a + pi: int
 *  }
 *
 *  We want to give a nice error message in these cases
 *  *)
and overParseConstrainedOrCoercedOrArrowExpression p expr =
  match p.Parser.token with
  | ColonGreaterThan ->
    parseCoercedExpr ~expr p
  | Colon ->
    Parser.next p;
    let typ = parseTypExpr ~es6Arrow:false p in
    begin match p.Parser.token with
    | EqualGreater ->
      Parser.next p;
      let body = parseExpr p in
      let pat = match expr.pexp_desc with
      | Pexp_ident longident ->
        Ast_helper.Pat.var ~loc:expr.pexp_loc
          (Location.mkloc
            (Longident.flatten longident.txt |> String.concat ".")
            longident.loc)
      (* TODO: can we convert more expressions to patterns?*)
      | _ ->
        Ast_helper.Pat.var ~loc:expr.pexp_loc (Location.mkloc "pattern" expr.pexp_loc)
      in
      let arrow1 = Ast_helper.Exp.fun_
        ~loc:(mkLoc expr.pexp_loc.loc_start body.pexp_loc.loc_end)
        Asttypes.Nolabel
        None
        pat
        (Ast_helper.Exp.constraint_ body typ)
      in
      let arrow2 = Ast_helper.Exp.fun_
        ~loc:(mkLoc expr.pexp_loc.loc_start body.pexp_loc.loc_end)
        Asttypes.Nolabel
        None
        (Ast_helper.Pat.constraint_ pat typ)
        body
      in
      let msg =
        Doc.breakableGroup ~forceBreak:true (
          Doc.concat [
            Doc.text "Did you mean to annotate the parameter type or the return type?";
            Doc.indent (
              Doc.concat [
                Doc.line;
                Doc.text "1) ";
                NapkinscriptPrinter.printExpression arrow1 CommentTable.empty;
                Doc.line;
                Doc.text "2) ";
                NapkinscriptPrinter.printExpression arrow2 CommentTable.empty;
              ]
            )
          ]
        ) |> Doc.toString ~width:80
      in
      Parser.err
        ~startPos:expr.pexp_loc.loc_start
        ~endPos:body.pexp_loc.loc_end
        p
        (Diagnostics.message msg);
      arrow1
    | _ ->
      let open Parsetree in
      let loc = mkLoc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
      let expr = Ast_helper.Exp.constraint_ ~loc expr typ in
      let () = Parser.err
        ~startPos:expr.pexp_loc.loc_start
        ~endPos:typ.ptyp_loc.loc_end
        p
        (Diagnostics.message
          (Doc.breakableGroup ~forceBreak:true (Doc.concat [
            Doc.text "Expressions with type constraints need to be wrapped in parens:";
            Doc.indent (
              Doc.concat [
              Doc.line;
              NapkinscriptPrinter.addParens (NapkinscriptPrinter.printExpression expr CommentTable.empty);
              ]
            )
          ]) |> Doc.toString ~width:80
        ))
      in
      expr
    end
  | _ -> expr

and parseLetBindingBody ~startPos ~attrs p =
  Parser.beginRegion p;
  Parser.leaveBreadcrumb p Grammar.LetBinding;
  let pat, exp =
    Parser.leaveBreadcrumb p Grammar.Pattern;
    let pat = parsePattern p in
    Parser.eatBreadcrumb p;
    match p.Parser.token with
    | Colon ->
      Parser.next p;
      begin match p.token with
      | Typ -> (* locally abstract types *)
        Parser.next p;
        let newtypes = parseLidentList p in
        Parser.expect Dot p;
        let typ = parseTypExpr p in
        Parser.expect Equal p;
        let expr = parseExpr p in
        let loc = mkLoc startPos p.prevEndPos in
        let exp, poly = wrapTypeAnnotation ~loc newtypes typ expr in
        let pat = Ast_helper.Pat.constraint_ ~loc pat poly in
        (pat, exp)
      | _ ->
        let polyType = parsePolyTypeExpr p in
        let loc = {pat.ppat_loc with loc_end = polyType.Parsetree.ptyp_loc.loc_end} in
        let pat = Ast_helper.Pat.constraint_ ~loc pat polyType in
        Parser.expect Token.Equal p;
        let exp = parseExpr p in
        let exp = overParseConstrainedOrCoercedOrArrowExpression p exp in
        (pat, exp)
      end
    | _ ->
      Parser.expect Token.Equal p;
      let exp = overParseConstrainedOrCoercedOrArrowExpression p (parseExpr p) in
      (pat, exp)
  in
  let loc = mkLoc startPos p.prevEndPos in
  let vb = Ast_helper.Vb.mk ~loc ~attrs pat exp in
  Parser.eatBreadcrumb p;
  Parser.endRegion p;
  vb

(* TODO: find a better way? Is it possible?
 * let a = 1
 * @attr
 * and b = 2
 *
 * The problem is that without semi we need a lookahead to determine
 * if the attr is on the letbinding or the start of a new thing
 *
 * let a = 1
 * @attr
 * let b = 1
 *
 * Here @attr should attach to something "new": `let b = 1`
 * The parser state is forked, which is quite expensive…
 *)
and parseAttributesAndBinding (p : Parser.t) =
  let err = p.scanner.err in
  let ch = p.scanner.ch in
  let offset = p.scanner.offset in
  let rdOffset = p.scanner.rdOffset in
  let lineOffset = p.scanner.lineOffset in
  let lnum = p.scanner.lnum in
  let mode = p.scanner.mode in
  let token = p.token in
  let startPos = p.startPos in
  let endPos = p.endPos in
  let prevEndPos = p.prevEndPos in
  let breadcrumbs = p.breadcrumbs in
  let errors = p.errors in
  let diagnostics = p.diagnostics in
  let comments = p.comments in

  match p.Parser.token with
  | At ->
    let attrs = parseAttributes p in
    begin match p.Parser.token with
    | And ->
      attrs
    | _ ->
      p.scanner.err <- err;
      p.scanner.ch <- ch;
      p.scanner.offset <- offset;
      p.scanner.rdOffset <- rdOffset;
      p.scanner.lineOffset <- lineOffset;
      p.scanner.lnum <- lnum;
      p.scanner.mode <- mode;
      p.token <- token;
      p.startPos <- startPos;
      p.endPos <- endPos;
      p.prevEndPos <- prevEndPos;
      p.breadcrumbs <- breadcrumbs;
      p.errors <- errors;
      p.diagnostics <- diagnostics;
      p.comments <- comments;
      []
    end
  | _ -> []

(* definition	::=	let [rec] let-binding  { and let-binding }   *)
and parseLetBindings ~attrs p =
  let startPos = p.Parser.startPos in
  Parser.optional p Let |> ignore;
  let recFlag = if Parser.optional p Token.Rec then
    Asttypes.Recursive
  else
    Asttypes.Nonrecursive
  in
  let first = parseLetBindingBody ~startPos ~attrs p in

  let rec loop p bindings =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributesAndBinding p in
    match p.Parser.token with
    | And ->
      Parser.next p;
      let attrs = match p.token with
      | Export ->
        let exportLoc = mkLoc p.startPos p.endPos in
        Parser.next p;
        let genTypeAttr = (Location.mkloc "genType" exportLoc, Parsetree.PStr []) in
        genTypeAttr::attrs
      | _ -> attrs
      in
      ignore(Parser.optional p Let); (* overparse for fault tolerance *)
      let letBinding = parseLetBindingBody ~startPos ~attrs p in
      loop p (letBinding::bindings)
    | _ ->
      List.rev bindings
  in
  (recFlag, loop p [first])

(*
 * div -> div
 * Foo -> Foo.createElement
 * Foo.Bar -> Foo.Bar.createElement
 *)
and parseJsxName p =
  let longident = match p.Parser.token with
  | Lident ident ->
    let identStart = p.startPos in
    let identEnd = p.endPos in
    Parser.next p;
    let loc = mkLoc identStart identEnd in
    Location.mkloc (Longident.Lident ident) loc
  | Uident _ ->
    let longident = parseModuleLongIdent ~lowercase:false p in
    Location.mkloc (Longident.Ldot (longident.txt, "createElement")) longident.loc
  | _ ->
    let msg = "A jsx name should start with a lowercase or uppercase identifier, like: div in <div /> or Navbar in <Navbar />"
    in
    Parser.err p (Diagnostics.message msg);
    Location.mknoloc (Longident.Lident "_")
  in
  Ast_helper.Exp.ident ~loc:longident.loc longident

and parseJsxOpeningOrSelfClosingElement ~startPos p =
  let jsxStartPos = p.Parser.startPos in
  let name = parseJsxName p in
  let jsxProps = parseJsxProps p in
  let children = match p.Parser.token with
  | Forwardslash -> (* <foo a=b /> *)
    let childrenStartPos = p.Parser.startPos in
    Parser.next p;
    let childrenEndPos = p.Parser.startPos in
    Parser.expect GreaterThan p;
    let loc = mkLoc childrenStartPos childrenEndPos in
    makeListExpression loc [] None (* no children *)
  | GreaterThan -> (* <foo a=b> bar </foo> *)
    let childrenStartPos = p.Parser.startPos in
    Scanner.setJsxMode p.scanner;
    Parser.next p;
    let (spread, children) = parseJsxChildren p in
    let childrenEndPos = p.Parser.startPos in
    let () = match p.token with
    | LessThanSlash -> Parser.next p
    | LessThan -> Parser.next p; Parser.expect Forwardslash p
    | token when Grammar.isStructureItemStart token -> ()
    | _ -> Parser.expect LessThanSlash p
    in
    begin match p.Parser.token with
    | Lident _ | Uident _ when verifyJsxOpeningClosingName p name ->
      Parser.expect GreaterThan p;
      let loc = mkLoc childrenStartPos childrenEndPos in
      ( match spread, children with
        | true, child :: _ ->
          child
        | _ ->
          makeListExpression loc children None
      )
    | token ->
      let () = if Grammar.isStructureItemStart token then (
        let closing = "</" ^ (string_of_pexp_ident name) ^ ">" in
        let msg = Diagnostics.message ("Missing " ^ closing) in
        Parser.err ~startPos ~endPos:p.prevEndPos p msg;
      ) else (
        let opening = "</" ^ (string_of_pexp_ident name) ^ ">" in
        let msg = "Closing jsx name should be the same as the opening name. Did you mean " ^ opening ^ " ?" in
        Parser.err ~startPos ~endPos:p.prevEndPos p (Diagnostics.message msg);
        Parser.expect GreaterThan p
      )
      in
      let loc = mkLoc childrenStartPos childrenEndPos in
      ( match spread, children with
        | true, child :: _ ->
          child
        | _ ->
          makeListExpression loc children None
      )
    end
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    makeListExpression Location.none [] None
  in
  let jsxEndPos = p.prevEndPos in
  let loc = mkLoc jsxStartPos jsxEndPos in
  Ast_helper.Exp.apply
    ~loc
    name
    (List.concat [jsxProps; [
      (Asttypes.Labelled "children", children);
      (Asttypes.Nolabel, Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident "()")) None)
    ]])

(*
 *  jsx ::=
 *    | <> jsx-children </>
 *    | <element-name {jsx-prop} />
 *    | <element-name {jsx-prop}> jsx-children </element-name>
 *
 *  jsx-children ::= primary-expr*          * => 0 or more
 *)
and parseJsx p =
  Parser.leaveBreadcrumb p Grammar.Jsx;
  let startPos = p.Parser.startPos in
  Parser.expect LessThan p;
  let jsxExpr = match p.Parser.token with
  | Lident _ | Uident _ ->
    parseJsxOpeningOrSelfClosingElement ~startPos p
  | GreaterThan -> (* fragment: <> foo </> *)
    parseJsxFragment p
  | _ ->
    parseJsxName p
  in
  {jsxExpr with pexp_attributes = [jsxAttr]}

(*
 * jsx-fragment ::=
 *  | <> </>
 *  | <> jsx-children </>
 *)
and parseJsxFragment p =
  let childrenStartPos = p.Parser.startPos in
  Scanner.setJsxMode p.scanner;
  Parser.expect GreaterThan p;
  let (_spread, children) = parseJsxChildren p in
  let childrenEndPos = p.Parser.startPos in
  Parser.expect LessThanSlash p;
  Parser.expect GreaterThan p;
  let loc = mkLoc childrenStartPos childrenEndPos in
  makeListExpression loc children None


(*
 * jsx-prop ::=
 *   |  lident
 *   | ?lident
 *   |  lident =  jsx_expr
 *   |  lident = ?jsx_expr
 *)
and parseJsxProp p =
  Parser.leaveBreadcrumb p Grammar.JsxAttribute;
  match p.Parser.token with
  | Question | Lident _ ->
    let optional = Parser.optional p Question in
    let (name, loc) = parseLident p in
    let propLocAttr = (Location.mkloc "ns.namedArgLoc" loc, Parsetree.PStr []) in
    (* optional punning: <foo ?a /> *)
    if optional then
      Some (
        Asttypes.Optional name,
        Ast_helper.Exp.ident ~attrs:[propLocAttr]
          ~loc (Location.mkloc (Longident.Lident name) loc)
      )
    else begin
      match p.Parser.token with
      | Equal ->
        Parser.next p;
        (* no punning *)
        let optional = Parser.optional p Question in
        let attrExpr =
          let e = parsePrimaryExpr ~operand:(parseAtomicExpr p) p in
          {e with pexp_attributes = propLocAttr::e.pexp_attributes}
        in
        let label =
          if optional then Asttypes.Optional name else Asttypes.Labelled name
        in
        Some (label, attrExpr)
      | _ ->
        let attrExpr =
          Ast_helper.Exp.ident ~loc ~attrs:[propLocAttr]
            (Location.mknoloc (Longident.Lident name)) in
        let label =
          if optional then Asttypes.Optional name else Asttypes.Labelled name
        in
        Some (label, attrExpr)
    end
  | _ ->
    None

and parseJsxProps p =
  parseRegion
    ~grammar:Grammar.JsxAttribute
    ~f:parseJsxProp
    p

and parseJsxChildren p =
  let rec loop p children =
    match p.Parser.token  with
    | Token.Eof | LessThanSlash ->
      Scanner.popMode p.scanner Jsx;
      List.rev children
    | LessThan ->
      (* Imagine: <div> <Navbar /> <
       * is `<` the start of a jsx-child? <div …
       * or is it the start of a closing tag?  </div>
       * reconsiderLessThan peeks at the next token and
       * determines the correct token to disambiguate *)
      let token = Scanner.reconsiderLessThan p.scanner in
      if token = LessThan then
        let child = parsePrimaryExpr ~operand:(parseAtomicExpr p) ~noCall:true p in
        loop p (child::children)
      else (* LessThanSlash *)
        let () = p.token <- token in
        let () = Scanner.popMode p.scanner Jsx in
        List.rev children
    | token when Grammar.isJsxChildStart token ->
      let () = Scanner.popMode p.scanner Jsx in
      let child = parsePrimaryExpr ~operand:(parseAtomicExpr p) ~noCall:true p in
      loop p (child::children)
    | _ ->
      Scanner.popMode p.scanner Jsx;
      List.rev children
  in
  match p.Parser.token with
  | DotDotDot ->
    Parser.next p;
    (true, [parsePrimaryExpr ~operand:(parseAtomicExpr p) ~noCall:true p])
  | _ -> (false, loop p [])

and parseBracedOrRecordExpr  p =
  let startPos = p.Parser.startPos in
  Parser.expect Lbrace p;
  match p.Parser.token with
  | Rbrace ->
    Parser.err p (Diagnostics.unexpected Rbrace p.breadcrumbs);
    Parser.next p;
    let loc = mkLoc startPos p.prevEndPos in
    let braces = makeBracesAttr loc in
    Ast_helper.Exp.construct ~attrs:[braces] ~loc
      (Location.mkloc (Longident.Lident "()") loc) None
  | DotDotDot ->
    (* beginning of record spread, parse record *)
    Parser.next p;
    let spreadExpr = parseConstrainedOrCoercedExpr p in
    Parser.expect Comma p;
    let expr = parseRecordExpr ~startPos ~spread:(Some spreadExpr) [] p in
    Parser.expect Rbrace p;
    expr
  | String s ->
    let field =
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      Location.mkloc (Longident.Lident s) loc
    in
    begin match p.Parser.token with
    | Colon ->
      Parser.next p;
      let fieldExpr = parseExpr p in
      Parser.optional p Comma |> ignore;
      let expr = parseRecordExprWithStringKeys ~startPos (field, fieldExpr) p in
      Parser.expect Rbrace p;
      expr
    | _ ->
      let constant = Ast_helper.Exp.constant ~loc:field.loc (Parsetree.Pconst_string(s, None)) in
      let a = parsePrimaryExpr ~operand:constant p in
      let e = parseBinaryExpr ~a p 1 in
      let e = parseTernaryExpr e p in
      begin match p.Parser.token with
      | Semicolon ->
        let expr = parseExprBlock ~first:e p in
        Parser.expect Rbrace p;
        let loc = mkLoc startPos p.prevEndPos in
        let braces = makeBracesAttr loc in
        {expr with Parsetree.pexp_attributes = braces::expr.Parsetree.pexp_attributes}
      | Rbrace ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        let braces = makeBracesAttr loc in
        {e with pexp_attributes = braces::e.pexp_attributes}
      | _ ->
        let expr = parseExprBlock ~first:e p in
        Parser.expect Rbrace p;
        let loc = mkLoc startPos p.prevEndPos in
        let braces = makeBracesAttr loc in
        {expr with pexp_attributes = braces::expr.pexp_attributes}
      end
    end
  | Uident _ | Lident _ ->
    let valueOrConstructor = parseValueOrConstructor p in
    begin match valueOrConstructor.pexp_desc with
    | Pexp_ident pathIdent ->
      let identEndPos = p.prevEndPos in
      begin match p.Parser.token with
      | Comma ->
        Parser.next p;
        let expr = parseRecordExpr ~startPos [(pathIdent, valueOrConstructor)] p in
        Parser.expect Rbrace p;
        expr
      | Colon ->
        Parser.next p;
        let fieldExpr = parseExpr p in
        begin match p.token with
        | Rbrace ->
          Parser.next p;
          let loc = mkLoc startPos p.prevEndPos in
          Ast_helper.Exp.record ~loc [(pathIdent, fieldExpr)] None
        | _ ->
          Parser.expect Comma p;
          let expr = parseRecordExpr ~startPos [(pathIdent, fieldExpr)] p in
          Parser.expect Rbrace p;
          expr
        end
      (* error case *)
      | Lident _ ->
        if p.prevEndPos.pos_lnum < p.startPos.pos_lnum then (
          Parser.expect Comma p;
          let expr = parseRecordExpr ~startPos [(pathIdent, valueOrConstructor)] p in
          Parser.expect Rbrace p;
          expr
        ) else (
          Parser.expect Colon p;
          let expr = parseRecordExpr ~startPos [(pathIdent, valueOrConstructor)] p in
          Parser.expect Rbrace p;
          expr
        )
      | Semicolon ->
        let expr = parseExprBlock ~first:(Ast_helper.Exp.ident pathIdent) p in
        Parser.expect Rbrace p;
        let loc = mkLoc startPos p.prevEndPos in
        let braces = makeBracesAttr loc in
        {expr with pexp_attributes = braces::expr.pexp_attributes}
      | Rbrace ->
        Parser.next p;
        let expr = Ast_helper.Exp.ident ~loc:pathIdent.loc pathIdent in
        let loc = mkLoc startPos p.prevEndPos in
        let braces = makeBracesAttr loc in
        {expr with pexp_attributes = braces::expr.pexp_attributes}
      | EqualGreater ->
        let loc = mkLoc startPos identEndPos in
        let ident = Location.mkloc (Longident.last pathIdent.txt) loc in
        let a = parseEs6ArrowExpression
          ~parameters:[TermParameter {
            uncurried = false;
            attrs = [];
            label = Asttypes.Nolabel;
            expr = None;
            pat = Ast_helper.Pat.var ident;
            pos = startPos;
          }]
          p
        in
        let e = parseBinaryExpr ~a p 1 in
        let e = parseTernaryExpr e p in
        begin match p.Parser.token with
        | Semicolon ->
          let expr = parseExprBlock ~first:e p in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        | Rbrace ->
          Parser.next p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {e with pexp_attributes = braces::e.pexp_attributes}
        | _ ->
          let expr = parseExprBlock ~first:e p in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        end
      | _ ->
        Parser.leaveBreadcrumb p Grammar.ExprBlock;
        let a = parsePrimaryExpr ~operand:(Ast_helper.Exp.ident ~loc:pathIdent.loc pathIdent) p in
        let e = parseBinaryExpr ~a p 1 in
        let e = parseTernaryExpr e p in
        Parser.eatBreadcrumb p;
        begin match p.Parser.token with
        | Semicolon ->
          let expr = parseExprBlock ~first:e p in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        | Rbrace ->
          Parser.next p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {e with pexp_attributes = braces::e.pexp_attributes}
        | _ ->
          let expr = parseExprBlock ~first:e p in
          Parser.expect Rbrace p;
          let loc = mkLoc startPos p.prevEndPos in
          let braces = makeBracesAttr loc in
          {expr with pexp_attributes = braces::expr.pexp_attributes}
        end
       end
    | _ ->
      Parser.leaveBreadcrumb p Grammar.ExprBlock;
      let a = parsePrimaryExpr ~operand:valueOrConstructor p in
      let e = parseBinaryExpr ~a p 1 in
      let e = parseTernaryExpr e p in
      Parser.eatBreadcrumb p;
      begin match p.Parser.token with
      | Semicolon ->
        let expr = parseExprBlock ~first:e p in
        Parser.expect Rbrace p;
        let loc = mkLoc startPos p.prevEndPos in
        let braces = makeBracesAttr loc in
        {expr with pexp_attributes = braces::expr.pexp_attributes}
      | Rbrace ->
        Parser.next p;
        let loc = mkLoc startPos p.prevEndPos in
        let braces = makeBracesAttr loc in
        {e with pexp_attributes = braces::e.pexp_attributes}
      | _ ->
        let expr = parseExprBlock ~first:e p in
        Parser.expect Rbrace p;
        let loc = mkLoc startPos p.prevEndPos in
        let braces = makeBracesAttr loc in
        {expr with pexp_attributes = braces::expr.pexp_attributes}
      end
       end
  | _ ->
    let expr = parseExprBlock p in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    let braces = makeBracesAttr loc in
    {expr with pexp_attributes = braces::expr.pexp_attributes}

and parseRecordRowWithStringKey p =
  match p.Parser.token with
  | String s ->
    let loc = mkLoc p.startPos p.endPos in
    Parser.next p;
    let field = Location.mkloc (Longident.Lident s) loc in
    begin match p.Parser.token with
    | Colon ->
      Parser.next p;
      let fieldExpr = parseExpr p in
      Some (field, fieldExpr)
    | _ ->
      Some (field, Ast_helper.Exp.ident ~loc:field.loc field)
    end
  | _ -> None

and parseRecordRow p =
  let () = match p.Parser.token with
  | Token.DotDotDot ->
    Parser.err p (Diagnostics.message ErrorMessages.recordExprSpread);
    Parser.next p;
  | _ -> ()
  in
  match p.Parser.token with
  | Lident _ | Uident _ ->
    let field = parseValuePath p in
    begin match p.Parser.token with
    | Colon ->
      Parser.next p;
      let fieldExpr = parseExpr p in
      Some (field, fieldExpr)
    | _ ->
      Some (field, Ast_helper.Exp.ident ~loc:field.loc  field)
    end
  | _ -> None

and parseRecordExprWithStringKeys ~startPos firstRow p =
  let rows = firstRow::(
    parseCommaDelimitedRegion ~grammar:Grammar.RecordRowsStringKey ~closing:Rbrace ~f:parseRecordRowWithStringKey p
  ) in
  let loc = mkLoc startPos p.endPos in
  let recordStrExpr = Ast_helper.Str.eval ~loc (
    Ast_helper.Exp.record ~loc rows None
  ) in
  Ast_helper.Exp.extension ~loc
    (Location.mkloc "bs.obj" loc, Parsetree.PStr [recordStrExpr])

and parseRecordExpr ~startPos ?(spread=None) rows p =
  let exprs =
    parseCommaDelimitedRegion
      ~grammar:Grammar.RecordRows
      ~closing:Rbrace
      ~f:parseRecordRow p
  in
  let rows = List.concat [rows; exprs] in
  let () = match rows with
  | [] ->
    let msg = "Record spread needs at least one field that's updated" in
    Parser.err p (Diagnostics.message msg);
  | _rows -> ()
  in
  let loc = mkLoc startPos p.endPos in
  Ast_helper.Exp.record ~loc rows spread


and parseNewlineOrSemicolonExprBlock p =
  match p.Parser.token with
  | Semicolon ->
    Parser.next p
  | token when Grammar.isBlockExprStart token ->
    if p.prevEndPos.pos_lnum < p.startPos.pos_lnum then ()
    else
      Parser.err
        ~startPos:p.prevEndPos
        ~endPos: p.endPos
        p
        (Diagnostics.message "consecutive expressions on a line must be separated by ';' or a newline")
  | _ -> ()

and parseExprBlockItem p =
  let startPos = p.Parser.startPos in
  let attrs = parseAttributes p in
  match p.Parser.token with
  | Module ->
    Parser.next p;
    begin match p.token with
    | Lparen ->
      parseFirstClassModuleExpr ~startPos p
    | _ ->
      let name = match p.Parser.token with
      | Uident ident ->
        let loc = mkLoc p.startPos p.endPos in
        Parser.next p;
        Location.mkloc ident loc
      | t ->
        Parser.err p (Diagnostics.uident t);
        Location.mknoloc "_"
      in
      let body = parseModuleBindingBody p in
      parseNewlineOrSemicolonExprBlock p;
      let expr = parseExprBlock p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Exp.letmodule ~loc name body expr
    end
  | Exception ->
    let extensionConstructor = parseExceptionDef ~attrs p in
    parseNewlineOrSemicolonExprBlock p;
    let blockExpr = parseExprBlock  p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.letexception ~loc extensionConstructor blockExpr
  | Open ->
    let od = parseOpenDescription ~attrs p in
    parseNewlineOrSemicolonExprBlock p;
    let blockExpr = parseExprBlock p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.open_ ~loc od.popen_override od.popen_lid blockExpr
  | Let ->
    let (recFlag, letBindings) = parseLetBindings ~attrs p in
    parseNewlineOrSemicolonExprBlock p;
    let next = if Grammar.isBlockExprStart p.Parser.token then
      parseExprBlock p
    else
      let loc = mkLoc p.startPos p.endPos in
      Ast_helper.Exp.construct ~loc
        (Location.mkloc (Longident.Lident "()") loc) None
    in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.let_ ~loc recFlag letBindings next
  | _ ->
    let e1 =
      let expr = parseExpr p in
      {expr with pexp_attributes = List.concat [attrs; expr.pexp_attributes]}
    in
    parseNewlineOrSemicolonExprBlock p;
    if Grammar.isBlockExprStart p.Parser.token then
      let e2 = parseExprBlock p in
      let loc = {e1.pexp_loc with loc_end = e2.pexp_loc.loc_end} in
      Ast_helper.Exp.sequence ~loc e1 e2
    else e1

(* blockExpr ::= expr
 *            |  expr          ;
 *            |  expr          ; blockExpr
 *            |  module    ... ; blockExpr
 *            |  open      ... ; blockExpr
 *            |  exception ... ; blockExpr
 *            |  let       ...
 *            |  let       ... ;
 *            |  let       ... ; blockExpr
 *
 *  note: semi should be made optional
 *  a block of expression is always
 *)
and parseExprBlock ?first p =
  Parser.leaveBreadcrumb p Grammar.ExprBlock;
  let item = match first with
  | Some e -> e
  | None -> parseExprBlockItem p
  in
  parseNewlineOrSemicolonExprBlock p;
  let blockExpr = if Grammar.isBlockExprStart p.Parser.token then
    let next = parseExprBlockItem p in
    let loc = {item.pexp_loc with loc_end = next.pexp_loc.loc_end} in
    Ast_helper.Exp.sequence ~loc item next
  else
    item
  in
  Parser.eatBreadcrumb p;
  overParseConstrainedOrCoercedOrArrowExpression p blockExpr

and parseTryExpression p =
  let startPos = p.Parser.startPos in
  Parser.expect Try p;
  let expr = parseExpr ~context:WhenExpr p in
  Parser.expect Napkin_token.catch p;
  Parser.expect Lbrace p;
  let cases = parsePatternMatching p in
  Parser.expect Rbrace p;
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Exp.try_ ~loc expr cases

and parseIfCondition p =
  Parser.leaveBreadcrumb p Grammar.IfCondition;
  (* doesn't make sense to try es6 arrow here? *)
  let conditionExpr = parseExpr ~context:WhenExpr p in
  Parser.eatBreadcrumb p;
  conditionExpr

and parseThenBranch p =
  Parser.leaveBreadcrumb p IfBranch;
  Parser.expect Lbrace p;
  let thenExpr = parseExprBlock p in
  Parser.expect Rbrace p;
  Parser.eatBreadcrumb p;
  thenExpr

and parseElseBranch p =
  Parser.expect Lbrace p;
  let blockExpr = parseExprBlock p in
  Parser.expect Rbrace p;
  blockExpr;

and parseIfExpr startPos p =
  let conditionExpr = parseIfCondition p in
  let thenExpr = parseThenBranch p in
  let elseExpr = match p.Parser.token with
  | Else ->
    Parser.endRegion p;
    Parser.leaveBreadcrumb p Grammar.ElseBranch;
    Parser.next p;
    Parser.beginRegion p;
    let elseExpr = match p.token with
    | If ->
      parseIfOrIfLetExpression p
    | _ ->
      parseElseBranch p
    in
    Parser.eatBreadcrumb p;
    Parser.endRegion p;
    Some elseExpr
  | _ ->
    Parser.endRegion p;
    None
  in
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Exp.ifthenelse ~loc conditionExpr thenExpr elseExpr

and parseIfLetExpr startPos p =
  let pattern = parsePattern p in
  Parser.expect Equal p;
  let conditionExpr = parseIfCondition p in
  let thenExpr = parseThenBranch p in
  let elseExpr = match p.Parser.token with
  | Else ->
    Parser.endRegion p;
    Parser.leaveBreadcrumb p Grammar.ElseBranch;
    Parser.next p;
    Parser.beginRegion p;
    let elseExpr = match p.token with
    | If ->
      parseIfOrIfLetExpression p
    | _ ->
      parseElseBranch p
    in
    Parser.eatBreadcrumb p;
    Parser.endRegion p;
    elseExpr
  | _ ->
    Parser.endRegion p;
    let startPos = p.Parser.startPos in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.construct ~loc (Location.mkloc (Longident.Lident "()") loc) None
  in
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Exp.match_ ~attrs:[ifLetAttr; suppressFragileMatchWarningAttr] ~loc conditionExpr [
    Ast_helper.Exp.case pattern thenExpr;
    Ast_helper.Exp.case (Ast_helper.Pat.any ()) elseExpr;
  ]

and parseIfOrIfLetExpression p =
  Parser.beginRegion p;
  Parser.leaveBreadcrumb p Grammar.ExprIf;
  let startPos = p.Parser.startPos in
  Parser.expect If p;
  let expr = match p.Parser.token with
    | Let ->
      Parser.next p;
      let ifLetExpr = parseIfLetExpr startPos p in
      Parser.err
        ~startPos:ifLetExpr.pexp_loc.loc_start
        ~endPos:ifLetExpr.pexp_loc.loc_end
        p
        (Diagnostics.message (ErrorMessages.experimentalIfLet ifLetExpr));
      ifLetExpr
    | _ ->
      parseIfExpr startPos p
  in
  Parser.eatBreadcrumb p;
  expr;

and parseForRest hasOpeningParen pattern startPos p =
  Parser.expect In p;
  let e1 = parseExpr p in
  let direction = match p.Parser.token with
  | To -> Asttypes.Upto
  | Downto -> Asttypes.Downto
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    Asttypes.Upto
  in
  Parser.next p;
  let e2 = parseExpr ~context:WhenExpr p in
  if hasOpeningParen then Parser.expect Rparen p;
  Parser.expect Lbrace p;
  let bodyExpr = parseExprBlock p in
  Parser.expect Rbrace p;
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Exp.for_ ~loc pattern e1 e2 direction bodyExpr

and parseForExpression p =
  let startPos = p.Parser.startPos in
  Parser.leaveBreadcrumb p Grammar.ExprFor;
  Parser.expect For p;
  Parser.beginRegion p;
  let forExpr = match p.token with
  | Lparen ->
    let lparen = p.startPos in
    Parser.next p;
    begin match p.token with
    | Rparen ->
      Parser.next p;
      let unitPattern =
        let loc = mkLoc lparen p.prevEndPos in
        let lid = Location.mkloc (Longident.Lident "()") loc in
        Ast_helper.Pat.construct lid None
      in
      parseForRest false (parseAliasPattern ~attrs:[] unitPattern p) startPos p
    | _ ->
      Parser.leaveBreadcrumb p Grammar.Pattern;
      let pat = parsePattern p in
      Parser.eatBreadcrumb p;
      begin match p.token with
      | Comma ->
        Parser.next p;
        let tuplePattern =
          parseTuplePattern ~attrs:[] ~startPos:lparen ~first:pat p
        in
        let pattern = parseAliasPattern ~attrs:[] tuplePattern p in
        parseForRest false pattern startPos p
      | _ ->
        parseForRest true pat startPos p
      end
    end
  | _ ->
    Parser.leaveBreadcrumb p Grammar.Pattern;
    let pat = parsePattern p in
    Parser.eatBreadcrumb p;
    parseForRest false pat startPos p
  in
  Parser.eatBreadcrumb p;
  Parser.endRegion p;
  forExpr


and parseWhileExpression p =
  let startPos = p.Parser.startPos in
  Parser.expect While p;
  let expr1 = parseExpr ~context:WhenExpr p in
  Parser.expect Lbrace p;
  let expr2 = parseExprBlock p in
  Parser.expect Rbrace p;
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Exp.while_ ~loc expr1 expr2

and parsePatternGuard p =
  match p.Parser.token with
    | When ->
      Parser.next p;
      Some (parseExpr ~context:WhenExpr p)
    | _ ->
      None

and parsePatternMatchCase p =
  Parser.beginRegion p;
  Parser.leaveBreadcrumb p Grammar.PatternMatchCase;
  match p.Parser.token with
  | Token.Bar ->
    Parser.next p;
    Parser.leaveBreadcrumb p Grammar.Pattern;
    let lhs = parsePattern p in
    Parser.eatBreadcrumb p;
    let guard = parsePatternGuard p in
    let () = match p.token with
    | EqualGreater -> Parser.next p
    | _ -> Recover.recoverEqualGreater p
    in
    let rhs = parseExprBlock p in
    Parser.endRegion p;
    Parser.eatBreadcrumb p;
    Some (Ast_helper.Exp.case lhs ?guard rhs)
  | _ ->
    Parser.endRegion p;
    Parser.eatBreadcrumb p;
    None

and parsePatternMatching p =
  let cases =
    parseDelimitedRegion
      ~grammar:Grammar.PatternMatching
      ~closing:Rbrace
      ~f:parsePatternMatchCase
      p
  in
  let () = match cases with
  | [] -> Parser.err ~startPos:p.prevEndPos p (
      Diagnostics.message "Pattern matching needs at least one case"
    )
  | _ -> ()
  in
  cases

and parseSwitchExpression p =
  let startPos = p.Parser.startPos in
  Parser.expect Switch p;
  let switchExpr = parseExpr ~context:WhenExpr p in
  Parser.expect Lbrace p;
  let cases = parsePatternMatching p in
  Parser.expect Rbrace p;
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Exp.match_ ~loc switchExpr cases

(*
 * argument ::=
 *   | _                            (* syntax sugar *)
 *   | expr
 *   | expr : type
 *   | ~ label-name
 *   | ~ label-name
 *   | ~ label-name ?
 *   | ~ label-name =   expr
 *   | ~ label-name =   _           (* syntax sugar *)
 *   | ~ label-name =   expr : type
 *   | ~ label-name = ? expr
 *   | ~ label-name = ? _           (* syntax sugar *)
 *   | ~ label-name = ? expr : type
 *
 *  uncurried_argument ::=
 *   | . argument
 *)
and parseArgument p =
  if (
    p.Parser.token = Token.Tilde ||
    p.token = Dot ||
    p.token = Underscore ||
    Grammar.isExprStart p.token
  ) then (
    match p.Parser.token with
    | Dot ->
      let uncurried = true in
      let startPos = p.Parser.startPos in
      Parser.next(p);
      begin match p.token with
        (* apply(.) *)
        | Rparen ->
          let loc = mkLoc startPos p.prevEndPos in
          let unitExpr = Ast_helper.Exp.construct ~loc
            (Location.mkloc (Longident.Lident "()") loc) None
          in
          Some (uncurried, Asttypes.Nolabel, unitExpr)
        | _ ->
          parseArgument2 p ~uncurried
      end
    | _ ->
      parseArgument2 p ~uncurried:false
  ) else
    None

and parseArgument2 p ~uncurried =
  match p.Parser.token with
  (* foo(_), do not confuse with foo(_ => x), TODO: performance *)
  | Underscore when not (isEs6ArrowExpression ~inTernary:false p) ->
    let loc = mkLoc p.startPos p.endPos in
    Parser.next p;
    let exp = Ast_helper.Exp.ident ~loc (
      Location.mkloc (Longident.Lident "_") loc
    ) in
    Some (uncurried, Asttypes.Nolabel, exp)
  | Tilde ->
    Parser.next p;
    (* TODO: nesting of pattern matches not intuitive for error recovery *)
    begin match p.Parser.token with
    | Lident ident ->
      let startPos = p.startPos in
      Parser.next p;
      let endPos = p.prevEndPos in
      let loc = mkLoc startPos endPos in
      let propLocAttr = (Location.mkloc "ns.namedArgLoc" loc, Parsetree.PStr []) in
      let identExpr = Ast_helper.Exp.ident ~attrs:[propLocAttr] ~loc (
        Location.mkloc (Longident.Lident ident) loc
      ) in
      begin match p.Parser.token with
      | Question ->
        Parser.next p;
        Some (uncurried, Asttypes.Optional ident, identExpr)
      | Equal ->
        Parser.next p;
        let label = match p.Parser.token with
        | Question ->
          Parser.next p;
          Asttypes.Optional ident
        | _ ->
          Labelled ident
        in
        let expr = match p.Parser.token with
        | Underscore when not (isEs6ArrowExpression ~inTernary:false p) ->
          let loc = mkLoc p.startPos p.endPos in
          Parser.next p;
          Ast_helper.Exp.ident ~loc (
            Location.mkloc (Longident.Lident "_") loc
          )
        | _ ->
         let expr = parseConstrainedOrCoercedExpr p in
         {expr with pexp_attributes = propLocAttr::expr.pexp_attributes}
        in
        Some (uncurried, label, expr)
      | Colon ->
        Parser.next p;
        let typ = parseTypExpr p in
        let loc = mkLoc startPos p.prevEndPos in
        let expr = Ast_helper.Exp.constraint_ ~attrs:[propLocAttr] ~loc identExpr typ in
        Some (uncurried, Labelled ident, expr)
      | _ ->
        Some (uncurried, Labelled ident, identExpr)
      end
    | t ->
      Parser.err p (Diagnostics.lident t);
      Some (uncurried, Nolabel, Recover.defaultExpr ())
    end
  | _ -> Some (uncurried, Nolabel, parseConstrainedOrCoercedExpr p)

and parseCallExpr p funExpr =
  Parser.expect Lparen p;
  let startPos = p.Parser.startPos in
  Parser.leaveBreadcrumb p Grammar.ExprCall;
  let args =
    parseCommaDelimitedRegion
      ~grammar:Grammar.ArgumentList
      ~closing:Rparen
      ~f:parseArgument p
  in
  Parser.expect Rparen p;
  let args = match args with
  | [] ->
    let loc = mkLoc startPos p.prevEndPos in
   (* No args -> unit sugar: `foo()` *)
    [ false,
      Asttypes.Nolabel,
      Ast_helper.Exp.construct
        ~loc (Location.mkloc (Longident.Lident "()") loc) None
    ]
  | args -> args
  in
  let loc = {funExpr.pexp_loc with loc_end = p.prevEndPos} in
  let args = match args with
  | (u, lbl, expr)::args ->
    let group (grp, acc) (uncurried, lbl, expr) =
      let (_u, grp) = grp in
      if uncurried == true then
        ((true, [lbl, expr]), ((_u, (List.rev grp))::acc))
      else
        ((_u, ((lbl, expr)::grp)), acc)
    in
    let ((_u, grp), acc) = List.fold_left group((u, [lbl, expr]), []) args in
    List.rev ((_u, (List.rev grp))::acc)
  | [] -> []
  in
  let apply = List.fold_left (fun callBody group ->
    let (uncurried, args) = group in
    let (args, wrap) = processUnderscoreApplication args in
    let exp = if uncurried then
      let attrs = [uncurryAttr] in
      Ast_helper.Exp.apply ~loc ~attrs callBody args
    else
      Ast_helper.Exp.apply ~loc callBody args
    in
    wrap exp
  ) funExpr args
  in
  Parser.eatBreadcrumb p;
  apply

and parseValueOrConstructor p =
  let startPos = p.Parser.startPos in
  let rec aux p acc =
    match p.Parser.token with
    | Uident ident ->
      let endPosLident = p.endPos in
      Parser.next p;
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        aux p (ident::acc)
      | Lparen when p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
        let lparen = p.startPos in
        let args = parseConstructorArgs p in
        let rparen = p.prevEndPos in
        let lident = buildLongident (ident::acc) in
        let tail = match args with
        | [] -> None
        | [{Parsetree.pexp_desc = Pexp_tuple _} as arg] as args ->
          let loc = mkLoc lparen rparen in
          if p.mode = ParseForTypeChecker then
            (* Some(1, 2) for type-checker *)
            Some arg
          else
            (* Some((1, 2)) for printer *)
            Some (Ast_helper.Exp.tuple ~loc args)
        | [arg] ->
          Some arg
        | args ->
          let loc = mkLoc lparen rparen in
          Some (Ast_helper.Exp.tuple ~loc args)
        in
        let loc = mkLoc startPos p.prevEndPos in
        let identLoc = mkLoc startPos endPosLident in
        Ast_helper.Exp.construct ~loc (Location.mkloc lident identLoc) tail
      | _ ->
        let loc = mkLoc startPos p.prevEndPos in
        let lident = buildLongident (ident::acc) in
        Ast_helper.Exp.construct ~loc (Location.mkloc lident loc) None
      end
    | Lident ident ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      let lident = buildLongident (ident::acc) in
      Ast_helper.Exp.ident ~loc (Location.mkloc lident loc)
    | token ->
      Parser.next p;
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.defaultExpr()
  in
  aux p []

and parsePolyVariantExpr p =
  let startPos = p.startPos in
  let (ident, _loc) = parseHashIdent ~startPos p in
  begin match p.Parser.token with
  | Lparen when p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
    let lparen = p.startPos in
    let args = parseConstructorArgs p in
    let rparen = p.prevEndPos in
    let loc_paren = mkLoc lparen rparen in
    let tail = match args with
    | [] -> None
    | [{Parsetree.pexp_desc = Pexp_tuple _} as expr ] as args ->
      if p.mode = ParseForTypeChecker then
        (* #a(1, 2) for type-checker *)
        Some expr
      else
        (* #a((1, 2)) for type-checker *)
        Some (Ast_helper.Exp.tuple ~loc:loc_paren args)
    | [arg] -> Some arg
    | args ->
      (* #a((1, 2)) for printer *)
      Some (Ast_helper.Exp.tuple ~loc:loc_paren args)
    in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.variant ~loc ident tail
  | _ ->
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Exp.variant ~loc ident None
  end

and parseConstructorArgs p =
  let lparen = p.Parser.startPos in
  Parser.expect Lparen p;
  let args =
    parseCommaDelimitedRegion
      ~grammar:Grammar.ExprList ~f:parseConstrainedExprRegion ~closing:Rparen p
  in
  Parser.expect Rparen p;
  match args with
  | [] ->
    let loc = mkLoc lparen p.prevEndPos in
    [Ast_helper.Exp.construct
      ~loc (Location.mkloc (Longident.Lident "()") loc) None]
  | args -> args

and parseTupleExpr ~first ~startPos p =
  let exprs =
    parseCommaDelimitedRegion
      p ~grammar:Grammar.ExprList ~closing:Rparen ~f:parseConstrainedExprRegion
  in
  Parser.expect Rparen p;
  Ast_helper.Exp.tuple ~loc:(mkLoc startPos p.prevEndPos) (first::exprs)

and parseSpreadExprRegion p =
  match p.Parser.token with
  | DotDotDot ->
    Parser.next p;
    let expr = parseConstrainedOrCoercedExpr p in
    Some (true, expr)
  | token when Grammar.isExprStart token ->
    Some (false, parseConstrainedOrCoercedExpr p)
  | _ -> None

and parseListExpr ~startPos p =
  let listExprs =
    parseCommaDelimitedReversedList
    p ~grammar:Grammar.ListExpr ~closing:Rbrace ~f:parseSpreadExprRegion
  in
  Parser.expect Rbrace p;
  let loc = mkLoc startPos p.prevEndPos in
  match listExprs with
  | (true, expr)::exprs ->
    let exprs = exprs |> List.map snd |> List.rev in
    makeListExpression loc exprs (Some expr)
  | exprs ->
   let exprs =
      exprs
      |> List.map (fun (spread, expr) ->
          if spread then
            Parser.err p (Diagnostics.message ErrorMessages.listExprSpread);
          expr)
      |> List.rev
    in
    makeListExpression loc exprs None

(* Overparse ... and give a nice error message *)
and parseNonSpreadExp ~msg p =
  let () = match p.Parser.token with
  | DotDotDot ->
    Parser.err p (Diagnostics.message msg);
    Parser.next p;
  | _ -> ()
  in
  match p.Parser.token with
  | token when Grammar.isExprStart token ->
    let expr = parseExpr p in
    begin match p.Parser.token with
    | Colon ->
      Parser.next p;
      let typ = parseTypExpr p in
      let loc = mkLoc expr.pexp_loc.loc_start typ.ptyp_loc.loc_end in
      Some (Ast_helper.Exp.constraint_ ~loc expr typ)
    | _ -> Some expr
    end
  | _ -> None

and parseArrayExp p =
  let startPos = p.Parser.startPos in
  Parser.expect Lbracket p;
  let exprs =
    parseCommaDelimitedRegion
      p
      ~grammar:Grammar.ExprList
      ~closing:Rbracket
      ~f:(parseNonSpreadExp ~msg:ErrorMessages.arrayExprSpread)
  in
  Parser.expect Rbracket p;
  Ast_helper.Exp.array ~loc:(mkLoc startPos p.prevEndPos) exprs

(* TODO: check attributes in the case of poly type vars,
 * might be context dependend: parseFieldDeclaration (see ocaml) *)
and parsePolyTypeExpr p =
  let startPos = p.Parser.startPos in
  match p.Parser.token with
  | SingleQuote ->
    let vars = parseTypeVarList p in
    begin match vars with
    | _v1::_v2::_ ->
      Parser.expect Dot p;
      let typ = parseTypExpr p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Typ.poly ~loc vars typ
    | [var] ->
      begin match p.Parser.token with
      | Dot ->
        Parser.next p;
        let typ = parseTypExpr p in
        let loc = mkLoc startPos p.prevEndPos in
        Ast_helper.Typ.poly ~loc vars typ
      | EqualGreater ->
        Parser.next p;
        let typ = Ast_helper.Typ.var ~loc:var.loc var.txt in
        let returnType = parseTypExpr ~alias:false p in
        let loc = mkLoc typ.Parsetree.ptyp_loc.loc_start p.prevEndPos in
        Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ returnType
      | _ ->
        Ast_helper.Typ.var ~loc:var.loc var.txt
      end
    | _ -> assert false
    end
  | _ ->
    parseTypExpr p

(* 'a 'b 'c *)
and parseTypeVarList p =
  let rec loop p vars =
    match p.Parser.token with
    | SingleQuote ->
      Parser.next p;
      let (lident, loc) = parseLident p in
      let var = Location.mkloc lident loc in
      loop p (var::vars)
    | _ ->
      List.rev vars
  in
  loop p []

and parseLidentList p =
  let rec loop p ls =
    match p.Parser.token with
    | Lident lident ->
      let loc = mkLoc p.startPos p.endPos in
      Parser.next p;
      loop p ((Location.mkloc lident loc)::ls)
    | _ ->
      List.rev ls
  in
  loop p []

and parseAtomicTypExpr ~attrs p =
  Parser.leaveBreadcrumb p Grammar.AtomicTypExpr;
  let startPos = p.Parser.startPos in
  let typ = match p.Parser.token with
  | SingleQuote ->
    Parser.next p;
    let (ident, loc) = parseIdent ~msg:ErrorMessages.typeVar ~startPos:p.startPos p in
    Ast_helper.Typ.var ~loc ~attrs ident
  | Underscore ->
    let endPos = p.endPos in
    Parser.next p;
    Ast_helper.Typ.any ~loc:(mkLoc startPos endPos) ~attrs ()
  | Lparen ->
    Parser.next p;
    begin match p.Parser.token with
    | Rparen ->
      Parser.next p;
      let loc = mkLoc startPos p.prevEndPos in
      let unitConstr = Location.mkloc (Longident.Lident "unit") loc in
      Ast_helper.Typ.constr ~attrs unitConstr []
    | _ ->
      let t = parseTypExpr p in
      begin match p.token with
      | Comma ->
        Parser.next p;
        parseTupleType ~attrs ~first:t ~startPos p
      | _ ->
        Parser.expect Rparen p;
        {t with
          ptyp_loc = mkLoc startPos p.prevEndPos;
          ptyp_attributes = List.concat [attrs; t.ptyp_attributes]}
      end
    end
  | Lbracket ->
    parsePolymorphicVariantType ~attrs p
  | Uident _ | Lident _ ->
    let constr = parseValuePath p in
    let args =  parseTypeConstructorArgs ~constrName:constr p in
    Ast_helper.Typ.constr ~loc:(mkLoc startPos p.prevEndPos) ~attrs constr args
  | Module ->
    Parser.next p;
    Parser.expect Lparen p;
    let packageType = parsePackageType ~startPos ~attrs p in
    Parser.expect Rparen p;
    {packageType with ptyp_loc = mkLoc startPos p.prevEndPos}
  | Percent ->
    let extension = parseExtension p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Typ.extension ~attrs ~loc extension
  | Lbrace ->
    parseBsObjectType ~attrs p
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    begin match skipTokensAndMaybeRetry p ~isStartOfGrammar:Grammar.isAtomicTypExprStart with
    | Some () ->
      parseAtomicTypExpr ~attrs p
    | None ->
      Parser.err ~startPos:p.prevEndPos p (Diagnostics.unexpected token p.breadcrumbs);
      Recover.defaultType()
    end
  in
  Parser.eatBreadcrumb p;
  typ

(* package-type	::=
    | modtype-path
    ∣ modtype-path with package-constraint  { and package-constraint }
 *)
and parsePackageType ~startPos ~attrs p =
  let modTypePath = parseModuleLongIdent ~lowercase:true p in
  begin match p.Parser.token with
  | With ->
    Parser.next p;
    let constraints = parsePackageConstraints p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Typ.package ~loc ~attrs modTypePath constraints
  | _ ->
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Typ.package ~loc ~attrs modTypePath []
  end

(* package-constraint  { and package-constraint } *)
and parsePackageConstraints p =
  let first =
    Parser.expect Typ p;
    let typeConstr = parseValuePath p in
    Parser.expect Equal p;
    let typ = parseTypExpr p in
    (typeConstr, typ)
  in
  let rest = parseRegion
    ~grammar:Grammar.PackageConstraint
    ~f:parsePackageConstraint
    p
  in
  first::rest

(* and type typeconstr = typexpr *)
and parsePackageConstraint p =
  match p.Parser.token with
  | And ->
    Parser.next p;
    Parser.expect Typ p;
    let typeConstr = parseValuePath p in
    Parser.expect Equal p;
    let typ = parseTypExpr p in
    Some (typeConstr, typ)
  | _ -> None

and parseBsObjectType ~attrs p =
  let startPos = p.Parser.startPos in
  Parser.expect Lbrace p;
  let closedFlag = match p.token with
  | DotDot -> Parser.next p; Asttypes.Open
  | Dot -> Parser.next p; Asttypes.Closed
  | _ -> Asttypes.Closed
  in
  let fields =
    parseCommaDelimitedRegion
      ~grammar:Grammar.StringFieldDeclarations
      ~closing:Rbrace
      ~f:parseStringFieldDeclaration
      p
  in
  Parser.expect Rbrace p;
  let loc = mkLoc startPos p.prevEndPos in
  makeBsObjType ~attrs ~loc ~closed:closedFlag fields

(* TODO: check associativity in combination with attributes *)
and parseTypeAlias p typ =
  match p.Parser.token with
  | As ->
    Parser.next p;
    Parser.expect SingleQuote p;
    let (ident, _loc) = parseLident p in
    (* TODO: how do we parse attributes here? *)
    Ast_helper.Typ.alias ~loc:(mkLoc typ.Parsetree.ptyp_loc.loc_start p.prevEndPos) typ ident
  | _ -> typ


(* type_parameter ::=
  *  | type_expr
  *  | ~ident: type_expr
  *  | ~ident: type_expr=?
  *
  * note:
  *  | attrs ~ident: type_expr    -> attrs are on the arrow
  *  | attrs type_expr            -> attrs are here part of the type_expr
  *
  * uncurried_type_parameter ::=
  *  | . type_parameter
  *)
and parseTypeParameter p =
  if (
    p.Parser.token = Token.Tilde ||
    p.token = Dot ||
    Grammar.isTypExprStart p.token
  ) then (
    let startPos = p.Parser.startPos in
    let uncurried = Parser.optional p Dot in
    let attrs = parseAttributes p in
    match p.Parser.token with
    | Tilde ->
      Parser.next p;
      let (name, _loc) = parseLident p in
      Parser.expect ~grammar:Grammar.TypeExpression Colon p;
      let typ = parseTypExpr p in
      begin match p.Parser.token with
      | Equal ->
        Parser.next p;
        Parser.expect Question p;
        Some (uncurried, attrs, Asttypes.Optional name, typ, startPos)
      | _ ->
        Some (uncurried, attrs, Asttypes.Labelled name, typ, startPos)
      end
    | Lident _ ->
      let (name, loc) = parseLident p in
      begin match p.token with
      | Colon ->
        let () =
          let error = Diagnostics.message
            ("Parameter names start with a `~`, like: ~" ^ name)
          in
          Parser.err ~startPos:loc.loc_start ~endPos:loc.loc_end p error
        in
        Parser.next p;
        let typ = parseTypExpr p in
        begin match p.Parser.token with
        | Equal ->
          Parser.next p;
          Parser.expect Question p;
          Some (uncurried, attrs, Asttypes.Optional name, typ, startPos)
        | _ ->
          Some (uncurried, attrs, Asttypes.Labelled name, typ, startPos)
        end
      | _ ->
        let constr = Location.mkloc (Longident.Lident name) loc in
        let args =  parseTypeConstructorArgs ~constrName:constr p in
        let typ = Ast_helper.Typ.constr ~loc:(mkLoc startPos p.prevEndPos) ~attrs constr args
        in

        let typ = parseArrowTypeRest ~es6Arrow:true ~startPos typ p in
        let typ = parseTypeAlias p typ in
        Some (uncurried, [], Asttypes.Nolabel, typ, startPos)
      end
    | _ ->
      let typ = parseTypExpr p in
      let typWithAttributes = {typ with ptyp_attributes = List.concat[attrs; typ.ptyp_attributes]} in
      Some (uncurried, [], Asttypes.Nolabel, typWithAttributes, startPos)
  ) else
    None

(* (int, ~x:string, float) *)
and parseTypeParameters p =
  let startPos = p.Parser.startPos in
  Parser.expect Lparen p;
  match p.Parser.token with
  | Rparen ->
    Parser.next p;
    let loc = mkLoc startPos p.prevEndPos in
    let unitConstr = Location.mkloc (Longident.Lident "unit") loc in
    let typ = Ast_helper.Typ.constr unitConstr [] in
    [(false, [], Asttypes.Nolabel, typ, startPos)]
  | _ ->
    let params =
      parseCommaDelimitedRegion ~grammar:Grammar.TypeParameters ~closing:Rparen ~f:parseTypeParameter p
    in
    Parser.expect Rparen p;
    params

and parseEs6ArrowType ~attrs p =
  let startPos = p.Parser.startPos in
  match p.Parser.token with
  | Tilde ->
    Parser.next p;
    let (name, _loc) = parseLident p in
    Parser.expect ~grammar:Grammar.TypeExpression Colon p;
    let typ = parseTypExpr ~alias:false ~es6Arrow:false p in
    let arg = match p.Parser.token with
    | Equal ->
      Parser.next p;
      Parser.expect Question p;
      Asttypes.Optional name
    | _ ->
      Asttypes.Labelled name
    in
    Parser.expect EqualGreater p;
    let returnType = parseTypExpr ~alias:false p in
    Ast_helper.Typ.arrow ~attrs arg typ returnType
  | _ ->
    let parameters = parseTypeParameters p in
    Parser.expect EqualGreater p;
    let returnType = parseTypExpr ~alias:false p in
    let endPos = p.prevEndPos in
    let typ = List.fold_right (fun (uncurried, attrs, argLbl, typ, startPos) t ->
      let attrs = if uncurried then uncurryAttr::attrs else attrs in
      Ast_helper.Typ.arrow ~loc:(mkLoc startPos endPos) ~attrs argLbl typ t
    ) parameters returnType
    in
    {typ with
      ptyp_attributes = List.concat [typ.ptyp_attributes; attrs];
      ptyp_loc = mkLoc startPos p.prevEndPos}

(*
 * typexpr ::=
 *  | 'ident
 *  | _
 *  | (typexpr)
 *  | typexpr => typexpr            --> es6 arrow
 *  | (typexpr, typexpr) => typexpr --> es6 arrow
 *  | /typexpr, typexpr, typexpr/  --> tuple
 *  | typeconstr
 *  | typeconstr<typexpr>
 *  | typeconstr<typexpr, typexpr,>
 *  | typexpr as 'ident
 *  | %attr-id                      --> extension
 *  | %attr-id(payload)             --> extension
 *
 * typeconstr ::=
 *  | lident
 *  | uident.lident
 *  | uident.uident.lident     --> long module path
 *)
and parseTypExpr ?attrs ?(es6Arrow=true) ?(alias=true) p =
  (* Parser.leaveBreadcrumb p Grammar.TypeExpression; *)
  let startPos = p.Parser.startPos in
  let attrs = match attrs with
    | Some attrs ->
      attrs
    | None ->
      parseAttributes p in
  let typ = if es6Arrow && isEs6ArrowType p then
    parseEs6ArrowType ~attrs p
  else
    let typ = parseAtomicTypExpr ~attrs p in
    parseArrowTypeRest ~es6Arrow ~startPos typ p
  in
  let typ = if alias then parseTypeAlias p typ else typ in
  (* Parser.eatBreadcrumb p; *)
  typ

and parseArrowTypeRest ~es6Arrow ~startPos typ p =
  match p.Parser.token with
  | (EqualGreater | MinusGreater) as token when es6Arrow == true ->
    (* error recovery *)
    if token = MinusGreater then (
      Parser.expect EqualGreater p;
    );
    Parser.next p;
    let returnType = parseTypExpr ~alias:false p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ returnType
  | _ -> typ

and parseTypExprRegion p =
  if Grammar.isTypExprStart p.Parser.token then
    Some (parseTypExpr p)
  else
    None

and parseTupleType ~attrs ~first ~startPos p =
  let typexprs =
    parseCommaDelimitedRegion
      ~grammar:Grammar.TypExprList
      ~closing:Rparen
      ~f:parseTypExprRegion
      p
  in
  Parser.expect Rparen p;
  let tupleLoc = mkLoc startPos p.prevEndPos in
  Ast_helper.Typ.tuple ~attrs ~loc:tupleLoc (first::typexprs)

and parseTypeConstructorArgRegion p =
  if Grammar.isTypExprStart p.Parser.token then
    Some (parseTypExpr p)
  else if p.token = LessThan then (
    Parser.next p;
    parseTypeConstructorArgRegion p
  ) else
    None

(* Js.Nullable.value<'a> *)
and parseTypeConstructorArgs ~constrName p =
  let opening = p.Parser.token in
  let openingStartPos = p.startPos in
  match opening with
  | LessThan | Lparen ->
    Scanner.setDiamondMode p.scanner;
    Parser.next p;
    let typeArgs =
      (* TODO: change Grammar.TypExprList to TypArgList!!! Why did I wrote this? *)
      parseCommaDelimitedRegion
        ~grammar:Grammar.TypExprList
        ~closing:GreaterThan
        ~f:parseTypeConstructorArgRegion
        p
    in
    let () = match p.token with
    | Rparen when opening = Token.Lparen ->
      let typ = Ast_helper.Typ.constr constrName typeArgs in
      let msg =
        Doc.breakableGroup ~forceBreak:true (
          Doc.concat [
            Doc.text "Type parameters require angle brackets:";
            Doc.indent (
              Doc.concat [
                Doc.line;
                NapkinscriptPrinter.printTypExpr typ CommentTable.empty;
              ]
            )
          ]
        ) |> Doc.toString ~width:80
      in
      Parser.err ~startPos:openingStartPos p (Diagnostics.message msg);
      Parser.next p
    | _ ->
      Parser.expect GreaterThan p
    in
    Scanner.popMode p.scanner Diamond;
    typeArgs
  | _ -> []

(* string-field-decl ::=
 *  | string: poly-typexpr
 *  | attributes string-field-decl *)
and parseStringFieldDeclaration p =
  let attrs = parseAttributes p in
  match p.Parser.token with
  | String name ->
    let nameStartPos = p.startPos in
    let nameEndPos = p.endPos in
    Parser.next p;
    let fieldName = Location.mkloc name (mkLoc nameStartPos nameEndPos) in
    Parser.expect ~grammar:Grammar.TypeExpression Colon p;
    let typ = parsePolyTypeExpr p in
    Some(Parsetree.Otag (fieldName, attrs, typ))
  | _token ->
    None

(* field-decl	::=
 *  | [mutable] field-name : poly-typexpr
 *  | attributes field-decl *)
and parseFieldDeclaration p =
  let startPos = p.Parser.startPos in
  let attrs = parseAttributes p in
  let mut = if Parser.optional p Token.Mutable then
    Asttypes.Mutable
  else
    Asttypes.Immutable
  in
  let (lident, loc) = match p.token with
  | _ -> parseLident p
  in
  let name = Location.mkloc lident loc in
  let typ = match p.Parser.token with
  | Colon ->
    Parser.next p;
    parsePolyTypeExpr p
  | _ ->
    Ast_helper.Typ.constr ~loc:name.loc {name with txt = Lident name.txt} []
  in
  let loc = mkLoc startPos typ.ptyp_loc.loc_end in
  Ast_helper.Type.field ~attrs ~loc ~mut name typ


and parseFieldDeclarationRegion p =
  let startPos = p.Parser.startPos in
  let attrs = parseAttributes p in
  let mut = if Parser.optional p Token.Mutable then
    Asttypes.Mutable
  else
    Asttypes.Immutable
  in
  match p.token with
  | Lident _ ->
    let (lident, loc) = parseLident p in
    let name = Location.mkloc lident loc in
    let typ = match p.Parser.token with
    | Colon ->
      Parser.next p;
      parsePolyTypeExpr p
    | _ ->
      Ast_helper.Typ.constr ~loc:name.loc {name with txt = Lident name.txt} []
    in
    let loc = mkLoc startPos typ.ptyp_loc.loc_end in
    Some(Ast_helper.Type.field ~attrs ~loc ~mut name typ)
  | _ ->
    None

(* record-decl ::=
 *  | { field-decl }
 *  | { field-decl, field-decl }
 *  | { field-decl, field-decl, field-decl, }
 *)
and parseRecordDeclaration p =
  Parser.leaveBreadcrumb p Grammar.RecordDecl;
  Parser.expect Lbrace p;
  let rows =
    parseCommaDelimitedRegion
      ~grammar:Grammar.RecordDecl
      ~closing:Rbrace
      ~f:parseFieldDeclarationRegion
      p
  in
  Parser.expect Rbrace p;
  Parser.eatBreadcrumb p;
  rows

(* constr-args ::=
 *  | (typexpr)
 *  | (typexpr, typexpr)
 *  | (typexpr, typexpr, typexpr,)
 *  | (record-decl)
 *
 * TODO: should we overparse inline-records in every position?
 * Give a good error message afterwards?
 *)
and parseConstrDeclArgs p =
  let constrArgs = match p.Parser.token with
  | Lparen ->
    Parser.next p;
    (* TODO: this could use some cleanup/stratification *)
    begin match p.Parser.token with
    | Lbrace ->
      let lbrace = p.startPos in
      Parser.next p;
      let startPos = p.Parser.startPos in
      begin match p.Parser.token with
      | DotDot | Dot ->
        let closedFlag = match p.token with
        | DotDot -> Parser.next p; Asttypes.Open
        | Dot -> Parser.next p; Asttypes.Closed
        | _ -> Asttypes.Closed
        in
        let fields =
          parseCommaDelimitedRegion
            ~grammar:Grammar.StringFieldDeclarations
            ~closing:Rbrace
            ~f:parseStringFieldDeclaration
            p
        in
        Parser.expect Rbrace p;
        let loc = mkLoc startPos p.prevEndPos in
        let typ = makeBsObjType ~attrs:[] ~loc ~closed:closedFlag fields in
        Parser.optional p Comma |> ignore;
        let moreArgs =
          parseCommaDelimitedRegion
          ~grammar:Grammar.TypExprList
          ~closing:Rparen
          ~f:parseTypExprRegion
          p
        in
        Parser.expect Rparen p;
        Parsetree.Pcstr_tuple (typ::moreArgs)
      | _ ->
        let attrs = parseAttributes p in
        begin match p.Parser.token with
        | String _  ->
          let closedFlag = Asttypes.Closed in
          let fields = match attrs with
          | [] ->
            parseCommaDelimitedRegion
              ~grammar:Grammar.StringFieldDeclarations
              ~closing:Rbrace
              ~f:parseStringFieldDeclaration
              p
          | attrs ->
            let first =
              Parser.leaveBreadcrumb p Grammar.StringFieldDeclarations;
              let field = match parseStringFieldDeclaration p with
              | Some field -> field
              | None -> assert false
              in
              (* parse comma after first *)
              let () = match p.Parser.token with
              | Rbrace | Eof -> ()
              | Comma -> Parser.next p
              | _ -> Parser.expect Comma p
              in
              Parser.eatBreadcrumb p;
              begin match field with
              | Parsetree.Otag (label, _, ct) -> Parsetree.Otag (label, attrs, ct)
              | Oinherit ct -> Oinherit ct
              end
            in
            first::(
              parseCommaDelimitedRegion
                ~grammar:Grammar.StringFieldDeclarations
                ~closing:Rbrace
                ~f:parseStringFieldDeclaration
                p
            ) in
            Parser.expect Rbrace p;
            let loc = mkLoc startPos p.prevEndPos in
            let typ = makeBsObjType ~attrs:[]  ~loc ~closed:closedFlag fields in
            Parser.optional p Comma |> ignore;
            let moreArgs =
              parseCommaDelimitedRegion
                ~grammar:Grammar.TypExprList
                ~closing:Rparen
                ~f:parseTypExprRegion p
            in
            Parser.expect Rparen p;
            Parsetree.Pcstr_tuple (typ::moreArgs)
          | _ ->
            let fields = match attrs with
            | [] ->
              parseCommaDelimitedRegion
                ~grammar:Grammar.FieldDeclarations
                ~closing:Rbrace
                ~f:parseFieldDeclarationRegion
                p
            | attrs ->
              let first =
                let field = parseFieldDeclaration p in
                Parser.expect Comma p;
                {field with Parsetree.pld_attributes = attrs}
              in
              first::(
                parseCommaDelimitedRegion
                  ~grammar:Grammar.FieldDeclarations
                  ~closing:Rbrace
                  ~f:parseFieldDeclarationRegion
                  p
              )
            in
            let () = match fields with
            | [] -> Parser.err ~startPos:lbrace p (
                Diagnostics.message "An inline record declaration needs at least one field"
              )
            | _ -> ()
            in
            Parser.expect Rbrace p;
            Parser.optional p Comma |> ignore;
            Parser.expect Rparen p;
            Parsetree.Pcstr_record fields
          end
      end
      | _ ->
        let args =
          parseCommaDelimitedRegion
            ~grammar:Grammar.TypExprList
            ~closing:Rparen
            ~f:parseTypExprRegion
            p
        in
        Parser.expect Rparen p;
        Parsetree.Pcstr_tuple args
     end
  | _ -> Pcstr_tuple []
  in
  let res = match p.Parser.token with
  | Colon ->
    Parser.next p;
    Some (parseTypExpr p)
  | _ -> None
  in
  (constrArgs, res)

(* constr-decl ::=
 *  | constr-name
 *  | attrs constr-name
 *  | constr-name const-args
 *  | attrs constr-name const-args *)
 and parseTypeConstructorDeclarationWithBar p =
  match p.Parser.token with
  | Bar ->
    let startPos = p.Parser.startPos in
    Parser.next p;
    Some (parseTypeConstructorDeclaration ~startPos p)
  | _ -> None

 and parseTypeConstructorDeclaration ~startPos p =
   Parser.leaveBreadcrumb p Grammar.ConstructorDeclaration;
   let attrs = parseAttributes p in
   match p.Parser.token with
   | Uident uident ->
     let uidentLoc = mkLoc p.startPos p.endPos in
     Parser.next p;
     let (args, res) = parseConstrDeclArgs p in
     Parser.eatBreadcrumb p;
     let loc = mkLoc startPos p.prevEndPos in
     Ast_helper.Type.constructor ~loc ~attrs ?res ~args (Location.mkloc uident uidentLoc)
   | t ->
    Parser.err p (Diagnostics.uident t);
    Ast_helper.Type.constructor (Location.mknoloc "_")

 (* [|] constr-decl  { | constr-decl }   *)
 and parseTypeConstructorDeclarations ?first p =
  let firstConstrDecl = match first with
  | None ->
    let startPos = p.Parser.startPos in
    ignore (Parser.optional p Token.Bar);
    parseTypeConstructorDeclaration ~startPos p
  | Some firstConstrDecl ->
    firstConstrDecl
  in
  firstConstrDecl::(
    parseRegion
      ~grammar:Grammar.ConstructorDeclaration
      ~f:parseTypeConstructorDeclarationWithBar
      p
  )

(*
 * type-representation ::=
 *  ∣	 = [ | ] constr-decl  { | constr-decl }
 *  ∣	 = private [ | ] constr-decl  { | constr-decl }
 *  |  = |
 *  ∣	 = private |
 *  ∣	 = record-decl
 *  ∣	 = private record-decl
 *  |  = ..
 *)
and parseTypeRepresentation p =
  Parser.leaveBreadcrumb p Grammar.TypeRepresentation;
  (* = consumed *)
  let privateFlag =
    if Parser.optional p Token.Private
    then Asttypes.Private
    else Asttypes.Public
  in
  let kind = match p.Parser.token with
  | Bar | Uident _ ->
    Parsetree.Ptype_variant (parseTypeConstructorDeclarations p)
  | Lbrace ->
    Parsetree.Ptype_record (parseRecordDeclaration p)
  | DotDot ->
    Parser.next p;
    Ptype_open
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    (* TODO: I have no idea if this is even remotely a good idea *)
    Parsetree.Ptype_variant []
  in
  Parser.eatBreadcrumb p;
  (privateFlag, kind)

(* type-param	::=
 *  | variance 'lident
 *  | variance 'uident
 *  | variance _
 *
 * variance ::=
 *   | +
 *   | -
 *   | (* empty *)
 *)
and parseTypeParam p =
  let variance = match p.Parser.token with
  | Plus -> Parser.next p; Asttypes.Covariant
  | Minus -> Parser.next p; Contravariant
  | _ -> Invariant
  in
  match p.Parser.token with
  | SingleQuote ->
    Parser.next p;
    let (ident, loc) =
      parseIdent ~msg:ErrorMessages.typeParam ~startPos:p.startPos p in
    Some (Ast_helper.Typ.var ~loc ident, variance)
  | Underscore ->
    let loc = mkLoc p.startPos p.endPos in
    Parser.next p;
    Some (Ast_helper.Typ.any ~loc (), variance)
  | (Uident _ | Lident _) as token ->
    Parser.err p (Diagnostics.message (
      "Type params start with a singlequote: '" ^ (Token.toString token)
    ));
    let (ident, loc) =
      parseIdent ~msg:ErrorMessages.typeParam ~startPos:p.startPos p in
    Some (Ast_helper.Typ.var ~loc ident, variance)
  | _token ->
    None

(* type-params	::=
 *  | <type-param>
 *  ∣	<type-param, type-param>
 *  ∣	<type-param, type-param, type-param>
 *  ∣	<type-param, type-param, type-param,>
 *
 *  TODO: when we have pretty-printer show an error
 *  with the actual code corrected. *)
and parseTypeParams ~parent p =
  let opening = p.Parser.token in
  match opening with
  | LessThan | Lparen when p.startPos.pos_lnum == p.prevEndPos.pos_lnum ->
    Scanner.setDiamondMode p.scanner;
    let openingStartPos = p.startPos in
    Parser.leaveBreadcrumb p Grammar.TypeParams;
    Parser.next p;
    let params =
      parseCommaDelimitedRegion
        ~grammar:Grammar.TypeParams
        ~closing:GreaterThan
        ~f:parseTypeParam
        p
    in
    let () = match p.token with
    | Rparen when opening = Token.Lparen ->
      let msg =
        Doc.breakableGroup ~forceBreak:true (
          Doc.concat [
            Doc.text "Type parameters require angle brackets:";
            Doc.indent (
              Doc.concat [
                Doc.line;
                Doc.concat [
                  NapkinscriptPrinter.printLongident parent.Location.txt;
                  NapkinscriptPrinter.printTypeParams params CommentTable.empty;
                ]
              ]
            )
          ]
        ) |> Doc.toString ~width:80
      in
      Parser.err ~startPos:openingStartPos p (Diagnostics.message msg);
      Parser.next p
    | _ ->
      Parser.expect GreaterThan p
    in
    Scanner.popMode p.scanner Diamond;
    Parser.eatBreadcrumb p;
    params
  | _ -> []

(* type-constraint	::=	constraint ' ident =  typexpr *)
and parseTypeConstraint p =
  let startPos = p.Parser.startPos in
  match p.Parser.token with
  | Token.Constraint ->
    Parser.next p;
    Parser.expect SingleQuote p;
    begin match p.Parser.token with
    | Lident ident ->
      let identLoc = mkLoc startPos p.endPos in
      Parser.next p;
      Parser.expect Equal p;
      let typ = parseTypExpr p in
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Typ.var ~loc:identLoc ident, typ, loc)
    | t ->
      Parser.err p (Diagnostics.lident t);
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Typ.any (), parseTypExpr p, loc)
    end
  | _ -> None

(* type-constraints ::=
 *  | (* empty *)
 *  | type-constraint
 *  | type-constraint type-constraint
 *  | type-constraint type-constraint type-constraint (* 0 or more *)
 *)
and parseTypeConstraints p =
  parseRegion
    ~grammar:Grammar.TypeConstraint
    ~f:parseTypeConstraint
    p

and parseTypeEquationOrConstrDecl p =
  let uidentStartPos = p.Parser.startPos in
  match p.Parser.token with
  | Uident uident ->
    Parser.next p;
    begin match p.Parser.token with
    | Dot ->
      Parser.next p;
      let typeConstr =
        parseValuePathTail p uidentStartPos (Longident.Lident uident)
      in
      let loc = mkLoc uidentStartPos p.prevEndPos in
      let typ = parseTypeAlias p (
        Ast_helper.Typ.constr ~loc typeConstr (parseTypeConstructorArgs ~constrName:typeConstr p)
      ) in
      begin match p.token with
      | Equal ->
        Parser.next p;
        let (priv, kind) = parseTypeRepresentation p in
        (Some typ, priv, kind)
      | EqualGreater ->
        Parser.next p;
        let returnType = parseTypExpr ~alias:false p in
        let loc = mkLoc uidentStartPos p.prevEndPos in
        let arrowType = Ast_helper.Typ.arrow ~loc Asttypes.Nolabel typ returnType in
        let typ = parseTypeAlias p arrowType in
        (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
      | _ -> (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
      end
    | _ ->
      let uidentEndPos = p.endPos in
      let (args, res) = parseConstrDeclArgs p in
      let first = Some (
        let uidentLoc = mkLoc uidentStartPos uidentEndPos in
        Ast_helper.Type.constructor
          ~loc:(mkLoc uidentStartPos p.prevEndPos)
          ?res
          ~args
          (Location.mkloc uident uidentLoc)
      ) in
      (None, Asttypes.Public, Parsetree.Ptype_variant (parseTypeConstructorDeclarations p ?first))
    end
  | t ->
    Parser.err p (Diagnostics.uident t);
    (* TODO: is this a good idea? *)
    (None, Asttypes.Public, Parsetree.Ptype_abstract)

and parseRecordOrBsObjectDecl p =
  let startPos = p.Parser.startPos in
  Parser.expect Lbrace p;
  match p.Parser.token with
  | DotDot | Dot ->
    let closedFlag = match p.token with
    | DotDot -> Parser.next p; Asttypes.Open
    | Dot -> Parser.next p; Asttypes.Closed
    | _ -> Asttypes.Closed
    in
    let fields =
      parseCommaDelimitedRegion
        ~grammar:Grammar.StringFieldDeclarations
        ~closing:Rbrace
        ~f:parseStringFieldDeclaration
        p
    in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    let typ =
      makeBsObjType ~attrs:[] ~loc ~closed:closedFlag fields
      |> parseTypeAlias p
    in
    let typ = parseArrowTypeRest ~es6Arrow:true ~startPos typ p in
    (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
  | _ ->
    let attrs = parseAttributes p in
    begin match p.Parser.token with
    | String _  ->
      let closedFlag = Asttypes.Closed in
      let fields = match attrs with
      | [] ->
        parseCommaDelimitedRegion
          ~grammar:Grammar.StringFieldDeclarations
          ~closing:Rbrace
          ~f:parseStringFieldDeclaration
          p
      | attrs ->
        let first =
          Parser.leaveBreadcrumb p Grammar.StringFieldDeclarations;
          let field = match parseStringFieldDeclaration p with
          | Some field -> field
          | None -> assert false
          in
          (* parse comma after first *)
          let () = match p.Parser.token with
          | Rbrace | Eof -> ()
          | Comma -> Parser.next p
          | _ -> Parser.expect Comma p
          in
          Parser.eatBreadcrumb p;
          begin match field with
          | Parsetree.Otag (label, _, ct) -> Parsetree.Otag (label, attrs, ct)
          | Oinherit ct -> Oinherit ct
          end
        in
        first::(
          parseCommaDelimitedRegion
            ~grammar:Grammar.StringFieldDeclarations
            ~closing:Rbrace
            ~f:parseStringFieldDeclaration
            p
        )
        in
        Parser.expect Rbrace p;
        let loc = mkLoc startPos p.prevEndPos in
        let typ =
          makeBsObjType ~attrs:[] ~loc ~closed:closedFlag fields |> parseTypeAlias p
        in
        let typ = parseArrowTypeRest ~es6Arrow:true ~startPos typ p in
        (Some typ, Asttypes.Public, Parsetree.Ptype_abstract)
    | _ ->
      Parser.leaveBreadcrumb p Grammar.RecordDecl;
      let fields = match attrs with
      | [] ->
        parseCommaDelimitedRegion
          ~grammar:Grammar.FieldDeclarations
          ~closing:Rbrace
          ~f:parseFieldDeclarationRegion
          p
      | attr::_ as attrs ->
        let first =
          let field = parseFieldDeclaration p in
          Parser.optional p Comma |> ignore;
          {field with
            Parsetree.pld_attributes = attrs;
            pld_loc = {
              field.Parsetree.pld_loc with loc_start =
                (attr |> fst).loc.loc_start
            }
          }
        in
        first::(
          parseCommaDelimitedRegion
            ~grammar:Grammar.FieldDeclarations
            ~closing:Rbrace
            ~f:parseFieldDeclarationRegion
            p
        )
      in
      let () = match fields with
      | [] -> Parser.err ~startPos p (
          Diagnostics.message "A record needs at least one field"
        )
      | _ -> ()
      in
      Parser.expect Rbrace p;
      Parser.eatBreadcrumb p;
      (None, Asttypes.Public, Parsetree.Ptype_record fields)
    end

and parsePrivateEqOrRepr p =
  Parser.expect Private p;
  match p.Parser.token with
  | Lbrace ->
    let (manifest, _ ,kind) = parseRecordOrBsObjectDecl p in
    (manifest, Asttypes.Private, kind)
  | Uident _ ->
    let (manifest, _, kind) = parseTypeEquationOrConstrDecl p in
    (manifest, Asttypes.Private, kind)
  | Bar | DotDot ->
    let (_, kind) = parseTypeRepresentation p in
    (None, Asttypes.Private, kind)
  | t when Grammar.isTypExprStart t ->
    (Some (parseTypExpr p), Asttypes.Private, Parsetree.Ptype_abstract)
  | _ ->
    let (_, kind) = parseTypeRepresentation p in
    (None, Asttypes.Private, kind)

(*
  polymorphic-variant-type	::=
                            | [ tag-spec-first  { | tag-spec } ]
                            | [> [ tag-spec ]  { | tag-spec } ]
                            | [< [|] tag-spec-full  { | tag-spec-full }  [ > { `tag-name }+ ] ]

            tag-spec-first	::=	`tag-name  [ of typexpr ]
                            |	[ typexpr ] |  tag-spec

                  tag-spec	::=	`tag-name  [ of typexpr ]
                            |	typexpr

              tag-spec-full	::=	`tag-name  [ of [&] typexpr  { & typexpr } ]
                             |	typexpr
*)
and parsePolymorphicVariantType ~attrs p =
  let startPos = p.Parser.startPos in
  Parser.expect Lbracket p;
  match p.token with
  | GreaterThan ->
    Parser.next p;
    let rowFields =
      begin match p.token with
      | Rbracket ->
        []
      | Bar ->
        parseTagSpecs p
      | _ ->
        let rowField = parseTagSpec p in
        rowField :: parseTagSpecs p
      end
    in
    let variant =
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Typ.variant ~attrs ~loc rowFields Open None in
    Parser.expect Rbracket p;
    variant
  | LessThan ->
    Parser.next p;
    Parser.optional p Bar |> ignore;
    let rowField = parseTagSpecFull p in
    let rowFields = parseTagSpecFulls p in
    let tagNames =
      if p.token == GreaterThan
      then begin
        Parser.next p;
        let rec loop p = match p.Parser.token with
          | Rbracket -> []
          | _ ->
            let (ident, _loc) = parseHashIdent ~startPos:p.startPos p in
            ident :: loop p
        in
        loop p
      end
      else [] in
    let variant =
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Typ.variant ~attrs ~loc (rowField :: rowFields) Closed (Some tagNames) in
    Parser.expect Rbracket p;
    variant
  | _ ->
    let rowFields1 = parseTagSpecFirst p in
    let rowFields2 = parseTagSpecs p in
    let variant =
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Typ.variant ~attrs ~loc (rowFields1 @ rowFields2) Closed None in
    Parser.expect Rbracket p;
    variant

and parseTagSpecFulls p =
  match p.Parser.token with
  | Rbracket ->
    []
  | GreaterThan ->
    []
  | Bar ->
    Parser.next p;
    let rowField = parseTagSpecFull p in
    rowField ::parseTagSpecFulls p
  | _ ->
    []

and parseTagSpecFull p =
  let attrs = parseAttributes p in
  match p.Parser.token with
  | Hash ->
    parsePolymorphicVariantTypeSpecHash ~attrs ~full:true p
  | _ ->
    let typ = parseTypExpr ~attrs p in
    Parsetree.Rinherit typ

and parseTagSpecs p =
  match p.Parser.token with
  | Bar ->
    Parser.next p;
    let rowField = parseTagSpec p in
    rowField :: parseTagSpecs p
  | _ ->
    []

and parseTagSpec p =
  let attrs = parseAttributes p in
  match p.Parser.token with
  | Hash ->
    parsePolymorphicVariantTypeSpecHash ~attrs ~full:false p
  | _ ->
    let typ = parseTypExpr ~attrs p in
    Parsetree.Rinherit typ

and parseTagSpecFirst p =
  let attrs = parseAttributes p in
  match p.Parser.token with
  | Bar ->
    Parser.next p;
    [parseTagSpec p]
  | Hash ->
    [parsePolymorphicVariantTypeSpecHash ~attrs ~full:false p]
  | _ ->
    let typ = parseTypExpr ~attrs p in
    Parser.expect Bar p;
    [Parsetree.Rinherit typ; parseTagSpec p]

and parsePolymorphicVariantTypeSpecHash ~attrs ~full p : Parsetree.row_field =
  let startPos = p.Parser.startPos in
  let (ident, loc) = parseHashIdent ~startPos p in
  let rec loop p =
    match p.Parser.token with
    | Band when full ->
      Parser.next p;
      let rowField = parsePolymorphicVariantTypeArgs p in
      rowField :: loop p
    | _ ->
      []
  in
  let firstTuple, tagContainsAConstantEmptyConstructor =
    match p.Parser.token with
    | Band when full ->
      Parser.next p;
      [parsePolymorphicVariantTypeArgs p], true
    | Lparen ->
      [parsePolymorphicVariantTypeArgs p], false
    | _ ->
      [], true
  in
  let tuples = firstTuple @ loop p in
  Parsetree.Rtag (
    Location.mkloc ident loc,
    attrs,
    tagContainsAConstantEmptyConstructor,
    tuples
  )

and parsePolymorphicVariantTypeArgs p =
  let startPos = p.Parser.startPos in
  Parser.expect Lparen p;
  let args = parseCommaDelimitedRegion
    ~grammar:Grammar.TypExprList
    ~closing:Rparen
    ~f:parseTypExprRegion
    p
  in
  Parser.expect Rparen p;
  let attrs = [] in
  let loc = mkLoc startPos p.prevEndPos in
  match args with
  | [{ptyp_desc = Ptyp_tuple _} as typ] as types ->
    if p.mode = ParseForTypeChecker then
      typ
    else
      Ast_helper.Typ.tuple ~loc ~attrs types
  | [typ] -> typ
  | types -> Ast_helper.Typ.tuple ~loc ~attrs types

and parseTypeEquationAndRepresentation p =
  match p.Parser.token with
  | Equal | Bar as token ->
    if token = Bar then Parser.expect Equal p;
    Parser.next p;
    begin match p.Parser.token with
    | Uident _ ->
      parseTypeEquationOrConstrDecl p
    | Lbrace ->
      parseRecordOrBsObjectDecl p
    | Private ->
      parsePrivateEqOrRepr p
    | Bar | DotDot ->
      let (priv, kind) = parseTypeRepresentation p in
      (None, priv, kind)
    | _ ->
      let manifest = Some (parseTypExpr p) in
      begin match p.Parser.token with
      | Equal ->
        Parser.next p;
        let (priv, kind) = parseTypeRepresentation p in
        (manifest, priv, kind)
      | _ ->
        (manifest, Public, Parsetree.Ptype_abstract)
      end
    end
  | _ -> (None, Public, Parsetree.Ptype_abstract)

(* type-definition	::=	type [rec] typedef  { and typedef }
 * typedef	::=	typeconstr-name [type-params] type-information
 * type-information	::=	[type-equation]  [type-representation]  { type-constraint }
 * type-equation	::=	= typexpr *)
and parseTypeDef ~attrs ~startPos p =
  Parser.leaveBreadcrumb p Grammar.TypeDef;
  (* let attrs = match attrs with | Some attrs -> attrs | None -> parseAttributes p in *)
  Parser.leaveBreadcrumb p Grammar.TypeConstrName;
  let (name, loc) = parseLident p in
  let typeConstrName = Location.mkloc name loc in
  Parser.eatBreadcrumb p;
  let params =
    let constrName = Location.mkloc (Longident.Lident name) loc in
    parseTypeParams ~parent:constrName p in
  let typeDef =
    let (manifest, priv, kind) = parseTypeEquationAndRepresentation p in
    let cstrs = parseTypeConstraints p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Type.mk
      ~loc ~attrs ~priv ~kind ~params ~cstrs ?manifest typeConstrName
  in
  Parser.eatBreadcrumb p;
  typeDef

and parseTypeExtension ~params ~attrs ~name p =
  Parser.expect PlusEqual p;
  let priv =
    if Parser.optional p Token.Private
    then Asttypes.Private
    else Asttypes.Public
  in
  let constrStart = p.Parser.startPos in
  Parser.optional p Bar |> ignore;
  let first =
    let (attrs, name, kind) = match p.Parser.token with
    | Bar ->
      Parser.next p;
      parseConstrDef ~parseAttrs:true p
    | _ ->
      parseConstrDef ~parseAttrs:true p
    in
    let loc = mkLoc constrStart p.prevEndPos in
    Ast_helper.Te.constructor ~loc ~attrs name kind
  in
  let rec loop p cs =
    match p.Parser.token with
    | Bar ->
      let startPos = p.Parser.startPos in
      Parser.next p;
      let (attrs, name, kind) = parseConstrDef ~parseAttrs:true p in
      let extConstr =
        Ast_helper.Te.constructor ~attrs ~loc:(mkLoc startPos p.prevEndPos) name kind
      in
      loop p (extConstr::cs)
    | _ ->
      List.rev cs
  in
  let constructors = loop p [first] in
  Ast_helper.Te.mk ~attrs ~params ~priv name constructors

and parseTypeDefinitions ~attrs ~name ~params ~startPos p =
    let typeDef =
      let (manifest, priv, kind) = parseTypeEquationAndRepresentation p in
      let cstrs = parseTypeConstraints p in
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Type.mk
        ~loc ~attrs ~priv ~kind ~params ~cstrs ?manifest
        {name with txt = lidentOfPath name.Location.txt}
    in
    let rec loop p defs =
      let startPos = p.Parser.startPos in
      let attrs = parseAttributesAndBinding p in
      match p.Parser.token with
      | And ->
        Parser.next p;
        let attrs = match p.token with
        | Export ->
          let exportLoc = mkLoc p.startPos p.endPos in
          Parser.next p;
          let genTypeAttr = (Location.mkloc "genType" exportLoc, Parsetree.PStr []) in
          genTypeAttr::attrs
        | _ -> attrs
        in
        let typeDef = parseTypeDef ~attrs ~startPos p in
        loop p (typeDef::defs)
      | _ ->
        List.rev defs
    in
    loop p [typeDef]

(* TODO: decide if we really want type extensions (eg. type x += Blue)
 * It adds quite a bit of complexity that can be avoided,
 * implemented for now. Needed to get a feel for the complexities of
 * this territory of the grammar *)
and parseTypeDefinitionOrExtension ~attrs p =
  let startPos = p.Parser.startPos in
  Parser.expect Token.Typ p;
  let recFlag = match p.token with
    | Rec -> Parser.next p; Asttypes.Recursive
    | Lident "nonrec" ->
      Parser.next p;
      Asttypes.Nonrecursive
    | _ -> Asttypes.Nonrecursive
  in
  let name = parseValuePath p in
  let params = parseTypeParams ~parent:name p in
  match p.Parser.token with
  | PlusEqual ->
    TypeExt(parseTypeExtension ~params ~attrs ~name p)
  | _ ->
    let typeDefs = parseTypeDefinitions ~attrs ~name ~params ~startPos p in
    TypeDef {recFlag; types = typeDefs}

and parsePrimitive p =
  match p.Parser.token with
  | String s -> Parser.next p; Some s
  | _ -> None

and parsePrimitives p =
  match (parseRegion ~grammar:Grammar.Primitive ~f:parsePrimitive p) with
  | [] ->
    let msg = "An external definition should have at least one primitive. Example: \"setTimeout\"" in
    Parser.err p (Diagnostics.message msg);
    []
  | primitives -> primitives

(* external value-name : typexp = external-declaration *)
and parseExternalDef ~attrs ~startPos p =
  Parser.leaveBreadcrumb p Grammar.External;
  Parser.expect Token.External p;
  let (name, loc) = parseLident p in
  let name = Location.mkloc name loc in
  Parser.expect ~grammar:(Grammar.TypeExpression) Colon p;
  let typExpr = parseTypExpr p in
  Parser.expect Equal p;
  let prim = parsePrimitives p in
  let loc = mkLoc startPos p.prevEndPos in
  let vb = Ast_helper.Val.mk ~loc ~attrs ~prim name typExpr in
  Parser.eatBreadcrumb p;
  vb

(* constr-def ::=
 *  | constr-decl
 *  | constr-name = constr
 *
 *  constr-decl ::= constr-name constr-args
 *  constr-name ::= uident
 *  constr      ::= path-uident *)
and parseConstrDef ~parseAttrs p =
  let attrs = if parseAttrs then parseAttributes p else [] in
  let name = match p.Parser.token with
  | Uident name ->
    let loc = mkLoc p.startPos p.endPos in
    Parser.next p;
    Location.mkloc name loc
  | t ->
    Parser.err p (Diagnostics.uident t);
    Location.mknoloc "_"
  in
  let kind = match p.Parser.token with
  | Lparen ->
    let (args, res) = parseConstrDeclArgs p in
    Parsetree.Pext_decl (args, res)
  | Equal ->
    Parser.next p;
    let longident = parseModuleLongIdent ~lowercase:false p in
    Parsetree.Pext_rebind longident
  | _ ->
    Parsetree.Pext_decl (Pcstr_tuple [], None)
  in
  (attrs, name, kind)

(*
 * exception-definition	::=
 *  | exception constr-decl
 *  ∣	exception constr-name = constr
 *
 *  constr-name ::= uident
 *  constr ::= long_uident *)
and parseExceptionDef ~attrs p =
  let startPos = p.Parser.startPos in
  Parser.expect Token.Exception p;
  let (_, name, kind) = parseConstrDef ~parseAttrs:false p in
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Te.constructor ~loc ~attrs name kind

(* module structure on the file level *)
and parseImplementation p : Parsetree.structure =
  parseRegion p ~grammar:Grammar.Implementation ~f:parseStructureItemRegion
  [@@progress (Parser.next, Parser.expect, Parser.checkProgress)]

and parseNewlineOrSemicolonStructure p =
  match p.Parser.token with
  | Semicolon ->
    Parser.next p
  | token when Grammar.isStructureItemStart token ->
    if p.prevEndPos.pos_lnum < p.startPos.pos_lnum then ()
    else
      Parser.err
        ~startPos:p.prevEndPos
        ~endPos: p.endPos
        p
        (Diagnostics.message "consecutive statements on a line must be separated by ';' or a newline")
  | _ -> ()

and parseStructureItemRegion p =
  let startPos = p.Parser.startPos in
  let attrs = parseAttributes p in
  match p.Parser.token with
  | Open ->
    let openDescription = parseOpenDescription ~attrs p in
    parseNewlineOrSemicolonStructure p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Str.open_ ~loc openDescription)
  | Let ->
    let (recFlag, letBindings) = parseLetBindings ~attrs p in
    parseNewlineOrSemicolonStructure p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Str.value ~loc recFlag letBindings)
  | Typ ->
    Parser.beginRegion p;
    begin match parseTypeDefinitionOrExtension ~attrs p with
    | TypeDef {recFlag; types} ->
      parseNewlineOrSemicolonStructure p;
      let loc = mkLoc startPos p.prevEndPos in
      Parser.endRegion p;
      Some (Ast_helper.Str.type_ ~loc recFlag types)
    | TypeExt(ext) ->
      parseNewlineOrSemicolonStructure p;
      let loc = mkLoc startPos p.prevEndPos in
      Parser.endRegion p;
      Some (Ast_helper.Str.type_extension ~loc ext)
    end
  | External ->
    let externalDef = parseExternalDef ~attrs ~startPos p in
    parseNewlineOrSemicolonStructure p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Str.primitive ~loc externalDef)
  | Import ->
    let importDescr = parseJsImport ~startPos ~attrs p in
    parseNewlineOrSemicolonStructure p;
    let loc = mkLoc startPos p.prevEndPos in
    let structureItem = JsFfi.toParsetree importDescr in
    Some {structureItem with pstr_loc = loc}
  | Exception ->
    let exceptionDef = parseExceptionDef ~attrs p in
    parseNewlineOrSemicolonStructure p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Str.exception_ ~loc exceptionDef)
  | Include ->
    let includeStatement = parseIncludeStatement ~attrs p in
    parseNewlineOrSemicolonStructure p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Str.include_ ~loc includeStatement)
  | Export ->
    let structureItem = parseJsExport ~attrs p in
    parseNewlineOrSemicolonStructure p;
    let loc = mkLoc startPos p.prevEndPos in
    Some {structureItem with pstr_loc = loc}
  | Module ->
    let structureItem = parseModuleOrModuleTypeImplOrPackExpr ~attrs p in
    parseNewlineOrSemicolonStructure p;
    let loc = mkLoc startPos p.prevEndPos in
    Some {structureItem with pstr_loc = loc}
  | AtAt ->
    let attr = parseStandaloneAttribute p in
    parseNewlineOrSemicolonStructure p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Str.attribute ~loc attr)
  | PercentPercent ->
    let extension = parseExtension ~moduleLanguage:true p in
    parseNewlineOrSemicolonStructure p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Str.extension ~attrs ~loc extension)
  | token when Grammar.isExprStart token ->
    let prevEndPos = p.Parser.endPos in
    let exp = parseExpr p in
    parseNewlineOrSemicolonStructure p;
    let loc = mkLoc startPos p.prevEndPos in
    Parser.checkProgress ~prevEndPos ~result:(Ast_helper.Str.eval ~loc ~attrs exp) p
  | _ -> None

and parseJsImport ~startPos ~attrs p =
  Parser.expect Token.Import p;
  let importSpec = match p.Parser.token with
  | Token.Lident _ | Token.At ->
    let decl = match parseJsFfiDeclaration p with
    | Some decl -> decl
    | None -> assert false
    in
    JsFfi.Default decl
  | _ -> JsFfi.Spec(parseJsFfiDeclarations p)
  in
  let scope = parseJsFfiScope p in
  let loc = mkLoc startPos p.prevEndPos in
  JsFfi.importDescr ~attrs ~importSpec ~scope ~loc

and parseJsExport ~attrs p =
  let exportStart = p.Parser.startPos in
  Parser.expect Token.Export p;
  let exportLoc = mkLoc exportStart p.prevEndPos in
  let genTypeAttr = (Location.mkloc "genType" exportLoc, Parsetree.PStr []) in
  let attrs = genTypeAttr::attrs in
  match p.Parser.token with
  | Typ ->
    begin match parseTypeDefinitionOrExtension ~attrs p with
    | TypeDef {recFlag; types} ->
      Ast_helper.Str.type_ recFlag types
    | TypeExt(ext) ->
      Ast_helper.Str.type_extension ext
    end
  | (* Let *) _ ->
    let (recFlag, letBindings) = parseLetBindings ~attrs p in
    Ast_helper.Str.value recFlag letBindings

and parseSignJsExport ~attrs p =
  let exportStart = p.Parser.startPos in
  Parser.expect Token.Export p;
  let exportLoc = mkLoc exportStart p.prevEndPos in
  let genTypeAttr = (Location.mkloc "genType" exportLoc, Parsetree.PStr []) in
  let attrs = genTypeAttr::attrs in
  match p.Parser.token with
  | Typ ->
    begin match parseTypeDefinitionOrExtension ~attrs p with
    | TypeDef {recFlag; types} ->
      let loc = mkLoc exportStart p.prevEndPos in
      Ast_helper.Sig.type_ recFlag types ~loc
    | TypeExt(ext) ->
      let loc = mkLoc exportStart p.prevEndPos in
      Ast_helper.Sig.type_extension ext ~loc
    end
  | (* Let *) _ ->
    let valueDesc = parseSignLetDesc ~attrs p in
    let loc = mkLoc exportStart p.prevEndPos in
    Ast_helper.Sig.value valueDesc ~loc

and parseJsFfiScope p =
  match p.Parser.token with
  | Token.Lident "from" ->
    Parser.next p;
    begin match p.token with
    | String s -> Parser.next p; JsFfi.Module s
    | Uident _ | Lident _ ->
      let value = parseIdentPath p in
      JsFfi.Scope value
    | _ -> JsFfi.Global
    end
  | _ -> JsFfi.Global

and parseJsFfiDeclarations p =
  Parser.expect Token.Lbrace p;
  let decls = parseCommaDelimitedRegion
    ~grammar:Grammar.JsFfiImport
    ~closing:Rbrace
    ~f:parseJsFfiDeclaration
    p
  in
  Parser.expect Rbrace p;
  decls

and parseJsFfiDeclaration p =
  let startPos = p.Parser.startPos in
  let attrs = parseAttributes p in
  match p.Parser.token with
  | Lident _ ->
    let (ident, _) = parseLident p in
    let alias = match p.token with
    | As ->
      Parser.next p;
      let (ident, _) = parseLident p in
      ident
    | _ ->
      ident
    in
    Parser.expect Token.Colon p;
    let typ = parseTypExpr p in
    let loc = mkLoc startPos p.prevEndPos in
    Some (JsFfi.decl ~loc ~alias ~attrs ~name:ident ~typ)
  | _ -> None

(* include-statement ::= include module-expr *)
and parseIncludeStatement ~attrs p =
  let startPos = p.Parser.startPos in
  Parser.expect Token.Include p;
  let modExpr = parseModuleExpr p in
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Incl.mk ~loc ~attrs modExpr

and parseAtomicModuleExpr p =
  let startPos = p.Parser.startPos in
  match p.Parser.token with
  | Uident _ident ->
    let longident = parseModuleLongIdent ~lowercase:false p in
    Ast_helper.Mod.ident ~loc:longident.loc longident
  | Lbrace ->
    Parser.next p;
    let structure = Ast_helper.Mod.structure (
      parseDelimitedRegion
        ~grammar:Grammar.Structure
        ~closing:Rbrace
        ~f:parseStructureItemRegion
        p
    ) in
    Parser.expect Rbrace p;
    let endPos = p.prevEndPos in
    {structure with pmod_loc = mkLoc startPos endPos}
  | Lparen ->
    Parser.next p;
    let modExpr = match p.token with
    | Rparen ->
      Ast_helper.Mod.structure ~loc:(mkLoc startPos p.prevEndPos) []
    | _ ->
      parseConstrainedModExpr p
    in
    Parser.expect Rparen p;
    modExpr
  | Lident "unpack" -> (* TODO: should this be made a keyword?? *)
    Parser.next p;
    Parser.expect Lparen p;
    let expr = parseExpr p in
    begin match p.Parser.token with
    | Colon ->
      let colonStart = p.Parser.startPos in
      Parser.next p;
      let attrs = parseAttributes p in
      let packageType = parsePackageType ~startPos:colonStart ~attrs p in
      Parser.expect Rparen p;
      let loc = mkLoc startPos p.prevEndPos in
      let constraintExpr = Ast_helper.Exp.constraint_
        ~loc
        expr packageType
      in
      Ast_helper.Mod.unpack ~loc constraintExpr
    | _ ->
      Parser.expect Rparen p;
      let loc = mkLoc startPos p.prevEndPos in
      Ast_helper.Mod.unpack ~loc expr
    end
  | Percent ->
    let extension = parseExtension p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Mod.extension ~loc extension
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    Recover.defaultModuleExpr()

and parsePrimaryModExpr p =
  let startPos = p.Parser.startPos in
  let modExpr = parseAtomicModuleExpr p in
  let rec loop p modExpr =
    match p.Parser.token with
    | Lparen when p.prevEndPos.pos_lnum == p.startPos.pos_lnum ->
      loop p (parseModuleApplication p modExpr)
    | _ -> modExpr
  in
  let modExpr = loop p modExpr in
  {modExpr with pmod_loc = mkLoc startPos p.prevEndPos}

(*
 * functor-arg ::=
 *  | uident : modtype
 *  | _ : modtype
 *  | modtype           --> "punning" for _ : modtype
 *  | attributes functor-arg
 *)
and parseFunctorArg p =
  let startPos = p.Parser.startPos in
  let attrs = parseAttributes p in
  match p.Parser.token with
  | Uident ident ->
    Parser.next p;
    let uidentEndPos = p.prevEndPos in
    begin match p.Parser.token with
    | Colon ->
      Parser.next p;
      let moduleType = parseModuleType p in
      let loc = mkLoc startPos uidentEndPos in
      let argName = Location.mkloc ident loc in
      Some (attrs, argName, Some moduleType, startPos)
    | Dot ->
      Parser.next p;
      let moduleType =
        let moduleLongIdent =
          parseModuleLongIdentTail ~lowercase:false p startPos (Longident.Lident ident) in
        Ast_helper.Mty.ident ~loc:moduleLongIdent.loc moduleLongIdent
      in
      let argName = Location.mknoloc "_" in
      Some (attrs, argName, Some moduleType, startPos)
    | _ ->
      let loc = mkLoc startPos uidentEndPos in
      let modIdent = Location.mkloc (Longident.Lident ident) loc in
      let moduleType = Ast_helper.Mty.ident ~loc modIdent in
      let argName = Location.mknoloc "_" in
      Some (attrs, argName, Some moduleType, startPos)
    end
  | Underscore ->
    Parser.next p;
    let argName = Location.mkloc "_" (mkLoc startPos p.prevEndPos) in
    Parser.expect Colon p;
    let moduleType = parseModuleType p in
    Some (attrs, argName, Some moduleType, startPos)
  | _ ->
    None

and parseFunctorArgs p =
  let startPos = p.Parser.startPos in
  Parser.expect Lparen p;
  let args =
    parseCommaDelimitedRegion
      ~grammar:Grammar.FunctorArgs
      ~closing:Rparen
      ~f:parseFunctorArg
      p
  in
  Parser.expect Rparen p;
  match args with
  | [] ->
    [[], Location.mkloc "*" (mkLoc startPos p.prevEndPos), None, startPos]
  | args -> args

and parseFunctorModuleExpr p =
  let startPos = p.Parser.startPos in
  let args = parseFunctorArgs p in
  let returnType = match p.Parser.token with
  | Colon ->
    Parser.next p;
    Some (parseModuleType ~es6Arrow:false p)
  | _ -> None
  in
  Parser.expect EqualGreater p;
  let rhsModuleExpr =
    let modExpr = parseModuleExpr p in
    match returnType with
    | Some modType ->
      Ast_helper.Mod.constraint_
        ~loc:(mkLoc modExpr.pmod_loc.loc_start modType.Parsetree.pmty_loc.loc_end)
        modExpr modType
    | None -> modExpr
  in
  let endPos = p.prevEndPos in
  let modExpr = List.fold_right (fun (attrs, name, moduleType, startPos) acc ->
    Ast_helper.Mod.functor_
      ~loc:(mkLoc startPos endPos)
      ~attrs
      name moduleType acc
  ) args rhsModuleExpr
  in
  {modExpr with pmod_loc = mkLoc startPos endPos}

(* module-expr	::=
 *  | module-path
 *  ∣	{ structure-items }
 *  ∣	functorArgs =>  module-expr
 *  ∣	module-expr(module-expr)
 *  ∣	( module-expr )
 *  ∣	( module-expr : module-type )
 *  | extension
 *  | attributes module-expr *)
and parseModuleExpr p =
  let attrs = parseAttributes p in
  let modExpr = if isEs6ArrowFunctor p then
      parseFunctorModuleExpr p
    else
      parsePrimaryModExpr p
  in
  {modExpr with pmod_attributes = List.concat [modExpr.pmod_attributes; attrs]}

and parseConstrainedModExpr p =
  let modExpr = parseModuleExpr p in
  match p.Parser.token with
  | Colon ->
    Parser.next p;
    let modType = parseModuleType p in
    let loc = mkLoc modExpr.pmod_loc.loc_start modType.pmty_loc.loc_end in
    Ast_helper.Mod.constraint_ ~loc modExpr modType
  | _ -> modExpr

and parseConstrainedModExprRegion p =
  if Grammar.isModExprStart p.Parser.token then
    Some (parseConstrainedModExpr p)
  else
    None

and parseModuleApplication p modExpr =
  let startPos = p.Parser.startPos in
  Parser.expect Lparen p;
  let args =
    parseCommaDelimitedRegion
      ~grammar:Grammar.ModExprList
      ~closing:Rparen
      ~f:parseConstrainedModExprRegion
      p
  in
  Parser.expect Rparen p;
  let args = match args with
  | [] ->
    let loc = mkLoc startPos p.prevEndPos in
    [Ast_helper.Mod.structure ~loc []]
  | args -> args
  in
  List.fold_left (fun modExpr arg ->
    Ast_helper.Mod.apply
      ~loc:(mkLoc modExpr.Parsetree.pmod_loc.loc_start arg.Parsetree.pmod_loc.loc_end)
      modExpr arg
  ) modExpr args

and parseModuleOrModuleTypeImplOrPackExpr ~attrs p =
  let startPos = p.Parser.startPos in
  Parser.expect Module p;
  match p.Parser.token with
  | Typ -> parseModuleTypeImpl ~attrs startPos p
  | Lparen ->
    let expr = parseFirstClassModuleExpr ~startPos p in
    Ast_helper.Str.eval ~attrs expr
  | _ -> parseMaybeRecModuleBinding ~attrs ~startPos p

and parseModuleTypeImpl ~attrs startPos p =
  Parser.expect Typ p;
  let nameStart = p.Parser.startPos in
  let name = match p.Parser.token with
  | Lident ident ->
    Parser.next p;
    let loc = mkLoc nameStart p.prevEndPos in
    Location.mkloc ident loc
  | Uident ident ->
    Parser.next p;
    let loc = mkLoc nameStart p.prevEndPos in
    Location.mkloc ident loc
  | t ->
    Parser.err p (Diagnostics.uident t);
    Location.mknoloc "_"
  in
  Parser.expect Equal p;
  let moduleType = parseModuleType p in
  let moduleTypeDeclaration =
    Ast_helper.Mtd.mk
      ~attrs
      ~loc:(mkLoc nameStart p.prevEndPos)
      ~typ:moduleType
      name
  in
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Str.modtype ~loc moduleTypeDeclaration

(* definition	::=
  ∣	 module rec module-name :  module-type =  module-expr   { and module-name
  :  module-type =  module-expr } *)
and parseMaybeRecModuleBinding ~attrs ~startPos p =
  match p.Parser.token with
  | Token.Rec ->
    Parser.next p;
    Ast_helper.Str.rec_module (parseModuleBindings ~startPos ~attrs p)
  | _ ->
    Ast_helper.Str.module_ (parseModuleBinding ~attrs ~startPos:p.Parser.startPos p)

and parseModuleBinding ~attrs ~startPos p =
  let name = match p.Parser.token with
  | Uident ident ->
    let startPos = p.Parser.startPos in
    Parser.next p;
    let loc = mkLoc startPos p.prevEndPos in
    Location.mkloc ident loc
  | t ->
    Parser.err p (Diagnostics.uident t);
    Location.mknoloc "_"
  in
  let body = parseModuleBindingBody p in
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Mb.mk ~attrs ~loc name body

and parseModuleBindingBody p =
  (* TODO: make required with good error message when rec module binding *)
  let returnModType = match p.Parser.token with
  | Colon ->
    Parser.next p;
    Some (parseModuleType p)
  | _ -> None
  in
  Parser.expect Equal p;
  let modExpr = parseModuleExpr p in
  match returnModType with
  | Some modType ->
    Ast_helper.Mod.constraint_
      ~loc:(mkLoc modType.pmty_loc.loc_start modExpr.pmod_loc.loc_end)
      modExpr modType
  | None -> modExpr


(* module-name :  module-type =  module-expr
 * { and module-name :  module-type =  module-expr } *)
and parseModuleBindings ~attrs ~startPos p =
  let rec loop p acc =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributesAndBinding p in
    match p.Parser.token with
    | And ->
      Parser.next p;
      ignore(Parser.optional p Module); (* over-parse for fault-tolerance *)
      let modBinding = parseModuleBinding ~attrs ~startPos p in
      loop p (modBinding::acc)
    | _ -> List.rev acc
  in
  let first = parseModuleBinding ~attrs ~startPos p in
  loop p [first]

and parseAtomicModuleType p =
  let startPos = p.Parser.startPos in
  let moduleType = match p.Parser.token with
  | Uident _ | Lident _ ->
    (* Ocaml allows module types to end with lowercase: module Foo : bar = { ... }
     * lets go with uppercase terminal for now *)
    let moduleLongIdent = parseModuleLongIdent ~lowercase:true p in
    Ast_helper.Mty.ident ~loc:moduleLongIdent.loc moduleLongIdent
  | Lparen ->
    Parser.next p;
    let mty = parseModuleType p in
    Parser.expect Rparen p;
    {mty with pmty_loc = mkLoc startPos p.prevEndPos}
  | Lbrace ->
    Parser.next p;
    let spec =
      parseDelimitedRegion
        ~grammar:Grammar.Signature
        ~closing:Rbrace
        ~f:parseSignatureItemRegion
        p
    in
    Parser.expect Rbrace p;
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Mty.signature ~loc spec
  | Module -> (* TODO: check if this is still atomic when implementing first class modules*)
    parseModuleTypeOf p
  | Percent ->
    let extension = parseExtension p in
    let loc = mkLoc startPos p.prevEndPos in
    Ast_helper.Mty.extension ~loc extension
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    Recover.defaultModuleType()
  in
  let moduleTypeLoc = mkLoc startPos p.prevEndPos in
  {moduleType with pmty_loc = moduleTypeLoc}

and parseFunctorModuleType p =
  let startPos = p.Parser.startPos in
  let args = parseFunctorArgs p in
  Parser.expect EqualGreater p;
  let rhs = parseModuleType p in
  let endPos = p.prevEndPos in
  let modType = List.fold_right (fun (attrs, name, moduleType, startPos) acc ->
    Ast_helper.Mty.functor_
      ~loc:(mkLoc startPos endPos)
      ~attrs
      name moduleType acc
  ) args rhs
  in
  {modType with pmty_loc = mkLoc startPos endPos}

(* Module types are the module-level equivalent of type expressions: they
 * specify the general shape and type properties of modules.
 *
 * module-type ::=
 *  | modtype-path
 *  | { signature }
 *  | ( module-type )               --> parenthesized module-type
 *  | functor-args => module-type   --> functor
 *  | module-type => module-type    --> functor
 *  | module type of module-expr
 *  | attributes module-type
 *  | module-type with-mod-constraints
 *  | extension
 *)
 and parseModuleType ?(es6Arrow=true) ?(with_=true) p =
  let attrs = parseAttributes p in
  let modty = if es6Arrow && isEs6ArrowFunctor p then
    parseFunctorModuleType p
  else
    let modty = parseAtomicModuleType p in
    match p.Parser.token with
    | EqualGreater when es6Arrow == true ->
      Parser.next p;
      let rhs = parseModuleType ~with_:false p in
      let str = Location.mknoloc "_" in
      let loc = mkLoc modty.pmty_loc.loc_start p.prevEndPos in
      Ast_helper.Mty.functor_ ~loc str (Some modty) rhs
    | _ -> modty
  in
  let moduleType = { modty with
    pmty_attributes = List.concat [modty.pmty_attributes; attrs]
  } in
  if with_ then
    parseWithConstraints moduleType p
  else moduleType


and parseWithConstraints moduleType p =
  match p.Parser.token with
  | With ->
    Parser.next p;
    let first = parseWithConstraint p in
    let rec loop p acc =
      match p.Parser.token with
      | And ->
        Parser.next p;
        loop p ((parseWithConstraint p)::acc)
      | _ ->
        List.rev acc
    in
    let constraints = loop p [first] in
    let loc = mkLoc moduleType.pmty_loc.loc_start p.prevEndPos in
    Ast_helper.Mty.with_ ~loc moduleType constraints
  | _ ->
    moduleType

(* mod-constraint	::=
 *  |  type typeconstr<type-params> type-equation type-constraints?
 *  ∣	 type typeconstr-name<type-params> := typexpr
 *  ∣	 module module-path = extended-module-path
 *  ∣	 module module-path :=  extended-module-path
 *
 *  TODO: split this up into multiple functions, better errors *)
and parseWithConstraint p =
  match p.Parser.token with
  | Module ->
    Parser.next p;
    let modulePath = parseModuleLongIdent ~lowercase:false p in
    begin match p.Parser.token with
    | ColonEqual ->
      Parser.next p;
      let lident = parseModuleLongIdent ~lowercase:false p in
      Parsetree.Pwith_modsubst (modulePath, lident)
    | Equal ->
      Parser.next p;
      let lident = parseModuleLongIdent ~lowercase:false p in
      Parsetree.Pwith_module (modulePath, lident)
    | token ->
      (* TODO: revisit *)
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      let lident = parseModuleLongIdent ~lowercase:false p in
      Parsetree.Pwith_modsubst (modulePath, lident)
    end
  | Typ ->
    Parser.next p;
    let typeConstr = parseValuePath p in
    let params = parseTypeParams ~parent:typeConstr p in
    begin match p.Parser.token with
    | ColonEqual ->
      Parser.next p;
      let typExpr = parseTypExpr p in
      Parsetree.Pwith_typesubst (
        typeConstr,
        Ast_helper.Type.mk
          ~loc:typeConstr.loc
          ~params
          ~manifest:typExpr
          (Location.mkloc (Longident.last typeConstr.txt) typeConstr.loc)
      )
    | Equal ->
      Parser.next p;
      let typExpr = parseTypExpr p in
      let typeConstraints = parseTypeConstraints p in
      Parsetree.Pwith_type (
        typeConstr,
        Ast_helper.Type.mk
          ~loc:typeConstr.loc
          ~params
          ~manifest:typExpr
          ~cstrs:typeConstraints
          (Location.mkloc (Longident.last typeConstr.txt) typeConstr.loc)
      )
    | token ->
      (* TODO: revisit *)
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      let typExpr = parseTypExpr p in
      let typeConstraints = parseTypeConstraints p in
      Parsetree.Pwith_type (
        typeConstr,
        Ast_helper.Type.mk
          ~loc:typeConstr.loc
          ~params
          ~manifest:typExpr
          ~cstrs:typeConstraints
          (Location.mkloc (Longident.last typeConstr.txt) typeConstr.loc)
      )
    end
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    exit (-1) [@doesNotRaise] (* TODO: handle this case *)

and parseModuleTypeOf p =
  let startPos = p.Parser.startPos in
  Parser.expect Module p;
  Parser.expect Typ p;
  Parser.expect Of p;
  let moduleExpr = parseModuleExpr p in
  Ast_helper.Mty.typeof_ ~loc:(mkLoc startPos p.prevEndPos) moduleExpr

(* module signature on the file level *)
and parseSpecification p =
  parseRegion ~grammar:Grammar.Specification ~f:parseSignatureItemRegion p
  [@@progress (Parser.next, Parser.expect, Parser.checkProgress)]

and parseNewlineOrSemicolonSignature p =
  match p.Parser.token with
  | Semicolon ->
    Parser.next p
  | token when Grammar.isSignatureItemStart token ->
    if p.prevEndPos.pos_lnum < p.startPos.pos_lnum then ()
    else
      Parser.err
        ~startPos:p.prevEndPos
        ~endPos: p.endPos
        p
        (Diagnostics.message "consecutive specifications on a line must be separated by ';' or a newline")
  | _ -> ()

and parseSignatureItemRegion p =
  let startPos = p.Parser.startPos in
  let attrs = parseAttributes p in
  match p.Parser.token with
  | Let ->
    Parser.beginRegion p;
    let valueDesc = parseSignLetDesc ~attrs p in
    parseNewlineOrSemicolonSignature p;
    let loc = mkLoc startPos p.prevEndPos in
    Parser.endRegion p;
    Some (Ast_helper.Sig.value ~loc valueDesc)
  | Typ ->
    Parser.beginRegion p;
    begin match parseTypeDefinitionOrExtension ~attrs p with
    | TypeDef {recFlag; types} ->
      parseNewlineOrSemicolonSignature p;
      let loc = mkLoc startPos p.prevEndPos in
      Parser.endRegion p;
      Some (Ast_helper.Sig.type_ ~loc recFlag types)
    | TypeExt(ext) ->
      parseNewlineOrSemicolonSignature p;
      let loc = mkLoc startPos p.prevEndPos in
      Parser.endRegion p;
      Some (Ast_helper.Sig.type_extension ~loc ext)
    end
  | External ->
    let externalDef = parseExternalDef ~attrs ~startPos p in
    parseNewlineOrSemicolonSignature p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Sig.value ~loc externalDef)
  | Export ->
    let signatureItem = parseSignJsExport ~attrs p in
    parseNewlineOrSemicolonSignature p;
    let loc = mkLoc startPos p.prevEndPos in
    Some {signatureItem with psig_loc = loc}
  | Exception ->
    let exceptionDef = parseExceptionDef ~attrs p in
    parseNewlineOrSemicolonSignature p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Sig.exception_ ~loc exceptionDef)
  | Open ->
    let openDescription = parseOpenDescription ~attrs p in
    parseNewlineOrSemicolonSignature p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Sig.open_ ~loc openDescription)
  | Include ->
    Parser.next p;
    let moduleType = parseModuleType p in
    let includeDescription = Ast_helper.Incl.mk
      ~loc:(mkLoc startPos p.prevEndPos)
      ~attrs
      moduleType
    in
    parseNewlineOrSemicolonSignature p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Sig.include_ ~loc includeDescription)
  | Module ->
    Parser.next p;
    begin match p.Parser.token with
    | Uident _ ->
      let modDecl = parseModuleDeclarationOrAlias ~attrs p in
      parseNewlineOrSemicolonSignature p;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Sig.module_ ~loc modDecl)
    | Rec ->
      let recModule = parseRecModuleSpec ~attrs ~startPos p in
      parseNewlineOrSemicolonSignature p;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Sig.rec_module ~loc recModule)
    | Typ ->
      Some (parseModuleTypeDeclaration ~attrs ~startPos p)
    | _t ->
      let modDecl = parseModuleDeclarationOrAlias ~attrs p in
      parseNewlineOrSemicolonSignature p;
      let loc = mkLoc startPos p.prevEndPos in
      Some (Ast_helper.Sig.module_ ~loc modDecl)
    end
  | AtAt ->
    let attr = parseStandaloneAttribute p in
    parseNewlineOrSemicolonSignature p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Sig.attribute ~loc attr)
  | PercentPercent ->
    let extension = parseExtension ~moduleLanguage:true p in
    parseNewlineOrSemicolonSignature p;
    let loc = mkLoc startPos p.prevEndPos in
    Some (Ast_helper.Sig.extension ~attrs ~loc extension)
  | Import ->
    Parser.next p;
    parseSignatureItemRegion p
  | _ ->
    None

(* module rec module-name :  module-type  { and module-name:  module-type } *)
and parseRecModuleSpec ~attrs ~startPos p =
  Parser.expect Rec p;
  let rec loop p spec =
    let startPos = p.Parser.startPos in
    let attrs = parseAttributesAndBinding p in
    match p.Parser.token with
    | And ->
      (* TODO: give a good error message when with constraint, no parens
       * and ASet: (Set.S with type elt = A.t)
       * and BTree: (Btree.S with type elt = A.t)
       * Without parens, the `and` signals the start of another
       * `with-constraint`
       *)
      Parser.expect And p;
      let decl = parseRecModuleDeclaration ~attrs ~startPos p in
      loop p (decl::spec)
    | _ ->
      List.rev spec
  in
  let first = parseRecModuleDeclaration ~attrs ~startPos p in
  loop p [first]

(* module-name : module-type *)
and parseRecModuleDeclaration ~attrs ~startPos p =
  let name = match p.Parser.token with
  | Uident modName ->
    let loc = mkLoc p.startPos p.endPos in
    Parser.next p;
    Location.mkloc modName loc
  | t ->
    Parser.err p (Diagnostics.uident t);
    Location.mknoloc "_"
  in
  Parser.expect Colon p;
  let modType = parseModuleType p in
  Ast_helper.Md.mk ~loc:(mkLoc startPos p.prevEndPos) ~attrs name modType

and parseModuleDeclarationOrAlias ~attrs p =
  let startPos = p.Parser.startPos in
  let moduleName = match p.Parser.token with
  | Uident ident ->
    let loc = mkLoc p.Parser.startPos p.endPos in
    Parser.next p;
    Location.mkloc ident loc
  | t ->
    Parser.err p (Diagnostics.uident t);
    Location.mknoloc "_"
  in
  let body = match p.Parser.token with
  | Colon ->
    Parser.next p;
    parseModuleType p
  | Equal ->
    Parser.next p;
    let lident = parseModuleLongIdent ~lowercase:false p in
    Ast_helper.Mty.alias lident
  | token ->
    Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
    Recover.defaultModuleType()
  in
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Md.mk ~loc ~attrs moduleName body

and parseModuleTypeDeclaration ~attrs ~startPos p =
  Parser.expect Typ p;
  let moduleName = match p.Parser.token with
  | Uident ident ->
    let loc = mkLoc p.startPos p.endPos in
    Parser.next p;
    Location.mkloc ident loc
  | Lident ident ->
    let loc = mkLoc p.startPos p.endPos in
    Parser.next p;
    Location.mkloc ident loc
  | t ->
    Parser.err p (Diagnostics.uident t);
    Location.mknoloc "_"
  in
  let typ = match p.Parser.token with
  | Equal ->
    Parser.next p;
    Some (parseModuleType p)
  | _ -> None
  in
  let moduleDecl = Ast_helper.Mtd.mk ~attrs ?typ moduleName in
  Ast_helper.Sig.modtype ~loc:(mkLoc startPos p.prevEndPos) moduleDecl

and parseSignLetDesc ~attrs p =
  let startPos = p.Parser.startPos in
  Parser.optional p Let |> ignore;
  let (name, loc) = parseLident p in
  let name = Location.mkloc name loc in
  Parser.expect Colon p;
  let typExpr = parsePolyTypeExpr p in
  let loc = mkLoc startPos p.prevEndPos in
  Ast_helper.Val.mk ~loc ~attrs name typExpr

(*    attr-id	::=	lowercase-ident
∣	  capitalized-ident
∣	  attr-id .  attr-id   *)
and parseAttributeId p =
  let startPos = p.Parser.startPos in
  let rec loop p acc =
    match p.Parser.token with
    | Lident ident | Uident ident ->
      Parser.next p;
      let id = acc ^ ident in
      begin match p.Parser.token with
      | Dot -> Parser.next p; loop p (id ^ ".")
      | _ -> id
      end
    | token when Token.isKeyword token ->
      Parser.next p;
      let id = acc ^ (Token.toString token) in
      begin match p.Parser.token with
      | Dot -> Parser.next p; loop p (id ^ ".")
      | _ -> id
      end
    | token ->
      Parser.err p (Diagnostics.unexpected token p.breadcrumbs);
      acc
  in
  let id = loop p "" in
  let endPos = p.prevEndPos in
  Location.mkloc id (mkLoc startPos endPos)

(*
 * payload ::=  empty
 *          |  ( structure-item )
 *
 * TODO: what about multiple structure items?
 * @attr({let x = 1; let x = 2})
 *
 * Also what about type-expressions and specifications?
 * @attr(:myType) ???
 *)
and parsePayload p =
  match p.Parser.token with
  | Lparen when p.startPos.pos_cnum = p.prevEndPos.pos_cnum  ->
    Parser.next p;
    begin match p.token with
    | Colon ->
      Parser.next p;
      let typ = parseTypExpr p in
      Parser.expect Rparen p;
      Parsetree.PTyp typ
    | Question ->
      Parser.next p;
      let pattern = parsePattern p in
      let expr = match p.token with
      | When ->
        Parser.next p;
        Some (parseExpr p)
      | _ ->
        None
      in
      Parser.expect Rparen p;
      Parsetree.PPat (pattern, expr)
    | _ ->
      let items = parseDelimitedRegion
        ~grammar:Grammar.Structure
        ~closing:Rparen
        ~f:parseStructureItemRegion
        p
      in
      Parser.expect Rparen p;
      Parsetree.PStr items
    end
  | _ -> Parsetree.PStr []

(* type attribute = string loc * payload *)
and parseAttribute p =
  match p.Parser.token with
  | At ->
    Parser.next p;
    let attrId = parseAttributeId p in
    let payload = parsePayload p in
    Some(attrId, payload)
  | _ -> None

and parseAttributes p =
  parseRegion p
    ~grammar:Grammar.Attribute
    ~f:parseAttribute

(*
 * standalone-attribute ::=
 *  | @@ atribute-id
 *  | @@ attribute-id ( structure-item )
 *)
and parseStandaloneAttribute p =
  Parser.expect AtAt p;
  let attrId = parseAttributeId p in
  let payload = parsePayload p in
  (attrId, payload)

(* extension	::=	% attr-id  attr-payload
 *              | %% attr-id(
 *  expr	::=	 ...
 *    ∣	 extension
 *
 *  typexpr	::=	 ...
 *    ∣	 extension
 *
 *  pattern	::=	 ...
 *    ∣	 extension
 *
 *  module-expr	::=	 ...
 *    ∣	 extension
 *
 *  module-type	::=	 ...
 *    ∣	 extension
 *
 *  class-expr	::=	 ...
 *    ∣	 extension
 *
 *  class-type	::=	 ...
 *    ∣	 extension
 *
 *
 * item extension nodes usable in structures and signature
 *
 * item-extension ::= %% attr-id
 *                  | %% attr-id(structure-item)
 *
 *  attr-payload ::= structure-item
 *
 *  ~moduleLanguage represents whether we're on the module level or not
 *)
and parseExtension ?(moduleLanguage=false) p =
  if moduleLanguage then
    Parser.expect PercentPercent p
  else
    Parser.expect Percent p;
  let attrId = parseAttributeId p in
  let payload = parsePayload p in
  (attrId, payload)
