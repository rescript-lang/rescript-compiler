module CharacterCodes = Res_character_codes
module Diagnostics = Res_diagnostics
module Token = Res_token
module Comment = Res_comment

type mode = Jsx | Diamond

(* We hide the implementation detail of the scanner reading character. Our char
will also contain the special -1 value to indicate end-of-file. This isn't
ideal; we should clean this up *)
let hackyEOFChar = Char.unsafe_chr (-1)
type charEncoding = Char.t

type t = {
  filename: string;
  src: string;
  mutable err:
    startPos: Lexing.position
    -> endPos: Lexing.position
    -> Diagnostics.category
    -> unit;
  mutable ch: charEncoding; (* current character *)
  mutable offset: int; (* character offset *)
  mutable lineOffset: int; (* current line offset *)
  mutable lnum: int; (* current line number *)
  mutable mode: mode list;
}

let setDiamondMode scanner =
  scanner.mode <- Diamond::scanner.mode

let setJsxMode scanner =
  scanner.mode <- Jsx::scanner.mode

let popMode scanner mode =
  match scanner.mode with
  | m::ms when m = mode ->
    scanner.mode <- ms
  | _ -> ()

let inDiamondMode scanner = match scanner.mode with
  | Diamond::_ -> true
  | _ -> false

let inJsxMode scanner = match scanner.mode with
  | Jsx::_ -> true
  | _ -> false

let position scanner = Lexing.{
  pos_fname = scanner.filename;
  (* line number *)
  pos_lnum = scanner.lnum;
  (* offset of the beginning of the line (number
     of characters between the beginning of the scanner and the beginning
     of the line) *)
  pos_bol = scanner.lineOffset;
  (* [pos_cnum] is the offset of the position (number of
     characters between the beginning of the scanner and the position). *)
  pos_cnum = scanner.offset;
}

(* Small debugging util
❯ ./lib/rescript.exe baah/test.res
let a = 1
^ let
let a = 1
    ^ a
let a = 1
      ^ =
let a = 1
        ^ int 1
let a = 1
         ^ eof
let a = 1
*)
let _printDebug ~startPos scanner token =
  let open Lexing in
  print_endline scanner.src;
  print_string ((String.make [@doesNotRaise]) startPos.pos_cnum ' ');
  print_char '^';
  print_char ' ';
  print_endline (Res_token.toString token)
[@@live]

let next scanner =
  if scanner.offset + 1 < String.length scanner.src then (
    scanner.offset <- scanner.offset + 1;
    scanner.ch <- String.unsafe_get scanner.src scanner.offset
  ) else (
    scanner.offset <- String.length scanner.src;
    scanner.ch <- hackyEOFChar
  )

let next2 scanner =
  next scanner;
  next scanner

let next3 scanner =
  next scanner;
  next scanner;
  next scanner

let peek scanner =
  if scanner.offset + 1 < String.length scanner.src then
    String.unsafe_get scanner.src (scanner.offset + 1)
  else
    hackyEOFChar

let peek2 scanner =
  if scanner.offset + 2 < String.length scanner.src then
    String.unsafe_get scanner.src (scanner.offset + 2)
  else
    hackyEOFChar

let make ?(line=1) ~filename src =
  {
    filename;
    src = src;
    err = (fun ~startPos:_ ~endPos:_ _ -> ());
    ch = if src = "" then hackyEOFChar else String.unsafe_get src 0;
    offset = 0;
    lineOffset = 0;
    lnum = line;
    mode = [];
  }

let skipWhitespace scanner =
  let rec scan () =
    if scanner.ch == ' ' || scanner.ch == '\t' then (
      next scanner;
      scan()
    ) else if CharacterCodes.isLineBreak scanner.ch then (
      scanner.lineOffset <- scanner.offset + 1;
      scanner.lnum <- scanner.lnum + 1;
      next scanner;
      scan()
    ) else (
      ()
    )
  in
  scan()

let scanIdentifier scanner =
  let startOff = scanner.offset in
  while (
    CharacterCodes.isLetter scanner.ch ||
    CharacterCodes.isDigit scanner.ch ||
    '_' == scanner.ch ||
    '\'' == scanner.ch
  ) do
    next scanner
  done;
  let str = (String.sub [@doesNotRaise]) scanner.src startOff (scanner.offset - startOff) in
  if '{' == scanner.ch && str = "list"
  then begin
    next scanner;
    Token.lookupKeyword "list{"
  end
  else Token.lookupKeyword str

let scanDigits scanner ~base =
  if base <= 10 then (
    while CharacterCodes.isDigit scanner.ch || scanner.ch == '_' do
      next scanner
    done;
  ) else (
    while CharacterCodes.isHex scanner.ch || scanner.ch == '_' do
      next scanner
    done;
  )

(* float: (0…9) { 0…9∣ _ } [. { 0…9∣ _ }] [(e∣ E) [+∣ -] (0…9) { 0…9∣ _ }]   *)
let scanNumber scanner =
  let startOff = scanner.offset in

  (* integer part *)
  let base, _prefix = if scanner.ch != '.' then (
    if scanner.ch == '0' then (
      next scanner;
      match scanner.ch with
      | 'x' | 'X' ->
        next scanner;
        16, 'x'
      | 'o' | 'O' ->
        next scanner;
        8, 'o'
      | 'b' | 'B' ->
        next scanner;
        2, 'b'
      | _ ->
        8, '0'
    ) else (
      10, ' '
    )
  ) else (10, ' ')
  in
  scanDigits scanner ~base;

  (*  *)
  let isFloat = if '.' == scanner.ch then (
    next scanner;
    scanDigits scanner ~base;
    true
  ) else (
    false
  ) in

  (* exponent part *)
  let isFloat =
    match scanner.ch with
    | 'e' | 'E' | 'p' | 'P' ->
      next scanner;
      if scanner.ch == '+' || scanner.ch == '-' then
        next scanner;
      scanDigits scanner ~base;
      true
    | _ -> isFloat
  in
  let literal =
    (String.sub [@doesNotRaise]) scanner.src startOff (scanner.offset - startOff)
  in

  (* suffix *)
  let suffix =
    match scanner.ch with
    | 'g'..'z' | 'G'..'Z' ->
      let ch = scanner.ch in
      if 'n' = ch then (
        let msg =
          "Unsupported number type (nativeint). Did you mean `"
          ^ literal
          ^ "`?"
        in
        let pos = position scanner in
        scanner.err
          ~startPos:pos
          ~endPos:pos
          (Diagnostics.message msg)
      );
      next scanner;
      Some ch
    | _ ->
      None
  in
  if isFloat then
    Token.Float {f = literal; suffix}
  else
    Token.Int {i = literal; suffix}

let scanExoticIdentifier scanner =
  next scanner;
  let buffer = Buffer.create 20 in
  let startPos = position scanner in

  let rec scan () =
    if scanner.ch == hackyEOFChar then
      let endPos = position scanner in
      scanner.err ~startPos ~endPos (Diagnostics.message "Did you forget a \" here?")
    else if scanner.ch == '"' then (
      next scanner
    ) else if CharacterCodes.isLineBreak scanner.ch then (
      let endPos = position scanner in
      scanner.err ~startPos ~endPos (Diagnostics.message "A quoted identifier can't contain line breaks.");
      scanner.lineOffset <- scanner.offset + 1;
      scanner.lnum <- scanner.lnum + 1;
      next scanner
    ) else (
      Buffer.add_char buffer (scanner.ch);
      next scanner;
      scan()
    )
  in
  scan();
  Token.Lident (Buffer.contents buffer)

let scanStringEscapeSequence ~startPos scanner =
  match scanner.ch with
  (* \ already consumed *)
  | 'n' | 't' | 'b' | 'r' | '\\' | ' ' | '\'' | '"' ->
    next scanner
  | _ ->
    let (n, base, max) =
      if CharacterCodes.isDigit scanner.ch then
        (* decimal *)
        (3, 10, 255)
      else if scanner.ch == 'o' then
        (* octal *)
        let () = next scanner in
        (3, 8, 255)
      else if scanner.ch == 'x' then
        (* hex *)
        let () = next scanner in
        (2, 16, 255)
      else
        (* unknown escape sequence
         * TODO: we should warn the user here. Let's not make it a hard error for now, for reason compat *)
        (* let pos = position scanner in *)
        (* let () = *)
          (* let msg = if scanner.ch == -1 then *)
            (* "unclosed escape sequence" *)
          (* else "unknown escape sequence" *)
          (* in *)
          (* scanner.err ~startPos ~endPos:pos (Diagnostics.message msg) *)
        (* in *)
        (-1, -1, -1)
      in
      if n < 0 then ()
      else
        let rec while_ n x =
          if n == 0 then x
          else
            let d = CharacterCodes.digitValue scanner.ch in
            if d >= base then
              let pos = position scanner in
              let msg = if scanner.ch == hackyEOFChar then
                "unclosed escape sequence"
              else "unknown escape sequence"
              in
              scanner.err ~startPos ~endPos:pos (Diagnostics.message msg);
              -1
            else
              let () = next scanner in
              while_ (n - 1) (x * base + d)
        in
        let x = while_ n 0 in
        if x > max then
          let pos = position scanner in
          let msg = "invalid escape sequence (value too high)" in
          scanner.err ~startPos ~endPos:pos (Diagnostics.message msg);
        ()

let scanString scanner =
  let offs = scanner.offset in

  let startPos = position scanner in
  let rec scan () =
    if scanner.ch == hackyEOFChar then
      let endPos = position scanner in
      scanner.err ~startPos ~endPos Diagnostics.unclosedString
    else if scanner.ch == '"' then (
      next scanner;
    ) else if scanner.ch == '\\' then (
      let startPos = position scanner in
      next scanner;
      scanStringEscapeSequence ~startPos scanner;
      scan ()
    ) else if CharacterCodes.isLineBreak scanner.ch then (
      scanner.lineOffset <- scanner.offset + 1;
      scanner.lnum <- scanner.lnum + 1;
      next scanner;
      scan ()
    ) else (
      next scanner;
      scan ()
    )
  in
  scan ();
  Token.String ((String.sub [@doesNotRaise]) scanner.src offs (scanner.offset - offs - 1))

let scanEscape scanner =
  let convertNumber scanner ~n ~base =
    let x = ref 0 in
    for _ = n downto 1 do
      let d = CharacterCodes.digitValue scanner.ch in
      x := (!x * base) + d;
      next scanner
    done;
    (Char.chr [@doesNotRaise]) !x
  in
  (* let offset = scanner.offset in *)
  let c = match scanner.ch with
  | 'b' -> next scanner; '\008'
  | 'n' -> next scanner; '\010'
  | 'r' -> next scanner; '\013'
  | 't' -> next scanner; '\009'
  | 'x' ->
    next scanner;
    convertNumber scanner ~n:2 ~base:16
  | 'o' ->
    next scanner;
    convertNumber scanner ~n:3 ~base:8
  | ch when CharacterCodes.isDigit ch ->
    convertNumber scanner ~n:3 ~base:10
  | ch ->
    next scanner;
    ch
  in
  next scanner; (* Consume \' *)
  Token.Character c

let scanSingleLineComment scanner =
  let startOff = scanner.offset in
  let startPos = position scanner in
  while scanner.ch != hackyEOFChar && not (CharacterCodes.isLineBreak scanner.ch) do
    next scanner
  done;
  let endPos = position scanner in
  Token.Comment (
    Comment.makeSingleLineComment
      ~loc:(Location.{loc_start = startPos; loc_end = endPos; loc_ghost = false})
      ((String.sub [@doesNotRaise]) scanner.src startOff (scanner.offset - startOff))
  )

let scanMultiLineComment scanner =
  let startOff = scanner.offset in
  let startPos = position scanner in
  let rec scan ~depth () =
    if scanner.ch == '*' &&
       peek scanner == '/' then (
      next scanner;
      next scanner;
      if depth > 0 then scan ~depth:(depth - 1) () else ()
    ) else if scanner.ch == hackyEOFChar then (
      let endPos = position scanner in
      scanner.err ~startPos ~endPos Diagnostics.unclosedComment
    ) else if scanner.ch == '/'
      && peek scanner == '*' then (
      next scanner;
      next scanner;
      scan ~depth:(depth + 1) ()
    ) else (
      if CharacterCodes.isLineBreak scanner.ch then (
        scanner.lineOffset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
      );
      next scanner;
      scan ~depth ()
    )
  in
  scan ~depth:0 ();
  Token.Comment (
    Comment.makeMultiLineComment
      ~loc:(Location.{loc_start = startPos; loc_end = (position scanner); loc_ghost = false})
      ((String.sub [@doesNotRaise]) scanner.src startOff (scanner.offset - 2 - startOff))
  )

let scanTemplateLiteralToken scanner =
  let startOff = scanner.offset in

  (* if starting } here, consume it *)
  if scanner.ch == '}' then (
    next scanner
  );
  let startPos = position scanner in

  let rec scan () =
    if scanner.ch == hackyEOFChar then (
      let endPos = position scanner in
      scanner.err ~startPos ~endPos Diagnostics.unclosedTemplate;
      Token.TemplateTail(
        (String.sub [@doesNotRaise]) scanner.src startOff (scanner.offset - 1 - startOff)
      )
    ) else if scanner.ch == '`' then (
      next scanner;
      Token.TemplateTail(
        (String.sub [@doesNotRaise]) scanner.src startOff (scanner.offset - 1 - startOff)
      )
    ) else if scanner.ch == '$' &&
              (peek scanner) == '{' then (
      next scanner; (* consume $ *)
      next scanner; (* consume { *)
      let contents =
        (String.sub [@doesNotRaise]) scanner.src startOff (scanner.offset - 2 - startOff)
      in
      Token.TemplatePart contents
    ) else if scanner.ch == '\\' then (
      next scanner;
      if scanner.ch == '`'
        || scanner.ch == '\\'
        || scanner.ch == '$'
        || CharacterCodes.isLineBreak scanner.ch
      then next scanner;
      scan()
    ) else (
      if CharacterCodes.isLineBreak scanner.ch then (
        scanner.lineOffset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
      );
      next scanner;
      scan()
    )
  in
  let token = scan() in
  let endPos = position scanner in
  (startPos, endPos, token)

let rec scan scanner =
  skipWhitespace scanner;
  let startPos = position scanner in

  let token = match scanner.ch with
  (* peeking 0 char *)
  | 'A'..'Z' | 'a'..'z' -> scanIdentifier scanner
  | '0'..'9' -> scanNumber scanner
  | '`' -> next scanner; Token.Backtick
  | '~' -> next scanner; Token.Tilde
  | '?' -> next scanner; Token.Question
  | ';' -> next scanner; Token.Semicolon
  | '(' -> next scanner; Token.Lparen
  | ')' -> next scanner; Token.Rparen
  | '[' -> next scanner; Token.Lbracket
  | ']' -> next scanner; Token.Rbracket
  | '{' -> next scanner; Token.Lbrace
  | '}' -> next scanner; Token.Rbrace
  | ',' -> next scanner; Token.Comma
  | '"' -> next scanner; scanString scanner

  (* peeking 1 chars *)
  | '_' ->
    (match peek scanner with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> scanIdentifier scanner
    | _ -> next scanner; Token.Underscore)
  | '#' ->
    (match peek scanner with
    | '=' -> next2 scanner; Token.HashEqual
    | _ -> next scanner; Token.Hash)
  | '*' ->
    (match peek scanner with
    | '*' -> next2 scanner; Token.Exponentiation
    | '.' -> next2 scanner; Token.AsteriskDot
    | _ -> next scanner; Token.Asterisk)
  | '@' ->
    (match peek scanner with
    | '@' -> next2 scanner; Token.AtAt
    | _ -> next scanner; Token.At)
  | '%' ->
    (match peek scanner with
    | '%' -> next2 scanner; Token.PercentPercent
    | _ -> next scanner; Token.Percent)
  | '|' ->
    (match peek scanner with
    | '|' -> next2 scanner; Token.Lor
    | '>' -> next2 scanner; Token.BarGreater
    | _ -> next scanner; Token.Bar)
  | '&' ->
    (match peek scanner with
    | '&' -> next2 scanner; Token.Land
    | _ -> next scanner; Token.Band)
  | ':' ->
    (match peek scanner with
    | '=' -> next2 scanner; Token.ColonEqual
    | '>' -> next2 scanner; Token.ColonGreaterThan
    | _ -> next scanner; Token.Colon)
  | '\\' -> next scanner; scanExoticIdentifier scanner
  | '/' ->
    (match peek scanner with
    | '/' -> next2 scanner; scanSingleLineComment scanner
    | '*' -> next2 scanner; scanMultiLineComment scanner
    | '.' -> next2 scanner; Token.ForwardslashDot
    | _ -> next scanner; Token.Forwardslash)
  | '-' ->
    (match peek scanner with
    | '.' -> next2 scanner; Token.MinusDot
    | '>' -> next2 scanner; Token.MinusGreater
    | _ -> next scanner; Token.Minus)
  | '+' ->
    (match peek scanner with
    | '.' -> next2 scanner; Token.PlusDot
    | '+' -> next2 scanner; Token.PlusPlus
    | '=' -> next2 scanner; Token.PlusEqual
    | _ -> next scanner; Token.Plus)
  | '>' ->
    (match peek scanner with
    | '=' when not (inDiamondMode scanner) -> next2 scanner; Token.GreaterEqual
    | _ -> next scanner; Token.GreaterThan)
  | '<' when not (inJsxMode scanner) ->
    (match peek scanner with
    | '=' -> next2 scanner; Token.LessEqual
    | _ -> next scanner; Token.LessThan)
  (* special handling for JSX < *)
  | '<' ->
    (* Imagine the following: <div><
     * < indicates the start of a new jsx-element, the parser expects
     * the name of a new element after the <
     * Example: <div> <div
     * But what if we have a / here: example </ in  <div></div>
     * This signals a closing element. To simulate the two-token lookahead,
     * the </ is emitted as a single new token LessThanSlash *)
    next scanner;
    skipWhitespace scanner;
    (match scanner.ch with
    | '/' -> next scanner; Token.LessThanSlash
    | '=' -> next scanner; Token.LessEqual
    | _ -> Token.LessThan)

  (* peeking 2 chars *)
  | '.' ->
    (match peek scanner, peek2 scanner with
    | '.', '.' -> next3 scanner; Token.DotDotDot
    | '.', _ -> next2 scanner; Token.DotDot
    | _ -> next scanner; Token.Dot)
  | '\'' ->
    (match peek scanner, peek2 scanner with
    | '\\', '"' ->
      (* careful with this one! We're next-ing _once_ (not twice),
        then relying on matching on the quote *)
      next scanner; SingleQuote
    | '\\', _ -> next2 scanner; scanEscape scanner
    | ch, '\'' -> next3 scanner; Token.Character ch
    | _ -> next scanner; SingleQuote)
  | '!' ->
    (match peek scanner, peek2 scanner with
    | '=', '=' -> next3 scanner; Token.BangEqualEqual
    | '=', _ -> next2 scanner; Token.BangEqual
    | _ -> next scanner; Token.Bang)
  | '=' ->
    (match peek scanner, peek2 scanner with
    | '=', '=' -> next3 scanner; Token.EqualEqualEqual
    | '=', _ -> next2 scanner; Token.EqualEqual
    | '>', _ -> next2 scanner; Token.EqualGreater
    | _ -> next scanner; Token.Equal)

  (* special cases *)
  | ch when ch == hackyEOFChar -> next scanner; Token.Eof
  | ch ->
    (* if we arrive here, we're dealing with an unknown character,
     * report the error and continue scanning… *)
    next scanner;
    let endPos = position scanner in
    scanner.err ~startPos ~endPos (Diagnostics.unknownUchar ch);
    let (_, _, token) = scan scanner in
    token
  in
  let endPos = position scanner in
  (* _printDebug ~startPos scanner token; *)
  (startPos, endPos, token)

(* Imagine: <div> <Navbar /> <
 * is `<` the start of a jsx-child? <div …
 * or is it the start of a closing tag?  </div>
 * reconsiderLessThan peeks at the next token and
 * determines the correct token to disambiguate *)
let reconsiderLessThan scanner =
  (* < consumed *)
  skipWhitespace scanner;
  if scanner.ch == '/' then
    let () = next scanner in
    Token.LessThanSlash
  else
    Token.LessThan

(* If an operator has whitespace around both sides, it's a binary operator *)
let isBinaryOp src startCnum endCnum =
  if startCnum == 0 then false
  else
    let leftOk =
      let c =
        (startCnum - 1)
        |> (String.get [@doesNotRaise]) src
      in
      c == ' ' ||
      c == '\t' ||
      CharacterCodes.isLineBreak c
    in
    let rightOk =
      let c =
        if endCnum == String.length src then hackyEOFChar
        else endCnum |> (String.get [@doesNotRaise]) src
      in
      c == ' ' ||
      c == '\t' ||
      CharacterCodes.isLineBreak c ||
      c == hackyEOFChar
    in
    leftOk && rightOk

(* Assume `{` consumed, advances the scanner towards the ends of Reason quoted strings. (for conversion)
 * In {| foo bar |} the scanner will be advanced until after the `|}` *)
let tryAdvanceQuotedString scanner =
  let rec scanContents tag () =
    if scanner.ch == hackyEOFChar then (
      ()
    ) else if scanner.ch == '|' then (
      next scanner;
      if CharacterCodes.isLowerCase scanner.ch then (
        let startOff = scanner.offset in
        while CharacterCodes.isLowerCase scanner.ch do
          next scanner
        done;
        let suffix = (String.sub [@doesNotRaise]) scanner.src startOff (scanner.offset - startOff) in
        if tag = suffix then (
          if scanner.ch = '}' then
            next scanner
          else
            scanContents tag ()
        ) else
          scanContents tag ()
      ) else if '}' = scanner.ch then (
        next scanner
      ) else (
        scanContents tag ()
      )
    ) else (
      if CharacterCodes.isLineBreak scanner.ch then (
        scanner.lineOffset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
      );
      next scanner;
      scanContents tag ()
    )
  in
  if CharacterCodes.isLowerCase scanner.ch then (
    let startOff = scanner.offset in
    while CharacterCodes.isLowerCase scanner.ch do
      next scanner
    done;
    let tag = (String.sub [@doesNotRaise]) scanner.src startOff (scanner.offset - startOff) in
    if scanner.ch = '|' then
      scanContents tag ()
    else
      ()
  ) else if scanner.ch = '|' then
    scanContents "" ()
  else
    ()
