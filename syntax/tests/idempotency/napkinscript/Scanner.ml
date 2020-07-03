module Scanner = struct
  type mode = Template | Tuple | Jsx | Diamond

  let string_of_mode = function
    | Template -> "template"
    | Tuple -> "tuple"
    | Jsx -> "jsx"
    | Diamond -> "diamond"

  type t = {
    filename: string;
    src: bytes;
    mutable err:
      startPos: Lexing.position
      -> endPos: Lexing.position
      -> Diagnostics.category
      -> unit;
    mutable ch: int; (* current character *)
    mutable offset: int; (* character offset *)
    mutable rdOffset: int; (* reading offset (position after current character) *)
    mutable lineOffset: int; (* current line offset *)
    mutable lnum: int; (* current line number *)
    mutable mode: mode list;
  }

  let setDiamondMode scanner =
    scanner.mode <- Diamond::scanner.mode

  let setTemplateMode scanner =
    scanner.mode <- Template::scanner.mode

  let setTupleMode scanner =
    scanner.mode <- Tuple::scanner.mode

  let setJsxMode scanner =
    scanner.mode <- Jsx::scanner.mode

  let popMode scanner mode =
    match scanner.mode with
    | m::ms when m = mode ->
      scanner.mode <- ms
    | _ -> ()

  let inTupleMode scanner = match scanner.mode with
    | Tuple::_ -> true
    | _ -> false

  let inDiamondMode scanner = match scanner.mode with
    | Diamond::_ -> true
    | _ -> false

  let inJsxMode scanner = match scanner.mode with
    | Jsx::_ -> true
    | _ -> false

  let inTemplateMode scanner = match scanner.mode with
    | Template::_ -> true
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

  let printPos p =
    print_endline ("cnum: " ^ (string_of_int p.Lexing.pos_cnum));
    print_endline ("lnum: " ^ (string_of_int p.Lexing.pos_lnum));
    print_endline ("bol: " ^ (string_of_int p.Lexing.pos_bol));
    print_endline ("beginning of line: " ^ (string_of_int p.Lexing.pos_bol));
    print_endline ("-------------------")

  let next scanner =
    if scanner.rdOffset < (Bytes.length scanner.src) then (
      scanner.offset <- scanner.rdOffset;
      let ch = Bytes.get scanner.src scanner.rdOffset in
      scanner.rdOffset <- scanner.rdOffset + 1;
      scanner.ch <- int_of_char ch
    ) else (
      scanner.offset <- Bytes.length scanner.src;
      scanner.ch <- -1
    )

  let peek scanner =
    if scanner.rdOffset < (Bytes.length scanner.src) then
      int_of_char (Bytes.unsafe_get scanner.src scanner.rdOffset)
    else
      -1

  let make b filename =
    let scanner = {
      filename;
      src = b;
      err = (fun ~startPos:_ ~endPos:_ _ -> ());
      ch = CharacterCodes.space;
      offset = 0;
      rdOffset = 0;
      lineOffset = 0;
      lnum = 1;
      mode = [];
    } in
    next scanner;
    scanner


  (* black magic, use sparingly! *)
  let lookahead scanner callback =
    let err = scanner.err in
    let ch = scanner.ch in
    let offset = scanner.offset in
    let rdOffset = scanner.rdOffset in
    let lineOffset = scanner.lineOffset in
    let lnum = scanner.lnum in
    let mode = scanner.mode in
    let res = callback scanner in
    scanner.err <- err;
    scanner.ch <- ch;
    scanner.offset <- offset;
    scanner.rdOffset <- rdOffset;
    scanner.lineOffset <- lineOffset;
    scanner.lnum <- lnum;
    scanner.mode <- mode;
    res


  let skipWhitespace scanner =
    let rec scan () =
      if scanner.ch == CharacterCodes.space || scanner.ch == CharacterCodes.tab then (
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
      CharacterCodes.underscore == scanner.ch ||
      CharacterCodes.singleQuote == scanner.ch
    ) do
      next scanner
    done;
    let str = Bytes.sub_string scanner.src startOff (scanner.offset - startOff) in
    Token.lookupKeyword str

  let scanDigits scanner ~base =
    if base <= 10 then (
      while CharacterCodes.isDigit scanner.ch || scanner.ch == CharacterCodes.underscore do
        next scanner
      done;
    ) else (
      while CharacterCodes.isHex scanner.ch || scanner.ch == CharacterCodes.underscore do
        next scanner
      done;
    )

  (* float: (0‚Ä¶9) { 0‚Ä¶9‚à£ _ } [. { 0‚Ä¶9‚à£ _ }] [(e‚à£ E) [+‚à£ -] (0‚Ä¶9) { 0‚Ä¶9‚à£ _ }]   *)
  let scanNumber scanner =
    let startOff = scanner.offset in

    (* integer part *)
    let base, prefix = if scanner.ch != CharacterCodes.dot then (
      if scanner.ch == CharacterCodes._0 then (
        next scanner;
        let ch = CharacterCodes.lower scanner.ch in
        if ch == CharacterCodes.Lower.x then (
          next scanner;
          16, 'x'
        ) else if ch == CharacterCodes.Lower.o then (
          next scanner;
          8, 'o'
        ) else if ch == CharacterCodes.Lower.b then (
          next scanner;
          2, 'b'
        ) else (
          8, '0'
        )
      ) else (
        10, ' '
      )
    ) else (10, ' ')
    in
    scanDigits scanner ~base;

    (*  *)
    let isFloat = if CharacterCodes.dot == scanner.ch then (
      next scanner;
      scanDigits scanner ~base;
      true
    ) else (
      false
    ) in

    (* exponent part *)
    let isFloat =
      if let exp = CharacterCodes.lower scanner.ch in
        exp == CharacterCodes.Lower.e || exp == CharacterCodes.Lower.p
      then (
        next scanner;
        if scanner.ch == CharacterCodes.plus || scanner.ch == CharacterCodes.minus then
          next scanner;
        scanDigits scanner ~base;
        true
      ) else
        isFloat
    in
    let literal =
      Bytes.sub_string scanner.src startOff (scanner.offset - startOff)
    in

    (* suffix *)
    let suffix =
      if scanner.ch >= CharacterCodes.Lower.g && scanner.ch <= CharacterCodes.Lower.z
         || scanner.ch >= CharacterCodes.Upper.g && scanner.ch <= CharacterCodes.Upper.z
      then (
        let ch = scanner.ch in
        next scanner;
        Some (Char.unsafe_chr ch)
      ) else
        None
    in
    if isFloat then
      Token.Float (literal, suffix)
    else
      Token.Int (literal, suffix)

  let scanExoticIdentifier scanner =
    next scanner;
    let buffer = Buffer.create 20 in
    let startPos = position scanner in

    let rec scan () =
      if scanner.ch == CharacterCodes.eof then
        let endPos = position scanner in
        scanner.err ~startPos ~endPos (Diagnostics.message "Did you forget a \" here?")
      else if scanner.ch == CharacterCodes.doubleQuote then (
        next scanner
      ) else if CharacterCodes.isLineBreak scanner.ch then (
        scanner.lineOffset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
        let endPos = position scanner in
        scanner.err ~startPos ~endPos (Diagnostics.message "Did you forget a \" here?");
        next scanner
      ) else (
        Buffer.add_char buffer (Char.chr scanner.ch);
        next scanner;
        scan()
      )
    in
    scan();
    Token.Lident (Buffer.contents buffer)

  let scanString scanner =
    let buffer = Buffer.create 256 in

    let startPos = position scanner in
    let rec scan () =
      if scanner.ch == CharacterCodes.eof then
        let endPos = position scanner in
        scanner.err ~startPos ~endPos Diagnostics.unclosedString
      else if scanner.ch == CharacterCodes.doubleQuote then (
        next scanner;
      ) else if scanner.ch == CharacterCodes.backslash then (
        next scanner;
        let char_for_backslash = function
          | 98 -> '\008'
          | 110 -> '\010'
          | 114 -> '\013'
          | 116 -> '\009'
          | c   -> Char.chr c
        in
        Buffer.add_char buffer (char_for_backslash scanner.ch);
        next scanner;
        scan ()
      ) else if CharacterCodes.isLineBreak scanner.ch then (
        scanner.lineOffset <- scanner.offset + 1;
        scanner.lnum <- scanner.lnum + 1;
        let endPos = position scanner in
        scanner.err ~startPos ~endPos Diagnostics.unclosedString;
        next scanner
      ) else (
        Buffer.add_char buffer (Char.chr scanner.ch);
        next scanner;
        scan ()
      )
    in
    scan ();
    Token.String (Buffer.contents buffer)

  (* I wonder if this gets inlined *)
  let convertNumber scanner ~n ~base ~max =
    let x = ref 0 in
    for _ = n downto 1 do
      let d = CharacterCodes.digitValue scanner.ch in
      x := (!x * base) + d;
      next scanner
    done;
    !x

  let scanEscape scanner =
    (* let offset = scanner.offset in *)
    let c = match scanner.ch with
    | 98 (* b *)  -> next scanner; '\008'
    | 110 (* n *) -> next scanner; '\010'
    | 114 (* r *) -> next scanner; '\013'
    | 116 (* t *) -> next scanner; '\009'
    | ch when CharacterCodes.isDigit ch ->
      let x = convertNumber scanner ~n:3 ~base:10 ~max:255 in
      Char.chr x
    | ch when ch == CharacterCodes.Lower.x ->
      next scanner;
      let x = convertNumber scanner ~n:2 ~base:16 ~max:255 in
      Char.chr x
    | ch when ch == CharacterCodes.Lower.o ->
      next scanner;
      let x = convertNumber scanner ~n:3 ~base:8 ~max:255 in
      Char.chr x
    | ch -> next scanner; Char.chr ch
    in
    next scanner; (* Consume \' *)
    Token.Character c

  let scanSingleLineComment scanner =
    let startOff = scanner.offset in
    let startPos = position scanner in
    while not (CharacterCodes.isLineBreak scanner.ch) && scanner.ch >= 0 do
      next scanner
    done;
    let endPos = position scanner in
    Token.Comment (
      Comment.makeSingleLineComment
        ~loc:(Location.{loc_start = startPos; loc_end = endPos; loc_ghost = false})
        (Bytes.sub_string scanner.src startOff (scanner.offset - startOff))
    )

  let scanMultiLineComment scanner =
    let startOff = scanner.offset in
    let startPos = position scanner in
    let rec scan () =
      if scanner.ch == CharacterCodes.asterisk &&
         peek scanner == CharacterCodes.forwardslash then (
        next scanner;
        next scanner
      ) else if scanner.ch == CharacterCodes.eof then (
        let endPos = position scanner in
        scanner.err ~startPos ~endPos Diagnostics.unclosedComment
      ) else (
        if CharacterCodes.isLineBreak scanner.ch then (
          scanner.lineOffset <- scanner.offset + 1;
          scanner.lnum <- scanner.lnum + 1;
        );
        next scanner;
        scan ()
      )
    in
    scan();
    Token.Comment (
      Comment.makeMultiLineComment
        ~loc:(Location.{loc_start = startPos; loc_end = (position scanner); loc_ghost = false})
        (Bytes.sub_string scanner.src startOff (scanner.offset - 2 - startOff))
    )

  let scanTemplate scanner =
    let startOff = scanner.offset in
    let startPos = position scanner in

    let rec scan () =
      if scanner.ch == CharacterCodes.eof then (
        let endPos = position scanner in
        scanner.err ~startPos ~endPos Diagnostics.unclosedTemplate;
        popMode scanner Template;
        Token.TemplateTail(
          Bytes.sub_string scanner.src startOff (scanner.offset - 2 - startOff)
        )
      )
      else if scanner.ch == CharacterCodes.backtick then (
        next scanner;
        let contents =
          Bytes.sub_string scanner.src startOff (scanner.offset - 1 - startOff)
        in
        popMode scanner Template;
        Token.TemplateTail contents
      ) else if scanner.ch == CharacterCodes.dollar &&
                peek scanner == CharacterCodes.lbrace
        then (
          next scanner; (* consume $ *)
          next scanner; (* consume { *)
          let contents =
            Bytes.sub_string scanner.src startOff (scanner.offset - 2 - startOff)
          in
          popMode scanner Template;
          Token.TemplatePart contents
      ) else (
        if CharacterCodes.isLineBreak scanner.ch then (
          scanner.lineOffset <- scanner.offset + 1;
          scanner.lnum <- scanner.lnum + 1;
        );
        next scanner;
        scan()
      )
    in
    scan()

  let rec scan scanner =
    if not (inTemplateMode scanner) then skipWhitespace scanner;
    let startPos = position scanner in
    let ch = scanner.ch in
    let token = if inTemplateMode scanner then
      scanTemplate scanner
    else if ch == CharacterCodes.underscore then (
      let nextCh = peek scanner in
      if nextCh == CharacterCodes.underscore || CharacterCodes.isDigit nextCh || CharacterCodes.isLetter nextCh then
        scanIdentifier scanner
      else (
        next scanner;
        Token.Underscore
      )
    ) else if CharacterCodes.isLetter ch then
      scanIdentifier scanner
    else if CharacterCodes.isDigit ch then
      scanNumber scanner
    else begin
      next scanner;
      if ch == CharacterCodes.dot then
        if scanner.ch == CharacterCodes.dot then (
          next scanner;
          if scanner.ch == CharacterCodes.dot then (
            next scanner;
            Token.DotDotDot
          ) else (
            Token.DotDot
          )
        ) else (
          Token.Dot
        )
      else if ch == CharacterCodes.doubleQuote then
        scanString scanner
      else if ch == CharacterCodes.singleQuote then (
        if scanner.ch == CharacterCodes.backslash then (
          next scanner;
          scanEscape scanner
        ) else if (peek scanner) == CharacterCodes.singleQuote then (
          let ch = scanner.ch in
          next scanner;
          next scanner;
          Token.Character (Char.chr ch)
        ) else (
          SingleQuote
        )
      ) else if ch == CharacterCodes.bang then
        if scanner.ch == CharacterCodes.equal then (
          next scanner;
          if scanner.ch == CharacterCodes.equal then (
            next scanner;
            Token.BangEqualEqual
          ) else (
            Token.BangEqual
          )
        ) else (
          Token.Bang
        )
      else if ch == CharacterCodes.semicolon then
        Token.Semicolon
      else if ch == CharacterCodes.equal then (
        if scanner.ch == CharacterCodes.greaterThan then (
          next scanner;
          Token.EqualGreater
        ) else if scanner.ch == CharacterCodes.equal then (
          next scanner;
          if scanner.ch == CharacterCodes.equal then (
            next scanner;
            Token.EqualEqualEqual
          ) else (
            Token.EqualEqual
          )
        ) else (
          Token.Equal
        )
      ) else if ch == CharacterCodes.bar then
        if scanner.ch == CharacterCodes.bar then (
          next scanner;
          Token.Lor
        ) else if scanner.ch == CharacterCodes.greaterThan then (
          next scanner;
          Token.BarGreater
        ) else (
          Token.Bar
        )
      else if ch == CharacterCodes.ampersand then
        if scanner.ch == CharacterCodes.ampersand then (
          next scanner;
          Token.Land
        ) else (
          Token.Band
        )
      else if ch == CharacterCodes.lparen then
        Token.Lparen
      else if ch == CharacterCodes.rparen then
        Token.Rparen
      else if ch == CharacterCodes.lbracket then
        Token.Lbracket
      else if ch == CharacterCodes.rbracket then
        Token.Rbracket
      else if ch == CharacterCodes.lbrace then
        Token.Lbrace
      else if ch == CharacterCodes.rbrace then
        Token.Rbrace
      else if ch == CharacterCodes.comma then
        Token.Comma
      else if ch == CharacterCodes.colon then
       if scanner.ch == CharacterCodes.equal then(
          next scanner;
          Token.ColonEqual
        ) else (
          Token.Colon
        )
      else if ch == CharacterCodes.backslash then
        scanExoticIdentifier scanner
      else if ch == CharacterCodes.forwardslash then
        if scanner.ch == CharacterCodes.forwardslash then (
          next scanner;
          scanSingleLineComment scanner
        ) else if (scanner.ch == CharacterCodes.asterisk) then (
          next scanner;
          scanMultiLineComment scanner
        ) else if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.ForwardslashDot
        ) else (
          Token.Forwardslash
        )
      else if ch == CharacterCodes.minus then
        if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.MinusDot
        ) else if scanner.ch == CharacterCodes.greaterThan then (
          next scanner;
          Token.MinusGreater;
        ) else (
          Token.Minus
        )
      else if ch == CharacterCodes.plus then
        if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.PlusDot
        ) else if scanner.ch == CharacterCodes.plus then (
          next scanner;
          Token.PlusPlus
        ) else if scanner.ch == CharacterCodes.equal then (
          next scanner;
          Token.PlusEqual
        ) else (
          Token.Plus
        )
      else if ch == CharacterCodes.greaterThan then
        if scanner.ch == CharacterCodes.equal && not (inDiamondMode scanner) then (
          next scanner;
          Token.GreaterEqual
        ) else (
          Token.GreaterThan
        )
      else if ch == CharacterCodes.lessThan then
        (* Imagine the following: <div><
         * < indicates the start of a new jsx-element, the parser expects
         * the name of a new element after the <
         * Example: <div> <div
         * But what if we have a / here: example </ in  <div></div>
         * This signals a closing element. To simulate the two-token lookahead,
         * the </ is emitted as a single new token LessThanSlash *)
        if inJsxMode scanner then (
          skipWhitespace scanner;
          if scanner.ch == CharacterCodes.forwardslash then
            let () = next scanner in
            Token.LessThanSlash
          else
            Token.LessThan
        ) else if scanner.ch == CharacterCodes.equal then (
          next scanner;
          Token.LessEqual
        ) else (
          Token.LessThan
        )
      else if ch == CharacterCodes.hash then
        if scanner.ch == CharacterCodes.hash then(
          next scanner;
          Token.HashHash
        ) else if scanner.ch == CharacterCodes.equal then(
          next scanner;
          Token.HashEqual
        ) else (
          Token.Hash
        )
      else if ch == CharacterCodes.asterisk then
        if scanner.ch == CharacterCodes.asterisk then (
          next scanner;
          Token.Exponentiation;
        ) else if scanner.ch == CharacterCodes.dot then (
          next scanner;
          Token.AsteriskDot
        ) else (
          Token.Asterisk
        )
      else if ch == CharacterCodes.tilde then
        Token.Tilde
      else if ch == CharacterCodes.question then
        Token.Question
      else if ch == CharacterCodes.at then
        if scanner.ch == CharacterCodes.at then (
          next scanner;
          Token.AtAt
        ) else (
          Token.At
        )
    else if ch == CharacterCodes.percent then
      if scanner.ch == CharacterCodes.percent then (
        next scanner;
        Token.PercentPercent
      ) else (
        Token.Percent
      )
      else if ch == CharacterCodes.backtick  then
        Token.Backtick
      else if ch == -1 then
        Token.Eof
      else (
        (* if we arrive here, we're dealing with an unkown character,
         * report the error and continue scanning‚Ä¶ *)
        let endPos = position scanner in
        scanner.err ~startPos ~endPos (Diagnostics.unknownUchar ch);
        let (_, _, token) = scan scanner in
        token
      )
    end in
    let endPos = position scanner in
    (startPos, endPos, token)

  (* Imagine: <div> <Navbar /> <
   * is `<` the start of a jsx-child? <div ‚Ä¶
   * or is it the start of a closing tag?  </div>
   * reconsiderLessThan peeks at the next token and
   * determines the correct token to disambiguate *)
  let reconsiderLessThan scanner =
    (* < consumed *)
    skipWhitespace scanner;
    if scanner.ch == CharacterCodes.forwardslash then
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
          |> Bytes.get src
          |> Char.code
        in
        c == CharacterCodes.space ||
        c == CharacterCodes.tab ||
        CharacterCodes.isLineBreak c
      in
      let rightOk =
        let c =
          if endCnum == Bytes.length src then -1
          else endCnum |> Bytes.get src |> Char.code
        in
        c == CharacterCodes.space ||
        c == CharacterCodes.tab ||
        CharacterCodes.isLineBreak c ||
        c == CharacterCodes.eof
      in
      leftOk && rightOk
end
