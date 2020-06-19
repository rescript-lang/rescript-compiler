
module Comment = Napkin_comment
module CharacterCodes = Napkin_character_codes

type t =
  | Open
  | True | False
  | Character of char
  | Int of {i: string; suffix: char option}
  | Float of {f: string; suffix: char option}
  | String of string
  | Lident of string
  | Uident of string
  | As
  | Dot | DotDot | DotDotDot
  | Bang
  | Semicolon
  | Let
  | And
  | Rec
  | Underscore
  | SingleQuote
  | Equal | EqualEqual | EqualEqualEqual
  | Bar
  | Lparen
  | Rparen
  | Lbracket
  | Rbracket
  | Lbrace
  | Rbrace
  | Colon
  | Comma
  | Eof
  | Exception
  | Backslash [@live]
  | Forwardslash | ForwardslashDot
  | Asterisk | AsteriskDot | Exponentiation
  | Minus | MinusDot
  | Plus | PlusDot | PlusPlus | PlusEqual
  | ColonGreaterThan
  | GreaterThan
  | LessThan
  | LessThanSlash
  | Hash | HashEqual | HashHash
  | Assert
  | Lazy
  | Tilde
  | Question
  | If | Else | For | In | To | Downto | While | Switch
  | When
  | EqualGreater | MinusGreater
  | External
  | Typ
  | Private
  | Mutable
  | Constraint
  | Include
  | Module
  | Of
  | With
  | Land | Lor
  | Band (* Bitwise and: & *)
  | BangEqual | BangEqualEqual
  | LessEqual | GreaterEqual
  | ColonEqual
  | At | AtAt
  | Percent | PercentPercent
  | Comment of Comment.t
  | List
  | TemplateTail of string
  | TemplatePart of string
  | Backtick
  | BarGreater
  | Try | Catch
  | Import
  | Export

let precedence = function
  | HashEqual | ColonEqual -> 1
  | Lor -> 2
  | Land -> 3
  | Equal | EqualEqual | EqualEqualEqual | LessThan | GreaterThan
  | BangEqual | BangEqualEqual | LessEqual | GreaterEqual | BarGreater -> 4
  | Plus | PlusDot | Minus | MinusDot | PlusPlus -> 5
  | Asterisk | AsteriskDot | Forwardslash | ForwardslashDot -> 6
  | Exponentiation -> 7
  | MinusGreater -> 8
  | Dot -> 9
  | _ -> 0

let toString = function
  | Open -> "open"
  | True -> "true" | False -> "false"
  | Character c -> "'" ^ (Char.escaped c) ^ "'"
  | String s -> s
  | Lident str -> str
  | Uident str -> str
  | Dot -> "." | DotDot -> ".." | DotDotDot -> "..."
  | Int {i} -> "int " ^ i
  | Float {f} -> "Float: " ^ f
  | Bang -> "!"
  | Semicolon -> ";"
  | Let -> "let"
  | And -> "and"
  | Rec -> "rec"
  | Underscore -> "_"
  | SingleQuote -> "'"
  | Equal -> "=" | EqualEqual -> "==" | EqualEqualEqual -> "==="
  | Eof -> "eof"
  | Bar -> "|"
  | As -> "as"
  | Lparen -> "(" | Rparen -> ")"
  | Lbracket -> "[" | Rbracket -> "]"
  | Lbrace -> "{" | Rbrace -> "}"
  | ColonGreaterThan -> ":>"
  | Colon -> ":"
  | Comma -> ","
  | Minus -> "-" | MinusDot -> "-."
  | Plus -> "+" | PlusDot -> "+." | PlusPlus -> "++" | PlusEqual -> "+="
  | Backslash -> "\\"
  | Forwardslash -> "/" | ForwardslashDot -> "/."
  | Exception -> "exception"
  | Hash -> "#" | HashHash -> "##" | HashEqual -> "#="
  | GreaterThan -> ">"
  | LessThan -> "<"
  | LessThanSlash -> "</"
  | Asterisk -> "*" | AsteriskDot -> "*." | Exponentiation -> "**"
  | Assert -> "assert"
  | Lazy -> "lazy"
  | Tilde -> "tilde"
  | Question -> "?"
  | If -> "if"
  | Else -> "else"
  | For -> "for"
  | In -> "in"
  | To -> "to"
  | Downto -> "downto"
  | While -> "while"
  | Switch -> "switch"
  | When -> "when"
  | EqualGreater -> "=>" | MinusGreater -> "->"
  | External -> "external"
  | Typ -> "type"
  | Private -> "private"
  | Constraint -> "constraint"
  | Mutable -> "mutable"
  | Include -> "include"
  | Module -> "module"
  | Of -> "of"
  | With -> "with"
  | Lor -> "||"
  | Band -> "&" | Land -> "&&"
  | BangEqual -> "!=" | BangEqualEqual -> "!=="
  | GreaterEqual -> ">=" | LessEqual -> "<="
  | ColonEqual -> ":="
  | At -> "@" | AtAt -> "@@"
  | Percent -> "%" | PercentPercent -> "%%"
  | Comment c -> "Comment(" ^ (Comment.toString c) ^ ")"
  | List -> "list"
  | TemplatePart text -> text ^ "${"
  | TemplateTail text -> "TemplateTail(" ^ text ^ ")"
  | Backtick -> "`"
  | BarGreater -> "|>"
  | Try -> "try" | Catch -> "catch"
  | Import -> "import"
  | Export -> "export"

let keywordTable = function
| "true" -> True
| "false" -> False
| "open" -> Open
| "let" -> Let
| "rec" -> Rec
| "and" -> And
| "as" -> As
| "exception" -> Exception
| "assert" -> Assert
| "lazy" -> Lazy
| "if" -> If
| "else" -> Else
| "for" -> For
| "in" -> In
| "to" -> To
| "downto" -> Downto
| "while" -> While
| "switch" -> Switch
| "when" -> When
| "external" -> External
| "type" -> Typ
| "private" -> Private
| "mutable" -> Mutable
| "constraint" -> Constraint
| "include" -> Include
| "module" -> Module
| "of" -> Of
| "list" -> List
| "with" -> With
| "try" -> Try
| "catch" -> Catch
| "import" -> Import
| "export" -> Export
| _ -> raise Not_found
[@@raises Not_found]

let isKeyword = function
  | True | False | Open | Let | Rec | And | As
  | Exception | Assert | Lazy | If | Else | For | In | To
  | Downto | While | Switch | When | External | Typ | Private
  | Mutable | Constraint | Include | Module | Of
  | Land | Lor | List | With
  | Try | Catch | Import | Export -> true
  | _ -> false

let lookupKeyword str =
  try keywordTable str with
  | Not_found ->
    if CharacterCodes.isUpperCase (int_of_char (str.[0] [@doesNotRaise])) then
      Uident str
    else Lident str

let isKeywordTxt str =
  try let _ = keywordTable str in true with
  | Not_found -> false