module Comment = Res_comment

type t =
  | Await
  | Open
  | True
  | False
  | Codepoint of {c: int; original: string}
  | Int of {i: string; suffix: char option}
  | Float of {f: string; suffix: char option}
  | String of string
  | Lident of string
  | Uident of string
  | As
  | Dot
  | DotDot
  | DotDotDot
  | Bang
  | Semicolon
  | Let
  | And
  | Rec
  | Underscore
  | SingleQuote
  | Equal
  | EqualEqual
  | EqualEqualEqual
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
  | Forwardslash
  | ForwardslashDot
  | Regex of string * string
  | Asterisk
  | AsteriskDot
  | Exponentiation
  | Minus
  | MinusDot
  | Plus
  | PlusDot
  | PlusPlus
  | PlusEqual
  | ColonGreaterThan
  | GreaterThan
  | LessThan
  | LessThanLessThan
  | LessThanSlash
  | Hash
  | HashEqual
  | Assert
  | Tilde
  | Question
  | If
  | Else
  | For
  | In
  | While
  | Switch
  | When
  | EqualGreater
  | MinusGreater
  | External
  | Typ
  | Private
  | Mutable
  | Constraint
  | Include
  | Module
  | Of
  | Land
  | Lor
  | Band (* Bitwise and: & *)
  | BangEqual
  | BangEqualEqual
  | LessEqual
  | GreaterEqual
  | ColonEqual
  | At
  | AtAt
  | Percent
  | PercentPercent
  | Comment of Comment.t
  | List
  | Dict
  | TemplateTail of string * Lexing.position
  | TemplatePart of string * Lexing.position
  | Backtick
  | BarGreater
  | Try
  | DocComment of Location.t * string
  | ModuleComment of Location.t * string

let precedence = function
  | HashEqual | ColonEqual -> 1
  | Lor -> 2
  | Land -> 3
  | Equal | EqualEqual | EqualEqualEqual | LessThan | GreaterThan | BangEqual
  | BangEqualEqual | LessEqual | GreaterEqual | BarGreater ->
    4
  | Plus | PlusDot | Minus | MinusDot | PlusPlus | LessThanLessThan -> 5
  | Asterisk | AsteriskDot | Forwardslash | ForwardslashDot | Percent -> 6
  | Exponentiation -> 7
  | MinusGreater -> 8
  | Dot -> 9
  | _ -> 0

let to_string = function
  | Await -> "await"
  | Open -> "open"
  | True -> "true"
  | False -> "false"
  | Codepoint {original} -> "codepoint '" ^ original ^ "'"
  | String s -> "string \"" ^ s ^ "\""
  | Lident str -> str
  | Uident str -> str
  | Dot -> "."
  | DotDot -> ".."
  | DotDotDot -> "..."
  | Int {i} -> "int " ^ i
  | Float {f} -> "Float: " ^ f
  | Bang -> "!"
  | Semicolon -> ";"
  | Let -> "let"
  | And -> "and"
  | Rec -> "rec"
  | Underscore -> "_"
  | SingleQuote -> "'"
  | Equal -> "="
  | EqualEqual -> "=="
  | EqualEqualEqual -> "==="
  | Eof -> "eof"
  | Bar -> "|"
  | As -> "as"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbracket -> "["
  | Rbracket -> "]"
  | Lbrace -> "{"
  | Rbrace -> "}"
  | ColonGreaterThan -> ":>"
  | Colon -> ":"
  | Comma -> ","
  | Minus -> "-"
  | MinusDot -> "-."
  | Plus -> "+"
  | PlusDot -> "+."
  | PlusPlus -> "++"
  | PlusEqual -> "+="
  | Backslash -> "\\"
  | Regex (pattern, flags) -> "regex: /" ^ pattern ^ "/" ^ flags
  | Forwardslash -> "/"
  | ForwardslashDot -> "/."
  | Exception -> "exception"
  | Hash -> "#"
  | HashEqual -> "#="
  | GreaterThan -> ">"
  | LessThan -> "<"
  | LessThanLessThan -> "<<"
  | LessThanSlash -> "</"
  | Asterisk -> "*"
  | AsteriskDot -> "*."
  | Exponentiation -> "**"
  | Assert -> "assert"
  | Tilde -> "tilde"
  | Question -> "?"
  | If -> "if"
  | Else -> "else"
  | For -> "for"
  | In -> "in"
  | While -> "while"
  | Switch -> "switch"
  | When -> "when"
  | EqualGreater -> "=>"
  | MinusGreater -> "->"
  | External -> "external"
  | Typ -> "type"
  | Private -> "private"
  | Constraint -> "constraint"
  | Mutable -> "mutable"
  | Include -> "include"
  | Module -> "module"
  | Of -> "of"
  | Lor -> "||"
  | Band -> "&"
  | Land -> "&&"
  | BangEqual -> "!="
  | BangEqualEqual -> "!=="
  | GreaterEqual -> ">="
  | LessEqual -> "<="
  | ColonEqual -> ":="
  | At -> "@"
  | AtAt -> "@@"
  | Percent -> "%"
  | PercentPercent -> "%%"
  | Comment c -> "Comment" ^ Comment.to_string c
  | List -> "list{"
  | Dict -> "dict{"
  | TemplatePart (text, _) -> text ^ "${"
  | TemplateTail (text, _) -> "TemplateTail(" ^ text ^ ")"
  | Backtick -> "`"
  | BarGreater -> "|>"
  | Try -> "try"
  | DocComment (_loc, s) -> "DocComment " ^ s
  | ModuleComment (_loc, s) -> "ModuleComment " ^ s

let keyword_table = function
  | "and" -> And
  | "as" -> As
  | "assert" -> Assert
  | "await" -> Await
  | "constraint" -> Constraint
  | "else" -> Else
  | "exception" -> Exception
  | "external" -> External
  | "false" -> False
  | "for" -> For
  | "if" -> If
  | "in" -> In
  | "include" -> Include
  | "let" -> Let
  | "list{" -> List
  | "dict{" -> Dict
  | "module" -> Module
  | "mutable" -> Mutable
  | "of" -> Of
  | "open" -> Open
  | "private" -> Private
  | "rec" -> Rec
  | "switch" -> Switch
  | "true" -> True
  | "try" -> Try
  | "type" -> Typ
  | "when" -> When
  | "while" -> While
  | _ -> raise Not_found
[@@raises Not_found]

let is_keyword = function
  | Await | And | As | Assert | Constraint | Else | Exception | External | False
  | For | If | In | Include | Land | Let | List | Lor | Module | Mutable | Of
  | Open | Private | Rec | Switch | True | Try | Typ | When | While | Dict ->
    true
  | _ -> false

let lookup_keyword str =
  try keyword_table str
  with Not_found -> (
    match str.[0] [@doesNotRaise] with
    | 'A' .. 'Z' -> Uident str
    | _ -> Lident str)

let is_keyword_txt str =
  try
    let _ = keyword_table str in
    true
  with Not_found -> false

let catch = Lident "catch"
