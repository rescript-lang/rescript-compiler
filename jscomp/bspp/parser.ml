type token =
  | AMPERAMPER
  | AMPERSAND
  | AND
  | AS
  | ASSERT
  | BACKQUOTE
  | BANG
  | BAR
  | BARBAR
  | BARRBRACKET
  | BEGIN
  | CHAR of char
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
  | CONSTRAINT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | END
  | EOF
  | EQUAL
  | EXCEPTION
  | EXTERNAL
  | FALSE
  | FLOAT of string
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | IF
  | IN
  | INCLUDE
  | INFIXOP0 of string
  | INFIXOP1 of string
  | INFIXOP2 of string
  | INFIXOP3 of string
  | INFIXOP4 of string
  | INHERIT
  | INITIALIZER
  | INT of int
  | INT32 of int32
  | INT64 of int64
  | LABEL of string
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | LBRACKETGREATER
  | LBRACKETPERCENT
  | LBRACKETPERCENTPERCENT
  | LESS
  | LESSMINUS
  | LET
  | LIDENT of string
  | LPAREN
  | LBRACKETAT
  | LBRACKETATAT
  | LBRACKETATATAT
  | MATCH
  | METHOD
  | MINUS
  | MINUSDOT
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NATIVEINT of nativeint
  | NEW
  | NONREC
  | OBJECT
  | OF
  | OPEN
  | OPTLABEL of string
  | OR
  | PERCENT
  | PLUS
  | PLUSDOT
  | PLUSEQ
  | PREFIXOP of string
  | PRIVATE
  | QUESTION
  | QUOTE
  | RBRACE
  | RBRACKET
  | REC
  | RPAREN
  | SEMI
  | SEMISEMI
  | SHARP
  | SHARPOP of string
  | SIG
  | STAR
  | STRING of (string * string option)
  | STRUCT
  | THEN
  | TILDE
  | TO
  | TRUE
  | TRY
  | TYPE
  | UIDENT of string
  | UNDERSCORE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH
  | COMMENT of (string * Location.t)
  | DOCSTRING of Docstrings.docstring
  | EOL
