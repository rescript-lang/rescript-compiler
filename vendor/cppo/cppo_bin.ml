module Cppo_command : sig 
#1 "cppo_command.mli"
type command_token =
  [ `Text of string
  | `Loc_file
  | `Loc_first_line
  | `Loc_last_line ]

type command_template = command_token list

val subst : command_template -> string -> int -> int -> string

val parse : string -> command_template

end = struct
#1 "cppo_command.ml"
open Printf

type command_token =
    [ `Text of string
    | `Loc_file
    | `Loc_first_line
    | `Loc_last_line ]

type command_template = command_token list

let parse s : command_template =
  let rec loop acc buf s len i =
    if i >= len then
      let s = Buffer.contents buf in
      if s = "" then acc
      else `Text s :: acc
    else if i = len - 1 then (
      Buffer.add_char buf s.[i];
      `Text (Buffer.contents buf) :: acc
    )
    else
      let c = s.[i] in
      if c = '%' then
        let acc =
          let s = Buffer.contents buf in
          Buffer.clear buf;
          if s = "" then acc
          else
            `Text s :: acc
        in
        let x =
          match s.[i+1] with
              'F' -> `Loc_file
            | 'B' -> `Loc_first_line
            | 'E' -> `Loc_last_line
            | '%' -> `Text "%"
            | _ ->
                failwith (
                  sprintf "Invalid escape sequence in command template %S. \
                             Use %%%% for a %% sign." s
                )
        in
        loop (x :: acc) buf s len (i + 2)
      else (
        Buffer.add_char buf c;
        loop acc buf s len (i + 1)
      )
  in
  let len = String.length s in
  List.rev (loop [] (Buffer.create len) s len 0)


let subst (cmd : command_template) file first last =
  let l =
    List.map (
      function
          `Text s -> s
        | `Loc_file -> file
        | `Loc_first_line -> string_of_int first
        | `Loc_last_line -> string_of_int last
    ) cmd
  in
  String.concat "" l

end
module Cppo_types : sig 
#1 "cppo_types.mli"
type loc = Lexing.position * Lexing.position

exception Cppo_error of string

type bool_expr =
    [ `True
    | `False
    | `Defined of string
    | `Not of bool_expr (* not *)
    | `And of (bool_expr * bool_expr) (* && *)
    | `Or of (bool_expr * bool_expr) (* || *)
    | `Eq of (arith_expr * arith_expr) (* = *)
    | `Lt of (arith_expr * arith_expr) (* < *)
    | `Gt of (arith_expr * arith_expr) (* > *)
        (* syntax for additional operators: <>, <=, >= *)
    ]

and arith_expr = (* signed int64 *)
    [ `Int of int64
    | `Ident of (loc * string)
        (* must be bound to a valid int literal.
           Expansion of macro functions is not supported. *)

    | `Tuple of (loc * arith_expr list)
        (* tuple of 2 or more elements guaranteed by the syntax *)

    | `Neg of arith_expr (* - *)
    | `Add of (arith_expr * arith_expr) (* + *)
    | `Sub of (arith_expr * arith_expr) (* - *)
    | `Mul of (arith_expr * arith_expr) (* * *)
    | `Div of (loc * arith_expr * arith_expr) (* / *)
    | `Mod of (loc * arith_expr * arith_expr) (* mod *)

    (* Bitwise operations on 64 bits *)
    | `Lnot of arith_expr (* lnot *)
    | `Lsl of (arith_expr * arith_expr) (* lsl *)
    | `Lsr of (arith_expr * arith_expr) (* lsr *)
    | `Asr of (arith_expr * arith_expr) (* asr *)
    | `Land of (arith_expr * arith_expr) (* land *)
    | `Lor of (arith_expr * arith_expr) (* lor *)
    | `Lxor of (arith_expr * arith_expr) (* lxor *)
    ]

and node =
    [ `Ident of (loc * string * node list list option)
    | `Def of (loc * string * node list)
    | `Defun of (loc * string * string list * node list)
    | `Undef of (loc * string)
    | `Include of (loc * string)
    | `Ext of (loc * string * string)
    | `Cond of (loc * bool_expr * node list * node list)
    | `Error of (loc * string)
    | `Warning of (loc * string)
    | `Text of (loc * bool * string) (* bool is true for space tokens *)
    | `Seq of node list
    | `Stringify of node
    | `Capitalize of node
    | `Concat of (node * node)
    | `Line of (loc * string option * int)
    | `Current_line of loc
    | `Current_file of loc ]

val dummy_loc : loc

val error : loc -> string -> _

val warning : loc -> string -> unit

val flatten_nodes : node list -> node list


end = struct
#1 "cppo_types.ml"
open Printf
open Lexing

module String_set = Set.Make (String)
module String_map = Map.Make (String)

type loc = position * position

type bool_expr =
    [ `True
    | `False
    | `Defined of string
    | `Not of bool_expr (* not *)
    | `And of (bool_expr * bool_expr) (* && *)
    | `Or of (bool_expr * bool_expr) (* || *)
    | `Eq of (arith_expr * arith_expr) (* = *)
    | `Lt of (arith_expr * arith_expr) (* < *)
    | `Gt of (arith_expr * arith_expr) (* > *)
        (* syntax for additional operators: <>, <=, >= *)
    ]

and arith_expr = (* signed int64 *)
    [ `Int of int64
    | `Ident of (loc * string)
        (* must be bound to a valid int literal.
           Expansion of macro functions is not supported. *)

    | `Tuple of (loc * arith_expr list)
        (* tuple of 2 or more elements guaranteed by the syntax *)

    | `Neg of arith_expr (* - *)
    | `Add of (arith_expr * arith_expr) (* + *)
    | `Sub of (arith_expr * arith_expr) (* - *)
    | `Mul of (arith_expr * arith_expr) (* * *)
    | `Div of (loc * arith_expr * arith_expr) (* / *)
    | `Mod of (loc * arith_expr * arith_expr) (* mod *)

    (* Bitwise operations on 64 bits *)
    | `Lnot of arith_expr (* lnot *)
    | `Lsl of (arith_expr * arith_expr) (* lsl *)
    | `Lsr of (arith_expr * arith_expr) (* lsr *)
    | `Asr of (arith_expr * arith_expr) (* asr *)
    | `Land of (arith_expr * arith_expr) (* land *)
    | `Lor of (arith_expr * arith_expr) (* lor *)
    | `Lxor of (arith_expr * arith_expr) (* lxor *)
    ]

and node =
    [ `Ident of (loc * string * node list list option)
    | `Def of (loc * string * node list)
    | `Defun of (loc * string * string list * node list)
    | `Undef of (loc * string)
    | `Include of (loc * string)
    | `Ext of (loc * string * string)
    | `Cond of (loc * bool_expr * node list * node list)
    | `Error of (loc * string)
    | `Warning of (loc * string)
    | `Text of (loc * bool * string) (* bool is true for space tokens *)
    | `Seq of node list
    | `Stringify of node
    | `Capitalize of node
    | `Concat of (node * node)
    | `Line of (loc * string option * int)
    | `Current_line of loc
    | `Current_file of loc ]



let string_of_loc (pos1, pos2) =
  let line1 = pos1.pos_lnum
  and start1 = pos1.pos_bol in
  Printf.sprintf "File %S, line %i, characters %i-%i"
    pos1.pos_fname line1
    (pos1.pos_cnum - start1)
    (pos2.pos_cnum - start1)


exception Cppo_error of string

let error loc s =
  let msg =
    sprintf "%s\nError: %s" (string_of_loc loc) s in
  raise (Cppo_error msg)

let warning loc s =
  let msg =
    sprintf "%s\nWarning: %s" (string_of_loc loc) s in
  eprintf "%s\n%!" msg

let dummy_loc = (Lexing.dummy_pos, Lexing.dummy_pos)

let rec flatten_nodes (l: node list): node list =
  List.flatten (List.map flatten_node l)

and flatten_node (node: node): node list =
  match node with
  | `Seq l -> flatten_nodes l
  | x -> [x]

end
module Cppo_parser : sig 
#1 "cppo_parser.mli"
type token =
  | DEF of ( Cppo_types.loc * string )
  | DEFUN of ( Cppo_types.loc * string )
  | UNDEF of ( Cppo_types.loc * string )
  | INCLUDE of ( Cppo_types.loc * string )
  | WARNING of ( Cppo_types.loc * string )
  | ERROR of ( Cppo_types.loc * string )
  | LINE of ( Cppo_types.loc * string option * int )
  | IFDEF of ( Cppo_types.loc * Cppo_types.bool_expr )
  | EXT of ( Cppo_types.loc * string * string )
  | ENDEF of ( Cppo_types.loc )
  | IF of ( Cppo_types.loc )
  | ELIF of ( Cppo_types.loc )
  | ELSE of ( Cppo_types.loc )
  | ENDIF of ( Cppo_types.loc )
  | ENDTEST of ( Cppo_types.loc )
  | TRUE
  | FALSE
  | DEFINED
  | NOT
  | AND
  | OR
  | EQ
  | LT
  | GT
  | NE
  | LE
  | GE
  | PLUS
  | MINUS
  | STAR
  | LNOT
  | LSL
  | LSR
  | ASR
  | LAND
  | LOR
  | LXOR
  | OP_PAREN of ( Cppo_types.loc )
  | SLASH of ( Cppo_types.loc )
  | MOD of ( Cppo_types.loc )
  | INT of ( int64 )
  | CL_PAREN of ( Cppo_types.loc )
  | COMMA of ( Cppo_types.loc )
  | CURRENT_LINE of ( Cppo_types.loc )
  | CURRENT_FILE of ( Cppo_types.loc )
  | IDENT of ( Cppo_types.loc * string )
  | FUNIDENT of ( Cppo_types.loc * string )
  | TEXT of ( Cppo_types.loc * bool * string )
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Cppo_types.node list 

end = struct
#1 "cppo_parser.ml"
type token =
  | DEF of ( Cppo_types.loc * string )
  | DEFUN of ( Cppo_types.loc * string )
  | UNDEF of ( Cppo_types.loc * string )
  | INCLUDE of ( Cppo_types.loc * string )
  | WARNING of ( Cppo_types.loc * string )
  | ERROR of ( Cppo_types.loc * string )
  | LINE of ( Cppo_types.loc * string option * int )
  | IFDEF of ( Cppo_types.loc * Cppo_types.bool_expr )
  | EXT of ( Cppo_types.loc * string * string )
  | ENDEF of ( Cppo_types.loc )
  | IF of ( Cppo_types.loc )
  | ELIF of ( Cppo_types.loc )
  | ELSE of ( Cppo_types.loc )
  | ENDIF of ( Cppo_types.loc )
  | ENDTEST of ( Cppo_types.loc )
  | TRUE
  | FALSE
  | DEFINED
  | NOT
  | AND
  | OR
  | EQ
  | LT
  | GT
  | NE
  | LE
  | GE
  | PLUS
  | MINUS
  | STAR
  | LNOT
  | LSL
  | LSR
  | ASR
  | LAND
  | LOR
  | LXOR
  | OP_PAREN of ( Cppo_types.loc )
  | SLASH of ( Cppo_types.loc )
  | MOD of ( Cppo_types.loc )
  | INT of ( int64 )
  | CL_PAREN of ( Cppo_types.loc )
  | COMMA of ( Cppo_types.loc )
  | CURRENT_LINE of ( Cppo_types.loc )
  | CURRENT_FILE of ( Cppo_types.loc )
  | IDENT of ( Cppo_types.loc * string )
  | FUNIDENT of ( Cppo_types.loc * string )
  | TEXT of ( Cppo_types.loc * bool * string )
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "src/cppo_parser.mly"
  open Cppo_types
# 57 "src/cppo_parser.ml"
let yytransl_const = [|
  272 (* TRUE *);
  273 (* FALSE *);
  274 (* DEFINED *);
  275 (* NOT *);
  276 (* AND *);
  277 (* OR *);
  278 (* EQ *);
  279 (* LT *);
  280 (* GT *);
  281 (* NE *);
  282 (* LE *);
  283 (* GE *);
  284 (* PLUS *);
  285 (* MINUS *);
  286 (* STAR *);
  287 (* LNOT *);
  288 (* LSL *);
  289 (* LSR *);
  290 (* ASR *);
  291 (* LAND *);
  292 (* LOR *);
  293 (* LXOR *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* DEF *);
  258 (* DEFUN *);
  259 (* UNDEF *);
  260 (* INCLUDE *);
  261 (* WARNING *);
  262 (* ERROR *);
  263 (* LINE *);
  264 (* IFDEF *);
  265 (* EXT *);
  266 (* ENDEF *);
  267 (* IF *);
  268 (* ELIF *);
  269 (* ELSE *);
  270 (* ENDIF *);
  271 (* ENDTEST *);
  294 (* OP_PAREN *);
  295 (* SLASH *);
  296 (* MOD *);
  297 (* INT *);
  298 (* CL_PAREN *);
  299 (* COMMA *);
  300 (* CURRENT_LINE *);
  301 (* CURRENT_FILE *);
  302 (* IDENT *);
  303 (* FUNIDENT *);
  304 (* TEXT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\004\000\004\000\002\000\002\000\
\002\000\002\000\005\000\005\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\011\000\
\011\000\011\000\008\000\008\000\007\000\007\000\007\000\009\000\
\009\000\012\000\012\000\010\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\000\000\002\000\000\000\001\000\001\000\
\001\000\001\000\001\000\003\000\001\000\001\000\003\000\002\000\
\001\000\001\000\003\000\005\000\002\000\001\000\001\000\001\000\
\001\000\001\000\005\000\005\000\004\000\004\000\001\000\004\000\
\002\000\000\000\003\000\001\000\002\000\002\000\000\000\004\000\
\002\000\002\000\000\000\002\000\001\000\001\000\002\000\003\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\022\000\025\000\023\000\024\000\
\031\000\000\000\026\000\000\000\008\000\009\000\010\000\017\000\
\018\000\014\000\000\000\013\000\002\000\076\000\000\000\007\000\
\000\000\000\000\021\000\000\000\000\000\000\000\000\000\045\000\
\046\000\000\000\000\000\000\000\000\000\000\000\058\000\059\000\
\000\000\000\000\000\000\016\000\000\000\000\000\000\000\011\000\
\000\000\001\000\003\000\019\000\042\000\000\000\000\000\000\000\
\000\000\000\000\047\000\049\000\000\000\073\000\072\000\000\000\
\000\000\000\000\000\000\044\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\000\015\000\000\000\000\000\000\000\
\033\000\030\000\029\000\000\000\048\000\000\000\060\000\000\000\
\050\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\066\000\067\000\068\000\069\000\070\000\
\071\000\000\000\065\000\038\000\037\000\012\000\035\000\020\000\
\040\000\000\000\074\000\028\000\027\000\032\000"

let yydgoto = "\002\000\
\022\000\025\000\026\000\046\000\047\000\024\000\090\000\049\000\
\029\000\041\000\058\000\030\000\042\000\043\000\066\000"

let yysindex = "\011\000\
\001\000\000\000\098\255\227\254\000\000\000\000\000\000\000\000\
\000\000\098\255\000\000\132\255\000\000\000\000\000\000\000\000\
\000\000\000\000\087\255\000\000\000\000\000\000\001\000\000\000\
\098\255\033\255\000\000\025\255\043\255\073\255\252\254\000\000\
\000\000\078\255\132\255\062\000\062\000\132\255\000\000\000\000\
\098\255\106\255\050\000\000\000\109\255\095\255\031\255\000\000\
\088\255\000\000\000\000\000\000\000\000\098\255\096\255\132\255\
\098\255\070\255\000\000\000\000\062\000\000\000\000\000\239\254\
\028\000\116\255\252\254\000\000\132\255\132\255\062\000\062\000\
\062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
\062\000\062\000\062\000\062\000\062\000\062\000\062\000\109\255\
\109\255\117\255\031\255\000\000\000\000\150\255\025\255\098\255\
\000\000\000\000\000\000\077\000\000\000\062\000\000\000\083\255\
\000\000\142\255\093\000\093\000\093\000\093\000\093\000\093\000\
\104\000\104\000\252\255\000\000\000\000\000\000\000\000\000\000\
\000\000\252\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\252\254\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\154\255\119\255\000\000\000\000\000\000\000\000\
\000\000\241\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\038\255\000\000\000\000\000\000\000\000\000\000\
\010\255\000\000\000\000\119\255\000\000\000\000\108\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\241\255\000\000\000\000\000\000\125\255\126\255\038\255\000\000\
\000\000\000\000\000\000\000\000\000\000\154\255\127\255\000\000\
\123\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\133\255\000\000\108\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\125\255\
\125\255\000\000\038\255\000\000\000\000\000\000\119\255\241\255\
\000\000\000\000\000\000\133\255\000\000\000\000\000\000\000\000\
\000\000\176\255\006\255\151\255\175\255\200\255\225\255\228\255\
\209\255\254\255\159\255\000\000\000\000\000\000\000\000\000\000\
\000\000\184\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\108\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\153\000\006\000\246\255\130\000\217\255\239\255\040\000\101\000\
\099\000\144\000\194\255\175\000\232\255\236\255\114\000"

let yytablesize = 400
let yytable = "\031\000\
\021\000\048\000\069\000\070\000\104\000\089\000\023\000\056\000\
\057\000\004\000\060\000\001\000\027\000\064\000\051\000\062\000\
\063\000\065\000\028\000\004\000\052\000\004\000\004\000\004\000\
\101\000\052\000\052\000\048\000\023\000\048\000\067\000\003\000\
\004\000\005\000\006\000\007\000\008\000\009\000\010\000\011\000\
\100\000\012\000\052\000\094\000\105\000\106\000\097\000\052\000\
\089\000\089\000\107\000\108\000\109\000\110\000\111\000\112\000\
\113\000\114\000\115\000\116\000\117\000\118\000\119\000\120\000\
\121\000\122\000\123\000\134\000\045\000\098\000\048\000\048\000\
\028\000\048\000\016\000\017\000\018\000\019\000\020\000\006\000\
\006\000\100\000\132\000\099\000\054\000\130\000\044\000\003\000\
\004\000\005\000\006\000\007\000\008\000\009\000\010\000\011\000\
\133\000\012\000\003\000\004\000\005\000\006\000\007\000\008\000\
\009\000\010\000\011\000\034\000\012\000\003\000\004\000\005\000\
\006\000\007\000\008\000\009\000\010\000\011\000\055\000\012\000\
\068\000\034\000\004\000\059\000\045\000\069\000\070\000\124\000\
\125\000\093\000\016\000\017\000\018\000\019\000\020\000\013\000\
\004\000\091\000\095\000\014\000\015\000\016\000\017\000\018\000\
\019\000\020\000\045\000\032\000\033\000\034\000\035\000\088\000\
\016\000\017\000\018\000\019\000\020\000\103\000\126\000\128\000\
\036\000\069\000\037\000\004\000\043\000\053\000\039\000\036\000\
\041\000\038\000\053\000\053\000\039\000\063\000\075\000\050\000\
\092\000\040\000\063\000\063\000\063\000\063\000\063\000\063\000\
\063\000\063\000\063\000\063\000\063\000\054\000\051\000\127\000\
\053\000\129\000\054\000\054\000\051\000\063\000\064\000\096\000\
\063\000\063\000\053\000\064\000\064\000\064\000\064\000\064\000\
\064\000\064\000\064\000\064\000\064\000\064\000\055\000\131\000\
\054\000\051\000\000\000\055\000\055\000\000\000\064\000\061\000\
\000\000\064\000\064\000\000\000\061\000\061\000\061\000\061\000\
\061\000\061\000\061\000\061\000\061\000\061\000\000\000\056\000\
\004\000\055\000\057\000\000\000\056\000\056\000\000\000\057\000\
\057\000\000\000\061\000\061\000\004\000\004\000\004\000\000\000\
\000\000\003\000\004\000\005\000\006\000\007\000\008\000\009\000\
\010\000\011\000\056\000\012\000\062\000\057\000\000\000\000\000\
\000\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
\062\000\062\000\062\000\080\000\081\000\082\000\083\000\084\000\
\085\000\000\000\000\000\087\000\000\000\000\000\013\000\062\000\
\062\000\000\000\014\000\015\000\016\000\017\000\018\000\019\000\
\020\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\079\000\000\000\080\000\081\000\082\000\083\000\084\000\
\085\000\000\000\086\000\087\000\000\000\000\000\102\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\000\000\080\000\081\000\082\000\083\000\084\000\085\000\000\000\
\086\000\087\000\036\000\000\000\037\000\000\000\000\000\000\000\
\000\000\000\000\000\000\061\000\000\000\000\000\039\000\000\000\
\077\000\078\000\079\000\040\000\080\000\081\000\082\000\083\000\
\084\000\085\000\000\000\086\000\087\000\000\000\000\000\102\000\
\077\000\078\000\079\000\000\000\080\000\081\000\082\000\083\000\
\084\000\085\000\000\000\086\000\087\000\079\000\000\000\080\000\
\081\000\082\000\083\000\084\000\085\000\000\000\086\000\087\000"

let yycheck = "\010\000\
\000\000\019\000\020\001\021\001\067\000\045\000\001\000\012\001\
\013\001\000\001\035\000\001\000\042\001\038\000\025\000\036\000\
\037\000\038\000\048\001\010\001\015\001\012\001\013\001\014\001\
\042\001\020\001\021\001\045\000\023\000\047\000\041\000\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\061\000\011\001\010\001\054\000\069\000\070\000\057\000\042\001\
\088\000\089\000\071\000\072\000\073\000\074\000\075\000\076\000\
\077\000\078\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\087\000\130\000\038\001\000\001\088\000\089\000\
\048\001\091\000\044\001\045\001\046\001\047\001\048\001\042\001\
\043\001\102\000\000\001\014\001\042\001\096\000\000\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\014\001\011\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\000\001\011\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\046\001\011\001\
\015\001\014\001\000\001\046\001\038\001\020\001\021\001\088\000\
\089\000\042\001\044\001\045\001\046\001\047\001\048\001\038\001\
\014\001\043\001\043\001\042\001\043\001\044\001\045\001\046\001\
\047\001\048\001\038\001\016\001\017\001\018\001\019\001\043\001\
\044\001\045\001\046\001\047\001\048\001\042\001\042\001\010\001\
\029\001\020\001\031\001\010\001\046\001\015\001\042\001\042\001\
\042\001\038\001\020\001\021\001\041\001\015\001\042\001\023\000\
\047\000\046\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\015\001\015\001\091\000\
\042\001\095\000\020\001\021\001\021\001\039\001\015\001\056\000\
\042\001\043\001\028\000\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\015\001\102\000\
\042\001\042\001\255\255\020\001\021\001\255\255\039\001\015\001\
\255\255\042\001\043\001\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\255\255\015\001\
\000\001\042\001\015\001\255\255\020\001\021\001\255\255\020\001\
\021\001\255\255\042\001\043\001\012\001\013\001\014\001\255\255\
\255\255\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\042\001\011\001\015\001\042\001\255\255\255\255\
\255\255\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\032\001\033\001\034\001\035\001\036\001\
\037\001\255\255\255\255\040\001\255\255\255\255\038\001\042\001\
\043\001\255\255\042\001\043\001\044\001\045\001\046\001\047\001\
\048\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\255\255\032\001\033\001\034\001\035\001\036\001\
\037\001\255\255\039\001\040\001\255\255\255\255\043\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\255\255\032\001\033\001\034\001\035\001\036\001\037\001\255\255\
\039\001\040\001\029\001\255\255\031\001\255\255\255\255\255\255\
\255\255\255\255\255\255\038\001\255\255\255\255\041\001\255\255\
\028\001\029\001\030\001\046\001\032\001\033\001\034\001\035\001\
\036\001\037\001\255\255\039\001\040\001\255\255\255\255\043\001\
\028\001\029\001\030\001\255\255\032\001\033\001\034\001\035\001\
\036\001\037\001\255\255\039\001\040\001\030\001\255\255\032\001\
\033\001\034\001\035\001\036\001\037\001\255\255\039\001\040\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  DEFINED\000\
  NOT\000\
  AND\000\
  OR\000\
  EQ\000\
  LT\000\
  GT\000\
  NE\000\
  LE\000\
  GE\000\
  PLUS\000\
  MINUS\000\
  STAR\000\
  LNOT\000\
  LSL\000\
  LSR\000\
  ASR\000\
  LAND\000\
  LOR\000\
  LXOR\000\
  EOF\000\
  "

let yynames_block = "\
  DEF\000\
  DEFUN\000\
  UNDEF\000\
  INCLUDE\000\
  WARNING\000\
  ERROR\000\
  LINE\000\
  IFDEF\000\
  EXT\000\
  ENDEF\000\
  IF\000\
  ELIF\000\
  ELSE\000\
  ENDIF\000\
  ENDTEST\000\
  OP_PAREN\000\
  SLASH\000\
  MOD\000\
  INT\000\
  CL_PAREN\000\
  COMMA\000\
  CURRENT_LINE\000\
  CURRENT_FILE\000\
  IDENT\000\
  FUNIDENT\000\
  TEXT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unode) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.node list ) in
    Obj.repr(
# 42 "src/cppo_parser.mly"
             ( _1 :: _2 )
# 370 "src/cppo_parser.ml"
               :  Cppo_types.node list ))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "src/cppo_parser.mly"
             ( [] )
# 376 "src/cppo_parser.ml"
               :  Cppo_types.node list ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unode) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unode_list0) in
    Obj.repr(
# 47 "src/cppo_parser.mly"
                     ( _1 :: _2 )
# 384 "src/cppo_parser.ml"
               : 'unode_list0))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "src/cppo_parser.mly"
                     ( [] )
# 390 "src/cppo_parser.ml"
               : 'unode_list0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pnode) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pnode_list0) in
    Obj.repr(
# 52 "src/cppo_parser.mly"
                     ( _1 :: _2 )
# 398 "src/cppo_parser.ml"
               : 'pnode_list0))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "src/cppo_parser.mly"
                     ( [] )
# 404 "src/cppo_parser.ml"
               : 'pnode_list0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'node) in
    Obj.repr(
# 58 "src/cppo_parser.mly"
                ( _1 )
# 411 "src/cppo_parser.ml"
               : 'unode))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 59 "src/cppo_parser.mly"
                ( `Text (_1, false, "(") )
# 418 "src/cppo_parser.ml"
               : 'unode))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 60 "src/cppo_parser.mly"
                ( `Text (_1, false, ")") )
# 425 "src/cppo_parser.ml"
               : 'unode))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 61 "src/cppo_parser.mly"
                ( `Text (_1, false, ",") )
# 432 "src/cppo_parser.ml"
               : 'unode))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'node) in
    Obj.repr(
# 66 "src/cppo_parser.mly"
                ( _1 )
# 439 "src/cppo_parser.ml"
               : 'pnode))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Cppo_types.loc ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'pnode_or_comma_list0) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 68 "src/cppo_parser.mly"
                ( `Seq [`Text (_1, false, "(");
                        `Seq _2;
                        `Text (_3, false, ")")] )
# 450 "src/cppo_parser.ml"
               : 'pnode))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc * bool * string ) in
    Obj.repr(
# 75 "src/cppo_parser.mly"
                ( `Text _1 )
# 457 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc * string ) in
    Obj.repr(
# 77 "src/cppo_parser.mly"
                ( let loc, name = _1 in
                  `Ident (loc, name, None) )
# 465 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Cppo_types.loc * string ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args1) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 81 "src/cppo_parser.mly"
                (
                (* macro application that receives at least one argument,
                   possibly empty.  We cannot distinguish syntactically between
                   zero argument and one empty argument.
                *)
                  let (pos1, _), name = _1 in
                  let _, pos2 = _3 in
                  `Ident ((pos1, pos2), name, Some _2) )
# 481 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 :  Cppo_types.loc * string ) in
    Obj.repr(
# 90 "src/cppo_parser.mly"
                ( error (fst _1) "Invalid macro application" )
# 488 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 92 "src/cppo_parser.mly"
                ( `Current_line _1 )
# 495 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 93 "src/cppo_parser.mly"
                ( `Current_file _1 )
# 502 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Cppo_types.loc * string ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'unode_list0) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 96 "src/cppo_parser.mly"
                ( let (pos1, _), name = _1 in

                  (* Additional spacing is needed for cases like '+foo+'
                     expanding into '++' instead of '+ +'. *)
                  let safe_space = `Text (_3, true, " ") in

                  let body = _2 @ [safe_space] in
                  let _, pos2 = _3 in
                  `Def ((pos1, pos2), name, body) )
# 519 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 :  Cppo_types.loc * string ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'def_args1) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 :  Cppo_types.loc ) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'unode_list0) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 107 "src/cppo_parser.mly"
                ( let (pos1, _), name = _1 in
                  let args = _2 in

                  (* Additional spacing is needed for cases like 'foo()bar'
                     where 'foo()' expands into 'abc', giving 'abcbar'
                     instead of 'abc bar';
                     Also needed for '+foo()+' expanding into '++' instead
                     of '+ +'. *)
                  let safe_space = `Text (_5, true, " ") in

                  let body = _4 @ [safe_space] in
                  let _, pos2 = _5 in
                  `Defun ((pos1, pos2), name, args, body) )
# 542 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 :  Cppo_types.loc * string ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 122 "src/cppo_parser.mly"
                ( error (fst (fst _1), snd _2)
                    "At least one argument is required" )
# 551 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc * string ) in
    Obj.repr(
# 126 "src/cppo_parser.mly"
                ( `Undef _1 )
# 558 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc * string ) in
    Obj.repr(
# 128 "src/cppo_parser.mly"
                ( `Warning _1 )
# 565 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc * string ) in
    Obj.repr(
# 130 "src/cppo_parser.mly"
                ( `Error _1 )
# 572 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc * string ) in
    Obj.repr(
# 133 "src/cppo_parser.mly"
                ( `Include _1 )
# 579 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc * string * string ) in
    Obj.repr(
# 136 "src/cppo_parser.mly"
                ( `Ext _1 )
# 586 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 :  Cppo_types.loc ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'test) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'unode_list0) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'elif_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 139 "src/cppo_parser.mly"
                ( let pos1, _ = _1 in
                  let _, pos2 = _5 in
                  let loc = (pos1, pos2) in
                  let test = _2 in
                  let if_true = _3 in
                  let if_false =
                    List.fold_right (
                      fun (loc, test, if_true) if_false ->
                        [`Cond (loc, test, if_true, if_false) ]
                    ) _4 []
                  in
                  `Cond (loc, test, if_true, if_false)
                )
# 609 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 :  Cppo_types.loc ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'test) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'unode_list0) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'elif_list) in
    Obj.repr(
# 154 "src/cppo_parser.mly"
                ( (* BUG? ocamlyacc fails to reduce that rule but not menhir *)
                  error _1 "missing #endif" )
# 620 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 :  Cppo_types.loc * Cppo_types.bool_expr ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'unode_list0) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'elif_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 158 "src/cppo_parser.mly"
                ( let (pos1, _), test = _1 in
                  let _, pos2 = _4 in
                  let loc = (pos1, pos2) in
                  let if_true = _2 in
                  let if_false =
                    List.fold_right (
                      fun (loc, test, if_true) if_false ->
                        [`Cond (loc, test, if_true, if_false) ]
                    ) _3 []
                  in
                  `Cond (loc, test, if_true, if_false)
                )
# 641 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 :  Cppo_types.loc * Cppo_types.bool_expr ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'unode_list0) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'elif_list) in
    Obj.repr(
# 172 "src/cppo_parser.mly"
                ( error (fst _1) "missing #endif" )
# 650 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc * string option * int ) in
    Obj.repr(
# 174 "src/cppo_parser.mly"
                ( `Line _1 )
# 657 "src/cppo_parser.ml"
               : 'node))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 :  Cppo_types.loc ) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'test) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'unode_list0) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'elif_list) in
    Obj.repr(
# 180 "src/cppo_parser.mly"
                   ( let pos1, _ = _1 in
                     let pos2 = Parsing.rhs_end_pos 4 in
                     ((pos1, pos2), _2, _3) :: _4 )
# 669 "src/cppo_parser.ml"
               : 'elif_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 :  Cppo_types.loc ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'unode_list0) in
    Obj.repr(
# 184 "src/cppo_parser.mly"
                   ( let pos1, _ = _1 in
                     let pos2 = Parsing.rhs_end_pos 2 in
                     [ ((pos1, pos2), `True, _2) ] )
# 679 "src/cppo_parser.ml"
               : 'elif_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 187 "src/cppo_parser.mly"
                   ( [] )
# 685 "src/cppo_parser.ml"
               : 'elif_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pnode_list0) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 :  Cppo_types.loc ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args1) in
    Obj.repr(
# 191 "src/cppo_parser.mly"
                            ( _1 :: _3  )
# 694 "src/cppo_parser.ml"
               : 'args1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pnode_list0) in
    Obj.repr(
# 192 "src/cppo_parser.mly"
                            ( [ _1 ] )
# 701 "src/cppo_parser.ml"
               : 'args1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'pnode) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pnode_or_comma_list0) in
    Obj.repr(
# 196 "src/cppo_parser.mly"
                               ( _1 :: _2 )
# 709 "src/cppo_parser.ml"
               : 'pnode_or_comma_list0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 :  Cppo_types.loc ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pnode_or_comma_list0) in
    Obj.repr(
# 197 "src/cppo_parser.mly"
                               ( `Text (_1, false, ",") :: _2 )
# 717 "src/cppo_parser.ml"
               : 'pnode_or_comma_list0))
; (fun __caml_parser_env ->
    Obj.repr(
# 198 "src/cppo_parser.mly"
                               ( [] )
# 723 "src/cppo_parser.ml"
               : 'pnode_or_comma_list0))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'arg_blank) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 :  Cppo_types.loc * string ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 :  Cppo_types.loc ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'def_args1) in
    Obj.repr(
# 203 "src/cppo_parser.mly"
                               ( (snd _2) :: _4 )
# 733 "src/cppo_parser.ml"
               : 'def_args1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'arg_blank) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc * string ) in
    Obj.repr(
# 204 "src/cppo_parser.mly"
                               ( [ snd _2 ] )
# 741 "src/cppo_parser.ml"
               : 'def_args1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 :  Cppo_types.loc * bool * string ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'arg_blank) in
    Obj.repr(
# 208 "src/cppo_parser.mly"
                         ( let loc, is_space, _s = _1 in
                           if not is_space then
                             error loc "Invalid argument list"
                         )
# 752 "src/cppo_parser.ml"
               : 'arg_blank))
; (fun __caml_parser_env ->
    Obj.repr(
# 212 "src/cppo_parser.mly"
                         ( () )
# 758 "src/cppo_parser.ml"
               : 'arg_blank))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'bexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 216 "src/cppo_parser.mly"
                ( _1 )
# 766 "src/cppo_parser.ml"
               : 'test))
; (fun __caml_parser_env ->
    Obj.repr(
# 221 "src/cppo_parser.mly"
                                    ( `True )
# 772 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    Obj.repr(
# 222 "src/cppo_parser.mly"
                                    ( `False )
# 778 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc * string ) in
    Obj.repr(
# 223 "src/cppo_parser.mly"
                                    ( `Defined (snd _2) )
# 785 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Cppo_types.loc ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 224 "src/cppo_parser.mly"
                                    ( _2 )
# 794 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexpr) in
    Obj.repr(
# 225 "src/cppo_parser.mly"
                                    ( `Not _2 )
# 801 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexpr) in
    Obj.repr(
# 226 "src/cppo_parser.mly"
                                    ( `And (_1, _3) )
# 809 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexpr) in
    Obj.repr(
# 227 "src/cppo_parser.mly"
                                    ( `Or (_1, _3) )
# 817 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 228 "src/cppo_parser.mly"
                                    ( `Eq (_1, _3) )
# 825 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 229 "src/cppo_parser.mly"
                                    ( `Lt (_1, _3) )
# 833 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 230 "src/cppo_parser.mly"
                                    ( `Gt (_1, _3) )
# 841 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 231 "src/cppo_parser.mly"
                                    ( `Not (`Eq (_1, _3)) )
# 849 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 232 "src/cppo_parser.mly"
                                    ( `Not (`Gt (_1, _3)) )
# 857 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 233 "src/cppo_parser.mly"
                                    ( `Not (`Lt (_1, _3)) )
# 865 "src/cppo_parser.ml"
               : 'bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  int64 ) in
    Obj.repr(
# 238 "src/cppo_parser.mly"
                             ( `Int _1 )
# 872 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc * string ) in
    Obj.repr(
# 239 "src/cppo_parser.mly"
                             ( `Ident _1 )
# 879 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 :  Cppo_types.loc ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'aexpr_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Cppo_types.loc ) in
    Obj.repr(
# 241 "src/cppo_parser.mly"
                             ( match _2 with
                               | [x] -> x
                               | l ->
                                 let pos1, _ = _1 in
                                 let _, pos2 = _3 in
                                 `Tuple ((pos1, pos2), l)
                             )
# 894 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 248 "src/cppo_parser.mly"
                             ( `Add (_1, _3) )
# 902 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 249 "src/cppo_parser.mly"
                             ( `Sub (_1, _3) )
# 910 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 250 "src/cppo_parser.mly"
                             ( `Mul (_1, _3) )
# 918 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 :  Cppo_types.loc ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 251 "src/cppo_parser.mly"
                             ( `Div (_2, _1, _3) )
# 927 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 :  Cppo_types.loc ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 252 "src/cppo_parser.mly"
                             ( `Mod (_2, _1, _3) )
# 936 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 253 "src/cppo_parser.mly"
                             ( `Lsl (_1, _3) )
# 944 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 254 "src/cppo_parser.mly"
                             ( `Lsr (_1, _3) )
# 952 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 255 "src/cppo_parser.mly"
                             ( `Asr (_1, _3) )
# 960 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 256 "src/cppo_parser.mly"
                             ( `Land (_1, _3) )
# 968 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 257 "src/cppo_parser.mly"
                             ( `Lor (_1, _3) )
# 976 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 258 "src/cppo_parser.mly"
                             ( `Lxor (_1, _3) )
# 984 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 259 "src/cppo_parser.mly"
                             ( `Lnot _2 )
# 991 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 260 "src/cppo_parser.mly"
                             ( `Neg _2 )
# 998 "src/cppo_parser.ml"
               : 'aexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 :  Cppo_types.loc ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr_list) in
    Obj.repr(
# 264 "src/cppo_parser.mly"
                             ( _1 :: _3 )
# 1007 "src/cppo_parser.ml"
               : 'aexpr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aexpr) in
    Obj.repr(
# 265 "src/cppo_parser.mly"
                             ( [_1] )
# 1014 "src/cppo_parser.ml"
               : 'aexpr_list))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Cppo_types.node list )

end
module Cppo_lexer
= struct
#1 "cppo_lexer.ml"
# 1 "src/cppo_lexer.mll"
 
open Printf
open Lexing

open Cppo_types
open Cppo_parser

let pos1 lexbuf = lexbuf.lex_start_p
let pos2 lexbuf = lexbuf.lex_curr_p
let loc lexbuf = (pos1 lexbuf, pos2 lexbuf)

let lexer_error lexbuf descr =
  error (loc lexbuf) descr

let new_file lb name =
  lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = name }

let lex_new_lines lb =
  let n = ref 0 in
  let s = lb.lex_buffer in
  for i = lb.lex_start_pos to lb.lex_curr_pos do
    if Bytes.get s i = '\n' then
      incr n
  done;
  let p = lb.lex_curr_p in
  lb.lex_curr_p <-
    { p with
        pos_lnum = p.pos_lnum + !n;
        pos_bol = p.pos_cnum
    }

let count_new_lines lb n =
  let p = lb.lex_curr_p in
  lb.lex_curr_p <-
    { p with
        pos_lnum = p.pos_lnum + n;
        pos_bol = p.pos_cnum
    }

(* must start a new line *)
let update_pos lb p added_chars added_breaks =
  let cnum = p.pos_cnum + added_chars in
  lb.lex_curr_p <-
    { pos_fname = p.pos_fname;
      pos_lnum = p.pos_lnum + added_breaks;
      pos_bol = cnum;
      pos_cnum = cnum }

let set_lnum lb opt_file lnum =
  let p = lb.lex_curr_p in
  let cnum = p.pos_cnum in
  let fname =
    match opt_file with
        None -> p.pos_fname
      | Some file -> file
  in
  lb.lex_curr_p <-
    { pos_fname = fname;
      pos_bol = cnum;
      pos_cnum = cnum;
      pos_lnum = lnum }

let shift lb n =
  let p = lb.lex_curr_p in
  lb.lex_curr_p <- { p with pos_cnum = p.pos_cnum + n }

let read_hexdigit c =
  match c with
      '0'..'9' -> Char.code c - 48
    | 'A'..'F' -> Char.code c - 55
    | 'a'..'z' -> Char.code c - 87
    | _ -> invalid_arg "read_hexdigit"

let read_hex2 c1 c2 =
  Char.chr (read_hexdigit c1 * 16 + read_hexdigit c2)

type env = {
  preserve_quotations : bool;
  mutable lexer : [ `Ocaml | `Test ];
  mutable line_start : bool;
  mutable in_directive : bool; (* true while processing a directive, until the
                                  final newline *)
  buf : Buffer.t;
  mutable token_start : Lexing.position;
  lexbuf : Lexing.lexbuf;
}

let new_line env =
  env.line_start <- true;
  count_new_lines env.lexbuf 1

let clear env = Buffer.clear env.buf

let add env s =
  env.line_start <- false;
  Buffer.add_string env.buf s

let add_char env c =
  env.line_start <- false;
  Buffer.add_char env.buf c

let get env = Buffer.contents env.buf

let long_loc e = (e.token_start, pos2 e.lexbuf)

let cppo_directives = [
  "define";
  "elif";
  "else";
  "endif";
  "error";
  "if";
  "ifdef";
  "ifndef";
  "include";
  "undef";
  "warning";
]

let is_reserved_directive =
  let tbl = Hashtbl.create 20 in
  List.iter (fun s -> Hashtbl.add tbl s ()) cppo_directives;
  fun s -> Hashtbl.mem tbl s


# 128 "src/cppo_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\255\255\000\000\255\255\001\000\002\000\028\000\142\000\103\000\
    \138\000\164\000\190\000\216\000\242\000\012\001\038\001\064\001\
    \178\001\236\001\188\002\149\003\101\004\002\000\003\000\255\255\
    \114\001\006\002\032\002\181\003\083\005\035\006\243\006\195\007\
    \004\000\006\000\214\002\109\005\240\002\207\003\061\000\008\000\
    \009\000\127\004\098\008\212\008\164\009\116\010\068\011\010\000\
    \012\000\153\004\135\005\238\008\050\012\002\013\210\013\162\014\
    \014\000\015\000\061\006\087\006\013\007\039\007\144\015\244\255\
    \016\000\062\000\018\000\221\007\008\009\190\009\216\009\142\010\
    \168\010\076\012\065\000\027\000\020\000\094\011\170\015\028\016\
    \236\016\188\017\140\018\028\000\021\000\120\011\102\012\028\013\
    \054\013\236\013\122\019\243\255\029\000\235\001\033\000\006\014\
    \188\014\214\014\106\015\220\015\153\019\242\255\034\000\238\001\
    \035\000\240\255\241\255\036\000\050\000\214\008\038\000\093\005\
    \255\255\039\000\095\005\151\016\252\255\253\255\195\001\097\005\
    \094\017\055\018\010\005\147\002\016\003\011\005\012\005\222\008\
    \255\255\246\001\039\019\013\005\018\003\015\005\014\005\017\005\
    \224\008\254\255\248\001\011\020\232\255\233\255\116\016\095\019\
    \237\255\239\255\051\000\053\000\235\020\014\021\242\255\049\021\
    \004\000\102\000\086\021\121\021\158\021\006\000\011\000\020\000\
    \217\021\252\021\031\022\078\008\247\255\248\255\025\000\155\019\
    \061\022\022\023\230\023\191\024\251\255\152\025\113\026\074\027\
    \035\028\252\028\213\029\174\030\135\031\096\032\057\033\018\034\
    \235\034\235\020\029\000\146\000\036\000\250\255\037\000\066\017\
    \210\008\140\001\215\018\238\255\240\255\067\000\243\255\244\255\
    \245\255\246\255\046\000\021\000\113\002\019\018\032\000\117\000\
    \091\020\136\022\046\012\132\000\134\015\059\003\158\015\247\255\
    \142\016\250\255\072\000\187\020\253\255\042\000\042\000\255\255\
    \254\255\117\023\047\000\148\000\048\000\252\255\251\255\049\000\
    \058\024\255\015\026\004\010\025\035\021\250\255\251\255\252\255\
    \079\000\240\001\255\255\253\255\080\000\254\255\045\021\244\255\
    \253\007\001\008\070\021\225\035\255\255\246\255\247\255\248\255\
    \249\255\227\025\162\006\253\255\081\000\254\255\114\007\251\255\
    \188\026\250\255\063\017\250\255\187\019\252\255\082\000\169\000\
    \033\000\255\255\253\255\088\000\097\000\254\255\050\036\223\255\
    \224\255\150\000\165\000\152\000\134\036\242\036\228\255\070\037\
    \154\037\238\037\237\255\238\255\240\255\097\023\057\038\046\008\
    \102\000\127\000\247\255\130\038\049\000\139\000\251\255\252\255\
    \214\038\042\039\126\039\210\039\038\040\122\040\206\040\034\041\
    \118\041\202\041\030\042\114\042\198\042\026\043\110\043\194\043\
    \250\255\249\255\022\044\106\044\243\255\244\255\242\255\180\000\
    \211\001\149\027\110\028\058\017\151\001\190\044\018\045\102\045\
    \186\045\014\046\098\046\182\046\010\047\094\047\178\047\006\048\
    \090\048\174\048\002\049\086\049\170\049\254\049\082\050\225\255\
    \173\000\046\018\253\255\254\255\070\018\072\018\022\019\034\019\
    \255\255\060\012\163\019\169\019\032\019\255\255\070\012\254\255\
    ";
  Lexing.lex_backtrk = 
   "\255\255\001\000\255\255\255\255\016\000\017\000\255\255\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\255\255\008\000\001\000\255\255\255\255\255\255\255\255\
    \017\000\017\000\017\000\017\000\255\255\008\000\002\000\255\255\
    \255\255\255\255\017\000\017\000\017\000\017\000\003\000\255\255\
    \255\255\017\000\017\000\255\255\008\000\005\000\255\255\255\255\
    \255\255\017\000\017\000\017\000\255\255\008\000\006\000\255\255\
    \255\255\255\255\017\000\017\000\017\000\017\000\017\000\255\255\
    \255\255\255\255\255\255\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\004\000\255\255\255\255\009\000\017\000\255\255\
    \008\000\007\000\255\255\255\255\255\255\017\000\017\000\010\000\
    \017\000\017\000\017\000\255\255\255\255\255\255\255\255\017\000\
    \017\000\017\000\017\000\017\000\017\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\020\000\020\000\
    \255\255\255\255\022\000\014\000\013\000\013\000\255\255\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\022\000\013\000\
    \013\000\013\000\013\000\022\000\255\255\255\255\006\000\022\000\
    \003\000\002\000\013\000\002\000\255\255\002\000\002\000\002\000\
    \002\000\002\000\002\000\000\000\002\000\002\000\002\000\002\000\
    \001\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\020\000\255\255\255\255\
    \255\255\020\000\020\000\020\000\020\000\255\255\255\255\255\255\
    \006\000\255\255\007\000\007\000\255\255\007\000\007\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \004\000\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \010\000\003\000\010\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\004\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\032\000\032\000\029\000\028\000\032\000\255\255\028\000\
    \028\000\028\000\255\255\255\255\255\255\014\000\014\000\016\000\
    \010\000\009\000\255\255\028\000\032\000\032\000\255\255\255\255\
    \028\000\028\000\028\000\028\000\028\000\000\000\028\000\028\000\
    \028\000\001\000\028\000\028\000\028\000\028\000\028\000\002\000\
    \255\255\255\255\028\000\007\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\014\000\014\000\014\000\028\000\019\000\028\000\
    \028\000\028\000\028\000\028\000\021\000\020\000\028\000\023\000\
    \024\000\028\000\025\000\028\000\026\000\028\000\022\000\255\255\
    \255\255\255\255\255\255\255\255\002\000\002\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_default = 
   "\000\000\255\255\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\118\000\000\000\000\000\118\000\118\000\
    \118\000\118\000\118\000\118\000\118\000\118\000\118\000\118\000\
    \000\000\118\000\118\000\118\000\118\000\118\000\118\000\118\000\
    \118\000\000\000\118\000\141\000\000\000\000\000\255\255\255\255\
    \000\000\000\000\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000\000\000\255\255\186\000\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\186\000\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\000\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\216\000\000\000\
    \216\000\000\000\255\255\226\000\000\000\255\255\255\255\000\000\
    \000\000\226\000\255\255\255\255\255\255\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\238\000\000\000\000\000\000\000\
    \255\255\255\255\000\000\000\000\255\255\000\000\248\000\000\000\
    \248\000\248\000\248\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\255\255\255\255\000\000\255\255\000\000\255\255\000\000\
    \255\255\000\000\012\001\000\000\012\001\000\000\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\000\000\023\001\000\000\
    \000\000\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\000\000\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\255\255\255\255\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\100\001\000\000\000\000\102\001\102\001\102\001\102\001\
    \000\000\255\255\107\001\107\001\255\255\000\000\255\255\000\000\
    ";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\003\000\004\000\017\000\017\000\028\000\022\000\
    \028\000\033\000\038\000\038\000\043\000\040\000\043\000\048\000\
    \052\000\052\000\065\000\057\000\065\000\066\000\074\000\079\000\
    \003\000\003\000\004\000\002\000\002\000\074\000\079\000\093\000\
    \076\000\084\000\094\000\093\000\103\000\103\000\106\000\104\000\
    \109\000\112\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\109\000\145\000\147\000\110\000\
    \150\000\150\000\202\000\195\000\150\000\150\000\038\000\065\000\
    \150\000\150\000\074\000\189\000\189\000\196\000\203\000\150\000\
    \211\000\211\000\217\000\224\000\223\000\147\000\230\000\229\000\
    \229\000\239\000\243\000\003\001\013\001\038\000\065\000\017\001\
    \063\000\074\000\018\001\005\000\005\000\005\000\011\000\008\000\
    \005\000\005\000\005\000\009\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\010\000\
    \005\000\007\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\109\000\
    \106\000\039\000\064\000\107\000\190\000\075\000\231\000\021\001\
    \024\001\027\001\150\000\070\001\150\000\210\000\210\000\210\000\
    \210\000\210\000\210\000\210\000\210\000\065\001\109\000\095\001\
    \105\000\064\001\096\001\018\001\211\000\211\000\019\001\095\001\
    \027\001\150\000\000\000\230\000\068\001\069\001\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \095\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\150\000\211\000\076\001\076\001\000\000\020\001\
    \000\000\000\000\108\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\070\000\005\000\
    \068\000\005\000\005\000\005\000\067\000\005\000\005\000\005\000\
    \005\000\005\000\069\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\035\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\034\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\024\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\012\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \013\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\014\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\015\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\016\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\017\000\186\000\186\000\186\000\186\000\
    \186\000\186\000\186\000\186\000\186\000\186\000\000\000\076\001\
    \076\001\000\000\000\000\000\000\000\000\117\000\000\000\000\000\
    \000\000\000\000\017\000\005\000\005\000\005\000\025\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\093\000\017\000\076\001\103\000\
    \000\000\000\000\243\000\000\000\000\000\244\000\000\000\000\000\
    \128\000\000\000\137\000\075\001\075\001\075\001\075\001\075\001\
    \075\001\075\001\075\001\093\000\017\000\091\000\103\000\000\000\
    \101\000\000\000\245\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\092\000\
    \021\000\000\000\102\000\020\000\245\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\005\000\
    \005\000\005\000\005\000\026\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\027\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\213\000\117\000\213\000\000\000\
    \000\000\212\000\212\000\212\000\212\000\212\000\212\000\212\000\
    \212\000\212\000\212\000\000\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\255\255\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\255\255\124\000\
    \255\255\000\000\000\000\000\000\000\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\000\000\
    \000\000\000\000\117\000\018\000\117\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\005\000\
    \005\000\058\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\049\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\212\000\212\000\212\000\212\000\212\000\
    \212\000\212\000\212\000\212\000\212\000\125\000\133\000\000\000\
    \000\000\000\000\000\000\000\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\255\255\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\000\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\019\000\023\000\028\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\028\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \000\000\000\000\000\000\000\000\019\000\000\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \255\255\000\000\255\255\000\000\000\000\000\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\041\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\000\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\000\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\019\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \000\000\000\000\000\000\000\000\019\000\000\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \005\000\005\000\005\000\005\000\005\000\042\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\050\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\117\000\117\000\117\000\117\000\
    \117\000\117\000\000\000\117\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\000\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\028\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\000\000\114\000\112\000\
    \114\000\112\000\113\000\117\000\113\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\000\134\000\000\000\038\000\000\000\
    \123\000\000\000\000\000\132\000\000\000\114\000\000\000\114\000\
    \127\000\000\000\000\000\126\000\130\000\136\000\135\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\038\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\000\000\032\000\
    \000\000\000\000\031\000\000\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\005\000\005\000\
    \005\000\037\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\036\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\051\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\000\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\112\000\000\000\112\000\
    \000\000\255\255\000\000\000\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\000\000\000\000\
    \000\000\000\000\029\000\000\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\059\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\060\000\005\000\005\000\005\000\
    \005\000\005\000\006\001\006\001\006\001\006\001\006\001\006\001\
    \006\001\006\001\006\001\006\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\000\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\030\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\000\000\000\000\
    \000\000\000\000\030\000\000\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\005\000\005\000\
    \005\000\061\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\062\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\007\001\007\001\007\001\007\001\007\001\007\001\
    \007\001\007\001\007\001\007\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\000\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\030\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\000\000\255\255\
    \000\000\000\000\030\000\255\255\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\088\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \196\000\255\255\000\000\197\000\000\000\255\255\038\001\037\001\
    \037\001\037\001\037\001\037\001\037\001\037\001\037\001\037\001\
    \000\000\000\000\000\000\043\000\000\000\000\000\000\000\000\000\
    \000\000\198\000\000\000\000\000\000\000\000\000\199\000\201\000\
    \000\000\000\000\200\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\043\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\000\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\000\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\043\000\000\000\109\000\
    \106\000\000\000\000\000\107\000\000\000\000\000\000\000\127\000\
    \128\000\136\000\137\000\129\000\000\000\138\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\043\000\000\000\109\000\052\000\
    \105\000\150\000\000\000\000\000\000\000\255\255\127\000\000\000\
    \136\000\255\255\193\000\193\000\193\000\193\000\193\000\193\000\
    \193\000\193\000\193\000\193\000\000\000\000\000\052\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\000\000\
    \047\000\000\000\108\000\046\000\000\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\085\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\000\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\128\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\000\000\
    \000\000\000\000\000\000\044\000\000\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\078\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\072\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\071\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\000\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\045\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\000\000\
    \000\000\000\000\000\000\045\000\000\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\005\000\
    \005\000\005\000\005\000\077\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\073\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\000\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\045\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\000\000\
    \000\000\000\000\000\000\045\000\000\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\086\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\000\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\052\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\000\000\105\001\105\001\000\000\
    \000\000\105\001\000\000\000\000\000\000\000\000\000\000\110\001\
    \110\001\000\000\052\000\110\001\000\000\074\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\105\001\000\000\210\000\210\000\
    \210\000\210\000\210\000\210\000\210\000\210\000\110\001\000\000\
    \000\000\000\000\000\000\000\000\074\000\000\000\000\000\108\001\
    \000\000\000\000\109\001\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\210\000\056\000\000\000\
    \000\000\055\000\000\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\087\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\000\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\104\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\000\000\000\000\000\000\
    \000\000\053\000\000\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\089\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\000\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\054\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\000\000\000\000\000\000\
    \000\000\054\000\000\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\090\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \096\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\000\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\054\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\000\000\000\000\000\000\
    \000\000\054\000\000\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\097\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\098\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\000\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\065\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \217\000\000\000\000\000\218\000\000\000\000\000\000\000\000\000\
    \065\000\000\000\063\000\079\000\000\000\000\000\212\000\212\000\
    \212\000\212\000\212\000\212\000\212\000\212\000\212\000\212\000\
    \220\000\000\000\000\000\000\000\000\000\219\000\222\000\000\000\
    \221\000\000\000\079\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \099\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\212\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\064\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\079\000\230\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\234\000\
    \234\000\234\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\000\000\000\000\000\000\079\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\100\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\000\000\
    \083\000\000\000\000\000\082\000\000\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\000\000\
    \255\255\000\000\000\000\255\255\000\000\000\000\215\000\000\000\
    \121\000\117\000\205\000\000\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\000\000\000\000\
    \255\255\000\000\000\000\000\000\000\000\255\255\255\255\121\000\
    \255\255\204\000\120\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\142\000\000\000\000\000\000\000\000\000\
    \000\000\204\000\000\000\000\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\119\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\000\000\
    \000\000\013\001\000\000\080\000\014\001\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\120\000\
    \117\000\150\000\075\001\075\001\075\001\075\001\075\001\075\001\
    \075\001\075\001\194\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\194\000\194\000\194\000\000\000\016\001\120\000\000\000\
    \000\000\000\000\000\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\116\000\
    \000\000\075\001\000\000\015\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\000\000\000\000\000\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\122\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\081\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\000\000\
    \000\000\000\000\000\000\081\000\000\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\101\001\
    \101\001\000\000\000\000\101\001\000\000\000\000\000\000\011\001\
    \121\000\117\000\000\000\205\000\205\000\205\000\205\000\205\000\
    \205\000\205\000\205\000\205\000\205\000\000\000\101\001\105\001\
    \105\001\103\001\103\001\105\001\000\000\103\001\099\001\121\000\
    \204\000\000\000\120\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\105\001\000\000\
    \103\001\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \099\001\000\000\205\000\000\000\000\000\000\000\000\000\000\000\
    \204\000\000\000\000\000\000\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\119\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\081\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\000\000\
    \000\000\000\000\000\000\081\000\000\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\186\000\
    \186\000\186\000\186\000\186\000\186\000\186\000\186\000\186\000\
    \186\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \186\000\186\000\186\000\186\000\186\000\186\000\000\000\105\001\
    \105\001\000\000\000\000\105\001\000\000\000\000\000\000\000\000\
    \000\000\108\001\108\001\103\001\103\001\108\001\098\001\103\001\
    \130\000\117\000\000\000\000\000\000\000\000\000\105\001\255\255\
    \186\000\186\000\186\000\186\000\186\000\186\000\255\255\000\000\
    \108\001\000\000\103\001\000\000\000\000\000\000\104\001\130\000\
    \255\255\000\000\099\001\000\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\000\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\093\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\131\000\205\000\000\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\000\000\093\000\000\000\091\000\000\000\000\000\000\000\
    \000\000\206\000\103\000\000\000\204\000\188\000\000\000\000\000\
    \187\000\000\000\000\000\000\000\106\001\106\001\207\000\000\000\
    \106\001\000\000\110\001\110\001\000\000\000\000\110\001\208\000\
    \000\000\103\000\000\000\101\000\000\000\000\000\142\000\000\000\
    \000\000\206\000\255\255\106\001\204\000\255\255\000\000\000\000\
    \255\255\110\001\000\000\000\000\255\255\000\000\207\000\255\255\
    \000\000\000\000\108\001\000\000\000\000\109\001\092\000\208\000\
    \000\000\000\000\000\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\102\000\000\000\185\000\
    \000\000\255\255\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\147\000\145\000\104\001\255\255\
    \146\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \111\001\000\000\255\255\000\000\000\000\000\000\000\000\255\255\
    \000\000\000\000\000\000\147\000\162\000\144\000\150\000\148\000\
    \148\000\161\000\167\000\166\000\165\000\148\000\148\000\164\000\
    \160\000\159\000\148\000\143\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\158\000\157\000\156\000\
    \148\000\155\000\154\000\148\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\153\000\163\000\
    \150\000\148\000\170\000\150\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\152\000\151\000\
    \150\000\149\000\000\000\209\000\209\000\209\000\209\000\209\000\
    \209\000\209\000\209\000\209\000\209\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\209\000\209\000\209\000\209\000\
    \209\000\209\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\209\000\209\000\209\000\209\000\
    \209\000\209\000\000\000\000\000\000\000\228\000\000\000\000\000\
    \227\000\000\000\000\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\255\255\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\000\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\140\000\148\000\000\000\000\000\148\000\
    \148\000\148\000\000\000\000\000\000\000\148\000\148\000\225\000\
    \148\000\148\000\148\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\192\000\148\000\000\000\148\000\
    \148\000\148\000\148\000\148\000\000\000\239\000\000\000\149\000\
    \240\000\000\000\149\000\149\000\149\000\000\000\000\000\249\000\
    \149\000\149\000\250\000\149\000\149\000\149\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\242\000\000\000\000\000\
    \149\000\148\000\149\000\149\000\149\000\149\000\149\000\252\000\
    \249\000\000\000\148\000\000\000\000\000\148\000\148\000\148\000\
    \000\000\000\000\000\000\148\000\148\000\000\000\148\000\148\000\
    \148\000\000\000\000\000\191\000\000\000\000\000\000\000\148\000\
    \255\255\148\000\000\000\148\000\149\000\148\000\148\000\148\000\
    \148\000\148\000\000\000\000\000\000\000\000\000\000\000\149\000\
    \000\000\000\000\149\000\149\000\149\000\000\000\000\000\241\000\
    \149\000\149\000\000\000\149\000\149\000\149\000\000\000\000\000\
    \000\000\251\000\149\000\000\000\149\000\000\000\150\000\148\000\
    \149\000\000\000\149\000\149\000\149\000\149\000\149\000\000\000\
    \000\000\000\000\148\000\000\000\000\000\148\000\148\000\148\000\
    \000\000\000\000\255\255\148\000\148\000\000\000\148\000\148\000\
    \148\000\000\000\000\000\000\000\000\000\148\000\000\000\148\000\
    \000\000\000\000\000\000\148\000\149\000\148\000\148\000\148\000\
    \148\000\148\000\000\000\255\255\000\000\000\000\000\000\148\000\
    \000\000\000\000\148\000\148\000\148\000\000\000\000\000\000\000\
    \148\000\148\000\000\000\148\000\148\000\148\000\000\000\000\000\
    \000\000\000\000\149\000\000\000\149\000\000\000\150\000\148\000\
    \148\000\000\000\148\000\148\000\148\000\148\000\148\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\148\000\150\000\148\000\
    \000\000\000\000\148\000\000\000\148\000\148\000\148\000\148\000\
    \000\000\000\000\000\000\148\000\148\000\000\000\148\000\148\000\
    \148\000\143\000\142\000\142\000\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\148\000\000\000\148\000\148\000\148\000\
    \148\000\148\000\148\000\000\000\148\000\148\000\000\000\000\000\
    \148\000\148\000\148\000\237\000\000\000\000\000\148\000\148\000\
    \000\000\148\000\148\000\148\000\000\000\247\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\148\000\148\000\
    \148\000\148\000\148\000\148\000\148\000\000\000\000\000\000\000\
    \149\000\000\000\000\000\149\000\149\000\149\000\255\255\000\000\
    \000\000\149\000\149\000\000\000\149\000\149\000\149\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\148\000\000\000\148\000\
    \000\000\149\000\148\000\149\000\149\000\149\000\149\000\149\000\
    \000\000\000\000\000\000\000\000\168\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\000\000\
    \148\000\000\000\148\000\000\000\000\000\149\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \000\000\000\000\000\000\149\000\168\000\149\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \209\000\209\000\209\000\209\000\209\000\209\000\209\000\209\000\
    \209\000\209\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\209\000\209\000\209\000\209\000\209\000\209\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\209\000\
    \000\000\209\000\209\000\209\000\209\000\209\000\209\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\000\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\000\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\169\000\172\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\000\000\000\000\000\000\000\000\169\000\000\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\037\001\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\233\000\233\000\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \037\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\232\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\169\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\000\000\000\000\000\000\000\000\171\000\000\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\230\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\235\000\235\000\235\000\235\000\235\000\235\000\
    \235\000\235\000\235\000\235\000\000\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\235\000\235\000\235\000\235\000\235\000\
    \235\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\235\000\235\000\235\000\235\000\235\000\
    \235\000\000\000\000\000\000\000\000\000\000\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\000\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\000\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\169\000\172\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \169\000\169\000\169\000\169\000\169\000\173\000\169\000\169\000\
    \169\000\169\000\169\000\174\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\000\000\000\000\000\000\000\000\169\000\000\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\226\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\226\000\226\000\226\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\226\000\226\000\226\000\226\000\226\000\
    \226\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\000\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\000\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\169\000\
    \172\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\180\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\000\000\000\000\000\000\000\000\169\000\
    \000\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\008\001\008\001\008\001\008\001\008\001\
    \008\001\008\001\008\001\008\001\008\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\008\001\008\001\008\001\008\001\
    \008\001\008\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\008\001\008\001\008\001\008\001\
    \008\001\008\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\000\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\000\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \169\000\172\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\175\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\000\000\000\000\000\000\000\000\
    \169\000\000\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\009\001\009\001\009\001\009\001\
    \009\001\009\001\009\001\009\001\009\001\009\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\009\001\009\001\009\001\
    \009\001\009\001\009\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\009\001\009\001\009\001\
    \009\001\009\001\009\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \000\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \000\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\169\000\172\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \176\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\000\000\000\000\000\000\
    \000\000\169\000\000\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\074\001\074\001\074\001\
    \074\001\074\001\074\001\074\001\074\001\074\001\074\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\074\001\074\001\
    \074\001\074\001\074\001\074\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\074\001\074\001\
    \074\001\074\001\074\001\074\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\000\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\000\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\169\000\172\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\169\000\169\000\169\000\169\000\
    \177\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\000\000\000\000\
    \000\000\000\000\169\000\000\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\074\001\074\001\
    \074\001\074\001\074\001\074\001\074\001\074\001\074\001\074\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\074\001\
    \074\001\074\001\074\001\074\001\074\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\074\001\000\000\074\001\
    \074\001\074\001\074\001\074\001\074\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\000\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\000\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\169\000\172\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\000\000\
    \000\000\000\000\000\000\178\000\000\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\000\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\000\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\169\000\172\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \000\000\000\000\000\000\000\000\179\000\000\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\000\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\000\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\169\000\172\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\000\000\000\000\000\000\000\000\169\000\000\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\000\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\000\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\169\000\172\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\181\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\000\000\000\000\000\000\000\000\169\000\000\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\000\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\000\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\169\000\
    \172\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\169\000\169\000\169\000\169\000\182\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\000\000\000\000\000\000\000\000\169\000\
    \000\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\000\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\000\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \169\000\172\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\000\000\000\000\000\000\000\000\
    \183\000\000\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \000\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \000\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\169\000\172\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\000\000\000\000\000\000\
    \000\000\184\000\000\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\000\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\000\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\169\000\172\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\000\000\000\000\
    \000\000\000\000\169\000\000\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\000\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\000\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\003\001\000\000\000\000\004\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\005\001\000\000\000\000\000\000\000\000\
    \005\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\002\001\002\001\002\001\002\001\002\001\002\001\002\001\
    \002\001\002\001\002\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\027\001\024\001\005\001\000\000\025\001\
    \000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\000\
    \000\000\000\000\027\001\254\000\000\000\253\000\000\000\000\000\
    \045\001\001\001\047\001\046\001\035\001\036\001\030\001\039\001\
    \000\000\034\001\038\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\037\001\000\000\000\000\041\001\042\001\
    \040\001\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\026\001\000\000\
    \000\000\029\001\000\000\031\001\028\001\028\001\048\001\028\001\
    \049\001\028\001\028\001\028\001\028\001\028\001\032\001\033\001\
    \043\001\028\001\028\001\028\001\028\001\028\001\050\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\044\001\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\024\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\093\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\082\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \079\001\081\001\028\001\028\001\028\001\083\001\028\001\028\001\
    \028\001\028\001\080\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\077\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\037\001\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\071\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \072\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\073\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \037\001\000\000\000\000\071\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \072\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\073\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\066\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\058\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\054\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \051\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\052\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\053\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\055\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\056\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\057\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\059\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\060\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\061\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\062\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\063\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\067\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\078\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\091\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\089\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \088\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \086\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\085\001\028\001\028\001\028\001\028\001\028\001\
    \084\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\087\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \090\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\092\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\000\000\000\000\000\000\000\000\028\001\000\000\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \094\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\028\001\000\000\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\001\000\003\000\004\000\021\000\022\000\032\000\021\000\
    \033\000\032\000\039\000\040\000\047\000\039\000\048\000\047\000\
    \056\000\057\000\064\000\056\000\066\000\064\000\076\000\084\000\
    \001\000\003\000\004\000\001\000\003\000\075\000\083\000\092\000\
    \075\000\083\000\092\000\094\000\102\000\104\000\107\000\102\000\
    \110\000\113\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\108\000\146\000\147\000\108\000\
    \152\000\157\000\159\000\166\000\186\000\158\000\038\000\065\000\
    \158\000\158\000\074\000\188\000\190\000\197\000\202\000\203\000\
    \206\000\206\000\218\000\221\000\222\000\147\000\226\000\228\000\
    \231\000\240\000\244\000\004\001\014\001\038\000\065\000\016\001\
    \065\000\074\000\019\001\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\006\000\
    \006\000\038\000\065\000\006\000\187\000\074\000\227\000\020\001\
    \025\001\027\001\153\000\040\001\153\000\207\000\207\000\207\000\
    \207\000\207\000\207\000\207\000\207\000\044\001\006\000\026\001\
    \006\000\045\001\026\001\015\001\211\000\211\000\015\001\096\001\
    \027\001\187\000\255\255\227\000\041\001\041\001\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\153\000\211\000\071\001\071\001\255\255\015\001\
    \255\255\255\255\006\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
    \010\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
    \011\000\011\000\011\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\016\000\193\000\193\000\193\000\193\000\
    \193\000\193\000\193\000\193\000\193\000\193\000\255\255\076\001\
    \076\001\255\255\255\255\255\255\255\255\118\000\255\255\255\255\
    \255\255\255\255\016\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\093\000\017\000\076\001\103\000\
    \255\255\255\255\241\000\255\255\255\255\241\000\255\255\255\255\
    \129\000\255\255\138\000\072\001\072\001\072\001\072\001\072\001\
    \072\001\072\001\072\001\093\000\017\000\093\000\103\000\255\255\
    \103\000\255\255\241\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\093\000\
    \017\000\255\255\103\000\017\000\241\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\025\000\025\000\025\000\025\000\025\000\025\000\025\000\
    \025\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\255\255\204\000\123\000\204\000\255\255\
    \255\255\204\000\204\000\204\000\204\000\204\000\204\000\204\000\
    \204\000\204\000\204\000\255\255\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\118\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\018\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\129\000\123\000\
    \138\000\255\255\255\255\255\255\255\255\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\255\255\
    \255\255\255\255\124\000\018\000\132\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
    \034\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\213\000\213\000\213\000\213\000\213\000\
    \213\000\213\000\213\000\213\000\213\000\124\000\132\000\255\255\
    \255\255\255\255\255\255\255\255\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\123\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\255\255\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\019\000\019\000\027\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\027\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \255\255\255\255\255\255\255\255\019\000\255\255\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \124\000\255\255\132\000\255\255\255\255\255\255\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\234\000\234\000\234\000\234\000\234\000\234\000\
    \234\000\234\000\234\000\234\000\255\255\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\255\255\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\020\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \255\255\255\255\255\255\255\255\020\000\255\255\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\041\000\041\000\041\000\041\000\041\000\041\000\
    \041\000\041\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\049\000\049\000\049\000\049\000\
    \049\000\049\000\049\000\049\000\122\000\125\000\126\000\131\000\
    \134\000\133\000\255\255\135\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\255\255\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\028\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\255\255\111\000\111\000\
    \114\000\114\000\111\000\119\000\114\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\028\000\133\000\255\255\035\000\255\255\
    \122\000\255\255\255\255\131\000\255\255\111\000\255\255\114\000\
    \126\000\255\255\255\255\125\000\119\000\135\000\134\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\035\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\255\255\028\000\
    \255\255\255\255\028\000\255\255\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\122\000\125\000\126\000\131\000\134\000\133\000\
    \255\255\135\000\255\255\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\255\255\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\029\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\111\000\255\255\114\000\
    \255\255\119\000\255\255\255\255\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\255\255\255\255\
    \255\255\255\255\029\000\255\255\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \058\000\058\000\058\000\058\000\058\000\058\000\058\000\058\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\059\000\059\000\059\000\059\000\059\000\059\000\
    \059\000\059\000\002\001\002\001\002\001\002\001\002\001\002\001\
    \002\001\002\001\002\001\002\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\255\255\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\030\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\255\255\255\255\
    \255\255\255\255\030\000\255\255\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \060\000\060\000\060\000\060\000\060\000\060\000\060\000\060\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\061\000\061\000\061\000\061\000\061\000\061\000\
    \061\000\061\000\006\001\006\001\006\001\006\001\006\001\006\001\
    \006\001\006\001\006\001\006\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\255\255\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\031\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\255\255\248\000\
    \255\255\255\255\031\000\249\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \163\000\248\000\255\255\163\000\255\255\249\000\039\001\039\001\
    \039\001\039\001\039\001\039\001\039\001\039\001\039\001\039\001\
    \255\255\255\255\255\255\042\000\255\255\255\255\255\255\255\255\
    \255\255\163\000\255\255\255\255\255\255\255\255\163\000\163\000\
    \255\255\255\255\163\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\042\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\255\255\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\255\255\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\042\000\042\000\042\000\
    \042\000\042\000\042\000\042\000\042\000\043\000\255\255\109\000\
    \109\000\255\255\255\255\109\000\255\255\255\255\255\255\127\000\
    \127\000\136\000\136\000\127\000\255\255\136\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\043\000\255\255\109\000\051\000\
    \109\000\192\000\255\255\255\255\255\255\248\000\127\000\255\255\
    \136\000\249\000\192\000\192\000\192\000\192\000\192\000\192\000\
    \192\000\192\000\192\000\192\000\255\255\255\255\051\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\255\255\
    \043\000\255\255\109\000\043\000\255\255\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\051\000\051\000\051\000\051\000\051\000\051\000\051\000\
    \051\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\068\000\068\000\068\000\068\000\068\000\
    \068\000\068\000\068\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\255\255\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\044\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\127\000\255\255\
    \136\000\255\255\255\255\255\255\255\255\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\255\255\
    \255\255\255\255\255\255\044\000\255\255\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\069\000\069\000\069\000\069\000\069\000\069\000\069\000\
    \069\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\070\000\070\000\070\000\070\000\070\000\
    \070\000\070\000\070\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\255\255\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\045\000\044\000\044\000\044\000\044\000\
    \044\000\044\000\044\000\044\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\255\255\
    \255\255\255\255\255\255\045\000\255\255\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\071\000\071\000\071\000\071\000\071\000\071\000\071\000\
    \071\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\072\000\072\000\072\000\072\000\072\000\
    \072\000\072\000\072\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\255\255\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\046\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\255\255\
    \255\255\255\255\255\255\046\000\255\255\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\077\000\077\000\077\000\077\000\077\000\077\000\077\000\
    \077\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\085\000\085\000\085\000\085\000\085\000\
    \085\000\085\000\085\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\255\255\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\052\000\046\000\046\000\046\000\046\000\
    \046\000\046\000\046\000\046\000\255\255\105\001\105\001\255\255\
    \255\255\105\001\255\255\255\255\255\255\255\255\255\255\110\001\
    \110\001\255\255\052\000\110\001\255\255\073\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\105\001\255\255\210\000\210\000\
    \210\000\210\000\210\000\210\000\210\000\210\000\110\001\255\255\
    \255\255\255\255\255\255\255\255\073\000\255\255\255\255\110\001\
    \255\255\255\255\110\001\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\210\000\052\000\255\255\
    \255\255\052\000\255\255\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
    \073\000\073\000\073\000\073\000\073\000\073\000\073\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\086\000\086\000\086\000\086\000\086\000\086\000\086\000\
    \086\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\255\255\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\053\000\052\000\052\000\052\000\052\000\052\000\052\000\
    \052\000\052\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\105\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\255\255\255\255\255\255\
    \255\255\053\000\255\255\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\087\000\
    \087\000\087\000\087\000\087\000\087\000\087\000\087\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\088\000\088\000\088\000\088\000\088\000\088\000\088\000\
    \088\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\255\255\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\054\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\255\255\255\255\255\255\
    \255\255\054\000\255\255\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\089\000\
    \089\000\089\000\089\000\089\000\089\000\089\000\089\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\095\000\095\000\095\000\095\000\095\000\095\000\095\000\
    \095\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\255\255\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\055\000\054\000\054\000\054\000\054\000\054\000\054\000\
    \054\000\054\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\255\255\255\255\255\255\
    \255\255\055\000\255\255\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\096\000\
    \096\000\096\000\096\000\096\000\096\000\096\000\096\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\097\000\097\000\097\000\097\000\097\000\097\000\097\000\
    \097\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\255\255\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\062\000\055\000\055\000\055\000\055\000\055\000\055\000\
    \055\000\055\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \214\000\255\255\255\255\214\000\255\255\255\255\255\255\255\255\
    \062\000\255\255\062\000\078\000\255\255\255\255\212\000\212\000\
    \212\000\212\000\212\000\212\000\212\000\212\000\212\000\212\000\
    \214\000\255\255\255\255\255\255\255\255\214\000\214\000\255\255\
    \214\000\255\255\078\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\098\000\098\000\098\000\
    \098\000\098\000\098\000\098\000\098\000\212\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\062\000\255\255\255\255\255\255\
    \255\255\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\062\000\062\000\062\000\062\000\062\000\
    \062\000\062\000\062\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\078\000\078\000\078\000\
    \078\000\078\000\078\000\078\000\078\000\079\000\233\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\233\000\
    \233\000\233\000\233\000\233\000\233\000\233\000\233\000\233\000\
    \233\000\255\255\255\255\255\255\079\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\099\000\
    \099\000\099\000\099\000\099\000\099\000\099\000\099\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\255\255\
    \079\000\255\255\255\255\079\000\255\255\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\255\255\
    \216\000\255\255\255\255\216\000\255\255\255\255\214\000\255\255\
    \115\000\115\000\142\000\255\255\142\000\142\000\142\000\142\000\
    \142\000\142\000\142\000\142\000\142\000\142\000\255\255\255\255\
    \216\000\255\255\255\255\255\255\255\255\216\000\216\000\115\000\
    \216\000\142\000\115\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\142\000\255\255\255\255\255\255\255\255\
    \255\255\142\000\255\255\255\255\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\115\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\080\000\079\000\079\000\079\000\079\000\
    \079\000\079\000\079\000\079\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\255\255\
    \255\255\010\001\255\255\080\000\010\001\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\120\000\
    \120\000\191\000\075\001\075\001\075\001\075\001\075\001\075\001\
    \075\001\075\001\191\000\191\000\191\000\191\000\191\000\191\000\
    \191\000\191\000\191\000\191\000\255\255\010\001\120\000\255\255\
    \255\255\255\255\255\255\191\000\191\000\191\000\191\000\191\000\
    \191\000\255\255\255\255\255\255\255\255\255\255\216\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\115\000\
    \255\255\075\001\255\255\010\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\191\000\191\000\191\000\191\000\191\000\
    \191\000\255\255\255\255\255\255\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\120\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\081\000\080\000\080\000\080\000\080\000\
    \080\000\080\000\080\000\080\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\255\255\
    \255\255\255\255\255\255\081\000\255\255\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\097\001\
    \097\001\255\255\255\255\097\001\255\255\255\255\255\255\010\001\
    \121\000\121\000\255\255\205\000\205\000\205\000\205\000\205\000\
    \205\000\205\000\205\000\205\000\205\000\255\255\097\001\100\001\
    \100\001\101\001\101\001\100\001\255\255\101\001\097\001\121\000\
    \205\000\255\255\121\000\255\255\255\255\255\255\120\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\100\001\255\255\
    \101\001\255\255\255\255\255\255\255\255\255\255\100\001\255\255\
    \101\001\255\255\205\000\255\255\255\255\255\255\255\255\255\255\
    \205\000\255\255\255\255\255\255\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\121\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\082\000\081\000\081\000\081\000\081\000\
    \081\000\081\000\081\000\081\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\255\255\
    \255\255\255\255\255\255\082\000\255\255\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\194\000\
    \194\000\194\000\194\000\194\000\194\000\194\000\194\000\194\000\
    \194\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \194\000\194\000\194\000\194\000\194\000\194\000\255\255\102\001\
    \102\001\255\255\255\255\102\001\255\255\255\255\255\255\255\255\
    \255\255\108\001\108\001\103\001\103\001\108\001\097\001\103\001\
    \130\000\130\000\255\255\255\255\255\255\255\255\102\001\121\000\
    \194\000\194\000\194\000\194\000\194\000\194\000\102\001\255\255\
    \108\001\255\255\103\001\255\255\255\255\255\255\100\001\130\000\
    \101\001\255\255\103\001\255\255\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\255\255\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\090\000\082\000\082\000\082\000\082\000\
    \082\000\082\000\082\000\082\000\130\000\143\000\255\255\143\000\
    \143\000\143\000\143\000\143\000\143\000\143\000\143\000\143\000\
    \143\000\255\255\090\000\255\255\090\000\255\255\255\255\255\255\
    \255\255\143\000\100\000\255\255\143\000\167\000\255\255\255\255\
    \167\000\255\255\255\255\255\255\106\001\106\001\143\000\255\255\
    \106\001\255\255\107\001\107\001\255\255\255\255\107\001\143\000\
    \255\255\100\000\255\255\100\000\255\255\255\255\143\000\255\255\
    \255\255\143\000\167\000\106\001\143\000\012\001\255\255\255\255\
    \012\001\107\001\255\255\255\255\106\001\255\255\143\000\106\001\
    \255\255\255\255\107\001\255\255\255\255\107\001\090\000\143\000\
    \255\255\255\255\255\255\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\090\000\090\000\090\000\
    \090\000\090\000\090\000\090\000\090\000\100\000\255\255\167\000\
    \255\255\012\001\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\100\000\100\000\100\000\100\000\
    \100\000\100\000\100\000\100\000\139\000\139\000\102\001\012\001\
    \139\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \108\001\255\255\103\001\255\255\255\255\255\255\255\255\130\000\
    \255\255\255\255\255\255\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\255\255\208\000\208\000\208\000\208\000\208\000\
    \208\000\208\000\208\000\208\000\208\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\167\000\208\000\208\000\208\000\208\000\
    \208\000\208\000\255\255\106\001\255\255\255\255\255\255\255\255\
    \255\255\107\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\012\001\208\000\208\000\208\000\208\000\
    \208\000\208\000\255\255\255\255\255\255\219\000\255\255\255\255\
    \219\000\255\255\255\255\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\219\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\255\255\139\000\139\000\139\000\139\000\139\000\
    \139\000\139\000\139\000\139\000\148\000\255\255\255\255\148\000\
    \148\000\148\000\255\255\255\255\255\255\148\000\148\000\219\000\
    \148\000\148\000\148\000\185\000\185\000\185\000\185\000\185\000\
    \185\000\185\000\185\000\185\000\185\000\148\000\255\255\148\000\
    \148\000\148\000\148\000\148\000\255\255\236\000\255\255\149\000\
    \236\000\255\255\149\000\149\000\149\000\255\255\255\255\246\000\
    \149\000\149\000\246\000\149\000\149\000\149\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\236\000\255\255\255\255\
    \149\000\148\000\149\000\149\000\149\000\149\000\149\000\246\000\
    \250\000\255\255\151\000\255\255\255\255\151\000\151\000\151\000\
    \255\255\255\255\255\255\151\000\151\000\255\255\151\000\151\000\
    \151\000\255\255\255\255\185\000\255\255\255\255\255\255\148\000\
    \250\000\148\000\255\255\151\000\149\000\151\000\151\000\151\000\
    \151\000\151\000\255\255\255\255\255\255\255\255\255\255\154\000\
    \255\255\255\255\154\000\154\000\154\000\255\255\255\255\236\000\
    \154\000\154\000\255\255\154\000\154\000\154\000\255\255\255\255\
    \255\255\246\000\149\000\255\255\149\000\255\255\151\000\151\000\
    \154\000\255\255\154\000\154\000\154\000\154\000\154\000\255\255\
    \255\255\255\255\155\000\255\255\255\255\155\000\155\000\155\000\
    \255\255\255\255\250\000\155\000\155\000\255\255\155\000\155\000\
    \155\000\255\255\255\255\255\255\255\255\151\000\255\255\151\000\
    \255\255\255\255\255\255\155\000\154\000\155\000\155\000\155\000\
    \155\000\155\000\255\255\219\000\255\255\255\255\255\255\156\000\
    \255\255\255\255\156\000\156\000\156\000\255\255\255\255\255\255\
    \156\000\156\000\255\255\156\000\156\000\156\000\255\255\255\255\
    \255\255\255\255\154\000\255\255\154\000\255\255\155\000\155\000\
    \156\000\255\255\156\000\156\000\156\000\156\000\156\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\185\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\155\000\155\000\155\000\
    \255\255\255\255\160\000\255\255\156\000\160\000\160\000\160\000\
    \255\255\255\255\255\255\160\000\160\000\255\255\160\000\160\000\
    \160\000\160\000\160\000\160\000\160\000\160\000\160\000\160\000\
    \160\000\160\000\160\000\160\000\255\255\160\000\160\000\160\000\
    \160\000\160\000\156\000\255\255\156\000\161\000\255\255\255\255\
    \161\000\161\000\161\000\236\000\255\255\255\255\161\000\161\000\
    \255\255\161\000\161\000\161\000\255\255\246\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\161\000\160\000\
    \161\000\161\000\161\000\161\000\161\000\255\255\255\255\255\255\
    \162\000\255\255\255\255\162\000\162\000\162\000\250\000\255\255\
    \255\255\162\000\162\000\255\255\162\000\162\000\162\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\160\000\255\255\160\000\
    \255\255\162\000\161\000\162\000\162\000\162\000\162\000\162\000\
    \255\255\255\255\255\255\255\255\168\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\255\255\
    \161\000\255\255\161\000\255\255\255\255\162\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \255\255\255\255\255\255\162\000\168\000\162\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \209\000\209\000\209\000\209\000\209\000\209\000\209\000\209\000\
    \209\000\209\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\209\000\209\000\209\000\209\000\209\000\209\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\209\000\
    \255\255\209\000\209\000\209\000\209\000\209\000\209\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\255\255\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\255\255\168\000\168\000\168\000\
    \168\000\168\000\168\000\168\000\168\000\169\000\169\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\255\255\255\255\255\255\255\255\169\000\255\255\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\037\001\037\001\037\001\037\001\037\001\037\001\037\001\
    \037\001\037\001\037\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\225\000\225\000\225\000\
    \225\000\225\000\225\000\225\000\225\000\225\000\225\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \037\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\225\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\170\000\169\000\169\000\
    \169\000\169\000\169\000\169\000\169\000\169\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\255\255\255\255\255\255\255\255\170\000\255\255\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\232\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\232\000\232\000\232\000\232\000\232\000\232\000\
    \232\000\232\000\232\000\232\000\255\255\225\000\255\255\255\255\
    \255\255\255\255\255\255\232\000\232\000\232\000\232\000\232\000\
    \232\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\232\000\232\000\232\000\232\000\232\000\
    \232\000\255\255\255\255\255\255\255\255\255\255\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\255\255\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\255\255\170\000\170\000\
    \170\000\170\000\170\000\170\000\170\000\170\000\171\000\171\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\255\255\255\255\255\255\255\255\171\000\255\255\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\235\000\235\000\235\000\235\000\235\000\235\000\
    \235\000\235\000\235\000\235\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\235\000\235\000\235\000\235\000\235\000\
    \235\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\235\000\235\000\235\000\235\000\235\000\
    \235\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\255\255\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\255\255\171\000\
    \171\000\171\000\171\000\171\000\171\000\171\000\171\000\173\000\
    \173\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\255\255\255\255\255\255\255\255\173\000\
    \255\255\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\001\001\001\001\001\001\001\001\001\001\
    \001\001\001\001\001\001\001\001\001\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\001\001\001\001\001\001\001\001\
    \001\001\001\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\001\001\001\001\001\001\001\001\
    \001\001\001\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\255\255\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\255\255\
    \173\000\173\000\173\000\173\000\173\000\173\000\173\000\173\000\
    \174\000\174\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\255\255\255\255\255\255\255\255\
    \174\000\255\255\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\008\001\008\001\008\001\008\001\
    \008\001\008\001\008\001\008\001\008\001\008\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\008\001\008\001\008\001\
    \008\001\008\001\008\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\008\001\008\001\008\001\
    \008\001\008\001\008\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \255\255\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \255\255\174\000\174\000\174\000\174\000\174\000\174\000\174\000\
    \174\000\175\000\175\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\255\255\255\255\255\255\
    \255\255\175\000\255\255\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\073\001\073\001\073\001\
    \073\001\073\001\073\001\073\001\073\001\073\001\073\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\073\001\073\001\
    \073\001\073\001\073\001\073\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\073\001\073\001\
    \073\001\073\001\073\001\073\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\255\255\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\255\255\175\000\175\000\175\000\175\000\175\000\175\000\
    \175\000\175\000\176\000\176\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\255\255\255\255\
    \255\255\255\255\176\000\255\255\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\074\001\074\001\
    \074\001\074\001\074\001\074\001\074\001\074\001\074\001\074\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\074\001\
    \074\001\074\001\074\001\074\001\074\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\074\001\255\255\074\001\
    \074\001\074\001\074\001\074\001\074\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\255\255\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\255\255\176\000\176\000\176\000\176\000\176\000\
    \176\000\176\000\176\000\177\000\177\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\255\255\
    \255\255\255\255\255\255\177\000\255\255\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\255\255\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\255\255\177\000\177\000\177\000\177\000\
    \177\000\177\000\177\000\177\000\178\000\178\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \255\255\255\255\255\255\255\255\178\000\255\255\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\255\255\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\255\255\178\000\178\000\178\000\
    \178\000\178\000\178\000\178\000\178\000\179\000\179\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\255\255\255\255\255\255\255\255\179\000\255\255\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\255\255\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\255\255\179\000\179\000\
    \179\000\179\000\179\000\179\000\179\000\179\000\180\000\180\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\255\255\255\255\255\255\255\255\180\000\255\255\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\255\255\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\255\255\180\000\
    \180\000\180\000\180\000\180\000\180\000\180\000\180\000\181\000\
    \181\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\255\255\255\255\255\255\255\255\181\000\
    \255\255\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\255\255\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\255\255\
    \181\000\181\000\181\000\181\000\181\000\181\000\181\000\181\000\
    \182\000\182\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\255\255\255\255\255\255\255\255\
    \182\000\255\255\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \255\255\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \255\255\182\000\182\000\182\000\182\000\182\000\182\000\182\000\
    \182\000\183\000\183\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\255\255\255\255\255\255\
    \255\255\183\000\255\255\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\255\255\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\255\255\183\000\183\000\183\000\183\000\183\000\183\000\
    \183\000\183\000\184\000\184\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\255\255\255\255\
    \255\255\255\255\184\000\255\255\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\255\255\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\255\255\184\000\184\000\184\000\184\000\184\000\
    \184\000\184\000\184\000\251\000\255\255\255\255\251\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\251\000\255\255\255\255\255\255\255\255\
    \251\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\251\000\251\000\251\000\251\000\251\000\251\000\251\000\
    \251\000\251\000\251\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\022\001\022\001\251\000\255\255\022\001\
    \255\255\255\255\255\255\251\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\251\000\
    \255\255\255\255\022\001\251\000\255\255\251\000\255\255\255\255\
    \022\001\251\000\022\001\022\001\022\001\022\001\022\001\022\001\
    \255\255\022\001\022\001\022\001\022\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\255\255\255\255\022\001\022\001\
    \022\001\255\255\255\255\022\001\022\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\255\255\022\001\255\255\
    \255\255\022\001\255\255\022\001\022\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\022\001\022\001\022\001\
    \022\001\022\001\022\001\022\001\022\001\028\001\022\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\255\255\255\255\255\255\255\255\028\001\255\255\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\028\001\028\001\028\001\028\001\028\001\028\001\028\001\
    \028\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\029\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\022\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\255\255\255\255\255\255\
    \255\255\029\001\255\255\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\029\001\029\001\029\001\
    \029\001\029\001\029\001\029\001\029\001\031\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\255\255\255\255\255\255\255\255\031\001\255\255\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\031\001\031\001\031\001\031\001\031\001\031\001\031\001\
    \031\001\032\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\255\255\255\255\255\255\
    \255\255\032\001\255\255\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\032\001\032\001\032\001\
    \032\001\032\001\032\001\032\001\032\001\033\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\255\255\255\255\255\255\255\255\033\001\255\255\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\033\001\033\001\033\001\033\001\033\001\033\001\033\001\
    \033\001\038\001\038\001\038\001\038\001\038\001\038\001\038\001\
    \038\001\038\001\038\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\038\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \038\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\038\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \038\001\255\255\255\255\038\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \038\001\043\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\038\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\255\255\255\255\255\255\
    \255\255\043\001\255\255\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\043\001\043\001\043\001\
    \043\001\043\001\043\001\043\001\043\001\048\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\048\001\048\001\
    \048\001\048\001\048\001\048\001\048\001\048\001\048\001\048\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\048\001\
    \048\001\048\001\048\001\048\001\048\001\048\001\048\001\048\001\
    \048\001\048\001\048\001\048\001\048\001\048\001\048\001\048\001\
    \048\001\048\001\048\001\048\001\048\001\048\001\048\001\048\001\
    \048\001\255\255\255\255\255\255\255\255\048\001\255\255\048\001\
    \048\001\048\001\048\001\048\001\048\001\048\001\048\001\048\001\
    \048\001\048\001\048\001\048\001\048\001\048\001\048\001\048\001\
    \048\001\048\001\048\001\048\001\048\001\048\001\048\001\048\001\
    \048\001\049\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\049\001\049\001\049\001\049\001\049\001\049\001\
    \049\001\049\001\049\001\049\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\049\001\049\001\049\001\049\001\049\001\
    \049\001\049\001\049\001\049\001\049\001\049\001\049\001\049\001\
    \049\001\049\001\049\001\049\001\049\001\049\001\049\001\049\001\
    \049\001\049\001\049\001\049\001\049\001\255\255\255\255\255\255\
    \255\255\049\001\255\255\049\001\049\001\049\001\049\001\049\001\
    \049\001\049\001\049\001\049\001\049\001\049\001\049\001\049\001\
    \049\001\049\001\049\001\049\001\049\001\049\001\049\001\049\001\
    \049\001\049\001\049\001\049\001\049\001\050\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\050\001\050\001\
    \050\001\050\001\050\001\050\001\050\001\050\001\050\001\050\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\050\001\
    \050\001\050\001\050\001\050\001\050\001\050\001\050\001\050\001\
    \050\001\050\001\050\001\050\001\050\001\050\001\050\001\050\001\
    \050\001\050\001\050\001\050\001\050\001\050\001\050\001\050\001\
    \050\001\255\255\255\255\255\255\255\255\050\001\255\255\050\001\
    \050\001\050\001\050\001\050\001\050\001\050\001\050\001\050\001\
    \050\001\050\001\050\001\050\001\050\001\050\001\050\001\050\001\
    \050\001\050\001\050\001\050\001\050\001\050\001\050\001\050\001\
    \050\001\051\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\051\001\051\001\051\001\051\001\051\001\051\001\
    \051\001\051\001\051\001\051\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\051\001\051\001\051\001\051\001\051\001\
    \051\001\051\001\051\001\051\001\051\001\051\001\051\001\051\001\
    \051\001\051\001\051\001\051\001\051\001\051\001\051\001\051\001\
    \051\001\051\001\051\001\051\001\051\001\255\255\255\255\255\255\
    \255\255\051\001\255\255\051\001\051\001\051\001\051\001\051\001\
    \051\001\051\001\051\001\051\001\051\001\051\001\051\001\051\001\
    \051\001\051\001\051\001\051\001\051\001\051\001\051\001\051\001\
    \051\001\051\001\051\001\051\001\051\001\052\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\052\001\052\001\
    \052\001\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\052\001\
    \052\001\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \052\001\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \052\001\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \052\001\255\255\255\255\255\255\255\255\052\001\255\255\052\001\
    \052\001\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \052\001\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \052\001\052\001\052\001\052\001\052\001\052\001\052\001\052\001\
    \052\001\053\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\053\001\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\053\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\053\001\053\001\255\255\255\255\255\255\
    \255\255\053\001\255\255\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\053\001\053\001\053\001\053\001\053\001\
    \053\001\053\001\053\001\053\001\053\001\054\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\054\001\054\001\
    \054\001\054\001\054\001\054\001\054\001\054\001\054\001\054\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\054\001\
    \054\001\054\001\054\001\054\001\054\001\054\001\054\001\054\001\
    \054\001\054\001\054\001\054\001\054\001\054\001\054\001\054\001\
    \054\001\054\001\054\001\054\001\054\001\054\001\054\001\054\001\
    \054\001\255\255\255\255\255\255\255\255\054\001\255\255\054\001\
    \054\001\054\001\054\001\054\001\054\001\054\001\054\001\054\001\
    \054\001\054\001\054\001\054\001\054\001\054\001\054\001\054\001\
    \054\001\054\001\054\001\054\001\054\001\054\001\054\001\054\001\
    \054\001\055\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\055\001\055\001\055\001\055\001\055\001\055\001\
    \055\001\055\001\055\001\055\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\055\001\055\001\055\001\055\001\055\001\
    \055\001\055\001\055\001\055\001\055\001\055\001\055\001\055\001\
    \055\001\055\001\055\001\055\001\055\001\055\001\055\001\055\001\
    \055\001\055\001\055\001\055\001\055\001\255\255\255\255\255\255\
    \255\255\055\001\255\255\055\001\055\001\055\001\055\001\055\001\
    \055\001\055\001\055\001\055\001\055\001\055\001\055\001\055\001\
    \055\001\055\001\055\001\055\001\055\001\055\001\055\001\055\001\
    \055\001\055\001\055\001\055\001\055\001\056\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\056\001\056\001\
    \056\001\056\001\056\001\056\001\056\001\056\001\056\001\056\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\056\001\
    \056\001\056\001\056\001\056\001\056\001\056\001\056\001\056\001\
    \056\001\056\001\056\001\056\001\056\001\056\001\056\001\056\001\
    \056\001\056\001\056\001\056\001\056\001\056\001\056\001\056\001\
    \056\001\255\255\255\255\255\255\255\255\056\001\255\255\056\001\
    \056\001\056\001\056\001\056\001\056\001\056\001\056\001\056\001\
    \056\001\056\001\056\001\056\001\056\001\056\001\056\001\056\001\
    \056\001\056\001\056\001\056\001\056\001\056\001\056\001\056\001\
    \056\001\057\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\057\001\057\001\057\001\057\001\057\001\057\001\
    \057\001\057\001\057\001\057\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\057\001\057\001\057\001\057\001\057\001\
    \057\001\057\001\057\001\057\001\057\001\057\001\057\001\057\001\
    \057\001\057\001\057\001\057\001\057\001\057\001\057\001\057\001\
    \057\001\057\001\057\001\057\001\057\001\255\255\255\255\255\255\
    \255\255\057\001\255\255\057\001\057\001\057\001\057\001\057\001\
    \057\001\057\001\057\001\057\001\057\001\057\001\057\001\057\001\
    \057\001\057\001\057\001\057\001\057\001\057\001\057\001\057\001\
    \057\001\057\001\057\001\057\001\057\001\058\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\058\001\058\001\
    \058\001\058\001\058\001\058\001\058\001\058\001\058\001\058\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\058\001\
    \058\001\058\001\058\001\058\001\058\001\058\001\058\001\058\001\
    \058\001\058\001\058\001\058\001\058\001\058\001\058\001\058\001\
    \058\001\058\001\058\001\058\001\058\001\058\001\058\001\058\001\
    \058\001\255\255\255\255\255\255\255\255\058\001\255\255\058\001\
    \058\001\058\001\058\001\058\001\058\001\058\001\058\001\058\001\
    \058\001\058\001\058\001\058\001\058\001\058\001\058\001\058\001\
    \058\001\058\001\058\001\058\001\058\001\058\001\058\001\058\001\
    \058\001\059\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\059\001\059\001\059\001\059\001\059\001\059\001\
    \059\001\059\001\059\001\059\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\059\001\059\001\059\001\059\001\059\001\
    \059\001\059\001\059\001\059\001\059\001\059\001\059\001\059\001\
    \059\001\059\001\059\001\059\001\059\001\059\001\059\001\059\001\
    \059\001\059\001\059\001\059\001\059\001\255\255\255\255\255\255\
    \255\255\059\001\255\255\059\001\059\001\059\001\059\001\059\001\
    \059\001\059\001\059\001\059\001\059\001\059\001\059\001\059\001\
    \059\001\059\001\059\001\059\001\059\001\059\001\059\001\059\001\
    \059\001\059\001\059\001\059\001\059\001\060\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\060\001\060\001\
    \060\001\060\001\060\001\060\001\060\001\060\001\060\001\060\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\060\001\
    \060\001\060\001\060\001\060\001\060\001\060\001\060\001\060\001\
    \060\001\060\001\060\001\060\001\060\001\060\001\060\001\060\001\
    \060\001\060\001\060\001\060\001\060\001\060\001\060\001\060\001\
    \060\001\255\255\255\255\255\255\255\255\060\001\255\255\060\001\
    \060\001\060\001\060\001\060\001\060\001\060\001\060\001\060\001\
    \060\001\060\001\060\001\060\001\060\001\060\001\060\001\060\001\
    \060\001\060\001\060\001\060\001\060\001\060\001\060\001\060\001\
    \060\001\061\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\061\001\061\001\061\001\061\001\061\001\061\001\
    \061\001\061\001\061\001\061\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\061\001\061\001\061\001\061\001\061\001\
    \061\001\061\001\061\001\061\001\061\001\061\001\061\001\061\001\
    \061\001\061\001\061\001\061\001\061\001\061\001\061\001\061\001\
    \061\001\061\001\061\001\061\001\061\001\255\255\255\255\255\255\
    \255\255\061\001\255\255\061\001\061\001\061\001\061\001\061\001\
    \061\001\061\001\061\001\061\001\061\001\061\001\061\001\061\001\
    \061\001\061\001\061\001\061\001\061\001\061\001\061\001\061\001\
    \061\001\061\001\061\001\061\001\061\001\062\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\062\001\062\001\
    \062\001\062\001\062\001\062\001\062\001\062\001\062\001\062\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\062\001\
    \062\001\062\001\062\001\062\001\062\001\062\001\062\001\062\001\
    \062\001\062\001\062\001\062\001\062\001\062\001\062\001\062\001\
    \062\001\062\001\062\001\062\001\062\001\062\001\062\001\062\001\
    \062\001\255\255\255\255\255\255\255\255\062\001\255\255\062\001\
    \062\001\062\001\062\001\062\001\062\001\062\001\062\001\062\001\
    \062\001\062\001\062\001\062\001\062\001\062\001\062\001\062\001\
    \062\001\062\001\062\001\062\001\062\001\062\001\062\001\062\001\
    \062\001\063\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\063\001\255\255\255\255\255\255\
    \255\255\063\001\255\255\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\063\001\063\001\063\001\063\001\
    \063\001\063\001\063\001\063\001\063\001\066\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\066\001\066\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\255\255\255\255\255\255\255\255\066\001\255\255\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\066\001\066\001\066\001\066\001\066\001\066\001\066\001\
    \066\001\067\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\067\001\067\001\067\001\067\001\067\001\067\001\
    \067\001\067\001\067\001\067\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\067\001\067\001\067\001\067\001\067\001\
    \067\001\067\001\067\001\067\001\067\001\067\001\067\001\067\001\
    \067\001\067\001\067\001\067\001\067\001\067\001\067\001\067\001\
    \067\001\067\001\067\001\067\001\067\001\255\255\255\255\255\255\
    \255\255\067\001\255\255\067\001\067\001\067\001\067\001\067\001\
    \067\001\067\001\067\001\067\001\067\001\067\001\067\001\067\001\
    \067\001\067\001\067\001\067\001\067\001\067\001\067\001\067\001\
    \067\001\067\001\067\001\067\001\067\001\077\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\255\255\255\255\255\255\255\255\077\001\255\255\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\077\001\077\001\077\001\077\001\077\001\077\001\077\001\
    \077\001\078\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\078\001\078\001\078\001\078\001\078\001\078\001\
    \078\001\078\001\078\001\078\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\078\001\078\001\078\001\078\001\078\001\
    \078\001\078\001\078\001\078\001\078\001\078\001\078\001\078\001\
    \078\001\078\001\078\001\078\001\078\001\078\001\078\001\078\001\
    \078\001\078\001\078\001\078\001\078\001\255\255\255\255\255\255\
    \255\255\078\001\255\255\078\001\078\001\078\001\078\001\078\001\
    \078\001\078\001\078\001\078\001\078\001\078\001\078\001\078\001\
    \078\001\078\001\078\001\078\001\078\001\078\001\078\001\078\001\
    \078\001\078\001\078\001\078\001\078\001\079\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\079\001\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\079\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\079\001\
    \079\001\255\255\255\255\255\255\255\255\079\001\255\255\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\079\001\
    \079\001\079\001\079\001\079\001\079\001\079\001\079\001\079\001\
    \079\001\080\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\255\255\255\255\255\255\
    \255\255\080\001\255\255\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\080\001\080\001\080\001\
    \080\001\080\001\080\001\080\001\080\001\081\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\081\001\081\001\
    \081\001\081\001\081\001\081\001\081\001\081\001\081\001\081\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\081\001\
    \081\001\081\001\081\001\081\001\081\001\081\001\081\001\081\001\
    \081\001\081\001\081\001\081\001\081\001\081\001\081\001\081\001\
    \081\001\081\001\081\001\081\001\081\001\081\001\081\001\081\001\
    \081\001\255\255\255\255\255\255\255\255\081\001\255\255\081\001\
    \081\001\081\001\081\001\081\001\081\001\081\001\081\001\081\001\
    \081\001\081\001\081\001\081\001\081\001\081\001\081\001\081\001\
    \081\001\081\001\081\001\081\001\081\001\081\001\081\001\081\001\
    \081\001\082\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\082\001\082\001\082\001\082\001\082\001\082\001\
    \082\001\082\001\082\001\082\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\082\001\082\001\082\001\082\001\082\001\
    \082\001\082\001\082\001\082\001\082\001\082\001\082\001\082\001\
    \082\001\082\001\082\001\082\001\082\001\082\001\082\001\082\001\
    \082\001\082\001\082\001\082\001\082\001\255\255\255\255\255\255\
    \255\255\082\001\255\255\082\001\082\001\082\001\082\001\082\001\
    \082\001\082\001\082\001\082\001\082\001\082\001\082\001\082\001\
    \082\001\082\001\082\001\082\001\082\001\082\001\082\001\082\001\
    \082\001\082\001\082\001\082\001\082\001\083\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\083\001\083\001\
    \083\001\083\001\083\001\083\001\083\001\083\001\083\001\083\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\083\001\
    \083\001\083\001\083\001\083\001\083\001\083\001\083\001\083\001\
    \083\001\083\001\083\001\083\001\083\001\083\001\083\001\083\001\
    \083\001\083\001\083\001\083\001\083\001\083\001\083\001\083\001\
    \083\001\255\255\255\255\255\255\255\255\083\001\255\255\083\001\
    \083\001\083\001\083\001\083\001\083\001\083\001\083\001\083\001\
    \083\001\083\001\083\001\083\001\083\001\083\001\083\001\083\001\
    \083\001\083\001\083\001\083\001\083\001\083\001\083\001\083\001\
    \083\001\084\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\084\001\084\001\084\001\084\001\084\001\084\001\
    \084\001\084\001\084\001\084\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\084\001\084\001\084\001\084\001\084\001\
    \084\001\084\001\084\001\084\001\084\001\084\001\084\001\084\001\
    \084\001\084\001\084\001\084\001\084\001\084\001\084\001\084\001\
    \084\001\084\001\084\001\084\001\084\001\255\255\255\255\255\255\
    \255\255\084\001\255\255\084\001\084\001\084\001\084\001\084\001\
    \084\001\084\001\084\001\084\001\084\001\084\001\084\001\084\001\
    \084\001\084\001\084\001\084\001\084\001\084\001\084\001\084\001\
    \084\001\084\001\084\001\084\001\084\001\085\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\085\001\085\001\
    \085\001\085\001\085\001\085\001\085\001\085\001\085\001\085\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\085\001\
    \085\001\085\001\085\001\085\001\085\001\085\001\085\001\085\001\
    \085\001\085\001\085\001\085\001\085\001\085\001\085\001\085\001\
    \085\001\085\001\085\001\085\001\085\001\085\001\085\001\085\001\
    \085\001\255\255\255\255\255\255\255\255\085\001\255\255\085\001\
    \085\001\085\001\085\001\085\001\085\001\085\001\085\001\085\001\
    \085\001\085\001\085\001\085\001\085\001\085\001\085\001\085\001\
    \085\001\085\001\085\001\085\001\085\001\085\001\085\001\085\001\
    \085\001\086\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\086\001\086\001\086\001\086\001\086\001\086\001\
    \086\001\086\001\086\001\086\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\086\001\086\001\086\001\086\001\086\001\
    \086\001\086\001\086\001\086\001\086\001\086\001\086\001\086\001\
    \086\001\086\001\086\001\086\001\086\001\086\001\086\001\086\001\
    \086\001\086\001\086\001\086\001\086\001\255\255\255\255\255\255\
    \255\255\086\001\255\255\086\001\086\001\086\001\086\001\086\001\
    \086\001\086\001\086\001\086\001\086\001\086\001\086\001\086\001\
    \086\001\086\001\086\001\086\001\086\001\086\001\086\001\086\001\
    \086\001\086\001\086\001\086\001\086\001\087\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\087\001\087\001\
    \087\001\087\001\087\001\087\001\087\001\087\001\087\001\087\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\087\001\
    \087\001\087\001\087\001\087\001\087\001\087\001\087\001\087\001\
    \087\001\087\001\087\001\087\001\087\001\087\001\087\001\087\001\
    \087\001\087\001\087\001\087\001\087\001\087\001\087\001\087\001\
    \087\001\255\255\255\255\255\255\255\255\087\001\255\255\087\001\
    \087\001\087\001\087\001\087\001\087\001\087\001\087\001\087\001\
    \087\001\087\001\087\001\087\001\087\001\087\001\087\001\087\001\
    \087\001\087\001\087\001\087\001\087\001\087\001\087\001\087\001\
    \087\001\088\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\088\001\088\001\088\001\088\001\088\001\088\001\
    \088\001\088\001\088\001\088\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\088\001\088\001\088\001\088\001\088\001\
    \088\001\088\001\088\001\088\001\088\001\088\001\088\001\088\001\
    \088\001\088\001\088\001\088\001\088\001\088\001\088\001\088\001\
    \088\001\088\001\088\001\088\001\088\001\255\255\255\255\255\255\
    \255\255\088\001\255\255\088\001\088\001\088\001\088\001\088\001\
    \088\001\088\001\088\001\088\001\088\001\088\001\088\001\088\001\
    \088\001\088\001\088\001\088\001\088\001\088\001\088\001\088\001\
    \088\001\088\001\088\001\088\001\088\001\089\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\089\001\089\001\
    \089\001\089\001\089\001\089\001\089\001\089\001\089\001\089\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\089\001\
    \089\001\089\001\089\001\089\001\089\001\089\001\089\001\089\001\
    \089\001\089\001\089\001\089\001\089\001\089\001\089\001\089\001\
    \089\001\089\001\089\001\089\001\089\001\089\001\089\001\089\001\
    \089\001\255\255\255\255\255\255\255\255\089\001\255\255\089\001\
    \089\001\089\001\089\001\089\001\089\001\089\001\089\001\089\001\
    \089\001\089\001\089\001\089\001\089\001\089\001\089\001\089\001\
    \089\001\089\001\089\001\089\001\089\001\089\001\089\001\089\001\
    \089\001\090\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\090\001\090\001\090\001\090\001\090\001\090\001\
    \090\001\090\001\090\001\090\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\090\001\090\001\090\001\090\001\090\001\
    \090\001\090\001\090\001\090\001\090\001\090\001\090\001\090\001\
    \090\001\090\001\090\001\090\001\090\001\090\001\090\001\090\001\
    \090\001\090\001\090\001\090\001\090\001\255\255\255\255\255\255\
    \255\255\090\001\255\255\090\001\090\001\090\001\090\001\090\001\
    \090\001\090\001\090\001\090\001\090\001\090\001\090\001\090\001\
    \090\001\090\001\090\001\090\001\090\001\090\001\090\001\090\001\
    \090\001\090\001\090\001\090\001\090\001\091\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\091\001\091\001\
    \091\001\091\001\091\001\091\001\091\001\091\001\091\001\091\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\091\001\
    \091\001\091\001\091\001\091\001\091\001\091\001\091\001\091\001\
    \091\001\091\001\091\001\091\001\091\001\091\001\091\001\091\001\
    \091\001\091\001\091\001\091\001\091\001\091\001\091\001\091\001\
    \091\001\255\255\255\255\255\255\255\255\091\001\255\255\091\001\
    \091\001\091\001\091\001\091\001\091\001\091\001\091\001\091\001\
    \091\001\091\001\091\001\091\001\091\001\091\001\091\001\091\001\
    \091\001\091\001\091\001\091\001\091\001\091\001\091\001\091\001\
    \091\001\092\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\092\001\092\001\092\001\092\001\092\001\092\001\
    \092\001\092\001\092\001\092\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\092\001\092\001\092\001\092\001\092\001\
    \092\001\092\001\092\001\092\001\092\001\092\001\092\001\092\001\
    \092\001\092\001\092\001\092\001\092\001\092\001\092\001\092\001\
    \092\001\092\001\092\001\092\001\092\001\255\255\255\255\255\255\
    \255\255\092\001\255\255\092\001\092\001\092\001\092\001\092\001\
    \092\001\092\001\092\001\092\001\092\001\092\001\092\001\092\001\
    \092\001\092\001\092\001\092\001\092\001\092\001\092\001\092\001\
    \092\001\092\001\092\001\092\001\092\001\093\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\093\001\093\001\
    \093\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\093\001\
    \093\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\255\255\255\255\255\255\255\255\093\001\255\255\093\001\
    \093\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\093\001\093\001\093\001\093\001\093\001\093\001\093\001\
    \093\001\094\001\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\094\001\094\001\094\001\094\001\094\001\094\001\
    \094\001\094\001\094\001\094\001\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\094\001\094\001\094\001\094\001\094\001\
    \094\001\094\001\094\001\094\001\094\001\094\001\094\001\094\001\
    \094\001\094\001\094\001\094\001\094\001\094\001\094\001\094\001\
    \094\001\094\001\094\001\094\001\094\001\255\255\255\255\255\255\
    \255\255\094\001\255\255\094\001\094\001\094\001\094\001\094\001\
    \094\001\094\001\094\001\094\001\094\001\094\001\094\001\094\001\
    \094\001\094\001\094\001\094\001\094\001\094\001\094\001\094\001\
    \094\001\094\001\094\001\094\001\094\001\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\002\000\000\000\000\000\000\000\002\000\003\000\024\000\
    \000\000\000\000\000\000\005\000\006\000\000\000\000\000\000\000\
    \006\000\007\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\009\000\010\000\000\000\000\000\000\000\010\000\
    \011\000\000\000\000\000\013\000\014\000\000\000\000\000\000\000\
    \014\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\038\000\059\000\
    \000\000\000\000\000\000\016\000\017\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\051\000\056\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\060\000\000\000\000\000\000\000\000\000\
    \000\000\062\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\064\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\063\000\000\000\000\000\065\000\070\000\072\000\077\000\
    \073\000\000\000\098\000\105\000\000\000\088\000\000\000\093\000\
    ";
  Lexing.lex_backtrk_code = 
   "\000\000\000\000\000\000\000\000\000\000\013\000\000\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\000\000\000\000\021\000\000\000\000\000\000\000\000\000\
    \013\000\013\000\013\000\013\000\000\000\000\000\030\000\000\000\
    \000\000\000\000\013\000\013\000\013\000\013\000\000\000\000\000\
    \000\000\013\000\013\000\000\000\000\000\036\000\000\000\000\000\
    \000\000\013\000\013\000\013\000\000\000\000\000\042\000\000\000\
    \000\000\000\000\013\000\013\000\013\000\013\000\013\000\000\000\
    \000\000\000\000\000\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\000\000\000\000\000\000\000\000\013\000\000\000\
    \000\000\048\000\000\000\000\000\000\000\013\000\013\000\000\000\
    \013\000\013\000\013\000\000\000\000\000\000\000\000\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_default_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\067\000\000\000\000\000\067\000\067\000\067\000\067\000\
    \000\000\000\000\078\000\078\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_trans_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\016\000\016\000\016\000\016\000\027\000\027\000\
    \027\000\027\000\033\000\033\000\033\000\033\000\039\000\039\000\
    \039\000\039\000\045\000\045\000\000\000\000\000\000\000\000\000\
    \001\000\016\000\016\000\000\000\000\000\027\000\027\000\000\000\
    \000\000\033\000\033\000\000\000\000\000\039\000\039\000\045\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\045\000\061\000\045\000\061\000\
    \070\000\070\000\000\000\000\000\070\000\000\000\000\000\070\000\
    \070\000\000\000\000\000\070\000\000\000\000\000\070\000\070\000\
    \000\000\000\000\070\000\045\000\061\000\000\000\061\000\070\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\070\000\000\000\
    \000\000\000\000\000\000\083\000\083\000\070\000\000\000\083\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\083\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check_code = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\004\000\016\000\017\000\021\000\022\000\027\000\028\000\
    \032\000\033\000\042\000\043\000\047\000\048\000\051\000\052\000\
    \056\000\057\000\083\000\084\000\255\255\255\255\255\255\255\255\
    \004\000\016\000\017\000\255\255\255\255\027\000\028\000\255\255\
    \255\255\042\000\043\000\255\255\255\255\051\000\052\000\078\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\079\000\115\000\078\000\121\000\
    \097\001\097\001\100\001\100\001\097\001\255\255\100\001\101\001\
    \101\001\102\001\102\001\101\001\255\255\102\001\103\001\103\001\
    \255\255\255\255\103\001\079\000\115\000\255\255\121\000\097\001\
    \255\255\100\001\255\255\255\255\255\255\255\255\101\001\097\001\
    \102\001\100\001\255\255\106\001\106\001\103\001\101\001\106\001\
    \102\001\255\255\107\001\107\001\255\255\103\001\107\001\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\106\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\107\001\255\255\106\001\255\255\255\255\106\001\255\255\
    \255\255\255\255\107\001\255\255\255\255\107\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\097\001\
    \255\255\100\001\255\255\255\255\255\255\255\255\101\001\255\255\
    \102\001\255\255\255\255\255\255\255\255\103\001\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\106\001\255\255\255\255\255\255\255\255\255\255\
    \255\255\107\001";
  Lexing.lex_code = 
   "\255\004\255\003\255\002\255\255\006\255\005\255\255\000\004\255\
    \008\255\007\255\255\000\008\255\000\007\255\009\255\255\000\009\
    \255\010\255\255\000\010\255\011\255\255\000\011\255\012\255\255\
    \000\012\255\000\003\001\006\255\000\002\001\005\255\001\255\255\
    \000\001\255\003\255\255\002\255\255\000\002\001\003\255\005\255\
    \004\255\255\003\255\002\255\255\000\002\001\004\255\000\003\001\
    \005\255";
}

let rec token e lexbuf =
    __ocaml_lex_token_rec e lexbuf 0
and __ocaml_lex_token_rec e lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 168 "src/cppo_lexer.mll"
      (
        (*
          We use two different lexers for boolean expressions in #if directives
          and for regular OCaml tokens.
        *)
        match e.lexer with
            `Ocaml -> ocaml_token e lexbuf
          | `Test -> test_token e lexbuf
      )
# 3825 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec e lexbuf __ocaml_lex_state

and line e lexbuf =
    __ocaml_lex_line_rec e lexbuf 1
and __ocaml_lex_line_rec e lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 179 "src/cppo_lexer.mll"
                  s
# 3838 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 180 "src/cppo_lexer.mll"
        (
          match e.lexer with
              `Test -> lexer_error lexbuf "Syntax error in boolean expression"
            | `Ocaml ->
                if e.line_start then (
                  e.in_directive <- true;
                  clear e;
                  add e s;
                  e.token_start <- pos1 lexbuf;
                  e.line_start <- false;
                  directive e lexbuf
                )
                else (
                  e.line_start <- false;
                  clear e;
                  TEXT (loc lexbuf, false, s)
                )
        )
# 3859 "src/cppo_lexer.ml"

  | 1 ->
# 199 "src/cppo_lexer.mll"
        ( clear e;
          token e lexbuf )
# 3865 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_line_rec e lexbuf __ocaml_lex_state

and directive e lexbuf =
  lexbuf.Lexing.lex_mem <- Array.make 13 (-1) ; (* L=3 [4] <- p ; [3] <- p ; [2] <- p ;  *)
  lexbuf.Lexing.lex_mem.(4) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
  __ocaml_lex_directive_rec e lexbuf 4
and __ocaml_lex_directive_rec e lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 203 "src/cppo_lexer.mll"
                                      id
# 3882 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) (lexbuf.Lexing.lex_curr_pos + -1) in
# 204 "src/cppo_lexer.mll"
      ( DEFUN (long_loc e, id) )
# 3886 "src/cppo_lexer.ml"

  | 1 ->
let
# 206 "src/cppo_lexer.mll"
                                      id
# 3892 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 207 "src/cppo_lexer.mll"
      ( assert e.in_directive;
        DEF (long_loc e, id) )
# 3897 "src/cppo_lexer.ml"

  | 2 ->
let
# 210 "src/cppo_lexer.mll"
                                     id
# 3903 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 211 "src/cppo_lexer.mll"
      ( blank_until_eol e lexbuf;
        UNDEF (long_loc e, id) )
# 3908 "src/cppo_lexer.ml"

  | 3 ->
# 214 "src/cppo_lexer.mll"
                           ( e.lexer <- `Test;
                             IF (long_loc e) )
# 3914 "src/cppo_lexer.ml"

  | 4 ->
# 216 "src/cppo_lexer.mll"
                           ( e.lexer <- `Test;
                             ELIF (long_loc e) )
# 3920 "src/cppo_lexer.ml"

  | 5 ->
let
# 219 "src/cppo_lexer.mll"
                                     id
# 3926 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 220 "src/cppo_lexer.mll"
      ( blank_until_eol e lexbuf;
        IFDEF (long_loc e, `Defined id) )
# 3931 "src/cppo_lexer.ml"

  | 6 ->
let
# 223 "src/cppo_lexer.mll"
                                      id
# 3937 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 224 "src/cppo_lexer.mll"
      ( blank_until_eol e lexbuf;
        IFDEF (long_loc e, `Not (`Defined id)) )
# 3942 "src/cppo_lexer.ml"

  | 7 ->
let
# 227 "src/cppo_lexer.mll"
                                   id
# 3948 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 228 "src/cppo_lexer.mll"
      ( blank_until_eol e lexbuf;
        clear e;
        let s = read_ext e lexbuf in
        EXT (long_loc e, id, s) )
# 3955 "src/cppo_lexer.ml"

  | 8 ->
# 238 "src/cppo_lexer.mll"
      ( error (loc lexbuf)
          "Identifiers containing non-ASCII characters \
           may not be used as macro identifiers" )
# 3962 "src/cppo_lexer.ml"

  | 9 ->
# 243 "src/cppo_lexer.mll"
      ( blank_until_eol e lexbuf;
        ELSE (long_loc e) )
# 3968 "src/cppo_lexer.ml"

  | 10 ->
# 247 "src/cppo_lexer.mll"
      ( blank_until_eol e lexbuf;
        ENDIF (long_loc e) )
# 3974 "src/cppo_lexer.ml"

  | 11 ->
# 251 "src/cppo_lexer.mll"
      ( clear e;
        eval_string e lexbuf;
        blank_until_eol e lexbuf;
        INCLUDE (long_loc e, get e) )
# 3982 "src/cppo_lexer.ml"

  | 12 ->
# 257 "src/cppo_lexer.mll"
      ( clear e;
        eval_string e lexbuf;
        blank_until_eol e lexbuf;
        ERROR (long_loc e, get e) )
# 3990 "src/cppo_lexer.ml"

  | 13 ->
# 263 "src/cppo_lexer.mll"
      ( clear e;
        eval_string e lexbuf;
        blank_until_eol e lexbuf;
        WARNING (long_loc e, get e) )
# 3998 "src/cppo_lexer.ml"

  | 14 ->
let
# 268 "src/cppo_lexer.mll"
                          lnum
# 4004 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1) in
# 269 "src/cppo_lexer.mll"
      ( e.in_directive <- false;
        new_line e;
        let here = long_loc e in
        let fname = None in
        let lnum = int_of_string lnum in
        (* Apply line directive regardless of possible #if condition. *)
        set_lnum lexbuf fname lnum;
        LINE (here, None, lnum) )
# 4015 "src/cppo_lexer.ml"

  | 15 ->
let
# 278 "src/cppo_lexer.mll"
                          lnum
# 4021 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1) in
# 279 "src/cppo_lexer.mll"
      ( clear e;
        eval_string e lexbuf;
        blank_until_eol e lexbuf;
        let here = long_loc e in
        let fname = Some (get e) in
        let lnum = int_of_string lnum in
        (* Apply line directive regardless of possible #if condition. *)
        set_lnum lexbuf fname lnum;
        LINE (here, fname, lnum) )
# 4033 "src/cppo_lexer.ml"

  | 16 ->
# 290 "src/cppo_lexer.mll"
      ( e.in_directive <- false;
        add e (lexeme lexbuf);
        TEXT (long_loc e, true, get e) )
# 4040 "src/cppo_lexer.ml"

  | 17 ->
let
# 294 "src/cppo_lexer.mll"
                          s
# 4046 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_curr_pos in
# 295 "src/cppo_lexer.mll"
      ( if is_reserved_directive s then
          error (loc lexbuf) "cppo directive with missing or wrong arguments";
        e.in_directive <- false;
        add e (lexeme lexbuf);
        TEXT (long_loc e, false, get e) )
# 4054 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_directive_rec e lexbuf __ocaml_lex_state

and blank_until_eol e lexbuf =
    __ocaml_lex_blank_until_eol_rec e lexbuf 111
and __ocaml_lex_blank_until_eol_rec e lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 304 "src/cppo_lexer.mll"
                      ( new_line e;
                        e.in_directive <- false )
# 4067 "src/cppo_lexer.ml"

  | 1 ->
# 306 "src/cppo_lexer.mll"
                      ( lexer_error lexbuf "syntax error in directive" )
# 4072 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_blank_until_eol_rec e lexbuf __ocaml_lex_state

and read_ext e lexbuf =
  lexbuf.Lexing.lex_mem <- Array.make 2 (-1) ; (* L=1 [1] <- p ;  *)
  lexbuf.Lexing.lex_mem.(1) <- lexbuf.Lexing.lex_curr_pos ;
  __ocaml_lex_read_ext_rec e lexbuf 115
and __ocaml_lex_read_ext_rec e lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 310 "src/cppo_lexer.mll"
      ( let s = get e in
        clear e;
        new_line e;
        e.in_directive <- false;
        s )
# 4090 "src/cppo_lexer.ml"

  | 1 ->
let
# 316 "src/cppo_lexer.mll"
               a
# 4096 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_mem.(0)
and
# 316 "src/cppo_lexer.mll"
                                                                 b
# 4101 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_mem.(0) + 1) lexbuf.Lexing.lex_curr_pos in
# 317 "src/cppo_lexer.mll"
      ( add e a;
        add e b;
        new_line e;
        read_ext e lexbuf )
# 4108 "src/cppo_lexer.ml"

  | 2 ->
let
# 322 "src/cppo_lexer.mll"
                     x
# 4114 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 323 "src/cppo_lexer.mll"
      ( add e x;
        new_line e;
        read_ext e lexbuf )
# 4120 "src/cppo_lexer.ml"

  | 3 ->
# 328 "src/cppo_lexer.mll"
      ( lexer_error lexbuf "End of file within #ext ... #endext" )
# 4125 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_read_ext_rec e lexbuf __ocaml_lex_state

and ocaml_token e lexbuf =
    __ocaml_lex_ocaml_token_rec e lexbuf 139
and __ocaml_lex_ocaml_token_rec e lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 332 "src/cppo_lexer.mll"
      ( e.line_start <- false;
        CURRENT_LINE (loc lexbuf) )
# 4138 "src/cppo_lexer.ml"

  | 1 ->
# 336 "src/cppo_lexer.mll"
      ( e.line_start <- false;
        CURRENT_FILE (loc lexbuf) )
# 4144 "src/cppo_lexer.ml"

  | 2 ->
let
# 339 "src/cppo_lexer.mll"
             s
# 4150 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 340 "src/cppo_lexer.mll"
      ( e.line_start <- false;
        IDENT (loc lexbuf, s) )
# 4155 "src/cppo_lexer.ml"

  | 3 ->
let
# 343 "src/cppo_lexer.mll"
                s
# 4161 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 344 "src/cppo_lexer.mll"
      ( e.line_start <- false;
        TEXT (loc lexbuf, false, s) )
# 4166 "src/cppo_lexer.ml"

  | 4 ->
let
# 347 "src/cppo_lexer.mll"
             s
# 4172 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_curr_pos + -1) in
# 348 "src/cppo_lexer.mll"
      ( e.line_start <- false;
        FUNIDENT (loc lexbuf, s) )
# 4177 "src/cppo_lexer.ml"

  | 5 ->
# 353 "src/cppo_lexer.mll"
      ( new_line e;
        TEXT (loc lexbuf, false, lexeme lexbuf) )
# 4183 "src/cppo_lexer.ml"

  | 6 ->
# 356 "src/cppo_lexer.mll"
              ( e.line_start <- false; OP_PAREN (loc lexbuf) )
# 4188 "src/cppo_lexer.ml"

  | 7 ->
# 357 "src/cppo_lexer.mll"
              ( e.line_start <- false; CL_PAREN (loc lexbuf) )
# 4193 "src/cppo_lexer.ml"

  | 8 ->
# 358 "src/cppo_lexer.mll"
              ( e.line_start <- false; COMMA (loc lexbuf) )
# 4198 "src/cppo_lexer.ml"

  | 9 ->
# 360 "src/cppo_lexer.mll"
              ( e.line_start <- false; TEXT (loc lexbuf, false, " )") )
# 4203 "src/cppo_lexer.ml"

  | 10 ->
# 361 "src/cppo_lexer.mll"
              ( e.line_start <- false; TEXT (loc lexbuf, false, " ,") )
# 4208 "src/cppo_lexer.ml"

  | 11 ->
# 362 "src/cppo_lexer.mll"
              ( e.line_start <- false; TEXT (loc lexbuf, false, " (") )
# 4213 "src/cppo_lexer.ml"

  | 12 ->
# 363 "src/cppo_lexer.mll"
              ( e.line_start <- false; TEXT (loc lexbuf, false, " #") )
# 4218 "src/cppo_lexer.ml"

  | 13 ->
# 376 "src/cppo_lexer.mll"
      ( e.line_start <- false;
        TEXT (loc lexbuf, false, lexeme lexbuf) )
# 4224 "src/cppo_lexer.ml"

  | 14 ->
# 380 "src/cppo_lexer.mll"
      ( TEXT (loc lexbuf, true, lexeme lexbuf) )
# 4229 "src/cppo_lexer.ml"

  | 15 ->
let
# 382 "src/cppo_lexer.mll"
                        nl
# 4235 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) lexbuf.Lexing.lex_curr_pos in
# 384 "src/cppo_lexer.mll"
      (
        new_line e;
        if e.in_directive then
          TEXT (loc lexbuf, true, nl)
        else
          TEXT (loc lexbuf, false, lexeme lexbuf)
      )
# 4245 "src/cppo_lexer.ml"

  | 16 ->
# 393 "src/cppo_lexer.mll"
      (
        new_line e;
        if e.in_directive then (
          e.in_directive <- false;
          ENDEF (loc lexbuf)
        )
        else
          TEXT (loc lexbuf, true, lexeme lexbuf)
      )
# 4258 "src/cppo_lexer.ml"

  | 17 ->
# 404 "src/cppo_lexer.mll"
      ( clear e;
        add e "(*";
        e.token_start <- pos1 lexbuf;
        comment (loc lexbuf) e 1 lexbuf )
# 4266 "src/cppo_lexer.ml"

  | 18 ->
# 410 "src/cppo_lexer.mll"
      ( clear e;
        add e "\"";
        e.token_start <- pos1 lexbuf;
        string e lexbuf;
        e.line_start <- false;
        TEXT (long_loc e, false, get e) )
# 4276 "src/cppo_lexer.ml"

  | 19 ->
# 419 "src/cppo_lexer.mll"
      ( if e.preserve_quotations then (
          clear e;
          add e (lexeme lexbuf);
          e.token_start <- pos1 lexbuf;
          quotation e lexbuf;
          e.line_start <- false;
          TEXT (long_loc e, false, get e)
        )
        else (
          e.line_start <- false;
          TEXT (loc lexbuf, false, lexeme lexbuf)
        )
      )
# 4293 "src/cppo_lexer.ml"

  | 20 ->
# 441 "src/cppo_lexer.mll"
      ( e.line_start <- false;
        TEXT (loc lexbuf, false, lexeme lexbuf) )
# 4299 "src/cppo_lexer.ml"

  | 21 ->
# 445 "src/cppo_lexer.mll"
      ( TEXT (loc lexbuf, true, lexeme lexbuf) )
# 4304 "src/cppo_lexer.ml"

  | 22 ->
# 448 "src/cppo_lexer.mll"
      ( e.line_start <- false;
        TEXT (loc lexbuf, false, lexeme lexbuf) )
# 4310 "src/cppo_lexer.ml"

  | 23 ->
# 452 "src/cppo_lexer.mll"
      ( EOF )
# 4315 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_ocaml_token_rec e lexbuf __ocaml_lex_state

and comment startloc e depth lexbuf =
    __ocaml_lex_comment_rec startloc e depth lexbuf 214
and __ocaml_lex_comment_rec startloc e depth lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 457 "src/cppo_lexer.mll"
      ( add e "(*";
        comment startloc e (depth + 1) lexbuf )
# 4328 "src/cppo_lexer.ml"

  | 1 ->
# 461 "src/cppo_lexer.mll"
      ( let depth = depth - 1 in
        add e "*)";
        if depth > 0 then
          comment startloc e depth lexbuf
        else (
          e.line_start <- false;
          TEXT (long_loc e, false, get e)
        )
      )
# 4341 "src/cppo_lexer.ml"

  | 2 ->
# 471 "src/cppo_lexer.mll"
      ( add_char e '"';
        string e lexbuf;
        comment startloc e depth lexbuf )
# 4348 "src/cppo_lexer.ml"

  | 3 ->
# 477 "src/cppo_lexer.mll"
      ( new_line e;
        add e (lexeme lexbuf);
        comment startloc e depth lexbuf )
# 4355 "src/cppo_lexer.ml"

  | 4 ->
# 483 "src/cppo_lexer.mll"
      ( add e (lexeme lexbuf);
        comment startloc e depth lexbuf )
# 4361 "src/cppo_lexer.ml"

  | 5 ->
# 487 "src/cppo_lexer.mll"
      (
        new_line e;
        add e (lexeme lexbuf);
        comment startloc e depth lexbuf
      )
# 4370 "src/cppo_lexer.ml"

  | 6 ->
# 494 "src/cppo_lexer.mll"
      (
        add e (lexeme lexbuf);
        comment startloc e depth lexbuf
      )
# 4378 "src/cppo_lexer.ml"

  | 7 ->
# 500 "src/cppo_lexer.mll"
      ( add e (lexeme lexbuf);
        comment startloc e depth lexbuf )
# 4384 "src/cppo_lexer.ml"

  | 8 ->
# 504 "src/cppo_lexer.mll"
      ( error startloc "Unterminated comment reaching the end of file" )
# 4389 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_comment_rec startloc e depth lexbuf __ocaml_lex_state

and string e lexbuf =
    __ocaml_lex_string_rec e lexbuf 236
and __ocaml_lex_string_rec e lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 509 "src/cppo_lexer.mll"
      ( add_char e '"' )
# 4401 "src/cppo_lexer.ml"

  | 1 ->
# 513 "src/cppo_lexer.mll"
      ( add e (lexeme lexbuf);
        string e lexbuf )
# 4407 "src/cppo_lexer.ml"

  | 2 ->
# 517 "src/cppo_lexer.mll"
      (
        add e (lexeme lexbuf);
        new_line e;
        string e lexbuf
      )
# 4416 "src/cppo_lexer.ml"

  | 3 ->
# 524 "src/cppo_lexer.mll"
      (
        if e.in_directive then
          lexer_error lexbuf "Unterminated string literal"
        else (
          add e (lexeme lexbuf);
          new_line e;
          string e lexbuf
        )
      )
# 4429 "src/cppo_lexer.ml"

  | 4 ->
let
# 534 "src/cppo_lexer.mll"
         c
# 4435 "src/cppo_lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 535 "src/cppo_lexer.mll"
      ( add_char e c;
        string e lexbuf )
# 4440 "src/cppo_lexer.ml"

  | 5 ->
# 539 "src/cppo_lexer.mll"
      ( )
# 4445 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_string_rec e lexbuf __ocaml_lex_state

and eval_string e lexbuf =
    __ocaml_lex_eval_string_rec e lexbuf 246
and __ocaml_lex_eval_string_rec e lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 544 "src/cppo_lexer.mll"
      (  )
# 4457 "src/cppo_lexer.ml"

  | 1 ->
let
# 546 "src/cppo_lexer.mll"
                              c
# 4463 "src/cppo_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
# 547 "src/cppo_lexer.mll"
      ( add_char e c;
        eval_string e lexbuf )
# 4468 "src/cppo_lexer.ml"

  | 2 ->
# 551 "src/cppo_lexer.mll"
      ( assert e.in_directive;
        eval_string e lexbuf )
# 4474 "src/cppo_lexer.ml"

  | 3 ->
# 555 "src/cppo_lexer.mll"
      ( assert e.in_directive;
        lexer_error lexbuf "Unterminated string literal" )
# 4480 "src/cppo_lexer.ml"

  | 4 ->
let
# 558 "src/cppo_lexer.mll"
                               s
# 4486 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) (lexbuf.Lexing.lex_start_pos + 4) in
# 559 "src/cppo_lexer.mll"
      ( add_char e (Char.chr (int_of_string s));
        eval_string e lexbuf )
# 4491 "src/cppo_lexer.ml"

  | 5 ->
let
# 562 "src/cppo_lexer.mll"
                     c1
# 4497 "src/cppo_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2)
and
# 562 "src/cppo_lexer.mll"
                                 c2
# 4502 "src/cppo_lexer.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 3) in
# 563 "src/cppo_lexer.mll"
      ( add_char e (read_hex2 c1 c2);
        eval_string e lexbuf )
# 4507 "src/cppo_lexer.ml"

  | 6 ->
# 567 "src/cppo_lexer.mll"
      ( add_char e '\b';
        eval_string e lexbuf )
# 4513 "src/cppo_lexer.ml"

  | 7 ->
# 571 "src/cppo_lexer.mll"
      ( add_char e '\n';
        eval_string e lexbuf )
# 4519 "src/cppo_lexer.ml"

  | 8 ->
# 575 "src/cppo_lexer.mll"
      ( add_char e '\r';
        eval_string e lexbuf )
# 4525 "src/cppo_lexer.ml"

  | 9 ->
# 579 "src/cppo_lexer.mll"
      ( add_char e '\t';
        eval_string e lexbuf )
# 4531 "src/cppo_lexer.ml"

  | 10 ->
# 583 "src/cppo_lexer.mll"
      ( add e (lexeme lexbuf);
        eval_string e lexbuf )
# 4537 "src/cppo_lexer.ml"

  | 11 ->
# 587 "src/cppo_lexer.mll"
      ( lexer_error lexbuf "Unterminated string literal" )
# 4542 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_eval_string_rec e lexbuf __ocaml_lex_state

and quotation e lexbuf =
    __ocaml_lex_quotation_rec e lexbuf 266
and __ocaml_lex_quotation_rec e lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 592 "src/cppo_lexer.mll"
      ( add e ">>" )
# 4554 "src/cppo_lexer.ml"

  | 1 ->
# 595 "src/cppo_lexer.mll"
      ( add e "\\>>";
        quotation e lexbuf )
# 4560 "src/cppo_lexer.ml"

  | 2 ->
# 599 "src/cppo_lexer.mll"
      (
        if e.in_directive then (
          new_line e;
          quotation e lexbuf
        )
        else (
          add e (lexeme lexbuf);
          new_line e;
          quotation e lexbuf
        )
      )
# 4575 "src/cppo_lexer.ml"

  | 3 ->
# 612 "src/cppo_lexer.mll"
      (
        if e.in_directive then
          lexer_error lexbuf "Unterminated quotation"
        else (
          add e (lexeme lexbuf);
          new_line e;
          quotation e lexbuf
        )
      )
# 4588 "src/cppo_lexer.ml"

  | 4 ->
# 623 "src/cppo_lexer.mll"
      ( add e (lexeme lexbuf);
        quotation e lexbuf )
# 4594 "src/cppo_lexer.ml"

  | 5 ->
# 627 "src/cppo_lexer.mll"
      ( lexer_error lexbuf "Unterminated quotation" )
# 4599 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_quotation_rec e lexbuf __ocaml_lex_state

and test_token e lexbuf =
    __ocaml_lex_test_token_rec e lexbuf 278
and __ocaml_lex_test_token_rec e lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 630 "src/cppo_lexer.mll"
              ( TRUE )
# 4611 "src/cppo_lexer.ml"

  | 1 ->
# 631 "src/cppo_lexer.mll"
              ( FALSE )
# 4616 "src/cppo_lexer.ml"

  | 2 ->
# 632 "src/cppo_lexer.mll"
              ( DEFINED )
# 4621 "src/cppo_lexer.ml"

  | 3 ->
# 633 "src/cppo_lexer.mll"
              ( OP_PAREN (loc lexbuf) )
# 4626 "src/cppo_lexer.ml"

  | 4 ->
# 634 "src/cppo_lexer.mll"
              ( CL_PAREN (loc lexbuf) )
# 4631 "src/cppo_lexer.ml"

  | 5 ->
# 635 "src/cppo_lexer.mll"
              ( AND )
# 4636 "src/cppo_lexer.ml"

  | 6 ->
# 636 "src/cppo_lexer.mll"
              ( OR )
# 4641 "src/cppo_lexer.ml"

  | 7 ->
# 637 "src/cppo_lexer.mll"
              ( NOT )
# 4646 "src/cppo_lexer.ml"

  | 8 ->
# 638 "src/cppo_lexer.mll"
              ( EQ )
# 4651 "src/cppo_lexer.ml"

  | 9 ->
# 639 "src/cppo_lexer.mll"
              ( LT )
# 4656 "src/cppo_lexer.ml"

  | 10 ->
# 640 "src/cppo_lexer.mll"
              ( GT )
# 4661 "src/cppo_lexer.ml"

  | 11 ->
# 641 "src/cppo_lexer.mll"
              ( NE )
# 4666 "src/cppo_lexer.ml"

  | 12 ->
# 642 "src/cppo_lexer.mll"
              ( LE )
# 4671 "src/cppo_lexer.ml"

  | 13 ->
# 643 "src/cppo_lexer.mll"
              ( GE )
# 4676 "src/cppo_lexer.ml"

  | 14 ->
# 649 "src/cppo_lexer.mll"
      ( let s = Lexing.lexeme lexbuf in
        try INT (Int64.of_string s)
        with _ ->
          error (loc lexbuf)
            (sprintf "Integer constant %s is out the valid range for int64" s)
      )
# 4686 "src/cppo_lexer.ml"

  | 15 ->
# 656 "src/cppo_lexer.mll"
              ( PLUS )
# 4691 "src/cppo_lexer.ml"

  | 16 ->
# 657 "src/cppo_lexer.mll"
              ( MINUS )
# 4696 "src/cppo_lexer.ml"

  | 17 ->
# 658 "src/cppo_lexer.mll"
              ( STAR )
# 4701 "src/cppo_lexer.ml"

  | 18 ->
# 659 "src/cppo_lexer.mll"
              ( SLASH (loc lexbuf) )
# 4706 "src/cppo_lexer.ml"

  | 19 ->
# 660 "src/cppo_lexer.mll"
              ( MOD (loc lexbuf) )
# 4711 "src/cppo_lexer.ml"

  | 20 ->
# 661 "src/cppo_lexer.mll"
              ( LSL )
# 4716 "src/cppo_lexer.ml"

  | 21 ->
# 662 "src/cppo_lexer.mll"
              ( LSR )
# 4721 "src/cppo_lexer.ml"

  | 22 ->
# 663 "src/cppo_lexer.mll"
              ( ASR )
# 4726 "src/cppo_lexer.ml"

  | 23 ->
# 664 "src/cppo_lexer.mll"
              ( LAND )
# 4731 "src/cppo_lexer.ml"

  | 24 ->
# 665 "src/cppo_lexer.mll"
              ( LOR )
# 4736 "src/cppo_lexer.ml"

  | 25 ->
# 666 "src/cppo_lexer.mll"
              ( LXOR )
# 4741 "src/cppo_lexer.ml"

  | 26 ->
# 667 "src/cppo_lexer.mll"
              ( LNOT )
# 4746 "src/cppo_lexer.ml"

  | 27 ->
# 669 "src/cppo_lexer.mll"
              ( COMMA (loc lexbuf) )
# 4751 "src/cppo_lexer.ml"

  | 28 ->
# 672 "src/cppo_lexer.mll"
      ( IDENT (loc lexbuf, lexeme lexbuf) )
# 4756 "src/cppo_lexer.ml"

  | 29 ->
# 674 "src/cppo_lexer.mll"
                             ( test_token e lexbuf )
# 4761 "src/cppo_lexer.ml"

  | 30 ->
# 675 "src/cppo_lexer.mll"
                             ( new_line e;
                               test_token e lexbuf )
# 4767 "src/cppo_lexer.ml"

  | 31 ->
# 678 "src/cppo_lexer.mll"
               ( assert e.in_directive;
                 e.in_directive <- false;
                 new_line e;
                 e.lexer <- `Ocaml;
                 ENDTEST (loc lexbuf) )
# 4776 "src/cppo_lexer.ml"

  | 32 ->
# 683 "src/cppo_lexer.mll"
               ( error (loc lexbuf)
                   (sprintf "Invalid token %s" (Lexing.lexeme lexbuf)) )
# 4782 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_test_token_rec e lexbuf __ocaml_lex_state

and int_tuple lexbuf =
  lexbuf.Lexing.lex_mem <- Array.make 4 (-1) ; (* L=1 [2] <- p ;  *)
  lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
  __ocaml_lex_int_tuple_rec lexbuf 353
and __ocaml_lex_int_tuple_rec lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 689 "src/cppo_lexer.mll"
                               s
# 4797 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1) in
# 690 "src/cppo_lexer.mll"
                      ( [Int64.of_string s] )
# 4801 "src/cppo_lexer.ml"

  | 1 ->
# 692 "src/cppo_lexer.mll"
                      ( int_tuple_content lexbuf )
# 4806 "src/cppo_lexer.ml"

  | 2 ->
# 694 "src/cppo_lexer.mll"
                      ( failwith "Not an int nor a tuple" )
# 4811 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_int_tuple_rec lexbuf __ocaml_lex_state

and int_tuple_content lexbuf =
  lexbuf.Lexing.lex_mem <- Array.make 6 (-1) ; (* L=2 [3] <- p ; [2] <- p ;  *)
  lexbuf.Lexing.lex_mem.(3) <- lexbuf.Lexing.lex_curr_pos ;
  lexbuf.Lexing.lex_mem.(2) <- lexbuf.Lexing.lex_curr_pos ;
  __ocaml_lex_int_tuple_content_rec lexbuf 362
and __ocaml_lex_int_tuple_content_rec lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 697 "src/cppo_lexer.mll"
                                   s
# 4827 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1) in
# 698 "src/cppo_lexer.mll"
                      ( let x = Int64.of_string s in
                        x :: int_tuple_content lexbuf )
# 4832 "src/cppo_lexer.ml"

  | 1 ->
let
# 701 "src/cppo_lexer.mll"
                                   s
# 4838 "src/cppo_lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1) in
# 702 "src/cppo_lexer.mll"
                      ( [Int64.of_string s] )
# 4842 "src/cppo_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_int_tuple_content_rec lexbuf __ocaml_lex_state

;;

# 705 "src/cppo_lexer.mll"
 
  let init ~preserve_quotations file lexbuf =
    new_file lexbuf file;
    {
      preserve_quotations = preserve_quotations;
      lexer = `Ocaml;
      line_start = true;
      in_directive = false;
      buf = Buffer.create 200;
      token_start = Lexing.dummy_pos;
      lexbuf = lexbuf;
    }

  let int_tuple_of_string s =
    try Some (int_tuple (Lexing.from_string s))
    with _ -> None

# 4867 "src/cppo_lexer.ml"

end
module Cppo_eval : sig 
#1 "cppo_eval.mli"
(** The type signatures in this module are not yet for public consumption.

    Please don't rely on them in any way.*)

module S : Set.S with type elt = string
module M : Map.S with type key = string

val builtin_env
  : [> `Defun of
         Cppo_types.loc * string * string list *
         [> `Capitalize of Cppo_types.node
         | `Concat of (Cppo_types.node * Cppo_types.node)
         | `Stringify of Cppo_types.node ] list * 'a
    | `Special ] M.t as 'a

val include_inputs
  : extensions:(string, Cppo_command.command_template) Hashtbl.t
  -> preserve_quotations:bool
  -> incdirs:string list
  -> show_exact_locations:bool
  -> show_no_locations:bool
  -> Buffer.t
  -> (([< `Def of Cppo_types.loc * string * Cppo_types.node list * 'a
       | `Defun of Cppo_types.loc * string * string list * Cppo_types.node list * 'a
       | `Special
            > `Def `Defun ]
       as 'b)
        M.t as 'a)
  -> (string * string * (unit -> Lexing.lexbuf) * (unit -> unit)) list -> 'a

end = struct
#1 "cppo_eval.ml"
open Printf

open Cppo_types

module S = Set.Make (String)
module M = Map.Make (String)

let builtins = [
  "__FILE__", (fun _env -> `Special);
  "__LINE__", (fun _env -> `Special);
  "STRINGIFY", (fun env ->
                  `Defun (dummy_loc, "STRINGIFY",
                          ["x"],
                          [`Stringify (`Ident (dummy_loc, "x", None))],
                          env)
               );
  "CONCAT", (fun env ->
               `Defun (dummy_loc, "CONCAT",
                       ["x";"y"],
                       [`Concat (`Ident (dummy_loc, "x", None),
                                 `Ident (dummy_loc, "y", None))],
                       env)
            );
  "CAPITALIZE", (fun env ->
    `Defun (dummy_loc, "CAPITALIZE",
            ["x"],
            [`Capitalize (`Ident (dummy_loc, "x", None))],
            env)
  );

]

let is_reserved s =
  List.exists (fun (s', _) -> s = s') builtins

let builtin_env =
  List.fold_left (fun env (s, f) -> M.add s (f env) env) M.empty builtins

let line_directive buf pos =
  let len = Buffer.length buf in
  if len > 0 && Buffer.nth buf (len - 1) <> '\n' then
    Buffer.add_char buf '\n';
  bprintf buf "# %i %S\n"
    pos.Lexing.pos_lnum
    pos.Lexing.pos_fname;
  bprintf buf "%s" (String.make (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) ' ')

let rec add_sep sep last = function
    [] -> [ last ]
  | [x] -> [ x; last ]
  | x :: l -> x :: sep :: add_sep sep last l


let remove_space l =
  List.filter (function `Text (_, true, _) -> false | _ -> true) l

let trim_and_compact buf s =
  let started = ref false in
  let need_space = ref false in
  for i = 0 to String.length s - 1 do
    match s.[i] with
        ' ' | '\t' | '\n' | '\r' ->
          if !started then
            need_space := true
      | c ->
          if !need_space then
            Buffer.add_char buf ' ';
          (match c with
               '\"' -> Buffer.add_string buf "\\\""
             | '\\' -> Buffer.add_string buf "\\\\"
             | c -> Buffer.add_char buf c);
          started := true;
          need_space := false
  done

let stringify buf s =
  Buffer.add_char buf '\"';
  trim_and_compact buf s;
  Buffer.add_char buf '\"'

let trim_and_compact_string s =
  let buf = Buffer.create (String.length s) in
  trim_and_compact buf s;
  Buffer.contents buf
let trim_compact_and_capitalize_string s =
  let buf = Buffer.create (String.length s) in
  trim_and_compact buf s;
  String.capitalize (Buffer.contents buf)

let is_ident s =
  let len = String.length s in
  len > 0
  &&
    (match s.[0] with
         'A'..'Z' | 'a'..'z' -> true
       | '_' when len > 1 -> true
       | _ -> false)
  &&
    (try
       for i = 1 to len - 1 do
         match s.[i] with
             'A'..'Z' | 'a'..'z' | '_' | '0'..'9' -> ()
           | _ -> raise Exit
       done;
       true
     with Exit ->
       false)

let concat loc x y =
  let s = trim_and_compact_string x ^ trim_and_compact_string y in
  if not (s = "" || is_ident s) then
    error loc
      (sprintf "CONCAT() does not expand into a valid identifier nor \
                into whitespace:\n%S" s)
  else
    if s = "" then " "
    else " " ^ s ^ " "

(*
   Expand the contents of a variable used in a boolean expression.

   Ideally, we should first completely expand the contents bound
   to the variable, and then parse the result as an int or an int tuple.
   This is a bit complicated to do well, and we don't want to implement
   a full programming language here either.

   Instead we only accept int literals, int tuple literals, and variables that
   themselves expand into one those.

   In particular:
   - We do not support arithmetic operations
   - We do not support tuples containing variables such as (x, y)

   Example of contents that we support:
   - 123
   - (1, 2, 3)
   - x, where x expands into 123.
*)
let rec eval_ident env loc name =
  let l =
    try
      match M.find name env with
      | `Def (_, _, l, _) -> l
      | `Defun _ ->
          error loc (sprintf "%S expects arguments" name)
      | `Special -> assert false
    with Not_found -> error loc (sprintf "Undefined identifier %S" name)
  in
  let expansion_error () =
    error loc
      (sprintf "\
Variable %s found in cppo boolean expression must expand
into an int literal, into a tuple of int literals,
or into a variable with the same properties."
         name)
  in
  (try
     match remove_space l with
       [ `Ident (loc, name, None) ] ->
         (* single identifier that we expand recursively *)
         eval_ident env loc name
     | _ ->
         (* int literal or int tuple literal; variables not allowed *)
         let text =
           List.map (
             function
               `Text (_, _is_space, s) -> s
             | _ ->
                 expansion_error ()
           ) (Cppo_types.flatten_nodes l)
         in
         let s = String.concat "" text in
         (match Cppo_lexer.int_tuple_of_string s with
            Some [i] -> `Int i
          | Some l -> `Tuple (loc, List.map (fun i -> `Int i) l)
          | None ->
              expansion_error ()
         )
   with Cppo_error _ ->
     expansion_error ()
  )

let rec replace_idents env (x : arith_expr) : arith_expr =
  match x with
    | `Ident (loc, name) -> eval_ident env loc name

    | `Int x -> `Int x
    | `Neg x -> `Neg (replace_idents env x)
    | `Add (a, b) -> `Add (replace_idents env a, replace_idents env b)
    | `Sub (a, b) -> `Sub (replace_idents env a, replace_idents env b)
    | `Mul (a, b) -> `Mul (replace_idents env a, replace_idents env b)
    | `Div (loc, a, b) -> `Div (loc, replace_idents env a, replace_idents env b)
    | `Mod (loc, a, b) -> `Mod (loc, replace_idents env a, replace_idents env b)
    | `Lnot a -> `Lnot (replace_idents env a)
    | `Lsl (a, b) -> `Lsl (replace_idents env a, replace_idents env b)
    | `Lsr (a, b) -> `Lsr (replace_idents env a, replace_idents env b)
    | `Asr (a, b) -> `Asr (replace_idents env a, replace_idents env b)
    | `Land (a, b) -> `Land (replace_idents env a, replace_idents env b)
    | `Lor (a, b) -> `Lor (replace_idents env a, replace_idents env b)
    | `Lxor (a, b) -> `Lxor (replace_idents env a, replace_idents env b)
    | `Tuple (loc, l) -> `Tuple (loc, List.map (replace_idents env) l)

let rec eval_int env (x : arith_expr) : int64 =
  match x with
    | `Ident (loc, name) -> eval_int env (eval_ident env loc name)

    | `Int x -> x
    | `Neg x -> Int64.neg (eval_int env x)
    | `Add (a, b) -> Int64.add (eval_int env a) (eval_int env b)
    | `Sub (a, b) -> Int64.sub (eval_int env a) (eval_int env b)
    | `Mul (a, b) -> Int64.mul (eval_int env a) (eval_int env b)
    | `Div (loc, a, b) ->
        (try Int64.div (eval_int env a) (eval_int env b)
         with Division_by_zero ->
           error loc "Division by zero")

    | `Mod (loc, a, b) ->
        (try Int64.rem (eval_int env a) (eval_int env b)
         with Division_by_zero ->
           error loc "Division by zero")

    | `Lnot a -> Int64.lognot (eval_int env a)

    | `Lsl (a, b) ->
        let n = eval_int env a in
        let shift = eval_int env b in
        let shift =
          if shift >= 64L then 64L
          else if shift <= -64L then -64L
          else shift
        in
        Int64.shift_left n (Int64.to_int shift)

    | `Lsr (a, b) ->
        let n = eval_int env a in
        let shift = eval_int env b in
        let shift =
          if shift >= 64L then 64L
          else if shift <= -64L then -64L
          else shift
        in
        Int64.shift_right_logical n (Int64.to_int shift)

    | `Asr (a, b) ->
        let n = eval_int env a in
        let shift = eval_int env b in
        let shift =
          if shift >= 64L then 64L
          else if shift <= -64L then -64L
          else shift
        in
        Int64.shift_right n (Int64.to_int shift)

    | `Land (a, b) -> Int64.logand (eval_int env a) (eval_int env b)
    | `Lor (a, b) -> Int64.logor (eval_int env a) (eval_int env b)
    | `Lxor (a, b) -> Int64.logxor (eval_int env a) (eval_int env b)
    | `Tuple (loc, l) ->
        assert (List.length l <> 1);
        error loc "Operation not supported on tuples"

let rec compare_lists al bl =
  match al, bl with
  | a :: al, b :: bl ->
      let c = Int64.compare a b in
      if c <> 0 then c
      else compare_lists al bl
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1

let compare_tuples env (a : arith_expr) (b : arith_expr) =
  (* We replace the identifiers first to get a better error message
     on such input:

       #define x (1, 2)
       #if x >= (1, 2)

     since variables must represent a single int, not a tuple.
  *)
  let a = replace_idents env a in
  let b = replace_idents env b in
  match a, b with
  | `Tuple (_, al), `Tuple (_, bl) when List.length al = List.length bl ->
      let eval_list l = List.map (eval_int env) l in
      compare_lists (eval_list al) (eval_list bl)

  | `Tuple (_loc1, al), `Tuple (loc2, bl) ->
      error loc2
        (sprintf "Tuple of length %i cannot be compared to a tuple of length %i"
           (List.length bl) (List.length al)
        )

  | `Tuple (loc, _), _
  | _, `Tuple (loc, _) ->
      error loc "Tuple cannot be compared to an int"

  | a, b ->
      Int64.compare (eval_int env a) (eval_int env b)

let rec eval_bool env (x : bool_expr) =
  match x with
      `True -> true
    | `False -> false
    | `Defined s -> M.mem s env
    | `Not x -> not (eval_bool env x)
    | `And (a, b) -> eval_bool env a && eval_bool env b
    | `Or (a, b) -> eval_bool env a || eval_bool env b
    | `Eq (a, b) -> compare_tuples env a b = 0
    | `Lt (a, b) -> compare_tuples env a b < 0
    | `Gt (a, b) -> compare_tuples env a b > 0


type globals = {
  call_loc : Cppo_types.loc;
    (* location used to set the value of
       __FILE__ and __LINE__ global variables *)

  mutable buf : Buffer.t;
    (* buffer where the output is written *)

  included : S.t;
    (* set of already-included files *)

  require_location : bool ref;
    (* whether a line directive should be printed before outputting the next
       token *)

  show_exact_locations : bool;
    (* whether line directives should be printed even for expanded macro
       bodies *)

  enable_loc : bool ref;
    (* whether line directives should be printed *)

  g_preserve_quotations : bool;
    (* identify and preserve camlp4 quotations *)

  incdirs : string list;
    (* directories for finding included files *)

  current_directory : string;
    (* directory containing the current file *)

  extensions : (string, Cppo_command.command_template) Hashtbl.t;
    (* mapping from extension ID to pipeline command *)
}



let parse ~preserve_quotations file lexbuf =
  let lexer_env = Cppo_lexer.init ~preserve_quotations file lexbuf in
  try
    Cppo_parser.main (Cppo_lexer.line lexer_env) lexbuf
  with
      Parsing.Parse_error ->
        error (Cppo_lexer.loc lexbuf) "syntax error"
    | Cppo_types.Cppo_error _ as e ->
        raise e
    | e ->
        error (Cppo_lexer.loc lexbuf) (Printexc.to_string e)

let plural n =
  if abs n <= 1 then ""
  else "s"


let maybe_print_location g pos =
  if !(g.enable_loc) then
    if !(g.require_location) then (
      line_directive g.buf pos
    )

let expand_ext g loc id data =
  let cmd_tpl =
    try Hashtbl.find g.extensions id
    with Not_found ->
      error loc (sprintf "Undefined extension %s" id)
  in
  let p1, p2 = loc in
  let file = p1.Lexing.pos_fname in
  let first = p1.Lexing.pos_lnum in
  let last = p2.Lexing.pos_lnum in
  let cmd = Cppo_command.subst cmd_tpl file first last in
  Unix.putenv "CPPO_FILE" file;
  Unix.putenv "CPPO_FIRST_LINE" (string_of_int first);
  Unix.putenv "CPPO_LAST_LINE" (string_of_int last);
  let (ic, oc) as p = Unix.open_process cmd in
  output_string oc data;
  close_out oc;
  (try
     while true do
       bprintf g.buf "%s\n" (input_line ic)
     done
   with End_of_file -> ()
  );
  match Unix.close_process p with
      Unix.WEXITED 0 -> ()
    | Unix.WEXITED n ->
        failwith (sprintf "Command %S exited with status %i" cmd n)
    | _ ->
        failwith (sprintf "Command %S failed" cmd)

let rec include_file g loc rel_file env =
  let file =
    if not (Filename.is_relative rel_file) then
      if Sys.file_exists rel_file then
        rel_file
      else
        error loc (sprintf "Included file %S does not exist" rel_file)
    else
      try
        let dir =
          List.find (
            fun dir ->
              let file = Filename.concat dir rel_file in
              Sys.file_exists file
          ) (g.current_directory :: g.incdirs)
        in
        if dir = Filename.current_dir_name then
          rel_file
        else
          Filename.concat dir rel_file
      with Not_found ->
        error loc (sprintf "Cannot find included file %S" rel_file)
  in
  if S.mem file g.included then
    failwith (sprintf "Cyclic inclusion of file %S" file)
  else
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    let l = parse ~preserve_quotations:g.g_preserve_quotations file lexbuf in
    close_in ic;
    expand_list { g with
                    included = S.add file g.included;
                    current_directory = Filename.dirname file
                } env l

and expand_list ?(top = false) g env l =
  List.fold_left (expand_node ~top g) env l

and expand_node ?(top = false) g env0 (x : node) =
  match x with
      `Ident (loc, name, opt_args) ->

        let def =
          try Some (M.find name env0)
          with Not_found -> None
        in
        let g =
          if top && def <> None || g.call_loc == dummy_loc then
            { g with call_loc = loc }
          else g
        in

        let enable_loc0 = !(g.enable_loc) in

        if def <> None then (
          g.require_location := true;

          if not g.show_exact_locations then (
            (* error reports will point more or less to the point
               where the code is included rather than the source location
               of the macro definition *)
            maybe_print_location g (fst loc);
            g.enable_loc := false
          )
        );

        let env =
          match def, opt_args with
              None, None ->
                expand_node g env0 (`Text (loc, false, name))
            | None, Some args ->
                let with_sep =
                  add_sep
                    [`Text (loc, false, ",")]
                    [`Text (loc, false, ")")]
                    args in
                let l =
                  `Text (loc, false, name ^ "(") :: List.flatten with_sep in
                expand_list g env0 l

            | Some (`Defun (_, _, arg_names, _, _)), None ->
                error loc
                  (sprintf "%S expects %i arguments but is applied to none."
                     name (List.length arg_names))

            | Some (`Def _), Some _ ->
                error loc
                  (sprintf "%S expects no arguments" name)

            | Some (`Def (_, _, l, env)), None ->
                ignore (expand_list g env l);
                env0

            | Some (`Defun (_, _, arg_names, l, env)), Some args ->
                let argc = List.length arg_names in
                let n = List.length args in
                let args =
                  (* it's ok to pass an empty arg if one arg
                     is expected *)
                  if n = 0 && argc = 1 then [[]]
                  else args
                in
                if argc <> n then
                  error loc
                    (sprintf "%S expects %i argument%s but is applied to \
                              %i argument%s."
                       name argc (plural argc) n (plural n))
                else
                  let app_env =
                    List.fold_left2 (
                      fun env name l ->
                        M.add name (`Def (loc, name, l, env0)) env
                    ) env arg_names args
                  in
                  ignore (expand_list g app_env l);
                  env0

            | Some `Special, _ -> assert false
        in

        if def = None then
          g.require_location := false
        else
          g.require_location := true;

        (* restore initial setting *)
        g.enable_loc := enable_loc0;

        env


    | `Def (loc, name, body)->
        g.require_location := true;
        if M.mem name env0 then
          error loc (sprintf "%S is already defined" name)
        else
          M.add name (`Def (loc, name, body, env0)) env0

    | `Defun (loc, name, arg_names, body) ->
        g.require_location := true;
        if M.mem name env0 then
          error loc (sprintf "%S is already defined" name)
        else
          M.add name (`Defun (loc, name, arg_names, body, env0)) env0

    | `Undef (loc, name) ->
        g.require_location := true;
        if is_reserved name then
          error loc
            (sprintf "%S is a built-in variable that cannot be undefined" name)
        else
          M.remove name env0

    | `Include (loc, file) ->
        g.require_location := true;
        let env = include_file g loc file env0 in
        g.require_location := true;
        env

    | `Ext (loc, id, data) ->
        g.require_location := true;
        expand_ext g loc id data;
        g.require_location := true;
        env0

    | `Cond (_loc, test, if_true, if_false) ->
        let l =
          if eval_bool env0 test then if_true
          else if_false
        in
        g.require_location := true;
        let env = expand_list g env0 l in
        g.require_location := true;
        env

    | `Error (loc, msg) ->
        error loc msg

    | `Warning (loc, msg) ->
        warning loc msg;
        env0

    | `Text (loc, is_space, s) ->
        if not is_space then (
          maybe_print_location g (fst loc);
          g.require_location := false
        );
        Buffer.add_string g.buf s;
        env0

    | `Seq l ->
        expand_list g env0 l

    | `Stringify x ->
        let enable_loc0 = !(g.enable_loc) in
        g.enable_loc := false;
        let buf0 = g.buf in
        let local_buf = Buffer.create 100 in
        g.buf <- local_buf;
        ignore (expand_node g env0 x);
        stringify buf0 (Buffer.contents local_buf);
        g.buf <- buf0;
        g.enable_loc := enable_loc0;
        env0

    | `Capitalize (x : node) ->
        let enable_loc0 = !(g.enable_loc) in
        g.enable_loc := false;
        let buf0 = g.buf in
        let local_buf = Buffer.create 100 in
        g.buf <- local_buf;
        ignore (expand_node g env0 x);
        let xs = Buffer.contents local_buf in
        let s = trim_compact_and_capitalize_string xs in
          (* stringify buf0 (Buffer.contents local_buf); *)
        Buffer.add_string buf0 s ;
        g.buf <- buf0;
        g.enable_loc := enable_loc0;
        env0
    | `Concat (x, y) ->
        let enable_loc0 = !(g.enable_loc) in
        g.enable_loc := false;
        let buf0 = g.buf in
        let local_buf = Buffer.create 100 in
        g.buf <- local_buf;
        ignore (expand_node g env0 x);
        let xs = Buffer.contents local_buf in
        Buffer.clear local_buf;
        ignore (expand_node g env0 y);
        let ys = Buffer.contents local_buf in
        let s = concat g.call_loc xs ys in
        Buffer.add_string buf0 s;
        g.buf <- buf0;
        g.enable_loc := enable_loc0;
        env0

    | `Line (loc, opt_file, n) ->
        (* printing a line directive is not strictly needed *)
        (match opt_file with
             None ->
               maybe_print_location g (fst loc);
               bprintf g.buf "\n# %i\n" n
           | Some file ->
               bprintf g.buf "\n# %i %S\n" n file
        );
        (* printing the location next time is needed because it just changed *)
        g.require_location := true;
        env0

    | `Current_line loc ->
        maybe_print_location g (fst loc);
        g.require_location := true;
        let pos, _ = g.call_loc in
        bprintf g.buf " %i " pos.Lexing.pos_lnum;
        env0

    | `Current_file loc ->
        maybe_print_location g (fst loc);
        g.require_location := true;
        let pos, _ = g.call_loc in
        bprintf g.buf " %S " pos.Lexing.pos_fname;
        env0




let include_inputs
    ~extensions
    ~preserve_quotations
    ~incdirs
    ~show_exact_locations
    ~show_no_locations
    buf env l =

  let enable_loc = not show_no_locations in
  List.fold_left (
    fun env (dir, file, open_, close) ->
      let l = parse ~preserve_quotations file (open_ ()) in
      close ();
      let g = {
        call_loc = dummy_loc;
        buf = buf;
        included = S.empty;
        require_location = ref true;
        show_exact_locations = show_exact_locations;
        enable_loc = ref enable_loc;
        g_preserve_quotations = preserve_quotations;
        incdirs = incdirs;
        current_directory = dir;
        extensions = extensions;
      }
      in
      expand_list ~top:true { g with included = S.add file g.included } env l
  ) env l

end
module Cppo_version : sig 
#1 "cppo_version.mli"
val cppo_version : string

end = struct
#1 "cppo_version.ml"
let cppo_version = "1.6.4"
end
module Cppo_main
= struct
#1 "cppo_main.ml"
open Printf

let add_extension tbl s =
  let i =
    try String.index s ':'
    with Not_found ->
      failwith "Invalid -x argument"
  in
  let id = String.sub s 0 i in
  let raw_tpl = String.sub s (i+1) (String.length s - i - 1) in
  let cmd_tpl = Cppo_command.parse raw_tpl in
  if Hashtbl.mem tbl id then
    failwith ("Multiple definitions for extension " ^ id)
  else
    Hashtbl.add tbl id cmd_tpl

let semver_re = Str.regexp "\
\\([0-9]+\\)\
\\.\\([0-9]+\\)\
\\.\\([0-9]+\\)\
\\(-\\([^+]*\\)\\)?\
\\(\\+\\(.*\\)\\)?\
\r?$"

let parse_semver s =
  if not (Str.string_match semver_re s 0) then
    None
  else
    let major = Str.matched_group 1 s in
    let minor = Str.matched_group 2 s in
    let patch = Str.matched_group 3 s in
    let prerelease = try Some (Str.matched_group 5 s) with Not_found -> None in
    let build = try Some (Str.matched_group 7 s) with Not_found -> None in
    Some (major, minor, patch, prerelease, build)

let define var s =
  [sprintf "#define %s %s\n" var s]

let opt_define var o =
  match o with
  | None -> []
  | Some s -> define var s

let parse_version_spec s =
  let error () =
    failwith (sprintf "Invalid version specification: %S" s)
  in
  let prefix, version_full =
    try
      let len = String.index s ':' in
      String.sub s 0 len, String.sub s (len+1) (String.length s - (len+1))
    with Not_found ->
      error ()
  in
  match parse_semver version_full with
  | None ->
      error ()
  | Some (major, minor, patch, opt_prerelease, opt_build) ->
      let version = sprintf "(%s, %s, %s)" major minor patch in
      let version_string = sprintf "%s.%s.%s" major minor patch in
      List.flatten [
        define (prefix ^ "_MAJOR") major;
        define (prefix ^ "_MINOR") minor;
        define (prefix ^ "_PATCH") patch;
        opt_define (prefix ^ "_PRERELEASE") opt_prerelease;
        opt_define (prefix ^ "_BUILD") opt_build;
        define (prefix ^ "_VERSION") version;
        define (prefix ^ "_VERSION_STRING") version_string;
        define (prefix ^ "_VERSION_FULL") s;
      ]

let main () =
  let extensions = Hashtbl.create 10 in
  let files = ref [] in
  let header = ref [] in
  let incdirs = ref [] in
  let out_file = ref None in
  let preserve_quotations = ref false in
  let show_exact_locations = ref false in
  let show_no_locations = ref false in
  let options = [
    "-D", Arg.String (fun s -> header := ("#define " ^ s ^ "\n") :: !header),
    "DEF
          Equivalent of interpreting '#define DEF' before processing the
          input, e.g. `cppo -D 'VERSION \"1.2.3\"'` (no equal sign)";

    "-U", Arg.String (fun s -> header := ("#undef " ^ s ^ "\n") :: !header),
    "IDENT
          Equivalent of interpreting '#undef IDENT' before processing the
          input";

    "-I", Arg.String (fun s -> incdirs := s :: !incdirs),
    "DIR
          Add directory DIR to the search path for included files";

    "-V", Arg.String (fun s -> header := parse_version_spec s @ !header),
    "VAR:MAJOR.MINOR.PATCH-OPTPRERELEASE+OPTBUILD
          Define the following variables extracted from a version string
          (following the Semantic Versioning syntax http://semver.org/):

            VAR_MAJOR           must be a non-negative int
            VAR_MINOR           must be a non-negative int
            VAR_PATCH           must be a non-negative int
            VAR_PRERELEASE      if the OPTPRERELEASE part exists
            VAR_BUILD           if the OPTBUILD part exists
            VAR_VERSION         is the tuple (MAJOR, MINOR, PATCH)
            VAR_VERSION_STRING  is the string MAJOR.MINOR.PATCH
            VAR_VERSION_FULL    is the original string

          Example: cppo -V OCAML:4.02.1
";

    "-o", Arg.String (fun s -> out_file := Some s),
    "FILE
          Output file";

    "-q", Arg.Set preserve_quotations,
    "
          Identify and preserve camlp4 quotations";

    "-s", Arg.Set show_exact_locations,
    "
          Output line directives pointing to the exact source location of
          each token, including those coming from the body of macro
          definitions.  This behavior is off by default.";

    "-n", Arg.Set show_no_locations,
    "
          Do not output any line directive other than those found in the
          input (overrides -s).";

    "-version", Arg.Unit (fun () ->
                            print_endline Cppo_version.cppo_version;
                            exit 0),
    "
          Print the version of the program and exit.";

    "-x", Arg.String (fun s -> add_extension extensions s),
    "NAME:CMD_TEMPLATE
          Define a custom preprocessor target section starting with:
            #ext \"NAME\"
          and ending with:
            #endext

          NAME must be a lowercase identifier of the form [a-z][A-Za-z0-9_]*

          CMD_TEMPLATE is a command template supporting the following
          special sequences:
            %F  file name (unescaped; beware of potential scripting attacks)
            %B  number of the first line
            %E  number of the last line
            %%  a single percent sign

          Filename, first line number and last line number are also
          available from the following environment variables:
          CPPO_FILE, CPPO_FIRST_LINE, CPPO_LAST_LINE.

          The command produced is expected to read the data lines from stdin
          and to write its output to stdout."
  ]
  in
  let msg = sprintf "\
Usage: %s [OPTIONS] [FILE1 [FILE2 ...]]
Options:" Sys.argv.(0) in
  let add_file s = files := s :: !files in
  Arg.parse options add_file msg;

  let inputs =
    let preliminaries =
      match List.rev !header with
          [] -> []
        | l ->
            let s = String.concat "" l in
            [ Sys.getcwd (),
              "<command line>",
              (fun () -> Lexing.from_string s),
              (fun () -> ()) ]
    in
    let main =
      match List.rev !files with
          [] -> [ Sys.getcwd (),
                  "<stdin>",
                  (fun () -> Lexing.from_channel stdin),
                  (fun () -> ()) ]
        | l ->
            List.map (
              fun file ->
                let ic = lazy (open_in file) in
                Filename.dirname file,
                file,
                (fun () -> Lexing.from_channel (Lazy.force ic)),
                (fun () -> close_in (Lazy.force ic))
            ) l
    in
    preliminaries @ main
  in

  let env = Cppo_eval.builtin_env in
  let buf = Buffer.create 10_000 in
  let _env =
    Cppo_eval.include_inputs
      ~extensions
      ~preserve_quotations: !preserve_quotations
      ~incdirs: (List.rev !incdirs)
      ~show_exact_locations: !show_exact_locations
      ~show_no_locations: !show_no_locations
      buf env inputs
  in
  match !out_file with
      None ->
        print_string (Buffer.contents buf);
        flush stdout
    | Some file ->
        let oc = open_out file in
        output_string oc (Buffer.contents buf);
        close_out oc

let () =
  if not !Sys.interactive then
    try
      main ()
    with
    | Cppo_types.Cppo_error msg
    | Failure msg ->
        eprintf "Error: %s\n%!" msg;
        exit 1

end
