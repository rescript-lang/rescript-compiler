/* A simple parser for C-- */

%{
open Cmm
open Parsecmmaux

let rec make_letdef def body =
  match def with
    [] -> body
  | (id, def) :: rem ->
      unbind_ident id;
      Clet(id, def, make_letdef rem body)

let make_switch n selector caselist =
  let index = Array.make n 0 in
  let casev = Array.of_list caselist in
  let actv = Array.make (Array.length casev) (Cexit(0,[])) in
  for i = 0 to Array.length casev - 1 do
    let (posl, e) = casev.(i) in
    List.iter (fun pos -> index.(pos) <- i) posl;
    actv.(i) <- e
  done;
  Cswitch(selector, index, actv, Debuginfo.none)

let access_array base numelt size =
  match numelt with
    Cconst_int 0 -> base
  | Cconst_int n -> Cop(Cadda, [base; Cconst_int(n * size)], Debuginfo.none)
  | _ -> Cop(Cadda, [base;
                     Cop(Clsl, [numelt; Cconst_int(Misc.log2 size)],
                         Debuginfo.none)],
             Debuginfo.none)

%}

%token ABSF
%token ADDA
%token ADDF
%token ADDI
%token ADDV
%token ADDR
%token ALIGN
%token ALLOC
%token AND
%token APPLY
%token ASR
%token ASSIGN
%token BYTE
%token CASE
%token CATCH
%token CHECKBOUND
%token COLON
%token DATA
%token DIVF
%token DIVI
%token EOF
%token EQA
%token EQF
%token EQI
%token EXIT
%token EXTCALL
%token FLOAT
%token FLOAT32
%token FLOAT64
%token <string> FLOATCONST
%token FLOATOFINT
%token FUNCTION
%token GEA
%token GEF
%token GEI
%token GLOBAL
%token GTA
%token GTF
%token GTI
%token HALF
%token <string> IDENT
%token IF
%token INT
%token INT32
%token <int> INTCONST
%token INTOFFLOAT
%token KSTRING
%token LBRACKET
%token LEA
%token LEF
%token LEI
%token LET
%token LOAD
%token <Location.t> LOCATION
%token LPAREN
%token LSL
%token LSR
%token LTA
%token LTF
%token LTI
%token MODI
%token MULF
%token MULH
%token MULI
%token NEA
%token NEF
%token NEI
%token OR
%token <int> POINTER
%token PROJ
%token <Cmm.raise_kind> RAISE
%token RBRACKET
%token RPAREN
%token SEQ
%token SIGNED
%token SKIP
%token STAR
%token STORE
%token <string> STRING
%token SUBF
%token SUBI
%token SWITCH
%token TRY
%token UNIT
%token UNSIGNED
%token VAL
%token WHILE
%token WITH
%token XOR
%token ADDRAREF
%token INTAREF
%token FLOATAREF
%token ADDRASET
%token INTASET
%token FLOATASET

%start phrase
%type <Cmm.phrase> phrase

%%

phrase:
    fundecl     { Cfunction $1 }
  | datadecl    { Cdata $1 }
  | EOF         { raise End_of_file }
;
fundecl:
    LPAREN FUNCTION fun_name LPAREN params RPAREN sequence RPAREN
      { List.iter (fun (id, ty) -> unbind_ident id) $5;
        {fun_name = $3; fun_args = $5; fun_body = $7; fun_fast = true;
         fun_dbg = debuginfo ()} }
;
fun_name:
    STRING              { $1 }
  | IDENT               { $1 }
params:
    oneparam params     { $1 :: $2 }
  | /**/                { [] }
;
oneparam:
    IDENT COLON machtype { (bind_ident $1, $3) }
;
machtype:
    UNIT                        { [||] }
  | componentlist               { Array.of_list(List.rev $1) }
;
component:
    VAL                         { Val }
  | ADDR                        { Addr }
  | INT                         { Int }
  | FLOAT                       { Float }
;
componentlist:
    component                    { [$1] }
  | componentlist STAR component { $3 :: $1 }
;
expr:
    INTCONST    { Cconst_int $1 }
  | FLOATCONST  { Cconst_float (float_of_string $1) }
  | STRING      { Cconst_symbol $1 }
  | POINTER     { Cconst_pointer $1 }
  | IDENT       { Cvar(find_ident $1) }
  | LBRACKET RBRACKET { Ctuple [] }
  | LPAREN LET letdef sequence RPAREN { make_letdef $3 $4 }
  | LPAREN ASSIGN IDENT expr RPAREN { Cassign(find_ident $3, $4) }
  | LPAREN APPLY location expr exprlist machtype RPAREN
                { Cop(Capply $6, $4 :: List.rev $5, debuginfo ?loc:$3 ()) }
  | LPAREN EXTCALL STRING exprlist machtype RPAREN
               {Cop(Cextcall($3, $5, false, None), List.rev $4, debuginfo ())}
  | LPAREN ALLOC exprlist RPAREN { Cop(Calloc, List.rev $3, debuginfo ()) }
  | LPAREN SUBF expr RPAREN { Cop(Cnegf, [$3], debuginfo ()) }
  | LPAREN SUBF expr expr RPAREN { Cop(Csubf, [$3; $4], debuginfo ()) }
  | LPAREN unaryop expr RPAREN { Cop($2, [$3], debuginfo ()) }
  | LPAREN binaryop expr expr RPAREN { Cop($2, [$3; $4], debuginfo ()) }
  | LPAREN SEQ sequence RPAREN { $3 }
  | LPAREN IF expr expr expr RPAREN { Cifthenelse($3, $4, $5) }
  | LPAREN SWITCH INTCONST expr caselist RPAREN { make_switch $3 $4 $5 }
  | LPAREN WHILE expr sequence RPAREN
      { let body =
          match $3 with
            Cconst_int x when x <> 0 -> $4
          | _ -> Cifthenelse($3, $4, (Cexit(0,[]))) in
        Ccatch(Recursive, [0, [], Cloop body], Ctuple []) }
  | LPAREN EXIT IDENT exprlist RPAREN
    { Cexit(find_label $3, List.rev $4) }
  | LPAREN CATCH sequence WITH catch_handlers RPAREN
    { let handlers = $5 in
      List.iter (fun (_, l, _) -> List.iter unbind_ident l) handlers;
      Ccatch(Recursive, handlers, $3) }
  | EXIT        { Cexit(0,[]) }
  | LPAREN TRY sequence WITH bind_ident sequence RPAREN
                { unbind_ident $5; Ctrywith($3, $5, $6) }
  | LPAREN VAL expr expr RPAREN
      { Cop(Cload (Word_val, Mutable), [access_array $3 $4 Arch.size_addr],
          debuginfo ()) }
  | LPAREN ADDRAREF expr expr RPAREN
      { Cop(Cload (Word_val, Mutable), [access_array $3 $4 Arch.size_addr],
          Debuginfo.none) }
  | LPAREN INTAREF expr expr RPAREN
      { Cop(Cload (Word_int, Mutable), [access_array $3 $4 Arch.size_int],
          Debuginfo.none) }
  | LPAREN FLOATAREF expr expr RPAREN
      { Cop(Cload (Double_u, Mutable), [access_array $3 $4 Arch.size_float],
          Debuginfo.none) }
  | LPAREN ADDRASET expr expr expr RPAREN
      { Cop(Cstore (Word_val, Assignment),
            [access_array $3 $4 Arch.size_addr; $5], Debuginfo.none) }
  | LPAREN INTASET expr expr expr RPAREN
      { Cop(Cstore (Word_int, Assignment),
            [access_array $3 $4 Arch.size_int; $5], Debuginfo.none) }
  | LPAREN FLOATASET expr expr expr RPAREN
      { Cop(Cstore (Double_u, Assignment),
            [access_array $3 $4 Arch.size_float; $5], Debuginfo.none) }
;
exprlist:
    exprlist expr               { $2 :: $1 }
  | /**/                        { [] }
;
letdef:
    oneletdef                   { [$1] }
  | LPAREN letdefmult RPAREN    { $2 }
;
letdefmult:
    /**/                        { [] }
  | oneletdef letdefmult        { $1 :: $2 }
;
oneletdef:
    IDENT expr                  { (bind_ident $1, $2) }
;
chunk:
    UNSIGNED BYTE               { Byte_unsigned }
  | SIGNED BYTE                 { Byte_signed }
  | UNSIGNED HALF               { Sixteen_unsigned }
  | SIGNED HALF                 { Sixteen_signed }
  | UNSIGNED INT32              { Thirtytwo_unsigned }
  | SIGNED INT32                { Thirtytwo_signed }
  | INT                         { Word_int }
  | ADDR                        { Word_val }
  | FLOAT32                     { Single }
  | FLOAT64                     { Double }
  | FLOAT                       { Double_u }
  | VAL                         { Word_val }
;
unaryop:
    LOAD chunk                  { Cload ($2, Mutable) }
  | FLOATOFINT                  { Cfloatofint }
  | INTOFFLOAT                  { Cintoffloat }
  | RAISE                       { Craise $1 }
  | ABSF                        { Cabsf }
;
binaryop:
    STORE chunk                 { Cstore ($2, Assignment) }
  | ADDI                        { Caddi }
  | SUBI                        { Csubi }
  | STAR                        { Cmuli }
  | DIVI                        { Cdivi }
  | MODI                        { Cmodi }
  | AND                         { Cand }
  | OR                          { Cor }
  | XOR                         { Cxor }
  | LSL                         { Clsl }
  | LSR                         { Clsr }
  | ASR                         { Casr }
  | EQI                         { Ccmpi Ceq }
  | NEI                         { Ccmpi Cne }
  | LTI                         { Ccmpi Clt }
  | LEI                         { Ccmpi Cle }
  | GTI                         { Ccmpi Cgt }
  | GEI                         { Ccmpi Cge }
  | ADDA                        { Cadda }
  | ADDV                        { Caddv }
  | EQA                         { Ccmpa Ceq }
  | NEA                         { Ccmpa Cne }
  | LTA                         { Ccmpa Clt }
  | LEA                         { Ccmpa Cle }
  | GTA                         { Ccmpa Cgt }
  | GEA                         { Ccmpa Cge }
  | ADDF                        { Caddf }
  | MULF                        { Cmulf }
  | DIVF                        { Cdivf }
  | EQF                         { Ccmpf Ceq }
  | NEF                         { Ccmpf Cne }
  | LTF                         { Ccmpf Clt }
  | LEF                         { Ccmpf Cle }
  | GTF                         { Ccmpf Cgt }
  | GEF                         { Ccmpf Cge }
  | CHECKBOUND                  { Ccheckbound }
  | MULH                        { Cmulhi }
;
sequence:
    expr sequence               { Csequence($1, $2) }
  | expr                        { $1 }
;
caselist:
    onecase sequence caselist   { ($1, $2) :: $3 }
  | /**/                        { [] }
;
onecase:
    CASE INTCONST COLON onecase { $2 :: $4 }
  | CASE INTCONST COLON         { [$2] }
;
bind_ident:
    IDENT                       { bind_ident $1 }
;
datadecl:
    LPAREN datalist RPAREN      { List.rev $2 }
  | LPAREN DATA datalist RPAREN { List.rev $3 }
;
datalist:
    datalist dataitem           { $2 :: $1 }
  | /**/                        { [] }
;
dataitem:
    STRING COLON                { Cdefine_symbol $1 }
  | BYTE INTCONST               { Cint8 $2 }
  | HALF INTCONST               { Cint16 $2 }
  | INT INTCONST                { Cint(Nativeint.of_int $2) }
  | FLOAT FLOATCONST            { Cdouble (float_of_string $2) }
  | ADDR STRING                 { Csymbol_address $2 }
  | VAL STRING                 { Csymbol_address $2 }
  | KSTRING STRING              { Cstring $2 }
  | SKIP INTCONST               { Cskip $2 }
  | ALIGN INTCONST              { Calign $2 }
  | GLOBAL STRING               { Cglobal_symbol $2 }
;
catch_handlers:
  | catch_handler
    { [$1] }
  | catch_handler AND catch_handlers
    { $1 :: $3 }

catch_handler:
  | sequence
    { 0, [], $1 }
  | LPAREN IDENT bind_identlist RPAREN sequence
    { find_label $2, $3, $5 }

bind_identlist:
    /**/                        { [] }
  | bind_ident bind_identlist   { $1 :: $2 }

location:
    /**/                        { None }
  | LOCATION                    { Some $1 }
