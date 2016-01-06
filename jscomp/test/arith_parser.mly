%{
open Arith_syntax
%}


/* Lexemes */
%token <float> NUMERAL
%token <string> IDENT
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token UMINUS
%token LPAREN
%token RPAREN
%token EOF

/* Precedence and associativity */
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

/* Top level rule */
%start toplevel
%type <Arith_syntax.expression> toplevel

%%

/* Grammar */

toplevel: expression EOF { $1 }
;

expression:
  | NUMERAL                           { Numeral $1 }
  | IDENT                           { Variable $1 } 
  | expression PLUS   expression      { Plus ($1, $3) }
  | expression MINUS  expression      { Minus ($1, $3) }
  | expression TIMES  expression      { Times ($1, $3) }
  | expression DIVIDE expression      { Divide ($1, $3) }
  | MINUS expression %prec UMINUS     { Negate $2 } 
  | LPAREN expression RPAREN          { $2 }
;

%% 



