/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* The parser definition */

%{
open Location
open Asttypes
open Longident
open Parsetree
open Ast_helper
open Docstrings

let mktyp d = Typ.mk ~loc:(symbol_rloc()) d
let mkpat d = Pat.mk ~loc:(symbol_rloc()) d
let mkexp d = Exp.mk ~loc:(symbol_rloc()) d
let mkmty d = Mty.mk ~loc:(symbol_rloc()) d
let mksig d = Sig.mk ~loc:(symbol_rloc()) d
let mkmod d = Mod.mk ~loc:(symbol_rloc()) d
let mkstr d = Str.mk ~loc:(symbol_rloc()) d
let mkclass d = Cl.mk ~loc:(symbol_rloc()) d
let mkcty d = Cty.mk ~loc:(symbol_rloc()) d
let mkctf ?attrs ?docs d =
  Ctf.mk ~loc:(symbol_rloc()) ?attrs ?docs d
let mkcf ?attrs ?docs d =
  Cf.mk ~loc:(symbol_rloc()) ?attrs ?docs d

let mkrhs rhs pos = mkloc rhs (rhs_loc pos)
let mkoption d =
  let loc = {d.ptyp_loc with loc_ghost = true} in
  Typ.mk ~loc (Ptyp_constr(mkloc (Ldot (Lident "*predef*", "option")) loc,[d]))

let reloc_pat x = { x with ppat_loc = symbol_rloc () };;
let reloc_exp x = { x with pexp_loc = symbol_rloc () };;

let mkoperator name pos =
  let loc = rhs_loc pos in
  Exp.mk ~loc (Pexp_ident(mkloc (Lident name) loc))

let mkpatvar name pos =
  Pat.mk ~loc:(rhs_loc pos) (Ppat_var (mkrhs name pos))

(*
  Ghost expressions and patterns:
  expressions and patterns that do not appear explicitly in the
  source file they have the loc_ghost flag set to true.
  Then the profiler will not try to instrument them and the
  -annot option will not try to display their type.

  Every grammar rule that generates an element with a location must
  make at most one non-ghost element, the topmost one.

  How to tell whether your location must be ghost:
  A location corresponds to a range of characters in the source file.
  If the location contains a piece of code that is syntactically
  valid (according to the documentation), and corresponds to the
  AST node, then the location must be real; in all other cases,
  it must be ghost.
*)
let ghexp d = Exp.mk ~loc:(symbol_gloc ()) d
let ghpat d = Pat.mk ~loc:(symbol_gloc ()) d
let ghtyp d = Typ.mk ~loc:(symbol_gloc ()) d
let ghloc d = { txt = d; loc = symbol_gloc () }
let ghstr d = Str.mk ~loc:(symbol_gloc()) d

let ghunit () =
  ghexp (Pexp_construct (mknoloc (Lident "()"), None))

let mkinfix arg1 name arg2 =
  mkexp(Pexp_apply(mkoperator name 2, ["", arg1; "", arg2]))

let neg_float_string f =
  if String.length f > 0 && f.[0] = '-'
  then String.sub f 1 (String.length f - 1)
  else "-" ^ f

let mkuminus name arg =
  match name, arg.pexp_desc with
  | "-", Pexp_constant(Const_int n) ->
      mkexp(Pexp_constant(Const_int(-n)))
  | "-", Pexp_constant(Const_int32 n) ->
      mkexp(Pexp_constant(Const_int32(Int32.neg n)))
  | "-", Pexp_constant(Const_int64 n) ->
      mkexp(Pexp_constant(Const_int64(Int64.neg n)))
  | "-", Pexp_constant(Const_nativeint n) ->
      mkexp(Pexp_constant(Const_nativeint(Nativeint.neg n)))
  | ("-" | "-."), Pexp_constant(Const_float f) ->
      mkexp(Pexp_constant(Const_float(neg_float_string f)))
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, ["", arg]))

let mkuplus name arg =
  let desc = arg.pexp_desc in
  match name, desc with
  | "+", Pexp_constant(Const_int _)
  | "+", Pexp_constant(Const_int32 _)
  | "+", Pexp_constant(Const_int64 _)
  | "+", Pexp_constant(Const_nativeint _)
  | ("+" | "+."), Pexp_constant(Const_float _) -> mkexp desc
  | _ ->
      mkexp(Pexp_apply(mkoperator ("~" ^ name) 1, ["", arg]))

let mkexp_cons consloc args loc =
  Exp.mk ~loc (Pexp_construct(mkloc (Lident "::") consloc, Some args))

let mkpat_cons consloc args loc =
  Pat.mk ~loc (Ppat_construct(mkloc (Lident "::") consloc, Some args))

let rec mktailexp nilloc = function
    [] ->
      let loc = { nilloc with loc_ghost = true } in
      let nil = { txt = Lident "[]"; loc = loc } in
      Exp.mk ~loc (Pexp_construct (nil, None))
  | e1 :: el ->
      let exp_el = mktailexp nilloc el in
      let loc = {loc_start = e1.pexp_loc.loc_start;
               loc_end = exp_el.pexp_loc.loc_end;
               loc_ghost = true}
      in
      let arg = Exp.mk ~loc (Pexp_tuple [e1; exp_el]) in
      mkexp_cons {loc with loc_ghost = true} arg loc

let rec mktailpat nilloc = function
    [] ->
      let loc = { nilloc with loc_ghost = true } in
      let nil = { txt = Lident "[]"; loc = loc } in
      Pat.mk ~loc (Ppat_construct (nil, None))
  | p1 :: pl ->
      let pat_pl = mktailpat nilloc pl in
      let loc = {loc_start = p1.ppat_loc.loc_start;
               loc_end = pat_pl.ppat_loc.loc_end;
               loc_ghost = true}
      in
      let arg = Pat.mk ~loc (Ppat_tuple [p1; pat_pl]) in
      mkpat_cons {loc with loc_ghost = true} arg loc

let mkstrexp e attrs =
  { pstr_desc = Pstr_eval (e, attrs); pstr_loc = e.pexp_loc }

let mkexp_constraint e (t1, t2) =
  match t1, t2 with
  | Some t, None -> ghexp(Pexp_constraint(e, t))
  | _, Some t -> ghexp(Pexp_coerce(e, t1, t))
  | None, None -> assert false

let array_function str name =
  ghloc (Ldot(Lident str, (if !Clflags.fast then "unsafe_" ^ name else name)))

let syntax_error () =
  raise Syntaxerr.Escape_error

let unclosed opening_name opening_num closing_name closing_num =
  raise(Syntaxerr.Error(Syntaxerr.Unclosed(rhs_loc opening_num, opening_name,
                                           rhs_loc closing_num, closing_name)))

let expecting pos nonterm =
    raise Syntaxerr.(Error(Expecting(rhs_loc pos, nonterm)))

let not_expecting pos nonterm =
    raise Syntaxerr.(Error(Not_expecting(rhs_loc pos, nonterm)))

let bigarray_function str name =
  ghloc (Ldot(Ldot(Lident "Bigarray", str), name))

let bigarray_untuplify = function
    { pexp_desc = Pexp_tuple explist; pexp_loc = _ } -> explist
  | exp -> [exp]

let bigarray_get arr arg =
  let get = if !Clflags.fast then "unsafe_get" else "get" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" get)),
                       ["", arr; "", c1]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" get)),
                       ["", arr; "", c1; "", c2]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" get)),
                       ["", arr; "", c1; "", c2; "", c3]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "get")),
                       ["", arr; "", ghexp(Pexp_array coords)]))

let bigarray_set arr arg newval =
  let set = if !Clflags.fast then "unsafe_set" else "set" in
  match bigarray_untuplify arg with
    [c1] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array1" set)),
                       ["", arr; "", c1; "", newval]))
  | [c1;c2] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array2" set)),
                       ["", arr; "", c1; "", c2; "", newval]))
  | [c1;c2;c3] ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Array3" set)),
                       ["", arr; "", c1; "", c2; "", c3; "", newval]))
  | coords ->
      mkexp(Pexp_apply(ghexp(Pexp_ident(bigarray_function "Genarray" "set")),
                       ["", arr;
                        "", ghexp(Pexp_array coords);
                        "", newval]))

let lapply p1 p2 =
  if !Clflags.applicative_functors
  then Lapply(p1, p2)
  else raise (Syntaxerr.Error(Syntaxerr.Applicative_path (symbol_rloc())))

let exp_of_label lbl pos =
  mkexp (Pexp_ident(mkrhs (Lident(Longident.last lbl)) pos))

let pat_of_label lbl pos =
  mkpat (Ppat_var (mkrhs (Longident.last lbl) pos))

let check_variable vl loc v =
  if List.mem v vl then
    raise Syntaxerr.(Error(Variable_in_scope(loc,v)))

let varify_constructors var_names t =
  let rec loop t =
    let desc =
      match t.ptyp_desc with
      | Ptyp_any -> Ptyp_any
      | Ptyp_var x ->
          check_variable var_names t.ptyp_loc x;
          Ptyp_var x
      | Ptyp_arrow (label,core_type,core_type') ->
          Ptyp_arrow(label, loop core_type, loop core_type')
      | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
      | Ptyp_constr( { txt = Lident s }, []) when List.mem s var_names ->
          Ptyp_var s
      | Ptyp_constr(longident, lst) ->
          Ptyp_constr(longident, List.map loop lst)
      | Ptyp_object (lst, o) ->
          Ptyp_object
            (List.map (fun (s, attrs, t) -> (s, attrs, loop t)) lst, o)
      | Ptyp_class (longident, lst) ->
          Ptyp_class (longident, List.map loop lst)
      | Ptyp_alias(core_type, string) ->
          check_variable var_names t.ptyp_loc string;
          Ptyp_alias(loop core_type, string)
      | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
          Ptyp_variant(List.map loop_row_field row_field_list,
                       flag, lbl_lst_option)
      | Ptyp_poly(string_lst, core_type) ->
          List.iter (check_variable var_names t.ptyp_loc) string_lst;
          Ptyp_poly(string_lst, loop core_type)
      | Ptyp_package(longident,lst) ->
          Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
      | Ptyp_extension (s, arg) ->
          Ptyp_extension (s, arg)
    in
    {t with ptyp_desc = desc}
  and loop_row_field  =
    function
      | Rtag(label,attrs,flag,lst) ->
          Rtag(label,attrs,flag,List.map loop lst)
      | Rinherit t ->
          Rinherit (loop t)
  in
  loop t

let wrap_type_annotation newtypes core_type body =
  let exp = mkexp(Pexp_constraint(body,core_type)) in
  let exp =
    List.fold_right (fun newtype exp -> mkexp (Pexp_newtype (newtype, exp)))
      newtypes exp
  in
  (exp, ghtyp(Ptyp_poly(newtypes,varify_constructors newtypes core_type)))

let wrap_exp_attrs body (ext, attrs) =
  (* todo: keep exact location for the entire attribute *)
  let body = {body with pexp_attributes = attrs @ body.pexp_attributes} in
  match ext with
  | None -> body
  | Some id -> ghexp(Pexp_extension (id, PStr [mkstrexp body []]))

let mkexp_attrs d attrs =
  wrap_exp_attrs (mkexp d) attrs

let text_str pos = Str.text (rhs_text pos)
let text_sig pos = Sig.text (rhs_text pos)
let text_cstr pos = Cf.text (rhs_text pos)
let text_csig pos = Ctf.text (rhs_text pos)
let text_def pos = [Ptop_def (Str.text (rhs_text pos))]

let extra_text text pos items =
  let pre_extras = rhs_pre_extra_text pos in
  let post_extras = rhs_post_extra_text pos in
    text pre_extras @ items @ text post_extras

let extra_str pos items = extra_text Str.text pos items
let extra_sig pos items = extra_text Sig.text pos items
let extra_cstr pos items = extra_text Cf.text pos items
let extra_csig pos items = extra_text Ctf.text pos items
let extra_def pos items =
  extra_text (fun txt -> [Ptop_def (Str.text txt)]) pos items

let add_nonrec rf attrs pos =
  match rf with
  | Recursive -> attrs
  | Nonrecursive ->
      let name = { txt = "nonrec"; loc = rhs_loc pos } in
        (name, PStr []) :: attrs

type let_binding =
  { lb_pattern: pattern;
    lb_expression: expression;
    lb_attributes: attributes;
    lb_docs: docs Lazy.t;
    lb_text: text Lazy.t;
    lb_loc: Location.t; }

type let_bindings =
  { lbs_bindings: let_binding list;
    lbs_rec: rec_flag;
    lbs_extension: string Asttypes.loc option;
    lbs_attributes: attributes;
    lbs_loc: Location.t }

let mklb (p, e) attrs =
  { lb_pattern = p;
    lb_expression = e;
    lb_attributes = attrs;
    lb_docs = symbol_docs_lazy ();
    lb_text = symbol_text_lazy ();
    lb_loc = symbol_rloc (); }

let mklbs (ext, attrs) rf lb =
  { lbs_bindings = [lb];
    lbs_rec = rf;
    lbs_extension = ext ;
    lbs_attributes = attrs;
    lbs_loc = symbol_rloc (); }

let addlb lbs lb =
  { lbs with lbs_bindings = lb :: lbs.lbs_bindings }

let val_of_let_bindings lbs =
  let str =
    match lbs.lbs_bindings with
    | [ {lb_pattern = { ppat_desc = Ppat_any; ppat_loc = _ }; _} as lb ] ->
        let exp = wrap_exp_attrs lb.lb_expression
                    (None, lbs.lbs_attributes) in
        mkstr (Pstr_eval (exp, lb.lb_attributes))
    | bindings ->
        if lbs.lbs_attributes <> [] then
          raise Syntaxerr.(Error(Not_expecting(lbs.lbs_loc, "attributes")));
        let bindings =
          List.map
            (fun lb ->
               Vb.mk ~loc:lb.lb_loc ~attrs:lb.lb_attributes
                 ~docs:(Lazy.force lb.lb_docs)
                 ~text:(Lazy.force lb.lb_text)
                 lb.lb_pattern lb.lb_expression)
            bindings
        in
        mkstr(Pstr_value(lbs.lbs_rec, List.rev bindings))
  in
  match lbs.lbs_extension with
  | None -> str
  | Some id -> ghstr (Pstr_extension((id, PStr [str]), []))

let expr_of_let_bindings lbs body =
  let bindings =
    List.map
      (fun lb ->
         if lb.lb_attributes <> [] then
           raise Syntaxerr.(Error(Not_expecting(lb.lb_loc, "item attribute")));
         Vb.mk ~loc:lb.lb_loc lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
    mkexp_attrs (Pexp_let(lbs.lbs_rec, List.rev bindings, body))
      (lbs.lbs_extension, lbs.lbs_attributes)

let class_of_let_bindings lbs body =
  let bindings =
    List.map
      (fun lb ->
         if lb.lb_attributes <> [] then
           raise Syntaxerr.(Error(Not_expecting(lb.lb_loc, "item attribute")));
         Vb.mk ~loc:lb.lb_loc lb.lb_pattern lb.lb_expression)
      lbs.lbs_bindings
  in
    if lbs.lbs_extension <> None then
      raise Syntaxerr.(Error(Not_expecting(lbs.lbs_loc, "extension")));
    if lbs.lbs_attributes <> [] then
      raise Syntaxerr.(Error(Not_expecting(lbs.lbs_loc, "attributes")));
    mkclass(Pcl_let (lbs.lbs_rec, List.rev bindings, body))

%}

/* Tokens */

%token AMPERAMPER
%token AMPERSAND
%token AND
%token AS
%token ASSERT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COMMA
%token CONSTRAINT
%token DO
%token DONE
%token DOT
%token DOTDOT
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EQUAL
%token EXCEPTION
%token EXTERNAL
%token FALSE
%token <string> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <int> INT
%token <int32> INT32
%token <int64> INT64
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETBAR
%token LBRACKETLESS
%token LBRACKETGREATER
%token LBRACKETPERCENT
%token LBRACKETPERCENTPERCENT
%token LESS
%token LESSMINUS
%token LET
%token <string> LIDENT
%token LPAREN
%token LBRACKETAT
%token LBRACKETATAT
%token LBRACKETATATAT
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MODULE
%token MUTABLE
%token <nativeint> NATIVEINT
%token NEW
%token NONREC
%token OBJECT
%token OF
%token OPEN
%token <string> OPTLABEL
%token OR
/* %token PARSER */
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SHARP
%token <string> SHARPOP
%token SIG
%token STAR
%token <string * string option> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%token <string * Location.t> COMMENT
%token <Docstrings.docstring> DOCSTRING

%token EOL

/* Precedences and associativities.

Tokens and rules have precedences.  A reduce/reduce conflict is resolved
in favor of the first rule (in source file order).  A shift/reduce conflict
is resolved by comparing the precedence and associativity of the token to
be shifted with those of the rule to be reduced.

By default, a rule has the precedence of its rightmost terminal (if any).

When there is a shift/reduce conflict between a rule and a token that
have the same precedence, it is resolved using the associativity:
if the token is left-associative, the parser will reduce; if
right-associative, the parser will shift; if non-associative,
the parser will declare a syntax error.

We will only use associativities with operators of the kind  x * x -> x
for example, in the rules of the form    expr: expr BINOP expr
in all other cases, we define two precedences if needed to resolve
conflicts.

The precedences must be listed from low to high.
*/

%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%nonassoc LBRACKETATAT
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT PLUSEQ /* expr (e OP e OP e) */
%left     PERCENT INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%left     SHARPOP
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT
          LBRACKETPERCENT LBRACKETPERCENTPERCENT


/* Entry points */

%start implementation                   /* for implementation files */
%type <Parsetree.structure> implementation
%start interface                        /* for interface files */
%type <Parsetree.signature> interface
%start toplevel_phrase                  /* for interactive use */
%type <Parsetree.toplevel_phrase> toplevel_phrase
%start use_file                         /* for the #use directive */
%type <Parsetree.toplevel_phrase list> use_file
%start parse_core_type
%type <Parsetree.core_type> parse_core_type
%start parse_expression
%type <Parsetree.expression> parse_expression
%start parse_pattern
%type <Parsetree.pattern> parse_pattern
%%

/* Entry points */

implementation:
    structure EOF                        { extra_str 1 $1 }
;
interface:
    signature EOF                        { extra_sig 1 $1 }
;
toplevel_phrase:
    top_structure SEMISEMI               { Ptop_def (extra_str 1 $1) }
  | toplevel_directive SEMISEMI          { $1 }
  | EOF                                  { raise End_of_file }
;
top_structure:
    seq_expr post_item_attributes
      { (text_str 1) @ [mkstrexp $1 $2] }
  | top_structure_tail
      { $1 }
;
top_structure_tail:
    /* empty */                          { [] }
  | structure_item top_structure_tail    { (text_str 1) @ $1 :: $2 }
;
use_file:
    use_file_body                        { extra_def 1 $1 }
;
use_file_body:
    use_file_tail                        { $1 }
  | seq_expr post_item_attributes use_file_tail
      { (text_def 1) @ Ptop_def[mkstrexp $1 $2] :: $3 }
;
use_file_tail:
    EOF
      { [] }
  | SEMISEMI EOF
      { text_def 1 }
  | SEMISEMI seq_expr post_item_attributes use_file_tail
      {  mark_rhs_docs 2 3;
        (text_def 1) @ (text_def 2) @ Ptop_def[mkstrexp $2 $3] :: $4 }
  | SEMISEMI structure_item use_file_tail
      { (text_def 1) @ (text_def 2) @ Ptop_def[$2] :: $3 }
  | SEMISEMI toplevel_directive use_file_tail
      {  mark_rhs_docs 2 3;
        (text_def 1) @ (text_def 2) @ $2 :: $3 }
  | structure_item use_file_tail
      { (text_def 1) @ Ptop_def[$1] :: $2 }
  | toplevel_directive use_file_tail
      { mark_rhs_docs 1 1;
        (text_def 1) @ $1 :: $2 }
;
parse_core_type:
    core_type EOF { $1 }
;
parse_expression:
    seq_expr EOF { $1 }
;
parse_pattern:
    pattern EOF { $1 }
;

/* Module expressions */

functor_arg:
    LPAREN RPAREN
      { mkrhs "*" 2, None }
  | LPAREN functor_arg_name COLON module_type RPAREN
      { mkrhs $2 2, Some $4 }
;

functor_arg_name:
    UIDENT     { $1 }
  | UNDERSCORE { "_" }
;

functor_args:
    functor_args functor_arg
      { $2 :: $1 }
  | functor_arg
      { [ $1 ] }
;

module_expr:
    mod_longident
      { mkmod(Pmod_ident (mkrhs $1 1)) }
  | STRUCT structure END
      { mkmod(Pmod_structure(extra_str 2 $2)) }
  | STRUCT structure error
      { unclosed "struct" 1 "end" 3 }
  | FUNCTOR functor_args MINUSGREATER module_expr
      { List.fold_left (fun acc (n, t) -> mkmod(Pmod_functor(n, t, acc)))
                       $4 $2 }
  | module_expr LPAREN module_expr RPAREN
      { mkmod(Pmod_apply($1, $3)) }
  | module_expr LPAREN RPAREN
      { mkmod(Pmod_apply($1, mkmod (Pmod_structure []))) }
  | module_expr LPAREN module_expr error
      { unclosed "(" 2 ")" 4 }
  | LPAREN module_expr COLON module_type RPAREN
      { mkmod(Pmod_constraint($2, $4)) }
  | LPAREN module_expr COLON module_type error
      { unclosed "(" 1 ")" 5 }
  | LPAREN module_expr RPAREN
      { $2 }
  | LPAREN module_expr error
      { unclosed "(" 1 ")" 3 }
  | LPAREN VAL expr RPAREN
      { mkmod(Pmod_unpack $3) }
  | LPAREN VAL expr COLON package_type RPAREN
      { mkmod(Pmod_unpack(
              ghexp(Pexp_constraint($3, ghtyp(Ptyp_package $5))))) }
  | LPAREN VAL expr COLON package_type COLONGREATER package_type RPAREN
      { mkmod(Pmod_unpack(
              ghexp(Pexp_coerce($3, Some(ghtyp(Ptyp_package $5)),
                                    ghtyp(Ptyp_package $7))))) }
  | LPAREN VAL expr COLONGREATER package_type RPAREN
      { mkmod(Pmod_unpack(
              ghexp(Pexp_coerce($3, None, ghtyp(Ptyp_package $5))))) }
  | LPAREN VAL expr COLON error
      { unclosed "(" 1 ")" 5 }
  | LPAREN VAL expr COLONGREATER error
      { unclosed "(" 1 ")" 5 }
  | LPAREN VAL expr error
      { unclosed "(" 1 ")" 4 }
  | module_expr attribute
      { Mod.attr $1 $2 }
  | extension
      { mkmod(Pmod_extension $1) }
;

structure:
    seq_expr post_item_attributes structure_tail
      { mark_rhs_docs 1 2;
        (text_str 1) @ mkstrexp $1 $2 :: $3 }
  | structure_tail { $1 }
;
structure_tail:
    /* empty */          { [] }
  | SEMISEMI structure   { (text_str 1) @ $2 }
  | structure_item structure_tail { (text_str 1) @ $1 :: $2 }
;
structure_item:
    let_bindings
      { val_of_let_bindings $1 }
  | primitive_declaration
      { mkstr (Pstr_primitive $1) }
  | type_declarations
      { mkstr(Pstr_type (List.rev $1)) }
  | str_type_extension
      { mkstr(Pstr_typext $1) }
  | str_exception_declaration
      { mkstr(Pstr_exception $1) }
  | module_binding
      { mkstr(Pstr_module $1) }
  | rec_module_bindings
      { mkstr(Pstr_recmodule(List.rev $1)) }
  | module_type_declaration
      { mkstr(Pstr_modtype $1) }
  | open_statement { mkstr(Pstr_open $1) }
  | class_declarations
      { mkstr(Pstr_class (List.rev $1)) }
  | class_type_declarations
      { mkstr(Pstr_class_type (List.rev $1)) }
  | str_include_statement
      { mkstr(Pstr_include $1) }
  | item_extension post_item_attributes
      { mkstr(Pstr_extension ($1, (add_docs_attrs (symbol_docs ()) $2))) }
  | floating_attribute
      { mark_symbol_docs ();
        mkstr(Pstr_attribute $1) }
;
str_include_statement:
    INCLUDE module_expr post_item_attributes
      { Incl.mk $2 ~attrs:$3
                ~loc:(symbol_rloc()) ~docs:(symbol_docs ()) }
;
module_binding_body:
    EQUAL module_expr
      { $2 }
  | COLON module_type EQUAL module_expr
      { mkmod(Pmod_constraint($4, $2)) }
  | functor_arg module_binding_body
      { mkmod(Pmod_functor(fst $1, snd $1, $2)) }
;
module_binding:
    MODULE UIDENT module_binding_body post_item_attributes
      { Mb.mk (mkrhs $2 2) $3 ~attrs:$4
              ~loc:(symbol_rloc ()) ~docs:(symbol_docs ()) }
;
rec_module_bindings:
    rec_module_binding                            { [$1] }
  | rec_module_bindings and_module_binding        { $2 :: $1 }
;
rec_module_binding:
    MODULE REC UIDENT module_binding_body post_item_attributes
      { Mb.mk (mkrhs $3 3) $4 ~attrs:$5
              ~loc:(symbol_rloc ()) ~docs:(symbol_docs ()) }
;
and_module_binding:
    AND UIDENT module_binding_body post_item_attributes
      { Mb.mk (mkrhs $2 2) $3 ~attrs:$4 ~loc:(symbol_rloc ())
               ~text:(symbol_text ()) ~docs:(symbol_docs ()) }
;

/* Module types */

module_type:
    mty_longident
      { mkmty(Pmty_ident (mkrhs $1 1)) }
  | SIG signature END
      { mkmty(Pmty_signature (extra_sig 2 $2)) }
  | SIG signature error
      { unclosed "sig" 1 "end" 3 }
  | FUNCTOR functor_args MINUSGREATER module_type
      %prec below_WITH
      { List.fold_left (fun acc (n, t) -> mkmty(Pmty_functor(n, t, acc)))
                       $4 $2 }
  | module_type WITH with_constraints
      { mkmty(Pmty_with($1, List.rev $3)) }
  | MODULE TYPE OF module_expr %prec below_LBRACKETAT
      { mkmty(Pmty_typeof $4) }
/*  | LPAREN MODULE mod_longident RPAREN
      { mkmty (Pmty_alias (mkrhs $3 3)) } */
  | LPAREN module_type RPAREN
      { $2 }
  | LPAREN module_type error
      { unclosed "(" 1 ")" 3 }
  | extension
      { mkmty(Pmty_extension $1) }
  | module_type attribute
      { Mty.attr $1 $2 }
;
signature:
    /* empty */          { [] }
  | SEMISEMI signature   { (text_sig 1) @ $2 }
  | signature_item signature { (text_sig 1) @ $1 :: $2 }
;
signature_item:
    value_description
      { mksig(Psig_value $1) }
  | primitive_declaration
      { mksig(Psig_value $1) }
  | type_declarations
      { mksig(Psig_type (List.rev $1)) }
  | sig_type_extension
      { mksig(Psig_typext $1) }
  | sig_exception_declaration
      { mksig(Psig_exception $1) }
  | module_declaration
      { mksig(Psig_module $1) }
  | module_alias
      { mksig(Psig_module $1) }
  | rec_module_declarations
      { mksig(Psig_recmodule (List.rev $1)) }
  | module_type_declaration
      { mksig(Psig_modtype $1) }
  | open_statement
      { mksig(Psig_open $1) }
  | sig_include_statement
      { mksig(Psig_include $1) }
  | class_descriptions
      { mksig(Psig_class (List.rev $1)) }
  | class_type_declarations
      { mksig(Psig_class_type (List.rev $1)) }
  | item_extension post_item_attributes
      { mksig(Psig_extension ($1, (add_docs_attrs (symbol_docs ()) $2))) }
  | floating_attribute
      { mark_symbol_docs ();
        mksig(Psig_attribute $1) }
;
open_statement:
  | OPEN override_flag mod_longident post_item_attributes
      { Opn.mk (mkrhs $3 3) ~override:$2 ~attrs:$4
          ~loc:(symbol_rloc()) ~docs:(symbol_docs ()) }
;
sig_include_statement:
    INCLUDE module_type post_item_attributes %prec below_WITH
      { Incl.mk $2 ~attrs:$3
                ~loc:(symbol_rloc()) ~docs:(symbol_docs ()) }
;
module_declaration_body:
    COLON module_type
      { $2 }
  | LPAREN UIDENT COLON module_type RPAREN module_declaration_body
      { mkmty(Pmty_functor(mkrhs $2 2, Some $4, $6)) }
  | LPAREN RPAREN module_declaration_body
      { mkmty(Pmty_functor(mkrhs "*" 1, None, $3)) }
;
module_declaration:
    MODULE UIDENT module_declaration_body post_item_attributes
      { Md.mk (mkrhs $2 2) $3 ~attrs:$4
          ~loc:(symbol_rloc()) ~docs:(symbol_docs ()) }
;
module_alias:
    MODULE UIDENT EQUAL mod_longident post_item_attributes
      { Md.mk (mkrhs $2 2)
          (Mty.alias ~loc:(rhs_loc 4) (mkrhs $4 4)) ~attrs:$5
             ~loc:(symbol_rloc()) ~docs:(symbol_docs ()) }
;
rec_module_declarations:
    rec_module_declaration                          { [$1] }
  | rec_module_declarations and_module_declaration  { $2 :: $1 }
;
rec_module_declaration:
    MODULE REC UIDENT COLON module_type post_item_attributes
      { Md.mk (mkrhs $3 3) $5 ~attrs:$6
              ~loc:(symbol_rloc()) ~docs:(symbol_docs ()) }
;
and_module_declaration:
    AND UIDENT COLON module_type post_item_attributes
      { Md.mk (mkrhs $2 2) $4 ~attrs:$5 ~loc:(symbol_rloc())
              ~text:(symbol_text()) ~docs:(symbol_docs()) }
;
module_type_declaration_body:
    /* empty */               { None }
  | EQUAL module_type         { Some $2 }
;
module_type_declaration:
    MODULE TYPE ident module_type_declaration_body post_item_attributes
      { Mtd.mk (mkrhs $3 3) ?typ:$4 ~attrs:$5
          ~loc:(symbol_rloc()) ~docs:(symbol_docs ()) }
;
/* Class expressions */

class_declarations:
    class_declaration                           { [$1] }
  | class_declarations and_class_declaration    { $2 :: $1 }
;
class_declaration:
    CLASS virtual_flag class_type_parameters LIDENT class_fun_binding
    post_item_attributes
      { Ci.mk (mkrhs $4 4) $5 ~virt:$2 ~params:$3 ~attrs:$6
              ~loc:(symbol_rloc ()) ~docs:(symbol_docs ()) }
;
and_class_declaration:
    AND virtual_flag class_type_parameters LIDENT class_fun_binding
    post_item_attributes
      { Ci.mk (mkrhs $4 4) $5 ~virt:$2 ~params:$3
         ~attrs:$6 ~loc:(symbol_rloc ())
         ~text:(symbol_text ()) ~docs:(symbol_docs ()) }
;
class_fun_binding:
    EQUAL class_expr
      { $2 }
  | COLON class_type EQUAL class_expr
      { mkclass(Pcl_constraint($4, $2)) }
  | labeled_simple_pattern class_fun_binding
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $2)) }
;
class_type_parameters:
    /*empty*/                                   { [] }
  | LBRACKET type_parameter_list RBRACKET       { List.rev $2 }
;
class_fun_def:
    labeled_simple_pattern MINUSGREATER class_expr
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $3)) }
  | labeled_simple_pattern class_fun_def
      { let (l,o,p) = $1 in mkclass(Pcl_fun(l, o, p, $2)) }
;
class_expr:
    class_simple_expr
      { $1 }
  | FUN class_fun_def
      { $2 }
  | class_simple_expr simple_labeled_expr_list
      { mkclass(Pcl_apply($1, List.rev $2)) }
  | let_bindings IN class_expr
      { class_of_let_bindings $1 $3 }
  | class_expr attribute
      { Cl.attr $1 $2 }
  | extension
      { mkclass(Pcl_extension $1) }
;
class_simple_expr:
    LBRACKET core_type_comma_list RBRACKET class_longident
      { mkclass(Pcl_constr(mkloc $4 (rhs_loc 4), List.rev $2)) }
  | class_longident
      { mkclass(Pcl_constr(mkrhs $1 1, [])) }
  | OBJECT class_structure END
      { mkclass(Pcl_structure $2) }
  | OBJECT class_structure error
      { unclosed "object" 1 "end" 3 }
  | LPAREN class_expr COLON class_type RPAREN
      { mkclass(Pcl_constraint($2, $4)) }
  | LPAREN class_expr COLON class_type error
      { unclosed "(" 1 ")" 5 }
  | LPAREN class_expr RPAREN
      { $2 }
  | LPAREN class_expr error
      { unclosed "(" 1 ")" 3 }
;
class_structure:
  |  class_self_pattern class_fields
       { Cstr.mk $1 (extra_cstr 2 (List.rev $2)) }
;
class_self_pattern:
    LPAREN pattern RPAREN
      { reloc_pat $2 }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
  | /* empty */
      { ghpat(Ppat_any) }
;
class_fields:
    /* empty */
      { [] }
  | class_fields class_field
      { $2 :: (text_cstr 2) @ $1 }
;
class_field:
  | INHERIT override_flag class_expr parent_binder post_item_attributes
      { mkcf (Pcf_inherit ($2, $3, $4)) ~attrs:$5 ~docs:(symbol_docs ()) }
  | VAL value post_item_attributes
      { mkcf (Pcf_val $2) ~attrs:$3 ~docs:(symbol_docs ()) }
  | METHOD method_ post_item_attributes
      { mkcf (Pcf_method $2) ~attrs:$3 ~docs:(symbol_docs ()) }
  | CONSTRAINT constrain_field post_item_attributes
      { mkcf (Pcf_constraint $2) ~attrs:$3 ~docs:(symbol_docs ()) }
  | INITIALIZER seq_expr post_item_attributes
      { mkcf (Pcf_initializer $2) ~attrs:$3 ~docs:(symbol_docs ()) }
  | item_extension post_item_attributes
      { mkcf (Pcf_extension $1) ~attrs:$2 ~docs:(symbol_docs ()) }
  | floating_attribute
      { mark_symbol_docs ();
        mkcf (Pcf_attribute $1) }
;
parent_binder:
    AS LIDENT
          { Some $2 }
  | /* empty */
          { None }
;
value:
/* TODO: factorize these rules (also with method): */
    override_flag MUTABLE VIRTUAL label COLON core_type
      { if $1 = Override then syntax_error ();
        mkloc $4 (rhs_loc 4), Mutable, Cfk_virtual $6 }
  | VIRTUAL mutable_flag label COLON core_type
      { mkrhs $3 3, $2, Cfk_virtual $5 }
  | override_flag mutable_flag label EQUAL seq_expr
      { mkrhs $3 3, $2, Cfk_concrete ($1, $5) }
  | override_flag mutable_flag label type_constraint EQUAL seq_expr
      {
       let e = mkexp_constraint $6 $4 in
       mkrhs $3 3, $2, Cfk_concrete ($1, e)
      }
;
method_:
/* TODO: factorize those rules... */
    override_flag PRIVATE VIRTUAL label COLON poly_type
      { if $1 = Override then syntax_error ();
        mkloc $4 (rhs_loc 4), Private, Cfk_virtual $6 }
  | override_flag VIRTUAL private_flag label COLON poly_type
      { if $1 = Override then syntax_error ();
        mkloc $4 (rhs_loc 4), $3, Cfk_virtual $6 }
  | override_flag private_flag label strict_binding
      { mkloc $3 (rhs_loc 3), $2,
        Cfk_concrete ($1, ghexp(Pexp_poly ($4, None))) }
  | override_flag private_flag label COLON poly_type EQUAL seq_expr
      { mkloc $3 (rhs_loc 3), $2,
        Cfk_concrete ($1, ghexp(Pexp_poly($7, Some $5))) }
  | override_flag private_flag label COLON TYPE lident_list
    DOT core_type EQUAL seq_expr
      { let exp, poly = wrap_type_annotation $6 $8 $10 in
        mkloc $3 (rhs_loc 3), $2,
        Cfk_concrete ($1, ghexp(Pexp_poly(exp, Some poly))) }
;

/* Class types */

class_type:
    class_signature
      { $1 }
  | QUESTION LIDENT COLON simple_core_type_or_tuple_no_attr MINUSGREATER
    class_type
      { mkcty(Pcty_arrow("?" ^ $2 , mkoption $4, $6)) }
  | OPTLABEL simple_core_type_or_tuple_no_attr MINUSGREATER class_type
      { mkcty(Pcty_arrow("?" ^ $1, mkoption $2, $4)) }
  | LIDENT COLON simple_core_type_or_tuple_no_attr MINUSGREATER class_type
      { mkcty(Pcty_arrow($1, $3, $5)) }
  | simple_core_type_or_tuple_no_attr MINUSGREATER class_type
      { mkcty(Pcty_arrow("", $1, $3)) }
 ;
class_signature:
    LBRACKET core_type_comma_list RBRACKET clty_longident
      { mkcty(Pcty_constr (mkloc $4 (rhs_loc 4), List.rev $2)) }
  | clty_longident
      { mkcty(Pcty_constr (mkrhs $1 1, [])) }
  | OBJECT class_sig_body END
      { mkcty(Pcty_signature $2) }
  | OBJECT class_sig_body error
      { unclosed "object" 1 "end" 3 }
  | class_signature attribute
      { Cty.attr $1 $2 }
  | extension
      { mkcty(Pcty_extension $1) }
;
class_sig_body:
    class_self_type class_sig_fields
      { Csig.mk $1 (extra_csig 2 (List.rev $2)) }
;
class_self_type:
    LPAREN core_type RPAREN
      { $2 }
  | /* empty */
      { mktyp(Ptyp_any) }
;
class_sig_fields:
    /* empty */                                 { [] }
| class_sig_fields class_sig_field     { $2 :: (text_csig 2) @ $1 }
;
class_sig_field:
    INHERIT class_signature post_item_attributes
      { mkctf (Pctf_inherit $2) ~attrs:$3 ~docs:(symbol_docs ()) }
  | VAL value_type post_item_attributes
      { mkctf (Pctf_val $2) ~attrs:$3 ~docs:(symbol_docs ()) }
  | METHOD private_virtual_flags label COLON poly_type post_item_attributes
      {
       let (p, v) = $2 in
       mkctf (Pctf_method ($3, p, v, $5)) ~attrs:$6 ~docs:(symbol_docs ())
      }
  | CONSTRAINT constrain_field post_item_attributes
      { mkctf (Pctf_constraint $2) ~attrs:$3 ~docs:(symbol_docs ()) }
  | item_extension post_item_attributes
      { mkctf (Pctf_extension $1) ~attrs:$2 ~docs:(symbol_docs ()) }
  | floating_attribute
      { mark_symbol_docs ();
        mkctf(Pctf_attribute $1) }
;
value_type:
    VIRTUAL mutable_flag label COLON core_type
      { $3, $2, Virtual, $5 }
  | MUTABLE virtual_flag label COLON core_type
      { $3, Mutable, $2, $5 }
  | label COLON core_type
      { $1, Immutable, Concrete, $3 }
;
constrain:
        core_type EQUAL core_type          { $1, $3, symbol_rloc() }
;
constrain_field:
        core_type EQUAL core_type          { $1, $3 }
;
class_descriptions:
    class_description                           { [$1] }
  | class_descriptions and_class_description    { $2 :: $1 }
;
class_description:
    CLASS virtual_flag class_type_parameters LIDENT COLON class_type
    post_item_attributes
      { Ci.mk (mkrhs $4 4) $6 ~virt:$2 ~params:$3 ~attrs:$7
              ~loc:(symbol_rloc ()) ~docs:(symbol_docs ()) }
;
and_class_description:
    AND virtual_flag class_type_parameters LIDENT COLON class_type
    post_item_attributes
      { Ci.mk (mkrhs $4 4) $6 ~virt:$2 ~params:$3
              ~attrs:$7 ~loc:(symbol_rloc ())
              ~text:(symbol_text ()) ~docs:(symbol_docs ()) }
;
class_type_declarations:
    class_type_declaration                              { [$1] }
  | class_type_declarations and_class_type_declaration  { $2 :: $1 }
;
class_type_declaration:
    CLASS TYPE virtual_flag class_type_parameters LIDENT EQUAL
    class_signature post_item_attributes
      { Ci.mk (mkrhs $5 5) $7 ~virt:$3 ~params:$4 ~attrs:$8
              ~loc:(symbol_rloc ()) ~docs:(symbol_docs ()) }
;
and_class_type_declaration:
    AND virtual_flag class_type_parameters LIDENT EQUAL
    class_signature post_item_attributes
      { Ci.mk (mkrhs $4 4) $6 ~virt:$2 ~params:$3
         ~attrs:$7 ~loc:(symbol_rloc ())
         ~text:(symbol_text ()) ~docs:(symbol_docs ()) }
;

/* Core expressions */

seq_expr:
  | expr        %prec below_SEMI  { $1 }
  | expr SEMI                     { reloc_exp $1 }
  | expr SEMI seq_expr            { mkexp(Pexp_sequence($1, $3)) }
;
labeled_simple_pattern:
    QUESTION LPAREN label_let_pattern opt_default RPAREN
      { ("?" ^ fst $3, $4, snd $3) }
  | QUESTION label_var
      { ("?" ^ fst $2, None, snd $2) }
  | OPTLABEL LPAREN let_pattern opt_default RPAREN
      { ("?" ^ $1, $4, $3) }
  | OPTLABEL pattern_var
      { ("?" ^ $1, None, $2) }
  | TILDE LPAREN label_let_pattern RPAREN
      { (fst $3, None, snd $3) }
  | TILDE label_var
      { (fst $2, None, snd $2) }
  | LABEL simple_pattern
      { ($1, None, $2) }
  | simple_pattern
      { ("", None, $1) }
;
pattern_var:
    LIDENT            { mkpat(Ppat_var (mkrhs $1 1)) }
  | UNDERSCORE        { mkpat Ppat_any }
;
opt_default:
    /* empty */                         { None }
  | EQUAL seq_expr                      { Some $2 }
;
label_let_pattern:
    label_var
      { $1 }
  | label_var COLON core_type
      { let (lab, pat) = $1 in (lab, mkpat(Ppat_constraint(pat, $3))) }
;
label_var:
    LIDENT    { ($1, mkpat(Ppat_var (mkrhs $1 1))) }
;
let_pattern:
    pattern
      { $1 }
  | pattern COLON core_type
      { mkpat(Ppat_constraint($1, $3)) }
;
expr:
    simple_expr %prec below_SHARP
      { $1 }
  | simple_expr simple_labeled_expr_list
      { mkexp(Pexp_apply($1, List.rev $2)) }
  | let_bindings IN seq_expr
      { expr_of_let_bindings $1 $3 }
  | LET MODULE ext_attributes UIDENT module_binding_body IN seq_expr
      { mkexp_attrs (Pexp_letmodule(mkrhs $4 4, $5, $7)) $3 }
  | LET OPEN override_flag ext_attributes mod_longident IN seq_expr
      { mkexp_attrs (Pexp_open($3, mkrhs $5 5, $7)) $4 }
  | FUNCTION ext_attributes opt_bar match_cases
      { mkexp_attrs (Pexp_function(List.rev $4)) $2 }
  | FUN ext_attributes labeled_simple_pattern fun_def
      { let (l,o,p) = $3 in
        mkexp_attrs (Pexp_fun(l, o, p, $4)) $2 }
  | FUN ext_attributes LPAREN TYPE LIDENT RPAREN fun_def
      { mkexp_attrs (Pexp_newtype($5, $7)) $2 }
  | MATCH ext_attributes seq_expr WITH opt_bar match_cases
      { mkexp_attrs (Pexp_match($3, List.rev $6)) $2 }
  | TRY ext_attributes seq_expr WITH opt_bar match_cases
      { mkexp_attrs (Pexp_try($3, List.rev $6)) $2 }
  | TRY ext_attributes seq_expr WITH error
      { syntax_error() }
  | expr_comma_list %prec below_COMMA
      { mkexp(Pexp_tuple(List.rev $1)) }
  | constr_longident simple_expr %prec below_SHARP
      { mkexp(Pexp_construct(mkrhs $1 1, Some $2)) }
  | name_tag simple_expr %prec below_SHARP
      { mkexp(Pexp_variant($1, Some $2)) }
  | IF ext_attributes seq_expr THEN expr ELSE expr
      { mkexp_attrs(Pexp_ifthenelse($3, $5, Some $7)) $2 }
  | IF ext_attributes seq_expr THEN expr
      { mkexp_attrs (Pexp_ifthenelse($3, $5, None)) $2 }
  | WHILE ext_attributes seq_expr DO seq_expr DONE
      { mkexp_attrs (Pexp_while($3, $5)) $2 }
  | FOR ext_attributes pattern EQUAL seq_expr direction_flag seq_expr DO
    seq_expr DONE
      { mkexp_attrs(Pexp_for($3, $5, $7, $6, $9)) $2 }
  | expr COLONCOLON expr
      { mkexp_cons (rhs_loc 2) (ghexp(Pexp_tuple[$1;$3])) (symbol_rloc()) }
  | LPAREN COLONCOLON RPAREN LPAREN expr COMMA expr RPAREN
      { mkexp_cons (rhs_loc 2) (ghexp(Pexp_tuple[$5;$7])) (symbol_rloc()) }
  | expr INFIXOP0 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3 }
  | expr PLUS expr
      { mkinfix $1 "+" $3 }
  | expr PLUSDOT expr
      { mkinfix $1 "+." $3 }
  | expr PLUSEQ expr
      { mkinfix $1 "+=" $3 }
  | expr MINUS expr
      { mkinfix $1 "-" $3 }
  | expr MINUSDOT expr
      { mkinfix $1 "-." $3 }
  | expr STAR expr
      { mkinfix $1 "*" $3 }
  | expr PERCENT expr
      { mkinfix $1 "%" $3 }
  | expr EQUAL expr
      { mkinfix $1 "=" $3 }
  | expr LESS expr
      { mkinfix $1 "<" $3 }
  | expr GREATER expr
      { mkinfix $1 ">" $3 }
  | expr OR expr
      { mkinfix $1 "or" $3 }
  | expr BARBAR expr
      { mkinfix $1 "||" $3 }
  | expr AMPERSAND expr
      { mkinfix $1 "&" $3 }
  | expr AMPERAMPER expr
      { mkinfix $1 "&&" $3 }
  | expr COLONEQUAL expr
      { mkinfix $1 ":=" $3 }
  | subtractive expr %prec prec_unary_minus
      { mkuminus $1 $2 }
  | additive expr %prec prec_unary_plus
      { mkuplus $1 $2 }
  | simple_expr DOT label_longident LESSMINUS expr
      { mkexp(Pexp_setfield($1, mkrhs $3 3, $5)) }
  | simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS expr
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "set")),
                         ["",$1; "",$4; "",$7])) }
  | simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS expr
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "String" "set")),
                         ["",$1; "",$4; "",$7])) }
  | simple_expr DOT LBRACE expr RBRACE LESSMINUS expr
      { bigarray_set $1 $4 $7 }
  | label LESSMINUS expr
      { mkexp(Pexp_setinstvar(mkrhs $1 1, $3)) }
  | ASSERT ext_attributes simple_expr %prec below_SHARP
      { mkexp_attrs (Pexp_assert $3) $2 }
  | LAZY ext_attributes simple_expr %prec below_SHARP
      { mkexp_attrs (Pexp_lazy $3) $2 }
  | OBJECT ext_attributes class_structure END
      { mkexp_attrs (Pexp_object $3) $2 }
  | OBJECT ext_attributes class_structure error
      { unclosed "object" 1 "end" 4 }
  | expr attribute
      { Exp.attr $1 $2 }
;
simple_expr:
    val_longident
      { mkexp(Pexp_ident (mkrhs $1 1)) }
  | constant
      { mkexp(Pexp_constant $1) }
  | constr_longident %prec prec_constant_constructor
      { mkexp(Pexp_construct(mkrhs $1 1, None)) }
  | name_tag %prec prec_constant_constructor
      { mkexp(Pexp_variant($1, None)) }
  | LPAREN seq_expr RPAREN
      { reloc_exp $2 }
  | LPAREN seq_expr error
      { unclosed "(" 1 ")" 3 }
  | BEGIN ext_attributes seq_expr END
      { wrap_exp_attrs (reloc_exp $3) $2 (* check location *) }
  | BEGIN ext_attributes END
      { mkexp_attrs (Pexp_construct (mkloc (Lident "()") (symbol_rloc ()),
                               None)) $2 }
  | BEGIN ext_attributes seq_expr error
      { unclosed "begin" 1 "end" 3 }
  | LPAREN seq_expr type_constraint RPAREN
      { mkexp_constraint $2 $3 }
  | simple_expr DOT label_longident
      { mkexp(Pexp_field($1, mkrhs $3 3)) }
  | mod_longident DOT LPAREN seq_expr RPAREN
      { mkexp(Pexp_open(Fresh, mkrhs $1 1, $4)) }
  | mod_longident DOT LPAREN seq_expr error
      { unclosed "(" 3 ")" 5 }
  | simple_expr DOT LPAREN seq_expr RPAREN
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "Array" "get")),
                         ["",$1; "",$4])) }
  | simple_expr DOT LPAREN seq_expr error
      { unclosed "(" 3 ")" 5 }
  | simple_expr DOT LBRACKET seq_expr RBRACKET
      { mkexp(Pexp_apply(ghexp(Pexp_ident(array_function "String" "get")),
                         ["",$1; "",$4])) }
  | simple_expr DOT LBRACKET seq_expr error
      { unclosed "[" 3 "]" 5 }
  | simple_expr DOT LBRACE expr RBRACE
      { bigarray_get $1 $4 }
  | simple_expr DOT LBRACE expr_comma_list error
      { unclosed "{" 3 "}" 5 }
  | LBRACE record_expr RBRACE
      { let (exten, fields) = $2 in mkexp (Pexp_record(fields, exten)) }
  | LBRACE record_expr error
      { unclosed "{" 1 "}" 3 }
  | mod_longident DOT LBRACE record_expr RBRACE
      { let (exten, fields) = $4 in
        let rec_exp = mkexp(Pexp_record(fields, exten)) in
        mkexp(Pexp_open(Fresh, mkrhs $1 1, rec_exp)) }
  | mod_longident DOT LBRACE record_expr error
      { unclosed "{" 3 "}" 5 }
  | LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { mkexp (Pexp_array(List.rev $2)) }
  | LBRACKETBAR expr_semi_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
  | LBRACKETBAR BARRBRACKET
      { mkexp (Pexp_array []) }
  | mod_longident DOT LBRACKETBAR expr_semi_list opt_semi BARRBRACKET
      { mkexp(Pexp_open(Fresh, mkrhs $1 1, mkexp(Pexp_array(List.rev $4)))) }
  | mod_longident DOT LBRACKETBAR expr_semi_list opt_semi error
      { unclosed "[|" 3 "|]" 6 }
  | LBRACKET expr_semi_list opt_semi RBRACKET
      { reloc_exp (mktailexp (rhs_loc 4) (List.rev $2)) }
  | LBRACKET expr_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
  | mod_longident DOT LBRACKET expr_semi_list opt_semi RBRACKET
      { let list_exp = reloc_exp (mktailexp (rhs_loc 6) (List.rev $4)) in
        mkexp(Pexp_open(Fresh, mkrhs $1 1, list_exp)) }
  | mod_longident DOT LBRACKET expr_semi_list opt_semi error
      { unclosed "[" 3 "]" 6 }
  | PREFIXOP simple_expr
      { mkexp(Pexp_apply(mkoperator $1 1, ["",$2])) }
  | BANG simple_expr
      { mkexp(Pexp_apply(mkoperator "!" 1, ["",$2])) }
  | NEW ext_attributes class_longident
      { mkexp_attrs (Pexp_new(mkrhs $3 3)) $2 }
  | LBRACELESS field_expr_list opt_semi GREATERRBRACE
      { mkexp (Pexp_override(List.rev $2)) }
  | LBRACELESS field_expr_list opt_semi error
      { unclosed "{<" 1 ">}" 4 }
  | LBRACELESS GREATERRBRACE
      { mkexp (Pexp_override [])}
  | mod_longident DOT LBRACELESS field_expr_list opt_semi GREATERRBRACE
      { mkexp(Pexp_open(Fresh, mkrhs $1 1, mkexp (Pexp_override(List.rev $4))))}
  | mod_longident DOT LBRACELESS field_expr_list opt_semi error
      { unclosed "{<" 3 ">}" 6 }
  | simple_expr SHARP label
      { mkexp(Pexp_send($1, $3)) }
  | simple_expr SHARPOP simple_expr
      { mkinfix $1 $2 $3 }
  | LPAREN MODULE module_expr RPAREN
      { mkexp (Pexp_pack $3) }
  | LPAREN MODULE module_expr COLON package_type RPAREN
      { mkexp (Pexp_constraint (ghexp (Pexp_pack $3),
                                ghtyp (Ptyp_package $5))) }
  | LPAREN MODULE module_expr COLON error
      { unclosed "(" 1 ")" 5 }
  | mod_longident DOT LPAREN MODULE module_expr COLON package_type RPAREN
      { mkexp(Pexp_open(Fresh, mkrhs $1 1,
        mkexp (Pexp_constraint (ghexp (Pexp_pack $5),
                                ghtyp (Ptyp_package $7))))) }
  | mod_longident DOT LPAREN MODULE module_expr COLON error
      { unclosed "(" 3 ")" 7 }
  | extension
      { mkexp (Pexp_extension $1) }
;
simple_labeled_expr_list:
    labeled_simple_expr
      { [$1] }
  | simple_labeled_expr_list labeled_simple_expr
      { $2 :: $1 }
;
labeled_simple_expr:
    simple_expr %prec below_SHARP
      { ("", $1) }
  | label_expr
      { $1 }
;
label_expr:
    LABEL simple_expr %prec below_SHARP
      { ($1, $2) }
  | TILDE label_ident
      { $2 }
  | QUESTION label_ident
      { ("?" ^ fst $2, snd $2) }
  | OPTLABEL simple_expr %prec below_SHARP
      { ("?" ^ $1, $2) }
;
label_ident:
    LIDENT   { ($1, mkexp(Pexp_ident(mkrhs (Lident $1) 1))) }
;
lident_list:
    LIDENT                            { [$1] }
  | LIDENT lident_list                { $1 :: $2 }
;
let_binding_body:
    val_ident fun_binding
      { (mkpatvar $1 1, $2) }
  | val_ident COLON typevar_list DOT core_type EQUAL seq_expr
      { (ghpat(Ppat_constraint(mkpatvar $1 1,
                               ghtyp(Ptyp_poly(List.rev $3,$5)))),
         $7) }
  | val_ident COLON TYPE lident_list DOT core_type EQUAL seq_expr
      { let exp, poly = wrap_type_annotation $4 $6 $8 in
        (ghpat(Ppat_constraint(mkpatvar $1 1, poly)), exp) }
  | pattern EQUAL seq_expr
      { ($1, $3) }
  | simple_pattern_not_ident COLON core_type EQUAL seq_expr
      { (ghpat(Ppat_constraint($1, $3)), $5) }
;
let_bindings:
    let_binding                                 { $1 }
  | let_bindings and_let_binding                { addlb $1 $2 }
;
let_binding:
    LET ext_attributes rec_flag let_binding_body post_item_attributes
      { mklbs $2 $3 (mklb $4 $5) }
;
and_let_binding:
    AND let_binding_body post_item_attributes
      { mklb $2 $3 }
;
fun_binding:
    strict_binding
      { $1 }
  | type_constraint EQUAL seq_expr
      { mkexp_constraint $3 $1 }
;
strict_binding:
    EQUAL seq_expr
      { $2 }
  | labeled_simple_pattern fun_binding
      { let (l, o, p) = $1 in ghexp(Pexp_fun(l, o, p, $2)) }
  | LPAREN TYPE LIDENT RPAREN fun_binding
      { mkexp(Pexp_newtype($3, $5)) }
;
match_cases:
    match_case { [$1] }
  | match_cases BAR match_case { $3 :: $1 }
;
match_case:
    pattern MINUSGREATER seq_expr
      { Exp.case $1 $3 }
  | pattern WHEN seq_expr MINUSGREATER seq_expr
      { Exp.case $1 ~guard:$3 $5 }
;
fun_def:
    MINUSGREATER seq_expr                       { $2 }
/* Cf #5939: we used to accept (fun p when e0 -> e) */
  | labeled_simple_pattern fun_def
      {
       let (l,o,p) = $1 in
       ghexp(Pexp_fun(l, o, p, $2))
      }
  | LPAREN TYPE LIDENT RPAREN fun_def
      { mkexp(Pexp_newtype($3, $5)) }
;
expr_comma_list:
    expr_comma_list COMMA expr                  { $3 :: $1 }
  | expr COMMA expr                             { [$3; $1] }
;
record_expr:
    simple_expr WITH lbl_expr_list              { (Some $1, $3) }
  | lbl_expr_list                               { (None, $1) }
;
lbl_expr_list:
     lbl_expr { [$1] }
  |  lbl_expr SEMI lbl_expr_list { $1 :: $3 }
  |  lbl_expr SEMI { [$1] }
;
lbl_expr:
    label_longident EQUAL expr
      { (mkrhs $1 1,$3) }
  | label_longident
      { (mkrhs $1 1, exp_of_label $1 1) }
;
field_expr_list:
    label EQUAL expr
      { [mkrhs $1 1,$3] }
  | field_expr_list SEMI label EQUAL expr
      { (mkrhs $3 3, $5) :: $1 }
;
expr_semi_list:
    expr                                        { [$1] }
  | expr_semi_list SEMI expr                    { $3 :: $1 }
;
type_constraint:
    COLON core_type                             { (Some $2, None) }
  | COLON core_type COLONGREATER core_type      { (Some $2, Some $4) }
  | COLONGREATER core_type                      { (None, Some $2) }
  | COLON error                                 { syntax_error() }
  | COLONGREATER error                          { syntax_error() }
;

/* Patterns */

pattern:
    simple_pattern
      { $1 }
  | pattern AS val_ident
      { mkpat(Ppat_alias($1, mkrhs $3 3)) }
  | pattern AS error
      { expecting 3 "identifier" }
  | pattern_comma_list  %prec below_COMMA
      { mkpat(Ppat_tuple(List.rev $1)) }
  | constr_longident pattern %prec prec_constr_appl
      { mkpat(Ppat_construct(mkrhs $1 1, Some $2)) }
  | name_tag pattern %prec prec_constr_appl
      { mkpat(Ppat_variant($1, Some $2)) }
  | pattern COLONCOLON pattern
      { mkpat_cons (rhs_loc 2) (ghpat(Ppat_tuple[$1;$3])) (symbol_rloc()) }
  | pattern COLONCOLON error
      { expecting 3 "pattern" }
  | LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern RPAREN
      { mkpat_cons (rhs_loc 2) (ghpat(Ppat_tuple[$5;$7])) (symbol_rloc()) }
  | LPAREN COLONCOLON RPAREN LPAREN pattern COMMA pattern error
      { unclosed "(" 4 ")" 8 }
  | pattern BAR pattern
      { mkpat(Ppat_or($1, $3)) }
  | pattern BAR error
      { expecting 3 "pattern" }
  | LAZY simple_pattern
      { mkpat(Ppat_lazy $2) }
  | EXCEPTION pattern %prec prec_constr_appl
      { mkpat(Ppat_exception $2) }
  | pattern attribute
      { Pat.attr $1 $2 }
;
simple_pattern:
    val_ident %prec below_EQUAL
      { mkpat(Ppat_var (mkrhs $1 1)) }
  | simple_pattern_not_ident { $1 }
;
simple_pattern_not_ident:
  | UNDERSCORE
      { mkpat(Ppat_any) }
  | signed_constant
      { mkpat(Ppat_constant $1) }
  | signed_constant DOTDOT signed_constant
      { mkpat(Ppat_interval ($1, $3)) }
  | constr_longident
      { mkpat(Ppat_construct(mkrhs $1 1, None)) }
  | name_tag
      { mkpat(Ppat_variant($1, None)) }
  | SHARP type_longident
      { mkpat(Ppat_type (mkrhs $2 2)) }
  | LBRACE lbl_pattern_list RBRACE
      { let (fields, closed) = $2 in mkpat(Ppat_record(fields, closed)) }
  | LBRACE lbl_pattern_list error
      { unclosed "{" 1 "}" 3 }
  | LBRACKET pattern_semi_list opt_semi RBRACKET
      { reloc_pat (mktailpat (rhs_loc 4) (List.rev $2)) }
  | LBRACKET pattern_semi_list opt_semi error
      { unclosed "[" 1 "]" 4 }
  | LBRACKETBAR pattern_semi_list opt_semi BARRBRACKET
      { mkpat(Ppat_array(List.rev $2)) }
  | LBRACKETBAR BARRBRACKET
      { mkpat(Ppat_array []) }
  | LBRACKETBAR pattern_semi_list opt_semi error
      { unclosed "[|" 1 "|]" 4 }
  | LPAREN pattern RPAREN
      { reloc_pat $2 }
  | LPAREN pattern error
      { unclosed "(" 1 ")" 3 }
  | LPAREN pattern COLON core_type RPAREN
      { mkpat(Ppat_constraint($2, $4)) }
  | LPAREN pattern COLON core_type error
      { unclosed "(" 1 ")" 5 }
  | LPAREN pattern COLON error
      { expecting 4 "type" }
  | LPAREN MODULE UIDENT RPAREN
      { mkpat(Ppat_unpack (mkrhs $3 3)) }
  | LPAREN MODULE UIDENT COLON package_type RPAREN
      { mkpat(Ppat_constraint(mkpat(Ppat_unpack (mkrhs $3 3)),
                              ghtyp(Ptyp_package $5))) }
  | LPAREN MODULE UIDENT COLON package_type error
      { unclosed "(" 1 ")" 6 }
  | extension
      { mkpat(Ppat_extension $1) }
;

pattern_comma_list:
    pattern_comma_list COMMA pattern            { $3 :: $1 }
  | pattern COMMA pattern                       { [$3; $1] }
  | pattern COMMA error                         { expecting 3 "pattern" }
;
pattern_semi_list:
    pattern                                     { [$1] }
  | pattern_semi_list SEMI pattern              { $3 :: $1 }
;
lbl_pattern_list:
    lbl_pattern { [$1], Closed }
  | lbl_pattern SEMI { [$1], Closed }
  | lbl_pattern SEMI UNDERSCORE opt_semi { [$1], Open }
  | lbl_pattern SEMI lbl_pattern_list
      { let (fields, closed) = $3 in $1 :: fields, closed }
;
lbl_pattern:
    label_longident EQUAL pattern
      { (mkrhs $1 1,$3) }
  | label_longident
      { (mkrhs $1 1, pat_of_label $1 1) }
;

/* Value descriptions */

value_description:
    VAL val_ident COLON core_type post_item_attributes
      { Val.mk (mkrhs $2 2) $4 ~attrs:$5
               ~loc:(symbol_rloc()) ~docs:(symbol_docs ()) }
;

/* Primitive declarations */

primitive_declaration_body:
    STRING                                      { [fst $1] }
  | STRING primitive_declaration_body           { fst $1 :: $2 }
;
primitive_declaration:
    EXTERNAL val_ident COLON core_type EQUAL primitive_declaration_body
    post_item_attributes
      { Val.mk (mkrhs $2 2) $4 ~prim:$6 ~attrs:$7
               ~loc:(symbol_rloc ()) ~docs:(symbol_docs ()) }
;

/* Type declarations */

type_declarations:
    type_declaration                            { [$1] }
  | type_declarations and_type_declaration      { $2 :: $1 }
;

type_declaration:
    TYPE nonrec_flag optional_type_parameters LIDENT type_kind constraints
    post_item_attributes
      { let (kind, priv, manifest) = $5 in
          Type.mk (mkrhs $4 4) ~params:$3 ~cstrs:(List.rev $6) ~kind
            ~priv ?manifest ~attrs:(add_nonrec $2 $7 2)
            ~loc:(symbol_rloc ()) ~docs:(symbol_docs ()) }
;
and_type_declaration:
    AND optional_type_parameters LIDENT type_kind constraints
    post_item_attributes
      { let (kind, priv, manifest) = $4 in
          Type.mk (mkrhs $3 3) ~params:$2 ~cstrs:(List.rev $5)
            ~kind ~priv ?manifest ~attrs:$6 ~loc:(symbol_rloc ())
            ~text:(symbol_text ()) ~docs:(symbol_docs ()) }
;
constraints:
        constraints CONSTRAINT constrain        { $3 :: $1 }
      | /* empty */                             { [] }
;
type_kind:
    /*empty*/
      { (Ptype_abstract, Public, None) }
  | EQUAL core_type
      { (Ptype_abstract, Public, Some $2) }
  | EQUAL PRIVATE core_type
      { (Ptype_abstract, Private, Some $3) }
  | EQUAL constructor_declarations
      { (Ptype_variant(List.rev $2), Public, None) }
  | EQUAL PRIVATE constructor_declarations
      { (Ptype_variant(List.rev $3), Private, None) }
  | EQUAL DOTDOT
      { (Ptype_open, Public, None) }
  | EQUAL private_flag LBRACE label_declarations RBRACE
      { (Ptype_record $4, $2, None) }
  | EQUAL core_type EQUAL private_flag constructor_declarations
      { (Ptype_variant(List.rev $5), $4, Some $2) }
  | EQUAL core_type EQUAL DOTDOT
      { (Ptype_open, Public, Some $2) }
  | EQUAL core_type EQUAL private_flag LBRACE label_declarations RBRACE
      { (Ptype_record $6, $4, Some $2) }
;
optional_type_parameters:
    /*empty*/                                   { [] }
  | optional_type_parameter                     { [$1] }
  | LPAREN optional_type_parameter_list RPAREN  { List.rev $2 }
;
optional_type_parameter:
    type_variance optional_type_variable        { $2, $1 }
;
optional_type_parameter_list:
    optional_type_parameter                              { [$1] }
  | optional_type_parameter_list COMMA optional_type_parameter    { $3 :: $1 }
;
optional_type_variable:
    QUOTE ident                                 { mktyp(Ptyp_var $2) }
  | UNDERSCORE                                  { mktyp(Ptyp_any) }
;


type_parameters:
    /*empty*/                                   { [] }
  | type_parameter                              { [$1] }
  | LPAREN type_parameter_list RPAREN           { List.rev $2 }
;
type_parameter:
    type_variance type_variable                   { $2, $1 }
;
type_variance:
    /* empty */                                 { Invariant }
  | PLUS                                        { Covariant }
  | MINUS                                       { Contravariant }
;
type_variable:
    QUOTE ident                                 { mktyp(Ptyp_var $2) }
;
type_parameter_list:
    type_parameter                              { [$1] }
  | type_parameter_list COMMA type_parameter    { $3 :: $1 }
;
constructor_declarations:
    constructor_declaration                              { [$1] }
  | bar_constructor_declaration                          { [$1] }
  | constructor_declarations bar_constructor_declaration { $2 :: $1 }
;
constructor_declaration:
  | constr_ident generalized_constructor_arguments attributes
      {
       let args,res = $2 in
       Type.constructor (mkrhs $1 1) ~args ?res ~attrs:$3
         ~loc:(symbol_rloc()) ~info:(symbol_info ())
      }
;
bar_constructor_declaration:
  | BAR constr_ident generalized_constructor_arguments attributes
      {
       let args,res = $3 in
       Type.constructor (mkrhs $2 2) ~args ?res ~attrs:$4
         ~loc:(symbol_rloc()) ~info:(symbol_info ())
      }
;
str_exception_declaration:
  | sig_exception_declaration                    { $1 }
  | EXCEPTION constr_ident EQUAL constr_longident attributes
    post_item_attributes
      { Te.rebind (mkrhs $2 2) (mkrhs $4 4) ~attrs:($5 @ $6)
          ~loc:(symbol_rloc()) ~docs:(symbol_docs ()) }
;
sig_exception_declaration:
  | EXCEPTION constr_ident generalized_constructor_arguments attributes
    post_item_attributes
      { let args, res = $3 in
          Te.decl (mkrhs $2 2) ~args ?res ~attrs:($4 @ $5)
            ~loc:(symbol_rloc()) ~docs:(symbol_docs ()) }
;
generalized_constructor_arguments:
    /*empty*/                                   { ([],None) }
  | OF core_type_list_no_attr                   { (List.rev $2,None) }
  | COLON core_type_list_no_attr MINUSGREATER simple_core_type_no_attr
                                                { (List.rev $2,Some $4) }
  | COLON simple_core_type_no_attr
                                                { ([],Some $2) }
;



label_declarations:
    label_declaration                           { [$1] }
  | label_declaration_semi                      { [$1] }
  | label_declaration_semi label_declarations   { $1 :: $2 }
;
label_declaration:
    mutable_flag label COLON poly_type_no_attr attributes
      {
       Type.field (mkrhs $2 2) $4 ~mut:$1 ~attrs:$5
         ~loc:(symbol_rloc()) ~info:(symbol_info ())
      }
;
label_declaration_semi:
    mutable_flag label COLON poly_type_no_attr attributes SEMI attributes
      {
       let info =
         match rhs_info 5 with
         | Some _ as info_before_semi -> info_before_semi
         | None -> symbol_info ()
       in
       Type.field (mkrhs $2 2) $4 ~mut:$1 ~attrs:($5 @ $7)
         ~loc:(symbol_rloc()) ~info
      }
;

/* Type Extensions */

str_type_extension:
  TYPE nonrec_flag optional_type_parameters type_longident
  PLUSEQ private_flag str_extension_constructors post_item_attributes
      { if $2 <> Recursive then not_expecting 2 "nonrec flag";
        Te.mk (mkrhs $4 4) (List.rev $7) ~params:$3 ~priv:$6
          ~attrs:$8 ~docs:(symbol_docs ()) }
;
sig_type_extension:
  TYPE nonrec_flag optional_type_parameters type_longident
  PLUSEQ private_flag sig_extension_constructors post_item_attributes
      { if $2 <> Recursive then not_expecting 2 "nonrec flag";
        Te.mk (mkrhs $4 4) (List.rev $7) ~params:$3 ~priv:$6
          ~attrs:$8 ~docs:(symbol_docs ()) }
;
str_extension_constructors:
    extension_constructor_declaration                     { [$1] }
  | bar_extension_constructor_declaration                 { [$1] }
  | extension_constructor_rebind                          { [$1] }
  | bar_extension_constructor_rebind                      { [$1] }
  | str_extension_constructors bar_extension_constructor_declaration
      { $2 :: $1 }
  | str_extension_constructors bar_extension_constructor_rebind
      { $2 :: $1 }
;
sig_extension_constructors:
    extension_constructor_declaration                     { [$1] }
  | bar_extension_constructor_declaration                 { [$1] }
  | sig_extension_constructors bar_extension_constructor_declaration
      { $2 :: $1 }
;
extension_constructor_declaration:
  | constr_ident generalized_constructor_arguments attributes
      { let args, res = $2 in
        Te.decl (mkrhs $1 1) ~args ?res ~attrs:$3
          ~loc:(symbol_rloc()) ~info:(symbol_info ()) }
;
bar_extension_constructor_declaration:
  | BAR constr_ident generalized_constructor_arguments attributes
      { let args, res = $3 in
        Te.decl (mkrhs $2 2) ~args ?res ~attrs:$4
           ~loc:(symbol_rloc()) ~info:(symbol_info ()) }
;
extension_constructor_rebind:
  | constr_ident EQUAL constr_longident attributes
      { Te.rebind (mkrhs $1 1) (mkrhs $3 3) ~attrs:$4
          ~loc:(symbol_rloc()) ~info:(symbol_info ()) }
;
bar_extension_constructor_rebind:
  | BAR constr_ident EQUAL constr_longident attributes
      { Te.rebind (mkrhs $2 2) (mkrhs $4 4) ~attrs:$5
          ~loc:(symbol_rloc()) ~info:(symbol_info ()) }
;

/* "with" constraints (additional type equations over signature components) */

with_constraints:
    with_constraint                             { [$1] }
  | with_constraints AND with_constraint        { $3 :: $1 }
;
with_constraint:
    TYPE type_parameters label_longident with_type_binder core_type_no_attr constraints
      { Pwith_type
          (mkrhs $3 3,
           (Type.mk (mkrhs (Longident.last $3) 3)
              ~params:$2
              ~cstrs:(List.rev $6)
              ~manifest:$5
              ~priv:$4
              ~loc:(symbol_rloc()))) }
    /* used label_longident instead of type_longident to disallow
       functor applications in type path */
  | TYPE type_parameters label COLONEQUAL core_type_no_attr
      { Pwith_typesubst
          (Type.mk (mkrhs $3 3)
             ~params:$2
             ~manifest:$5
             ~loc:(symbol_rloc())) }
  | MODULE mod_longident EQUAL mod_ext_longident
      { Pwith_module (mkrhs $2 2, mkrhs $4 4) }
  | MODULE UIDENT COLONEQUAL mod_ext_longident
      { Pwith_modsubst (mkrhs $2 2, mkrhs $4 4) }
;
with_type_binder:
    EQUAL          { Public }
  | EQUAL PRIVATE  { Private }
;

/* Polymorphic types */

typevar_list:
        QUOTE ident                             { [$2] }
      | typevar_list QUOTE ident                { $3 :: $1 }
;
poly_type:
        core_type
          { $1 }
      | typevar_list DOT core_type
          { mktyp(Ptyp_poly(List.rev $1, $3)) }
;
poly_type_no_attr:
        core_type_no_attr
          { $1 }
      | typevar_list DOT core_type_no_attr
          { mktyp(Ptyp_poly(List.rev $1, $3)) }
;

/* Core types */

core_type:
    core_type_no_attr
      { $1 }
  | core_type attribute
      { Typ.attr $1 $2 }
;
core_type_no_attr:
    core_type2
      { $1 }
  | core_type2 AS QUOTE ident
      { mktyp(Ptyp_alias($1, $4)) }
;
core_type2:
    simple_core_type_or_tuple
      { $1 }
  | QUESTION LIDENT COLON core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("?" ^ $2 , mkoption $4, $6)) }
  | OPTLABEL core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("?" ^ $1 , mkoption $2, $4)) }
  | LIDENT COLON core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow($1, $3, $5)) }
  | core_type2 MINUSGREATER core_type2
      { mktyp(Ptyp_arrow("", $1, $3)) }
;

simple_core_type:
    simple_core_type2  %prec below_SHARP
      { $1 }
  | LPAREN core_type_comma_list RPAREN %prec below_SHARP
      { match $2 with [sty] -> sty | _ -> raise Parse_error }
;

simple_core_type_no_attr:
    simple_core_type2  %prec below_SHARP
      { $1 }
  | LPAREN core_type_comma_list RPAREN %prec below_SHARP
      { match $2 with [sty] -> sty | _ -> raise Parse_error }
;

simple_core_type2:
    QUOTE ident
      { mktyp(Ptyp_var $2) }
  | UNDERSCORE
      { mktyp(Ptyp_any) }
  | type_longident
      { mktyp(Ptyp_constr(mkrhs $1 1, [])) }
  | simple_core_type2 type_longident
      { mktyp(Ptyp_constr(mkrhs $2 2, [$1])) }
  | LPAREN core_type_comma_list RPAREN type_longident
      { mktyp(Ptyp_constr(mkrhs $4 4, List.rev $2)) }
  | LESS meth_list GREATER
      { let (f, c) = $2 in mktyp(Ptyp_object (f, c)) }
  | LESS GREATER
      { mktyp(Ptyp_object ([], Closed)) }
  | SHARP class_longident
      { mktyp(Ptyp_class(mkrhs $2 2, [])) }
  | simple_core_type2 SHARP class_longident
      { mktyp(Ptyp_class(mkrhs $3 3, [$1])) }
  | LPAREN core_type_comma_list RPAREN SHARP class_longident
      { mktyp(Ptyp_class(mkrhs $5 5, List.rev $2)) }
  | LBRACKET tag_field RBRACKET
      { mktyp(Ptyp_variant([$2], Closed, None)) }
/* PR#3835: this is not LR(1), would need lookahead=2
  | LBRACKET simple_core_type RBRACKET
      { mktyp(Ptyp_variant([$2], Closed, None)) }
*/
  | LBRACKET BAR row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, Closed, None)) }
  | LBRACKET row_field BAR row_field_list RBRACKET
      { mktyp(Ptyp_variant($2 :: List.rev $4, Closed, None)) }
  | LBRACKETGREATER opt_bar row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, Open, None)) }
  | LBRACKETGREATER RBRACKET
      { mktyp(Ptyp_variant([], Open, None)) }
  | LBRACKETLESS opt_bar row_field_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, Closed, Some [])) }
  | LBRACKETLESS opt_bar row_field_list GREATER name_tag_list RBRACKET
      { mktyp(Ptyp_variant(List.rev $3, Closed, Some (List.rev $5))) }
  | LPAREN MODULE package_type RPAREN
      { mktyp(Ptyp_package $3) }
  | extension
      { mktyp (Ptyp_extension $1) }
;
package_type:
    mty_longident { (mkrhs $1 1, []) }
  | mty_longident WITH package_type_cstrs { (mkrhs $1 1, $3) }
;
package_type_cstr:
    TYPE label_longident EQUAL core_type { (mkrhs $2 2, $4) }
;
package_type_cstrs:
    package_type_cstr { [$1] }
  | package_type_cstr AND package_type_cstrs { $1::$3 }
;
row_field_list:
    row_field                                   { [$1] }
  | row_field_list BAR row_field                { $3 :: $1 }
;
row_field:
    tag_field                                   { $1 }
  | simple_core_type                            { Rinherit $1 }
;
tag_field:
    name_tag OF opt_ampersand amper_type_list attributes
      { Rtag ($1, $5, $3, List.rev $4) }
  | name_tag attributes
      { Rtag ($1, $2, true, []) }
;
opt_ampersand:
    AMPERSAND                                   { true }
  | /* empty */                                 { false }
;
amper_type_list:
    core_type_no_attr                           { [$1] }
  | amper_type_list AMPERSAND core_type_no_attr { $3 :: $1 }
;
name_tag_list:
    name_tag                                    { [$1] }
  | name_tag_list name_tag                      { $2 :: $1 }
;
simple_core_type_or_tuple:
    simple_core_type %prec below_LBRACKETAT  { $1 }
  | simple_core_type STAR core_type_list
      { mktyp(Ptyp_tuple($1 :: List.rev $3)) }
;
simple_core_type_or_tuple_no_attr:
    simple_core_type_no_attr
      { $1 }
  | simple_core_type_no_attr STAR core_type_list_no_attr
      { mktyp(Ptyp_tuple($1 :: List.rev $3)) }
;
core_type_comma_list:
    core_type                                   { [$1] }
  | core_type_comma_list COMMA core_type        { $3 :: $1 }
;
core_type_list:
    simple_core_type %prec below_LBRACKETAT  { [$1] }
  | core_type_list STAR simple_core_type        { $3 :: $1 }
;
core_type_list_no_attr:
    simple_core_type_no_attr                     { [$1] }
  | core_type_list STAR simple_core_type_no_attr { $3 :: $1 }
;
meth_list:
    field SEMI meth_list                     { let (f, c) = $3 in ($1 :: f, c) }
  | field opt_semi                              { [$1], Closed }
  | DOTDOT                                      { [], Open }
;
field:
    label COLON poly_type_no_attr attributes    { ($1, $4, $3) }
;
label:
    LIDENT                                      { $1 }
;

/* Constants */

constant:
    INT                               { Const_int $1 }
  | CHAR                              { Const_char $1 }
  | STRING                            { let (s, d) = $1 in Const_string (s, d) }
  | FLOAT                             { Const_float $1 }
  | INT32                             { Const_int32 $1 }
  | INT64                             { Const_int64 $1 }
  | NATIVEINT                         { Const_nativeint $1 }
;
signed_constant:
    constant                               { $1 }
  | MINUS INT                              { Const_int(- $2) }
  | MINUS FLOAT                            { Const_float("-" ^ $2) }
  | MINUS INT32                            { Const_int32(Int32.neg $2) }
  | MINUS INT64                            { Const_int64(Int64.neg $2) }
  | MINUS NATIVEINT                        { Const_nativeint(Nativeint.neg $2) }
  | PLUS INT                               { Const_int $2 }
  | PLUS FLOAT                             { Const_float $2 }
  | PLUS INT32                             { Const_int32 $2 }
  | PLUS INT64                             { Const_int64 $2 }
  | PLUS NATIVEINT                         { Const_nativeint $2 }
;

/* Identifiers and long identifiers */

ident:
    UIDENT                                      { $1 }
  | LIDENT                                      { $1 }
;
val_ident:
    LIDENT                                      { $1 }
  | LPAREN operator RPAREN                      { $2 }
  | LPAREN operator error                       { unclosed "(" 1 ")" 3 }
  | LPAREN error                                { expecting 2 "operator" }
  | LPAREN MODULE error                         { expecting 3 "module-expr" }
;
operator:
    PREFIXOP                                    { $1 }
  | INFIXOP0                                    { $1 }
  | INFIXOP1                                    { $1 }
  | INFIXOP2                                    { $1 }
  | INFIXOP3                                    { $1 }
  | INFIXOP4                                    { $1 }
  | SHARPOP                                     { $1 }
  | BANG                                        { "!" }
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
  | STAR                                        { "*" }
  | EQUAL                                       { "=" }
  | LESS                                        { "<" }
  | GREATER                                     { ">" }
  | OR                                          { "or" }
  | BARBAR                                      { "||" }
  | AMPERSAND                                   { "&" }
  | AMPERAMPER                                  { "&&" }
  | COLONEQUAL                                  { ":=" }
  | PLUSEQ                                      { "+=" }
  | PERCENT                                     { "%" }
;
constr_ident:
    UIDENT                                      { $1 }
/*  | LBRACKET RBRACKET                           { "[]" } */
  | LPAREN RPAREN                               { "()" }
  | COLONCOLON                                  { "::" }
/*  | LPAREN COLONCOLON RPAREN                    { "::" } */
  | FALSE                                       { "false" }
  | TRUE                                        { "true" }
;

val_longident:
    val_ident                                   { Lident $1 }
  | mod_longident DOT val_ident                 { Ldot($1, $3) }
;
constr_longident:
    mod_longident       %prec below_DOT         { $1 }
  | LBRACKET RBRACKET                           { Lident "[]" }
  | LPAREN RPAREN                               { Lident "()" }
  | FALSE                                       { Lident "false" }
  | TRUE                                        { Lident "true" }
;
label_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;
type_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
mod_longident:
    UIDENT                                      { Lident $1 }
  | mod_longident DOT UIDENT                    { Ldot($1, $3) }
;
mod_ext_longident:
    UIDENT                                      { Lident $1 }
  | mod_ext_longident DOT UIDENT                { Ldot($1, $3) }
  | mod_ext_longident LPAREN mod_ext_longident RPAREN { lapply $1 $3 }
;
mty_longident:
    ident                                       { Lident $1 }
  | mod_ext_longident DOT ident                 { Ldot($1, $3) }
;
clty_longident:
    LIDENT                                      { Lident $1 }
  | mod_ext_longident DOT LIDENT                { Ldot($1, $3) }
;
class_longident:
    LIDENT                                      { Lident $1 }
  | mod_longident DOT LIDENT                    { Ldot($1, $3) }
;

/* Toplevel directives */

toplevel_directive:
    SHARP ident                 { Ptop_dir($2, Pdir_none) }
  | SHARP ident STRING          { Ptop_dir($2, Pdir_string (fst $3)) }
  | SHARP ident INT             { Ptop_dir($2, Pdir_int $3) }
  | SHARP ident val_longident   { Ptop_dir($2, Pdir_ident $3) }
  | SHARP ident mod_longident   { Ptop_dir($2, Pdir_ident $3) }
  | SHARP ident FALSE           { Ptop_dir($2, Pdir_bool false) }
  | SHARP ident TRUE            { Ptop_dir($2, Pdir_bool true) }
;

/* Miscellaneous */

name_tag:
    BACKQUOTE ident                             { $2 }
;
rec_flag:
    /* empty */                                 { Nonrecursive }
  | REC                                         { Recursive }
;
nonrec_flag:
    /* empty */                                 { Recursive }
  | NONREC                                      { Nonrecursive }
;
direction_flag:
    TO                                          { Upto }
  | DOWNTO                                      { Downto }
;
private_flag:
    /* empty */                                 { Public }
  | PRIVATE                                     { Private }
;
mutable_flag:
    /* empty */                                 { Immutable }
  | MUTABLE                                     { Mutable }
;
virtual_flag:
    /* empty */                                 { Concrete }
  | VIRTUAL                                     { Virtual }
;
private_virtual_flags:
    /* empty */  { Public, Concrete }
  | PRIVATE { Private, Concrete }
  | VIRTUAL { Public, Virtual }
  | PRIVATE VIRTUAL { Private, Virtual }
  | VIRTUAL PRIVATE { Private, Virtual }
;
override_flag:
    /* empty */                                 { Fresh }
  | BANG                                        { Override }
;
opt_bar:
    /* empty */                                 { () }
  | BAR                                         { () }
;
opt_semi:
  | /* empty */                                 { () }
  | SEMI                                        { () }
;
subtractive:
  | MINUS                                       { "-" }
  | MINUSDOT                                    { "-." }
;
additive:
  | PLUS                                        { "+" }
  | PLUSDOT                                     { "+." }
;

/* Attributes and extensions */

single_attr_id:
    LIDENT { $1 }
  | UIDENT { $1 }
  | AND { "and" }
  | AS { "as" }
  | ASSERT { "assert" }
  | BEGIN { "begin" }
  | CLASS { "class" }
  | CONSTRAINT { "constraint" }
  | DO { "do" }
  | DONE { "done" }
  | DOWNTO { "downto" }
  | ELSE { "else" }
  | END { "end" }
  | EXCEPTION { "exception" }
  | EXTERNAL { "external" }
  | FALSE { "false" }
  | FOR { "for" }
  | FUN { "fun" }
  | FUNCTION { "function" }
  | FUNCTOR { "functor" }
  | IF { "if" }
  | IN { "in" }
  | INCLUDE { "include" }
  | INHERIT { "inherit" }
  | INITIALIZER { "initializer" }
  | LAZY { "lazy" }
  | LET { "let" }
  | MATCH { "match" }
  | METHOD { "method" }
  | MODULE { "module" }
  | MUTABLE { "mutable" }
  | NEW { "new" }
  | OBJECT { "object" }
  | OF { "of" }
  | OPEN { "open" }
  | OR { "or" }
  | PRIVATE { "private" }
  | REC { "rec" }
  | SIG { "sig" }
  | STRUCT { "struct" }
  | THEN { "then" }
  | TO { "to" }
  | TRUE { "true" }
  | TRY { "try" }
  | TYPE { "type" }
  | VAL { "val" }
  | VIRTUAL { "virtual" }
  | WHEN { "when" }
  | WHILE { "while" }
  | WITH { "with" }
/* mod/land/lor/lxor/lsl/lsr/asr are not supported for now */
;

attr_id:
    single_attr_id { mkloc $1 (symbol_rloc()) }
  | single_attr_id DOT attr_id { mkloc ($1 ^ "." ^ $3.txt) (symbol_rloc())}
;
attribute:
  LBRACKETAT attr_id payload RBRACKET { ($2, $3) }
;
post_item_attribute:
  LBRACKETATAT attr_id payload RBRACKET { ($2, $3) }
;
floating_attribute:
  LBRACKETATATAT attr_id payload RBRACKET { ($2, $3) }
;
post_item_attributes:
    /* empty */  { [] }
  | post_item_attribute post_item_attributes { $1 :: $2 }
;
attributes:
    /* empty */{ [] }
  | attribute attributes { $1 :: $2 }
;
ext_attributes:
    /* empty */  { None, [] }
  | attribute attributes { None, $1 :: $2 }
  | PERCENT attr_id attributes { Some $2, $3 }
;
extension:
  LBRACKETPERCENT attr_id payload RBRACKET { ($2, $3) }
;
item_extension:
  LBRACKETPERCENTPERCENT attr_id payload RBRACKET { ($2, $3) }
;
payload:
    structure { PStr $1 }
  | COLON core_type { PTyp $2 }
  | QUESTION pattern { PPat ($2, None) }
  | QUESTION pattern WHEN seq_expr { PPat ($2, Some $4) }
;
%%
