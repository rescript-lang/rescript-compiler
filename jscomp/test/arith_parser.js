// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Parsing = require("../../lib/js/parsing.js");

var yytransl_const = [
  259,
  260,
  261,
  262,
  263,
  264,
  265,
  0,
  0
];

var yytransl_block = [
  257,
  258,
  0
];

var yylhs = "\xff\xff\
\x01\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\x02\x00\
\x02\x00\x00\x00";

var yylen = "\x02\x00\
\x02\x00\x01\x00\x01\x00\x03\x00\x03\x00\x03\x00\x03\x00\x02\x00\
\x03\x00\x02\x00";

var yydefred = "\x00\x00\
\x00\x00\x00\x00\x02\x00\x03\x00\x00\x00\x00\x00\x0a\x00\x00\x00\
\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x09\x00\
\x00\x00\x00\x00\x06\x00\x07\x00";

var yydgoto = "\x02\x00\
\x07\x00\x08\x00";

var yysindex = "\xff\xff\
\x10\xff\x00\x00\x00\x00\x00\x00\x10\xff\x10\xff\x00\x00\x0a\x00\
\x00\x00\x16\xff\x10\xff\x10\xff\x10\xff\x10\xff\x00\x00\x00\x00\
\xff\xfe\xff\xfe\x00\x00\x00\x00";

var yyrindex = "\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x01\x00\x03\x00\x00\x00\x00\x00";

var yygindex = "\x00\x00\
\x00\x00\x02\x00";

var yytable = "\x01\x00\
\x04\x00\x00\x00\x05\x00\x0d\x00\x0e\x00\x00\x00\x09\x00\x0a\x00\
\x00\x00\x0f\x00\x00\x00\x00\x00\x11\x00\x12\x00\x13\x00\x14\x00\
\x03\x00\x04\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x06\x00\
\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x00\x00\x00\x00\x10\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x04\x00\x04\x00\x05\x00\x05\x00\x00\x00\
\x00\x00\x04\x00\x00\x00\x05\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00";

var yycheck = "\x01\x00\
\x00\x00\xff\xff\x00\x00\x05\x01\x06\x01\xff\xff\x05\x00\x06\x00\
\xff\xff\x00\x00\xff\xff\xff\xff\x0b\x00\x0c\x00\x0d\x00\x0e\x00\
\x01\x01\x02\x01\xff\xff\x04\x01\xff\xff\xff\xff\xff\xff\x08\x01\
\x03\x01\x04\x01\x05\x01\x06\x01\xff\xff\xff\xff\x09\x01\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
\xff\xff\xff\xff\xff\xff\x03\x01\x04\x01\x03\x01\x04\x01\xff\xff\
\xff\xff\x09\x01\xff\xff\x09\x01\x03\x01\x04\x01\x05\x01\x06\x01";

var yynames_const = "\
  PLUS\x00\
  MINUS\x00\
  TIMES\x00\
  DIVIDE\x00\
  UMINUS\x00\
  LPAREN\x00\
  RPAREN\x00\
  EOF\x00\
  ";

var yynames_block = "\
  NUMERAL\x00\
  IDENT\x00\
  ";

var yyact = [
  (function (param) {
      throw new Error("Failure", {
                cause: {
                  RE_EXN_ID: "Failure",
                  _1: "parser"
                }
              });
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: "Numeral",
              _0: _1
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: "Variable",
              _0: _1
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: "Plus",
              _0: _1,
              _1: _3
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: "Minus",
              _0: _1,
              _1: _3
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: "Times",
              _0: _1,
              _1: _3
            };
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: "Divide",
              _0: _1,
              _1: _3
            };
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return {
              TAG: "Negate",
              _0: _2
            };
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      throw new Error(Parsing.YYexit, {
                cause: {
                  RE_EXN_ID: Parsing.YYexit,
                  _1: Parsing.peek_val(__caml_parser_env, 0)
                }
              });
    })
];

var yytables = {
  actions: yyact,
  transl_const: yytransl_const,
  transl_block: yytransl_block,
  lhs: yylhs,
  len: yylen,
  defred: yydefred,
  dgoto: yydgoto,
  sindex: yysindex,
  rindex: yyrindex,
  gindex: yygindex,
  tablesize: 272,
  table: yytable,
  check: yycheck,
  error_function: Parsing.parse_error,
  names_const: yynames_const,
  names_block: yynames_block
};

function toplevel(lexfun, lexbuf) {
  return Parsing.yyparse(yytables, 1, lexfun, lexbuf);
}

var yytablesize = 272;

exports.yytransl_const = yytransl_const;
exports.yytransl_block = yytransl_block;
exports.yylhs = yylhs;
exports.yylen = yylen;
exports.yydefred = yydefred;
exports.yydgoto = yydgoto;
exports.yysindex = yysindex;
exports.yyrindex = yyrindex;
exports.yygindex = yygindex;
exports.yytablesize = yytablesize;
exports.yytable = yytable;
exports.yycheck = yycheck;
exports.yynames_const = yynames_const;
exports.yynames_block = yynames_block;
exports.yyact = yyact;
exports.yytables = yytables;
exports.toplevel = toplevel;
/* No side effect */
