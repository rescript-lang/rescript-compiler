'use strict';

var Block = require("../../lib/js/block.js");
var Parsing = require("../../lib/js/parsing.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var yytransl_const = /* array */[
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

var yytransl_block = /* array */[
  257,
  258,
  0
];

var yylhs = "\xff\xff\x01\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\x02\0\0\0";

var yylen = "\x02\0\x02\0\x01\0\x01\0\x03\0\x03\0\x03\0\x03\0\x02\0\x03\0\x02\0";

var yydefred = "\0\0\0\0\0\0\x02\0\x03\0\0\0\0\0\n\0\0\0\b\0\0\0\0\0\0\0\0\0\0\0\x01\0\t\0\0\0\0\0\x06\0\x07\0";

var yydgoto = "\x02\0\x07\0\b\0";

var yysindex = "\xff\xff\x10\xff\0\0\0\0\0\0\x10\xff\x10\xff\0\0\n\0\0\0\x16\xff\x10\xff\x10\xff\x10\xff\x10\xff\0\0\0\0\xff\xfe\xff\xfe\0\0\0\0";

var yyrindex = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x01\0\x03\0\0\0\0\0";

var yygindex = "\0\0\0\0\x02\0";

var yytable = "\x01\0\x04\0\0\0\x05\0\r\0\x0e\0\0\0\t\0\n\0\0\0\x0f\0\0\0\0\0\x11\0\x12\0\x13\0\x14\0\x03\0\x04\0\0\0\x05\0\0\0\0\0\0\0\x06\0\x0b\0\f\0\r\0\x0e\0\0\0\0\0\x10\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x04\0\x04\0\x05\0\x05\0\0\0\0\0\x04\0\0\0\x05\0\x0b\0\f\0\r\0\x0e\0";

var yycheck = "\x01\0\0\0\xff\xff\0\0\x05\x01\x06\x01\xff\xff\x05\0\x06\0\xff\xff\0\0\xff\xff\xff\xff\x0b\0\f\0\r\0\x0e\0\x01\x01\x02\x01\xff\xff\x04\x01\xff\xff\xff\xff\xff\xff\b\x01\x03\x01\x04\x01\x05\x01\x06\x01\xff\xff\xff\xff\t\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x03\x01\x04\x01\x03\x01\x04\x01\xff\xff\xff\xff\t\x01\xff\xff\t\x01\x03\x01\x04\x01\x05\x01\x06\x01";

var yynames_const = "PLUS\0MINUS\0TIMES\0DIVIDE\0UMINUS\0LPAREN\0RPAREN\0EOF\0";

var yynames_block = "NUMERAL\0IDENT\0";

var yyact = /* array */[
  (function () {
      throw [
            Caml_builtin_exceptions.failure,
            "parser"
          ];
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return /* Numeral */Block.__(0, [_1]);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 0);
      return /* Variable */Block.__(6, [_1]);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return /* Plus */Block.__(1, [
                _1,
                _3
              ]);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return /* Minus */Block.__(2, [
                _1,
                _3
              ]);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return /* Times */Block.__(3, [
                _1,
                _3
              ]);
    }),
  (function (__caml_parser_env) {
      var _1 = Parsing.peek_val(__caml_parser_env, 2);
      var _3 = Parsing.peek_val(__caml_parser_env, 0);
      return /* Divide */Block.__(4, [
                _1,
                _3
              ]);
    }),
  (function (__caml_parser_env) {
      var _2 = Parsing.peek_val(__caml_parser_env, 0);
      return /* Negate */Block.__(5, [_2]);
    }),
  (function (__caml_parser_env) {
      return Parsing.peek_val(__caml_parser_env, 1);
    }),
  (function (__caml_parser_env) {
      throw [
            Parsing.YYexit,
            Parsing.peek_val(__caml_parser_env, 0)
          ];
    })
];

var yytables = /* record */[
  /* actions */yyact,
  /* transl_const */yytransl_const,
  /* transl_block */yytransl_block,
  /* lhs */yylhs,
  /* len */yylen,
  /* defred */yydefred,
  /* dgoto */yydgoto,
  /* sindex */yysindex,
  /* rindex */yyrindex,
  /* gindex */yygindex,
  /* tablesize */272,
  /* table */yytable,
  /* check */yycheck,
  /* error_function */Parsing.parse_error,
  /* names_const */yynames_const,
  /* names_block */yynames_block
];

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
