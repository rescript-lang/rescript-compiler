'use strict';

var Mt = require("./mt.js");
var Fs = require("fs");
var Sys = require("../../lib/js/sys.js");
var Char = require("../../lib/js/char.js");
var List = require("../../lib/js/list.js");
var Path = require("path");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Queue = require("../../lib/js/queue.js");
var $$Buffer = require("../../lib/js/buffer.js");
var Lexing = require("../../lib/js/lexing.js");
var Printf = require("../../lib/js/printf.js");
var $$String = require("../../lib/js/string.js");
var Hashtbl = require("../../lib/js/hashtbl.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Filename = require("../../lib/js/filename.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");
var Caml_module = require("../../lib/js/caml_module.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var none = /* record */[
  /* source */undefined,
  /* start : record */[
    /* line */0,
    /* column */0,
    /* offset */0
  ],
  /* _end : record */[
    /* line */0,
    /* column */0,
    /* offset */0
  ]
];

function from_lb_p(source, start, _end) {
  return /* record */[
          /* source */source,
          /* start : record */[
            /* line */start[/* pos_lnum */1],
            /* column */start[/* pos_cnum */3] - start[/* pos_bol */2] | 0,
            /* offset */start[/* pos_cnum */3]
          ],
          /* _end : record */[
            /* line */_end[/* pos_lnum */1],
            /* column */Caml_primitive.caml_int_max(0, _end[/* pos_cnum */3] - _end[/* pos_bol */2] | 0),
            /* offset */_end[/* pos_cnum */3]
          ]
        ];
}

function from_lb(source, lb) {
  var start = lb[/* lex_start_p */10];
  var _end = lb[/* lex_curr_p */11];
  return from_lb_p(source, start, _end);
}

function from_curr_lb(source, lb) {
  var curr = lb[/* lex_curr_p */11];
  return from_lb_p(source, curr, curr);
}

function btwn(loc1, loc2) {
  return /* record */[
          /* source */loc1[/* source */0],
          /* start */loc1[/* start */1],
          /* _end */loc2[/* _end */2]
        ];
}

function btwn_exclusive(loc1, loc2) {
  return /* record */[
          /* source */loc1[/* source */0],
          /* start */loc1[/* _end */2],
          /* _end */loc2[/* start */1]
        ];
}

function string_of_filename(param) {
  if (typeof param === "number") {
    return "(global)";
  } else {
    return param[0];
  }
}

function order_of_filename(param) {
  if (typeof param === "number") {
    return 1;
  } else {
    switch (param.tag | 0) {
      case 0 : 
          return 2;
      case 1 : 
      case 2 : 
          return 3;
      
    }
  }
}

function source_cmp(a, b) {
  if (a !== undefined) {
    if (b !== undefined) {
      var fn2 = b;
      var fn1 = a;
      var k = order_of_filename(fn1) - order_of_filename(fn2) | 0;
      if (k !== 0) {
        return k;
      } else {
        return Caml_primitive.caml_string_compare(string_of_filename(fn1), string_of_filename(fn2));
      }
    } else {
      return -1;
    }
  } else if (b !== undefined) {
    return 1;
  } else {
    return 0;
  }
}

function pos_cmp(a, b) {
  return Caml_obj.caml_compare(/* tuple */[
              a[/* line */0],
              a[/* column */1]
            ], /* tuple */[
              b[/* line */0],
              b[/* column */1]
            ]);
}

function compare(loc1, loc2) {
  var k = source_cmp(loc1[/* source */0], loc2[/* source */0]);
  if (k === 0) {
    var k$1 = pos_cmp(loc1[/* start */1], loc2[/* start */1]);
    if (k$1 === 0) {
      return pos_cmp(loc1[/* _end */2], loc2[/* _end */2]);
    } else {
      return k$1;
    }
  } else {
    return k;
  }
}

var $$Error = Caml_exceptions.create("Flow_parser_reg_test.Parse_error.Error");

function error(param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
          return "Unexpected number";
      case 1 : 
          return "Unexpected string";
      case 2 : 
          return "Unexpected identifier";
      case 3 : 
          return "Unexpected reserved word";
      case 4 : 
          return "Unexpected end of input";
      case 5 : 
          return "Type aliases are not allowed in untyped mode";
      case 6 : 
          return "Type annotations are not allowed in untyped mode";
      case 7 : 
          return "Type declarations are not allowed in untyped mode";
      case 8 : 
          return "Type imports are not allowed in untyped mode";
      case 9 : 
          return "Type exports are not allowed in untyped mode";
      case 10 : 
          return "Interfaces are not allowed in untyped mode";
      case 11 : 
          return "Illegal newline after throw";
      case 12 : 
          return "Invalid regular expression";
      case 13 : 
          return "Invalid regular expression: missing /";
      case 14 : 
          return "Invalid left-hand side in assignment";
      case 15 : 
          return "Invalid left-hand side in exponentiation expression";
      case 16 : 
          return "Invalid left-hand side in for-in";
      case 17 : 
          return "Invalid left-hand side in for-of";
      case 18 : 
          return "Expected an object pattern, array pattern, or an identifier but found an expression instead";
      case 19 : 
          return "More than one default clause in switch statement";
      case 20 : 
          return "Missing catch or finally after try";
      case 21 : 
          return "Illegal continue statement";
      case 22 : 
          return "Illegal break statement";
      case 23 : 
          return "Illegal return statement";
      case 24 : 
          return "Illegal yield expression";
      case 25 : 
          return "Strict mode code may not include a with statement";
      case 26 : 
          return "Catch variable may not be eval or arguments in strict mode";
      case 27 : 
          return "Variable name may not be eval or arguments in strict mode";
      case 28 : 
          return "Parameter name eval or arguments is not allowed in strict mode";
      case 29 : 
          return "Strict mode function may not have duplicate parameter names";
      case 30 : 
          return "Function name may not be eval or arguments in strict mode";
      case 31 : 
          return "Octal literals are not allowed in strict mode.";
      case 32 : 
          return "Delete of an unqualified identifier in strict mode.";
      case 33 : 
          return "Duplicate data property in object literal not allowed in strict mode";
      case 34 : 
          return "Object literal may not have data and accessor property with the same name";
      case 35 : 
          return "Object literal may not have multiple get/set accessors with the same name";
      case 36 : 
          return "Assignment to eval or arguments is not allowed in strict mode";
      case 37 : 
          return "Postfix increment/decrement may not have eval or arguments operand in strict mode";
      case 38 : 
          return "Prefix increment/decrement may not have eval or arguments operand in strict mode";
      case 39 : 
          return "Use of future reserved word in strict mode";
      case 40 : 
          return "JSX attributes must only be assigned a non-empty expression";
      case 41 : 
          return "JSX value should be either an expression or a quoted JSX text";
      case 42 : 
          return "Const must be initialized";
      case 43 : 
          return "Destructuring assignment must be initialized";
      case 44 : 
          return "Illegal newline before arrow";
      case 45 : 
          return "In strict mode code, functions can only be declared at top level or immediately within another function.";
      case 46 : 
          return "Unexpected token <. Remember, adjacent JSX elements must be wrapped in an enclosing parent tag";
      case 47 : 
          return "Rest parameter must be final parameter of an argument list";
      case 48 : 
          return "A function may not be both async and a generator";
      case 49 : 
          return "async is an implementation detail and isn't necessary for your declare function statement. It is sufficient for your declare function to just have a Promise return type.";
      case 50 : 
          return "`declare export let` is not supported. Use `declare export var` instead.";
      case 51 : 
          return "`declare export const` is not supported. Use `declare export var` instead.";
      case 52 : 
          return "`declare export type` is not supported. Use `export type` instead.";
      case 53 : 
          return "`declare export interface` is not supported. Use `export interface` instead.";
      case 54 : 
          return "`export * as` is an early-stage proposal and is not enabled by default. To enable support in the parser, use the `esproposal_export_star_as` option";
      case 55 : 
          return "When exporting a class as a named export, you must specify a class name. Did you mean `export default class ...`?";
      case 56 : 
          return "When exporting a function as a named export, you must specify a function name. Did you mean `export default function ...`?";
      case 57 : 
          return "Found a decorator in an unsupported position.";
      case 58 : 
          return "Type parameter declaration needs a default, since a preceding type parameter declaration has a default.";
      case 59 : 
          return "The Windows version of OCaml has a bug in how it parses hexidecimal numbers. It is fixed in OCaml 4.03.0. Until we can switch to 4.03.0, please avoid either hexidecimal notation or Windows.";
      case 60 : 
          return "Duplicate `declare module.exports` statement!";
      case 61 : 
          return "Found both `declare module.exports` and `declare export` in the same module. Modules can only have 1 since they are either an ES module xor they are a CommonJS module.";
      
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
          return "Unexpected parser state: " + param[0];
      case 1 : 
          return "Unexpected token " + param[0];
      case 2 : 
          return Curry._2(Printf.sprintf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "Unexpected token `",
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* String_literal */Block.__(11, [
                                      "`. Did you mean `",
                                      /* String */Block.__(2, [
                                          /* No_padding */0,
                                          /* String_literal */Block.__(11, [
                                              "`?",
                                              /* End_of_format */0
                                            ])
                                        ])
                                    ])
                                ])
                            ]),
                          "Unexpected token `%s`. Did you mean `%s`?"
                        ]), param[0], param[1]);
      case 3 : 
          return "Invalid flags supplied to RegExp constructor '" + (param[0] + "'");
      case 4 : 
          return "Undefined label '" + (param[0] + "'");
      case 5 : 
          return param[0] + (" '" + (param[1] + "' has already been declared"));
      case 6 : 
          return "Expected corresponding JSX closing tag for " + param[0];
      case 7 : 
          return Curry._1(Printf.sprintf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "Duplicate export for `",
                              /* String */Block.__(2, [
                                  /* No_padding */0,
                                  /* Char_literal */Block.__(12, [
                                      /* "`" */96,
                                      /* End_of_format */0
                                    ])
                                ])
                            ]),
                          "Duplicate export for `%s`"
                        ]), param[0]);
      
    }
  }
}

var Literal = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      44,
      6
    ], [[[[]]]]);

var Type = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      191,
      6
    ], [[
        [[[[]]]],
        [[
            [[]],
            [[]],
            [[]]
          ]],
        [[[[]]]],
        [[]],
        [[]],
        [[]],
        [[[[[[]]]]]],
        [[]]
      ]]);

var Statement = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      493,
      6
    ], [[
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[[[]]]],
        [[]],
        [[]],
        [[[[]]]],
        [[[[]]]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[[[]]]],
        [[]],
        [[[[]]]],
        [[]]
      ]]);

var Expression = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      758,
      6
    ], [[
        [[]],
        [[]],
        [[[[]]]],
        [[]],
        [[
            [[]],
            [[]]
          ]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[[[]]]],
        [[]],
        [[]],
        [[]]
      ]]);

var JSX = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      861,
      6
    ], [[
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]]
      ]]);

var Pattern = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      919,
      6
    ], [[
        [[
            [[]],
            [[]]
          ]],
        [[[[]]]],
        [[]]
      ]]);

var Class = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      978,
      6
    ], [[
        [[]],
        [[]],
        [[]],
        [[]]
      ]]);

Caml_module.update_mod([[[[]]]], Literal, Literal);

Caml_module.update_mod([[
        [[[[]]]],
        [[
            [[]],
            [[]],
            [[]]
          ]],
        [[[[]]]],
        [[]],
        [[]],
        [[]],
        [[[[[[]]]]]],
        [[]]
      ]], Type, Type);

Caml_module.update_mod([[
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[[[]]]],
        [[]],
        [[]],
        [[[[]]]],
        [[[[]]]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[[[]]]],
        [[]],
        [[[[]]]],
        [[]]
      ]], Statement, Statement);

Caml_module.update_mod([[
        [[]],
        [[]],
        [[[[]]]],
        [[]],
        [[
            [[]],
            [[]]
          ]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[[[]]]],
        [[]],
        [[]],
        [[]]
      ]], Expression, Expression);

Caml_module.update_mod([[
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]],
        [[]]
      ]], JSX, JSX);

Caml_module.update_mod([[
        [[
            [[]],
            [[]]
          ]],
        [[[[]]]],
        [[]]
      ]], Pattern, Pattern);

Caml_module.update_mod([[
        [[]],
        [[]],
        [[]],
        [[]]
      ]], Class, Class);

function token_to_string(param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
          return "T_IDENTIFIER";
      case 1 : 
          return "T_LCURLY";
      case 2 : 
          return "T_RCURLY";
      case 3 : 
          return "T_LPAREN";
      case 4 : 
          return "T_RPAREN";
      case 5 : 
          return "T_LBRACKET";
      case 6 : 
          return "T_RBRACKET";
      case 7 : 
          return "T_SEMICOLON";
      case 8 : 
          return "T_COMMA";
      case 9 : 
          return "T_PERIOD";
      case 10 : 
          return "T_ARROW";
      case 11 : 
          return "T_ELLIPSIS";
      case 12 : 
          return "T_AT";
      case 13 : 
          return "T_FUNCTION";
      case 14 : 
          return "T_IF";
      case 15 : 
          return "T_IN";
      case 16 : 
          return "T_INSTANCEOF";
      case 17 : 
          return "T_RETURN";
      case 18 : 
          return "T_SWITCH";
      case 19 : 
          return "T_THIS";
      case 20 : 
          return "T_THROW";
      case 21 : 
          return "T_TRY";
      case 22 : 
          return "T_VAR";
      case 23 : 
          return "T_WHILE";
      case 24 : 
          return "T_WITH";
      case 25 : 
          return "T_CONST";
      case 26 : 
          return "T_LET";
      case 27 : 
          return "T_NULL";
      case 28 : 
          return "T_FALSE";
      case 29 : 
          return "T_TRUE";
      case 30 : 
          return "T_BREAK";
      case 31 : 
          return "T_CASE";
      case 32 : 
          return "T_CATCH";
      case 33 : 
          return "T_CONTINUE";
      case 34 : 
          return "T_DEFAULT";
      case 35 : 
          return "T_DO";
      case 36 : 
          return "T_FINALLY";
      case 37 : 
          return "T_FOR";
      case 38 : 
          return "T_CLASS";
      case 39 : 
          return "T_EXTENDS";
      case 40 : 
          return "T_STATIC";
      case 41 : 
          return "T_ELSE";
      case 42 : 
          return "T_NEW";
      case 43 : 
          return "T_DELETE";
      case 44 : 
          return "T_TYPEOF";
      case 45 : 
          return "T_VOID";
      case 46 : 
          return "T_ENUM";
      case 47 : 
          return "T_EXPORT";
      case 48 : 
          return "T_IMPORT";
      case 49 : 
          return "T_SUPER";
      case 50 : 
          return "T_IMPLEMENTS";
      case 51 : 
          return "T_INTERFACE";
      case 52 : 
          return "T_PACKAGE";
      case 53 : 
          return "T_PRIVATE";
      case 54 : 
          return "T_PROTECTED";
      case 55 : 
          return "T_PUBLIC";
      case 56 : 
          return "T_YIELD";
      case 57 : 
          return "T_DEBUGGER";
      case 58 : 
          return "T_DECLARE";
      case 59 : 
          return "T_TYPE";
      case 60 : 
          return "T_OF";
      case 61 : 
          return "T_ASYNC";
      case 62 : 
          return "T_AWAIT";
      case 63 : 
          return "T_RSHIFT3_ASSIGN";
      case 64 : 
          return "T_RSHIFT_ASSIGN";
      case 65 : 
          return "T_LSHIFT_ASSIGN";
      case 66 : 
          return "T_BIT_XOR_ASSIGN";
      case 67 : 
          return "T_BIT_OR_ASSIGN";
      case 68 : 
          return "T_BIT_AND_ASSIGN";
      case 69 : 
          return "T_MOD_ASSIGN";
      case 70 : 
          return "T_DIV_ASSIGN";
      case 71 : 
          return "T_MULT_ASSIGN";
      case 72 : 
          return "T_EXP_ASSIGN";
      case 73 : 
          return "T_MINUS_ASSIGN";
      case 74 : 
          return "T_PLUS_ASSIGN";
      case 75 : 
          return "T_ASSIGN";
      case 76 : 
          return "T_PLING";
      case 77 : 
          return "T_COLON";
      case 78 : 
          return "T_OR";
      case 79 : 
          return "T_AND";
      case 80 : 
          return "T_BIT_OR";
      case 81 : 
          return "T_BIT_XOR";
      case 82 : 
          return "T_BIT_AND";
      case 83 : 
          return "T_EQUAL";
      case 84 : 
          return "T_NOT_EQUAL";
      case 85 : 
          return "T_STRICT_EQUAL";
      case 86 : 
          return "T_STRICT_NOT_EQUAL";
      case 87 : 
          return "T_LESS_THAN_EQUAL";
      case 88 : 
          return "T_GREATER_THAN_EQUAL";
      case 89 : 
          return "T_LESS_THAN";
      case 90 : 
          return "T_GREATER_THAN";
      case 91 : 
          return "T_LSHIFT";
      case 92 : 
          return "T_RSHIFT";
      case 93 : 
          return "T_RSHIFT3";
      case 94 : 
          return "T_PLUS";
      case 95 : 
          return "T_MINUS";
      case 96 : 
          return "T_DIV";
      case 97 : 
          return "T_MULT";
      case 98 : 
          return "T_EXP";
      case 99 : 
          return "T_MOD";
      case 100 : 
          return "T_NOT";
      case 101 : 
          return "T_BIT_NOT";
      case 102 : 
          return "T_INCR";
      case 103 : 
          return "T_DECR";
      case 104 : 
          return "T_ERROR";
      case 105 : 
          return "T_EOF";
      case 106 : 
          return "T_JSX_IDENTIFIER";
      case 107 : 
          return "T_ANY_TYPE";
      case 108 : 
          return "T_BOOLEAN_TYPE";
      case 109 : 
          return "T_NUMBER_TYPE";
      case 110 : 
          return "T_STRING_TYPE";
      case 111 : 
          return "T_VOID_TYPE";
      
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
          return "T_NUMBER";
      case 1 : 
          return "T_STRING";
      case 2 : 
          return "T_TEMPLATE_PART";
      case 3 : 
          return "T_REGEXP";
      case 4 : 
          return "T_JSX_TEXT";
      case 5 : 
          return "T_NUMBER_SINGLETON_TYPE";
      
    }
  }
}

function yyback(n, lexbuf) {
  lexbuf[/* lex_curr_pos */5] = lexbuf[/* lex_curr_pos */5] - n | 0;
  var currp = lexbuf[/* lex_curr_p */11];
  lexbuf[/* lex_curr_p */11] = /* record */[
    /* pos_fname */currp[/* pos_fname */0],
    /* pos_lnum */currp[/* pos_lnum */1],
    /* pos_bol */currp[/* pos_bol */2],
    /* pos_cnum */currp[/* pos_cnum */3] - n | 0
  ];
  return /* () */0;
}

function back(lb) {
  var n = lb[/* lex_curr_p */11][/* pos_cnum */3] - lb[/* lex_start_p */10][/* pos_cnum */3] | 0;
  return yyback(n, lb);
}

var empty_lex_state = /* record */[
  /* lex_errors_acc : [] */0,
  /* lex_comments_acc : [] */0
];

function new_lex_env(lex_source, lex_lb, enable_types_in_comments) {
  return /* record */[
          /* lex_source */lex_source,
          /* lex_lb */lex_lb,
          /* lex_in_comment_syntax */false,
          /* lex_enable_comment_syntax */enable_types_in_comments,
          /* lex_state */empty_lex_state
        ];
}

function get_and_clear_state(env) {
  var state = env[/* lex_state */4];
  var env$1 = state !== empty_lex_state ? /* record */[
      /* lex_source */env[/* lex_source */0],
      /* lex_lb */env[/* lex_lb */1],
      /* lex_in_comment_syntax */env[/* lex_in_comment_syntax */2],
      /* lex_enable_comment_syntax */env[/* lex_enable_comment_syntax */3],
      /* lex_state */empty_lex_state
    ] : env;
  return /* tuple */[
          env$1,
          state
        ];
}

function with_lexbuf(lexbuf, env) {
  return /* record */[
          /* lex_source */env[/* lex_source */0],
          /* lex_lb */lexbuf,
          /* lex_in_comment_syntax */env[/* lex_in_comment_syntax */2],
          /* lex_enable_comment_syntax */env[/* lex_enable_comment_syntax */3],
          /* lex_state */env[/* lex_state */4]
        ];
}

function in_comment_syntax(is_in, env) {
  if (is_in !== env[/* lex_in_comment_syntax */2]) {
    return /* record */[
            /* lex_source */env[/* lex_source */0],
            /* lex_lb */env[/* lex_lb */1],
            /* lex_in_comment_syntax */is_in,
            /* lex_enable_comment_syntax */env[/* lex_enable_comment_syntax */3],
            /* lex_state */env[/* lex_state */4]
          ];
  } else {
    return env;
  }
}

function get_result_and_clear_state(param) {
  var lex_token = param[1];
  var match = get_and_clear_state(param[0]);
  var state = match[1];
  var env = match[0];
  var match$1;
  var exit = 0;
  if (typeof lex_token === "number") {
    exit = 2;
  } else {
    switch (lex_token.tag | 0) {
      case 2 : 
          var match$2 = lex_token[0];
          match$1 = /* tuple */[
            match$2[0],
            match$2[1][/* literal */2]
          ];
          break;
      case 3 : 
          var match$3 = lex_token[0];
          match$1 = /* tuple */[
            match$3[0],
            "/" + (match$3[1] + ("/" + match$3[2]))
          ];
          break;
      case 1 : 
      case 4 : 
          exit = 1;
          break;
      default:
        exit = 2;
    }
  }
  switch (exit) {
    case 1 : 
        var match$4 = lex_token[0];
        match$1 = /* tuple */[
          match$4[0],
          match$4[2]
        ];
        break;
    case 2 : 
        match$1 = /* tuple */[
          from_lb(env[/* lex_source */0], env[/* lex_lb */1]),
          Lexing.lexeme(env[/* lex_lb */1])
        ];
        break;
    
  }
  return /* tuple */[
          env,
          /* record */[
            /* lex_token */lex_token,
            /* lex_loc */match$1[0],
            /* lex_value */match$1[1],
            /* lex_errors */List.rev(state[/* lex_errors_acc */0]),
            /* lex_comments */List.rev(state[/* lex_comments_acc */1])
          ]
        ];
}

function lex_error(env, loc, err) {
  var lex_errors_acc_000 = /* tuple */[
    loc,
    err
  ];
  var lex_errors_acc_001 = env[/* lex_state */4][/* lex_errors_acc */0];
  var lex_errors_acc = /* :: */[
    lex_errors_acc_000,
    lex_errors_acc_001
  ];
  var init = env[/* lex_state */4];
  return /* record */[
          /* lex_source */env[/* lex_source */0],
          /* lex_lb */env[/* lex_lb */1],
          /* lex_in_comment_syntax */env[/* lex_in_comment_syntax */2],
          /* lex_enable_comment_syntax */env[/* lex_enable_comment_syntax */3],
          /* lex_state : record */[
            /* lex_errors_acc */lex_errors_acc,
            /* lex_comments_acc */init[/* lex_comments_acc */1]
          ]
        ];
}

function unexpected_error(env, loc, value) {
  return lex_error(env, loc, /* UnexpectedToken */Block.__(1, [value]));
}

function unexpected_error_w_suggest(env, loc, value, suggest) {
  return lex_error(env, loc, /* UnexpectedTokenWithSuggestion */Block.__(2, [
                value,
                suggest
              ]));
}

function illegal_number(env, lexbuf, word, token) {
  var loc = from_lb(env[/* lex_source */0], lexbuf);
  yyback(word.length, lexbuf);
  var env$1 = lex_error(env, loc, /* UnexpectedToken */Block.__(1, ["ILLEGAL"]));
  return /* tuple */[
          env$1,
          token
        ];
}

var No_good = Caml_exceptions.create("Flow_parser_reg_test.Lexer_flow.FloatOfString.No_good");

function eat(f) {
  var match = f[/* todo */4];
  if (match) {
    return /* record */[
            /* negative */f[/* negative */0],
            /* mantissa */f[/* mantissa */1],
            /* exponent */f[/* exponent */2],
            /* decimal_exponent */f[/* decimal_exponent */3],
            /* todo */match[1]
          ];
  } else {
    throw No_good;
  }
}

function start(str) {
  var todo = /* record */[/* contents : [] */0];
  $$String.iter((function (c) {
          todo[0] = /* :: */[
            c,
            todo[0]
          ];
          return /* () */0;
        }), str);
  return /* record */[
          /* negative */false,
          /* mantissa */0,
          /* exponent */0,
          /* decimal_exponent */undefined,
          /* todo */List.rev(todo[0])
        ];
}

function parse_sign(f) {
  var match = f[/* todo */4];
  if (match) {
    switch (match[0]) {
      case 43 : 
          return eat(f);
      case 44 : 
          return f;
      case 45 : 
          var init = eat(f);
          return /* record */[
                  /* negative */true,
                  /* mantissa */init[/* mantissa */1],
                  /* exponent */init[/* exponent */2],
                  /* decimal_exponent */init[/* decimal_exponent */3],
                  /* todo */init[/* todo */4]
                ];
      default:
        return f;
    }
  } else {
    return f;
  }
}

function parse_hex_symbol(f) {
  var match = f[/* todo */4];
  if (match) {
    if (match[0] !== 48) {
      throw No_good;
    }
    var match$1 = match[1];
    if (match$1) {
      var match$2 = match$1[0];
      if (match$2 !== 88) {
        if (match$2 !== 120) {
          throw No_good;
        }
        return eat(eat(f));
      } else {
        return eat(eat(f));
      }
    } else {
      throw No_good;
    }
  } else {
    throw No_good;
  }
}

function parse_exponent(f) {
  var todo_str = $$String.concat("", List.map(Char.escaped, f[/* todo */4]));
  var exponent;
  try {
    exponent = Caml_format.caml_int_of_string(todo_str);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === Caml_builtin_exceptions.failure) {
      throw No_good;
    }
    throw exn;
  }
  return /* record */[
          /* negative */f[/* negative */0],
          /* mantissa */f[/* mantissa */1],
          /* exponent */exponent,
          /* decimal_exponent */f[/* decimal_exponent */3],
          /* todo : [] */0
        ];
}

function parse_body(_f) {
  while(true) {
    var f = _f;
    var match = f[/* todo */4];
    if (match) {
      var c = match[0];
      var exit = 0;
      if (c >= 81) {
        if (c !== 95) {
          if (c !== 112) {
            exit = 1;
          } else {
            return parse_exponent(eat(f));
          }
        } else {
          _f = eat(f);
          continue ;
        }
      } else if (c !== 46) {
        if (c >= 80) {
          return parse_exponent(eat(f));
        } else {
          exit = 1;
        }
      } else if (f[/* decimal_exponent */3] === undefined) {
        var init = eat(f);
        _f = /* record */[
          /* negative */init[/* negative */0],
          /* mantissa */init[/* mantissa */1],
          /* exponent */init[/* exponent */2],
          /* decimal_exponent */0,
          /* todo */init[/* todo */4]
        ];
        continue ;
      } else {
        throw No_good;
      }
      if (exit === 1) {
        var ref_char_code;
        if (c >= /* "0" */48 && c <= /* "9" */57) {
          ref_char_code = /* "0" */48;
        } else if (c >= /* "A" */65 && c <= /* "F" */70) {
          ref_char_code = 55;
        } else if (c >= /* "a" */97 && c <= /* "f" */102) {
          ref_char_code = 87;
        } else {
          throw No_good;
        }
        var value = c - ref_char_code | 0;
        var match$1 = f[/* decimal_exponent */3];
        var decimal_exponent = match$1 !== undefined ? match$1 - 4 | 0 : undefined;
        var mantissa = (f[/* mantissa */1] << 4) + value | 0;
        var init$1 = eat(f);
        _f = /* record */[
          /* negative */init$1[/* negative */0],
          /* mantissa */mantissa,
          /* exponent */init$1[/* exponent */2],
          /* decimal_exponent */decimal_exponent,
          /* todo */init$1[/* todo */4]
        ];
        continue ;
      }
      
    } else {
      return f;
    }
  };
}

function float_of_string(str) {
  try {
    return Caml_format.caml_float_of_string(str);
  }
  catch (e){
    if (Sys.win32) {
      try {
        var f = parse_body(parse_hex_symbol(parse_sign(start(str))));
        if (f[/* todo */4] !== /* [] */0) {
          throw [
                Caml_builtin_exceptions.assert_failure,
                /* tuple */[
                  "lexer_flow.mll",
                  546,
                  4
                ]
              ];
        }
        var ret = f[/* mantissa */1];
        var match = f[/* decimal_exponent */3];
        var exponent = match !== undefined ? f[/* exponent */2] + match | 0 : f[/* exponent */2];
        var ret$1 = exponent === 0 ? ret : Math.pow(ret, exponent);
        if (f[/* negative */0]) {
          return -ret$1;
        } else {
          return ret$1;
        }
      }
      catch (exn){
        if (exn === No_good) {
          throw e;
        }
        throw exn;
      }
    } else {
      throw e;
    }
  }
}

function save_comment(env, start, _end, buf, multiline) {
  var loc = btwn(start, _end);
  var s = $$Buffer.contents(buf);
  var c = multiline ? /* Block */Block.__(0, [s]) : /* Line */Block.__(1, [s]);
  var lex_comments_acc_000 = /* tuple */[
    loc,
    c
  ];
  var lex_comments_acc_001 = env[/* lex_state */4][/* lex_comments_acc */1];
  var lex_comments_acc = /* :: */[
    lex_comments_acc_000,
    lex_comments_acc_001
  ];
  var init = env[/* lex_state */4];
  return /* record */[
          /* lex_source */env[/* lex_source */0],
          /* lex_lb */env[/* lex_lb */1],
          /* lex_in_comment_syntax */env[/* lex_in_comment_syntax */2],
          /* lex_enable_comment_syntax */env[/* lex_enable_comment_syntax */3],
          /* lex_state : record */[
            /* lex_errors_acc */init[/* lex_errors_acc */0],
            /* lex_comments_acc */lex_comments_acc
          ]
        ];
}

function unicode_fix_cols(lb) {
  var count = function (_start, stop, _acc) {
    while(true) {
      var acc = _acc;
      var start = _start;
      if (start === stop) {
        return acc;
      } else {
        var c = Caml_bytes.get(lb[/* lex_buffer */1], start);
        var acc$1 = (c & 192) === 128 ? acc + 1 | 0 : acc;
        _acc = acc$1;
        _start = start + 1 | 0;
        continue ;
      }
    };
  };
  var bytes = count(lb[/* lex_start_pos */4], lb[/* lex_curr_pos */5], 0);
  var new_bol = lb[/* lex_curr_p */11][/* pos_bol */2] + bytes | 0;
  var init = lb[/* lex_curr_p */11];
  lb[/* lex_curr_p */11] = /* record */[
    /* pos_fname */init[/* pos_fname */0],
    /* pos_lnum */init[/* pos_lnum */1],
    /* pos_bol */new_bol,
    /* pos_cnum */init[/* pos_cnum */3]
  ];
  return /* () */0;
}

function oct_to_int(x) {
  if (x > 55 || x < 48) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "lexer_flow.mll",
            604,
            11
          ]
        ];
  }
  return x - /* "0" */48 | 0;
}

function hexa_to_int(x) {
  var exit = 0;
  if (x >= 65) {
    if (x >= 97) {
      if (x >= 103) {
        exit = 1;
      } else {
        return (x - /* "a" */97 | 0) + 10 | 0;
      }
    } else if (x >= 71) {
      exit = 1;
    } else {
      return (x - /* "A" */65 | 0) + 10 | 0;
    }
  } else if (x > 57 || x < 48) {
    exit = 1;
  } else {
    return x - /* "0" */48 | 0;
  }
  if (exit === 1) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "lexer_flow.mll",
            610,
            11
          ]
        ];
  }
  
}

function utf16to8(code) {
  if (code >= 65536) {
    return /* :: */[
            Char.chr(240 | (code >>> 18)),
            /* :: */[
              Char.chr(128 | (code >>> 12) & 63),
              /* :: */[
                Char.chr(128 | (code >>> 6) & 63),
                /* :: */[
                  Char.chr(128 | code & 63),
                  /* [] */0
                ]
              ]
            ]
          ];
  } else if (code >= 2048) {
    return /* :: */[
            Char.chr(224 | (code >>> 12)),
            /* :: */[
              Char.chr(128 | (code >>> 6) & 63),
              /* :: */[
                Char.chr(128 | code & 63),
                /* [] */0
              ]
            ]
          ];
  } else if (code >= 128) {
    return /* :: */[
            Char.chr(192 | (code >>> 6)),
            /* :: */[
              Char.chr(128 | code & 63),
              /* [] */0
            ]
          ];
  } else {
    return /* :: */[
            Char.chr(code),
            /* [] */0
          ];
  }
}

function mk_num_singleton(number_type, num, neg) {
  var value;
  if (number_type !== 0) {
    switch (number_type - 1 | 0) {
      case 0 : 
          value = Caml_format.caml_int_of_string("0o" + num);
          break;
      case 1 : 
          value = Caml_format.caml_int_of_string(num);
          break;
      case 2 : 
          value = float_of_string(num);
          break;
      
    }
  } else {
    value = Caml_format.caml_int_of_string(num);
  }
  var value$1 = neg === "" ? value : -value;
  return /* T_NUMBER_SINGLETON_TYPE */Block.__(5, [
            number_type,
            value$1
          ]);
}

var keywords = Hashtbl.create(undefined, 53);

var type_keywords = Hashtbl.create(undefined, 53);

List.iter((function (param) {
        return Hashtbl.add(keywords, param[0], param[1]);
      }), /* :: */[
      /* tuple */[
        "function",
        /* T_FUNCTION */13
      ],
      /* :: */[
        /* tuple */[
          "if",
          /* T_IF */14
        ],
        /* :: */[
          /* tuple */[
            "in",
            /* T_IN */15
          ],
          /* :: */[
            /* tuple */[
              "instanceof",
              /* T_INSTANCEOF */16
            ],
            /* :: */[
              /* tuple */[
                "return",
                /* T_RETURN */17
              ],
              /* :: */[
                /* tuple */[
                  "switch",
                  /* T_SWITCH */18
                ],
                /* :: */[
                  /* tuple */[
                    "this",
                    /* T_THIS */19
                  ],
                  /* :: */[
                    /* tuple */[
                      "throw",
                      /* T_THROW */20
                    ],
                    /* :: */[
                      /* tuple */[
                        "try",
                        /* T_TRY */21
                      ],
                      /* :: */[
                        /* tuple */[
                          "var",
                          /* T_VAR */22
                        ],
                        /* :: */[
                          /* tuple */[
                            "while",
                            /* T_WHILE */23
                          ],
                          /* :: */[
                            /* tuple */[
                              "with",
                              /* T_WITH */24
                            ],
                            /* :: */[
                              /* tuple */[
                                "const",
                                /* T_CONST */25
                              ],
                              /* :: */[
                                /* tuple */[
                                  "let",
                                  /* T_LET */26
                                ],
                                /* :: */[
                                  /* tuple */[
                                    "null",
                                    /* T_NULL */27
                                  ],
                                  /* :: */[
                                    /* tuple */[
                                      "false",
                                      /* T_FALSE */28
                                    ],
                                    /* :: */[
                                      /* tuple */[
                                        "true",
                                        /* T_TRUE */29
                                      ],
                                      /* :: */[
                                        /* tuple */[
                                          "break",
                                          /* T_BREAK */30
                                        ],
                                        /* :: */[
                                          /* tuple */[
                                            "case",
                                            /* T_CASE */31
                                          ],
                                          /* :: */[
                                            /* tuple */[
                                              "catch",
                                              /* T_CATCH */32
                                            ],
                                            /* :: */[
                                              /* tuple */[
                                                "continue",
                                                /* T_CONTINUE */33
                                              ],
                                              /* :: */[
                                                /* tuple */[
                                                  "default",
                                                  /* T_DEFAULT */34
                                                ],
                                                /* :: */[
                                                  /* tuple */[
                                                    "do",
                                                    /* T_DO */35
                                                  ],
                                                  /* :: */[
                                                    /* tuple */[
                                                      "finally",
                                                      /* T_FINALLY */36
                                                    ],
                                                    /* :: */[
                                                      /* tuple */[
                                                        "for",
                                                        /* T_FOR */37
                                                      ],
                                                      /* :: */[
                                                        /* tuple */[
                                                          "class",
                                                          /* T_CLASS */38
                                                        ],
                                                        /* :: */[
                                                          /* tuple */[
                                                            "extends",
                                                            /* T_EXTENDS */39
                                                          ],
                                                          /* :: */[
                                                            /* tuple */[
                                                              "static",
                                                              /* T_STATIC */40
                                                            ],
                                                            /* :: */[
                                                              /* tuple */[
                                                                "else",
                                                                /* T_ELSE */41
                                                              ],
                                                              /* :: */[
                                                                /* tuple */[
                                                                  "new",
                                                                  /* T_NEW */42
                                                                ],
                                                                /* :: */[
                                                                  /* tuple */[
                                                                    "delete",
                                                                    /* T_DELETE */43
                                                                  ],
                                                                  /* :: */[
                                                                    /* tuple */[
                                                                      "typeof",
                                                                      /* T_TYPEOF */44
                                                                    ],
                                                                    /* :: */[
                                                                      /* tuple */[
                                                                        "void",
                                                                        /* T_VOID */45
                                                                      ],
                                                                      /* :: */[
                                                                        /* tuple */[
                                                                          "enum",
                                                                          /* T_ENUM */46
                                                                        ],
                                                                        /* :: */[
                                                                          /* tuple */[
                                                                            "export",
                                                                            /* T_EXPORT */47
                                                                          ],
                                                                          /* :: */[
                                                                            /* tuple */[
                                                                              "import",
                                                                              /* T_IMPORT */48
                                                                            ],
                                                                            /* :: */[
                                                                              /* tuple */[
                                                                                "super",
                                                                                /* T_SUPER */49
                                                                              ],
                                                                              /* :: */[
                                                                                /* tuple */[
                                                                                  "implements",
                                                                                  /* T_IMPLEMENTS */50
                                                                                ],
                                                                                /* :: */[
                                                                                  /* tuple */[
                                                                                    "interface",
                                                                                    /* T_INTERFACE */51
                                                                                  ],
                                                                                  /* :: */[
                                                                                    /* tuple */[
                                                                                      "package",
                                                                                      /* T_PACKAGE */52
                                                                                    ],
                                                                                    /* :: */[
                                                                                      /* tuple */[
                                                                                        "private",
                                                                                        /* T_PRIVATE */53
                                                                                      ],
                                                                                      /* :: */[
                                                                                        /* tuple */[
                                                                                          "protected",
                                                                                          /* T_PROTECTED */54
                                                                                        ],
                                                                                        /* :: */[
                                                                                          /* tuple */[
                                                                                            "public",
                                                                                            /* T_PUBLIC */55
                                                                                          ],
                                                                                          /* :: */[
                                                                                            /* tuple */[
                                                                                              "yield",
                                                                                              /* T_YIELD */56
                                                                                            ],
                                                                                            /* :: */[
                                                                                              /* tuple */[
                                                                                                "debugger",
                                                                                                /* T_DEBUGGER */57
                                                                                              ],
                                                                                              /* :: */[
                                                                                                /* tuple */[
                                                                                                  "declare",
                                                                                                  /* T_DECLARE */58
                                                                                                ],
                                                                                                /* :: */[
                                                                                                  /* tuple */[
                                                                                                    "type",
                                                                                                    /* T_TYPE */59
                                                                                                  ],
                                                                                                  /* :: */[
                                                                                                    /* tuple */[
                                                                                                      "of",
                                                                                                      /* T_OF */60
                                                                                                    ],
                                                                                                    /* :: */[
                                                                                                      /* tuple */[
                                                                                                        "async",
                                                                                                        /* T_ASYNC */61
                                                                                                      ],
                                                                                                      /* :: */[
                                                                                                        /* tuple */[
                                                                                                          "await",
                                                                                                          /* T_AWAIT */62
                                                                                                        ],
                                                                                                        /* [] */0
                                                                                                      ]
                                                                                                    ]
                                                                                                  ]
                                                                                                ]
                                                                                              ]
                                                                                            ]
                                                                                          ]
                                                                                        ]
                                                                                      ]
                                                                                    ]
                                                                                  ]
                                                                                ]
                                                                              ]
                                                                            ]
                                                                          ]
                                                                        ]
                                                                      ]
                                                                    ]
                                                                  ]
                                                                ]
                                                              ]
                                                            ]
                                                          ]
                                                        ]
                                                      ]
                                                    ]
                                                  ]
                                                ]
                                              ]
                                            ]
                                          ]
                                        ]
                                      ]
                                    ]
                                  ]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

List.iter((function (param) {
        return Hashtbl.add(type_keywords, param[0], param[1]);
      }), /* :: */[
      /* tuple */[
        "static",
        /* T_STATIC */40
      ],
      /* :: */[
        /* tuple */[
          "typeof",
          /* T_TYPEOF */44
        ],
        /* :: */[
          /* tuple */[
            "any",
            /* T_ANY_TYPE */107
          ],
          /* :: */[
            /* tuple */[
              "bool",
              /* T_BOOLEAN_TYPE */108
            ],
            /* :: */[
              /* tuple */[
                "boolean",
                /* T_BOOLEAN_TYPE */108
              ],
              /* :: */[
                /* tuple */[
                  "true",
                  /* T_TRUE */29
                ],
                /* :: */[
                  /* tuple */[
                    "false",
                    /* T_FALSE */28
                  ],
                  /* :: */[
                    /* tuple */[
                      "number",
                      /* T_NUMBER_TYPE */109
                    ],
                    /* :: */[
                      /* tuple */[
                        "string",
                        /* T_STRING_TYPE */110
                      ],
                      /* :: */[
                        /* tuple */[
                          "void",
                          /* T_VOID_TYPE */111
                        ],
                        /* :: */[
                          /* tuple */[
                            "null",
                            /* T_NULL */27
                          ],
                          /* [] */0
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]);

var __ocaml_lex_tables = /* record */[
  /* lex_base */"\0\0\xb2\xff\xb3\xff\xb9\xffB\0C\0T\0W\0F\0I\0J\0K\0M\0e\0\xdd\xff\xde\xff\xdf\xff\xe0\xff\xe3\xff\xe4\xff\xe5\xff\xe6\xff\xe7\xff\xe8\xff\xc0\0L\0e\0\x17\x01n\x01\xf6\xff\xf7\xffl\0u\0v\0\0\0\x0e\0\x0f\0\x07\x003\x01\xfe\xff\xff\xff\x01\0\x12\0(\0\f\0\x15\0*\0\f\0=\0-\0\t\0\xb6\xff\xf9\xff\xe0\x01B\0u\0\x0f\x000\x004\0\x17\0\xe5\x01(\x008\0\x1a\0K\0:\0\x17\0\xfb\xffh\0a\0\xac\0q\0m\0y\0q\0i\0{\0{\0\xa8\0\xca\xff\xfa\xff\xc9\xff\xf8\xff\x0b\x02\xa5\x02\xfc\x02S\x03\xaa\x03\x01\x04X\x04\xaf\x04\x06\x05]\x05\xb4\x05\x0b\x06b\x06\xb9\x06\xc3\x01\x10\x07g\x07\xbe\x07\x15\bl\b\xc3\b\x1a\tq\t\xc8\t\xb8\0\xe2\xffE\x02\xc7\xff\xdc\xff\xc6\xff\xdb\xff\xb7\xff\xaa\0\xda\xff\xab\0\xd9\xff\xac\0\xd8\xff\xd2\xff\xad\0\xd7\xff\xb0\0\xd0\xff\xcf\xff\xcc\xff\xd4\xff\xcb\xff\xd3\xff\xc8\xff\xc5\xff:\n\xcf\xff\xd0\xff\xd2\xff\xd6\xff\xd7\xff\xb0\0\xdc\xff\xdd\xff\xe0\xff\xe1\xff\xe2\xff\xe3\xff\xe6\xff\xe7\xff\xe8\xff\xe9\xff\xea\xff\xeb\xff\x94\n\xfa\n\xd6\x01Q\x0b\xa8\x0b\x1a\f\xf9\xff\xcc\0\xf1\0A\0}\0~\0\xa3\0\xc4\x0b\xff\xffa\0\x9d\0\xc1\0\xa4\0\x90\0\xc6\0\xb2\0\xcb\t\xd2\0\x95\0\xfa\xff\x1f\f\xe9\0\x1c\x01\x9c\0\xf2\0\xf3\0\xf9\0$\f\xe7\0\xf7\0\xf5\0\xdf\x0b\x15\x01\xd7\0\xfc\xff(\x01!\x01m\x012\x01/\x01E\x01=\x015\x01G\x01G\x01\xfb\xff\xf3\x01\xf2\0.\x01I\x01P\x01K\f=\x01L\x01/\x01\xec\x0bk\x010\x01x\f\xff\fV\r\xad\r\0\x02\x04\x0e[\x0e\xb2\x0e\t\x0f`\x0f\xb7\x0f\x0e\x10e\x10\xbc\x10\x13\x11j\x11\xc1\x11\x18\x12o\x12\xc6\x12\x1d\x13t\x13\xcb\x13\"\x14\xcf\x01\xe5\xffy\x14\xd0\x14'\x15~\x15\xd4\xff\x1b\f\xfc\xff\xfd\xff\xfe\xff\xff\xff\xcf\x15\xee\xff\x01\0\xef\xff\x18\x16\xf4\xff\xf5\xff\xf6\xff\xf7\xff\xf8\xff\xf9\xff\xf1\x02H\x03>\x16\xfe\xff\xff\xffU\x16\xfd\xff\x9f\x03\xfc\xff{\x16\x92\x16\xb8\x16\xcf\x16\xf2\xff\xf5\x16\xf1\xff\xd7\x02\xfb\xff\xd2\x01\xfe\xff\xff\xff\xcf\x01\xfd\xff\xfc\xff;\x02\xfd\xff\xfe\xff\xff\xff\0\x17\xf9\xff\xe8\x01G\x01\x83\x01\x90\x01y\x01)\fC\x15\xfe\xff\xff\xff]\x01\x9b\x01\x9c\x01*\x02\x90\x01\xa0\x01\x82\x01\x87\x15\xad\x01o\x01\xfb\xff\xfc\xff\x0b\x16\xf8\xff\x04\0\xf9\xff\xfa\xff8\x17,\x03\xff\xff\xfd\xff\x05\0\xfe\xff\xc0\x17\x96\t\xfb\xff\xfc\xff\xeb\x01\xff\xff\xfd\xff\xfe\xff2\x18\xf1\xff\xf2\xff\x8a\x18\xf4\xff\xf5\xff\xf6\xff\xf7\xff\xf8\xff\xfa\xff<\x02\x7f\x01\xc9\x01\xe7\x01+\x02\x88\x167\x18\xfe\xff\xff\xff\x8f\x01 \x02!\x023\x02\x15\x02%\x02!\x02\xbd\x16L\x02\x0f\x02\xfb\xff\xfc\xff|\f\xfb\xff\xfc\xff\xfd\xff\xfe\xff\x06\0\xff\xff\xfc\x18\xf9\xff\xf8\x18\x07\0\xfd\xff\xfe\xff\xff\xffO\x19\xdf\n_\f\x84\x17\x9c\x19\xfc\xff\xfb\xff\xd3\x19\xfa\xff*\x1a\x81\x1a\xd8\x1a/\x1b\x86\x1b\x96\x02\xf8\x1b\xfa\xff\xfb\xff\xb5\x02%\x02b\x02\x82\x02\xf3\x02\x04\x19K\x1b\xff\xff(\x02e\x02\xa9\x02J\x03r\x02\x85\x02\x8c\x02\xc9\x16\xb7\x02y\x02\xfc\xff\xfd\xff\xc3\x16\xf9\xff\xfa\xff\b\0\xfc\xff\xbf\x02\xfe\xff\xff\xff\xfd\xff\xfb\xff",
  /* lex_backtrk */"\xff\xff\xff\xff\xff\xff\xff\xffD\0A\0>\0=\0<\0;\0E\0G\0B\0C\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\0K\0\x1e\0\x15\0\x15\0\xff\xff\xff\xffM\0?\0J\0M\0M\0M\0M\0\x02\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x03\0\xff\xff\x04\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff@\0\xff\xff\xff\xff\xff\xff\xff\xff\x14\0\x14\0\x15\0\x14\0\x0f\0\x14\0\x14\0\x0b\0\n\0\r\0\f\0\x0e\0\x0e\0\x0e\0\xff\xff\x0e\0\x0e\0\x13\0\x12\0\x11\0\x10\0\x15\0\x13\0\x12\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff)\0\xff\xff*\0\xff\xff.\0\xff\xff\xff\xff2\0\xff\xff1\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff$\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x13\0\x13\0\x1b\0\x12\0\x12\0.\0\xff\xff&\x000\x000\x000\x000\x000\0\x01\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x02\0\xff\xff\x03\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\0\x11\0\x11\0\x10\0\xff\xff\x10\0\x0f\0\x0f\0\x12\0\x11\0\f\0\x11\0\x11\0\b\0\x07\0\n\0\t\0\x0b\0\x0b\0\x0b\0\x0b\0\x0b\0\x0e\0\r\0\xff\xff\xff\xff\x13\0\x13\0\x13\0\x13\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x10\0\xff\xff\x0f\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\f\0\x05\0\x0f\0\xff\xff\xff\xff\xff\xff\xff\xff\x04\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x04\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x05\0\x06\0\x06\0\x06\0\x06\0\x02\0\x01\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x06\0\xff\xff\xff\xff\x04\0\x07\0\xff\xff\xff\xff\x01\0\xff\xff\x03\0\xff\xff\xff\xff\xff\xff\x04\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\f\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x06\0\x0e\0\x0e\0\x0e\0\x0e\0\x02\0\x01\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\x06\0\x02\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x05\0\x05\0\x05\0\x05\0\x05\0\x01\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x05\0\xff\xff\x06\0\xff\xff\xff\xff\xff\xff\xff\xff",
  /* lex_default */"\x01\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\xff\xff\0\0\xff\xff\0\0\xff\xff\0\0\0\0\xff\xff\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x86\0\0\0\0\0\0\0\0\0\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xf8\0\0\0\0\0\0\0\0\0\xfd\0\0\0\xff\xff\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\0\0\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\0\0\x18\x01\0\0\xff\xff\0\0\0\0\xff\xff\0\0\0\0 \x01\0\0\0\0\0\0$\x01\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0;\x01\0\0\xff\xff\0\0\0\0\xff\xffB\x01\0\0\0\0\xff\xff\0\0\xff\xffG\x01\0\0\0\0\xff\xff\0\0\0\0\0\0N\x01\0\0\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0m\x01\0\0\0\0\0\0\0\0\xff\xff\0\0t\x01\0\0\xff\xff\xff\xff\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x8a\x01\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xa1\x01\0\0\0\0\xff\xff\0\0\xff\xff\0\0\0\0\0\0\0\0",
  /* lex_trans */"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0&\0(\0\xff\0&\0&\0=\x01D\x01r\x01w\x01\xa9\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0&\0\n\0\x1e\0\x1f\0\x18\0\x05\0\r\0\x1e\0\x15\0\x14\0 \0\x07\0\x10\0\x06\0\x1a\0!\0\x1c\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x0f\0\x11\0\t\0\x0b\0\b\0\x0e\0\x19\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x13\0'\0\x12\0\x04\0\x18\0\x1d\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x17\0\f\0\x16\0\x03\0\x84\0\x83\0\x82\0\x80\0{\0z\0w\0x\0u\0s\0r\0p\0o\0m\0R\x001\x000\0/\0\x81\x001\0k\0\x7f\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0N\x005\0.\0n\0&\0P\x004\0.\0-\x000\0/\0&\0&\0-\0&\0D\0C\0A\0>\0O\x003\0@\0?\0<\0=\0<\0<\0<\x002\x002\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0q\0B\0<\0<\0<\0<\0<\0<\0<\0<\0<\0<\0<\0<\0E\0F\0G\0H\0I\0J\0K\0L\0M\0C\0%\0$\0#\0\x18\0Q\0l\0t\0v\0y\0}\0|\0&\0~\0\xf6\0\"\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0<\0\xcb\0\xb0\0\xaf\0\xae\0\xad\0\x02\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\xb2\0\xb0\0\xaf\0\xa5\0\x18\0\xb1\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0S\0&\0\xac\0\xac\0&\0&\0\xae\0\xad\0\xab\0\xab\0U\0\xa5\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\xa5\0\xa5\0&\0\xa5\0\xc1\0\xc0\0\xbf\0S\0S\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\xbe\0\xbd\0\xbc\0\xb9\0S\0\xb9\0S\0S\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\xbb\0\xb9\0\xb9\0\xb9\0\xc2\0\xc3\0\xba\0\xc4\0\xc5\0U\0\xc6\0W\0W\0W\0W\0W\0W\0W\0W\0\x1b\0\x1b\0\xc7\0\xc8\0\xc9\0\xca\0\xc0\0\xd7\0\xd6\0S\0Y\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0X\0S\0S\0S\0S\0S\0S\0S\0S\0V\0S\0S\0\xd5\0\xd4\0\xd1\0\xd1\0S\0\xd1\0S\0Y\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0X\0S\0S\0S\0S\0S\0S\0S\0S\0V\0S\0S\0<\0\xd3\0\xd1\0<\0<\0<\0\xd1\0\xd2\0<\0<\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0\xf1\0\x1e\x01\x1c\x01<\0\x1d\x017\x016\x01\xf0\0<\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\x005\x014\x018\x013\x01,\0+\0*\x009\x017\x012\x017\x006\x015\x014\x01*\x017\0*\x01*\x01)\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0*\x01*\x01S\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0i\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0!\x016\0L\x01K\x01h\x01i\x016\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0j\x01g\x01f\x01\x18\0S\0k\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0h\x01g\x01f\x01\\\x01\x18\0\\\x01\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\\\x01;\0:\x009\x003\x01e\x01;\0:\x009\0S\x002\x01d\x01\\\x01e\x01\\\x018\0a\0\x82\x01a\0d\x018\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0\x9e\x01\x9d\x01\x1a\x01\x9c\x01\x9d\x01\x9f\x01\x9c\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\x91\x01\x19\x01\x9b\x01\x9a\x01S\0\x91\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x9b\x01\x9a\x01\x91\x01h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0D\x01\x91\x01\x91\x01C\x01\xa8\x01\"\x01\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0S\0\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\0\0\0\0\0\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0\x99\x01\0\0\0\0\0\0\0\0\0\0\x98\x01f\0f\0f\0f\0f\0f\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0S\0\0\0f\0f\0f\0f\0f\0f\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0_\0\x0f\x01\x0f\x01\x0f\x01\x0f\x01\x0f\x01\x0f\x01\x0f\x01\x0f\x01\x1b\x01U\0\0\0W\0W\0W\0W\0W\0W\0W\0W\0^\0^\0\x99\x01\0\0\0\0\0\0\0\0\0\0\x98\x01_\0_\0_\0_\0`\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0_\0\0\0_\0_\0_\0_\0`\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0S\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\0\0\0\0\0\0\0\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0S\0S\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0S\0\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0Z\0Z\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0S\0\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0[\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0Z\0Z\0[\0[\0[\0[\0[\0[\0[\0[\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\0\0\0\0\0\0\0\0[\0\0\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\0\0\0\0\0\0\0\0[\0\0\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0]\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0]\0]\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\0\0\0\0\0\0\0\0]\0\0\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\0\0\0\0\0\0\0\0]\0\0\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0_\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0U\0\0\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0_\0_\0_\0_\0`\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0_\0\0\0_\0_\0_\0_\0`\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0_\0\0\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0\0\0\0\0a\0\0\0a\0\0\0\0\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0_\0\0\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0c\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\0\0\0\0\0\0\0\0c\0\0\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\0\0\0\0\0\0\0\0c\0\0\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0e\0\0\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0e\0\0\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0g\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0f\0f\0f\0f\0f\0f\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\0\0\0\0\0\0\0\0g\0\0\0f\0f\0f\0f\0f\0f\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\0\0\0\0\0\0\0\0g\0\0\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0S\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0S\0S\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0S\0\0\0S\0S\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0j\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\0\0\0\0\0\0\0\0j\0\0\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\0\0\0\0\0\0\0\0\0\0I\x01H\x01\0\0\0\0\0\0\0\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\0\0\0\0\0\0\0\0j\0\0\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\xa5\0\xa6\0\0\0\xa5\0\xa5\0\0\0\0\0\0\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\0\0\0\0\0\0\0\0\xa5\0\0\0\x9e\0\0\0\x98\0\0\0\x89\0\x9e\0\x93\0\x92\0\x9f\0\x88\0\x90\0\x9d\0\x9a\0\xa0\0\x9c\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x8f\0\x91\0\x8d\0\x8b\0\x8c\0\x8e\0\xa5\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x97\0J\x01\x96\0\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x99\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x95\0\x8a\0\x94\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01\0\0\0\0\xa4\0\xa3\0\xa2\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xa1\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\x87\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0}\x01\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xf2\0\x98\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe0\0\0\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xa5\0\0\0\0\0\xa5\0\xa5\0\0\0\0\0\0\0\0\0\xe0\0\0\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\x9b\0\x9b\0\0\0\0\0\xa5\0\0\0\0\0\0\0\0\0\xd9\0\xe4\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xe3\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xe1\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xe4\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xe3\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xe1\0\xd9\0\xd9\0\xd1\0\0\0\xf9\0\xd1\0\xd1\0\xb9\0\0\0\0\0\xb9\0\xb9\0\xb9\0\0\0\0\0\xb9\0\xb9\0*\x01\0\0\0\0*\x01*\x01\0\0\0\0\0\0\xd1\0\0\0\0\0\xfb\0\0\0\xb9\0\0\0\0\0\xfb\0\0\0\xb9\0\0\0\0\0\0\0\xcc\0*\x01\x9c\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\xd1\0\0\0\0\0\xd1\0\xd1\0\xb4\0\0\0\0\0\0\0\0\0\xb4\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\0\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xfa\0\0\0\xcc\0\0\0\x9c\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\xb3\0r\x01\0\0\0\0q\x01\xb3\0\0\0\0\0\0\0\xb9\0|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01\0\0\x80\x01\xd1\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xaa\0\xa9\0\xa8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\0\0\xa7\0\0\0\0\0\0\0\0\0o\x01\xd9\0\xd9\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0n\x01\0\0\0\0\0\0\xd0\0\xcf\0\xce\0\0\0\0\0\xb8\0\xb7\0\xb6\0\0\0\0\0\xb8\0\xb7\0\xb6\0\0\0\xcd\x001\x010\x01/\x01\0\0\xb5\0\0\0\0\0\0\0\0\0\xb5\0\0\0\0\0\0\0\0\0.\x01\0\0\0\0\xf9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd0\0\xcf\0\xce\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\xcd\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0p\x01\0\0\0\0\0\0\0\0\xdc\0\0\0\xdc\0\0\0\0\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xdf\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\0\0\0\0\0\0\0\0\xdf\0\0\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xde\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\0\0\0\0\0\0\0\0\xde\0\0\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\0\0\0\0\0\0\0\0\xde\0\0\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xdf\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\0\0\0\0\0\0\0\0\xdf\0\0\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xea\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe0\0\0\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe9\0\xe9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xea\0\xea\0\xea\0\xea\0\xeb\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\xea\0\0\0\xea\0\xea\0\xea\0\xea\0\xeb\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe5\0\xe5\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xe6\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe5\0\xe5\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\0\0\0\0\0\0\0\0\xe6\0\0\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\0\0\0\0\0\0\0\0\xe6\0\0\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe8\0\xe8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\0\0\0\0\0\0\0\0\xe8\0\0\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\0\0\0\0\0\0\0\0\xe8\0\0\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xea\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe0\0\0\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xea\0\xea\0\xea\0\xea\0\xeb\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\xea\0\0\0\xea\0\xea\0\xea\0\xea\0\xeb\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\xea\0\0\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\0\0\0\0\xdc\0\0\0\xdc\0\0\0\0\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\xea\0\0\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xed\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\0\0\0\0\0\0\0\0\xed\0\0\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\0\0\0\0\0\0\0\0\xed\0\0\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xef\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\0\0\0\0\0\0\0\0\xef\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\0\0\0\0\0\0\0\0\xef\0\0\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xf3\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\xf4\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0*\x01,\x01\0\0*\x01*\x01\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0*\x01\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xf5\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xff\0\0\0\0\0\xfe\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\b\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01*\x01*\x01*\x01*\x01*\x01*\x01*\x01*\x01*\x01*\x01*\x01\0\0\0\0\0\0=\x01\0\0\0\0<\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x001\x010\x01/\x01\0\0\0\0\0\0\0\0\n\x01\0\0\0\0\0\0\0\0\0\0\x06\x01.\x01\0\0\0\0\x05\x01*\x01\0\0\0\0\0\0?\x01\0\0\0\0\x04\x01\0\0\0\0\0\0\x03\x01\0\0\x02\x01\0\x01\x01\x01\0\0\t\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0>\x01@\x01\0\0\0\0\0\0\0\0\0\0\0\0\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\0\0\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\0\0\0\0\\\x01\0\0\x10\x01\\\x01\\\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\0\0\0\0\0\0\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\0\0\0\0\0\0\\\x01\0\0\0\0\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\0\0\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\0\0\xa2\x01\0\0\x0b\x01\xa3\x01\0\0\0\0\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\0\0\0\0\0\0\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\0\0\0\0\0\0\0\0\0\0\xa5\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\0\0\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01*\x01,\x01A\x01*\x01+\x01\0\0\0\0\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\0\0\0\0\0\0\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\xa4\x01*\x01\0\0\0\0\xa6\x01\0\0\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01%\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\0\0\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\0\0\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\0\0\0\0\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01E\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0c\x01b\x01a\x01\\\x01\0\0\0\0\0\0\0\0\0\0\x16\x01\0\0\0\0\0\0\0\0`\x01\x91\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01\0\0\0\0\0\0\0\0E\x01\0\0E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01\0\0~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01\0\0\0\0\0\0\0\0\0\0\xa7\x01\0\0~\x01~\x01~\x01~\x01~\x01~\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0)\x01(\x01'\x01E\x01~\x01~\x01~\x01~\x01~\x01~\x01\0\0\0\0\0\0\0\0&\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0-\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01\0\0\0\0\0\0\0\0E\x01\0\0E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01\\\x01^\x01\0\0\\\x01]\x01\\\x01^\x01\0\0\\\x01\\\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\\\x01\0\0O\x01\0\0P\x01\\\x01\0\0O\x01\0\0\0\0\0\0\0\0\0\0\0\0R\x01W\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0S\x01\0\0V\x01Q\x01U\x01\0\0\0\0P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\0\0\0\0\0\0\0\0P\x01\0\0P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01T\x01P\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0P\x01\0\0\0\0P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\0\0\0\0\0\0\0\0P\x01\0\0P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\0\0w\x01\0\0\0\0v\x01\0\0\0\0\0\0\x91\x01\0\0\0\0\x91\x01\x91\x01\0\0[\x01Z\x01Y\x01\0\0\0\0c\x01b\x01a\x01{\x01z\x01\0\0y\x01\0\0\0\0X\x01u\x01y\x01\x91\x01\0\0`\x01\0\0z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01_\x01\0\0\0\0\0\0\0\0\0\0y\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01\0\0\0\0\0\0\0\0z\x01\0\0z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01\x81\x01\0\0\0\0\0\0y\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\0\0\0\0\0\0\0\0\x81\x01\0\0\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\0\0\0\0~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01\0\0\x7f\x01\0\0\0\0\0\0\0\0\0\0~\x01~\x01~\x01~\x01~\x01~\x01\0\0\0\0\x97\x01\x96\x01\x95\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x94\x01\0\0\0\0\0\0\x83\x01\0\0\0\0\0\0\0\0x\x01~\x01~\x01~\x01~\x01~\x01~\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\0\0\x82\x01\0\0\0\0\0\0\0\0\0\0\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\0\0\0\0\0\0\0\0\x83\x01\0\0\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x84\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\0\0\x82\x01\0\0\0\0\0\0\0\0\0\0\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\0\0\0\0\0\0\0\0\x84\x01\0\0\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x85\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\0\0\x82\x01\0\0\0\0\0\0\0\0\0\0\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\0\0\0\0\0\0\0\0\x85\x01\0\0\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x86\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\0\0\x82\x01\0\0\0\0\0\0\0\0\0\0\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\0\0\0\0\0\0\0\0\x86\x01\0\0\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x87\x01\x91\x01\x93\x01\0\0\x91\x01\x91\x01\0\0\0\0\0\0\0\0\0\0\0\0\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\0\0\x82\x01\x91\x01\0\0\0\0\0\0\0\0\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\0\0\0\0\0\0\0\0\x87\x01\0\0\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x88\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\0\0\x82\x01\0\0\0\0\0\0\0\0\0\0\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\0\0\0\0\0\0\0\0\x88\x01\0\0\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x91\x01\x93\x01\0\0\x91\x01\x92\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x91\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x8c\x01\0\0\0\0\0\0\0\0\x97\x01\x96\x01\x95\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x94\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x8b\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x90\x01\x8f\x01\x8e\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x8d\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff",
  /* lex_check */"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xfe\0\0\0\0\0<\x01C\x01q\x01v\x01\xa3\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x04\0\x05\0\x06\0\x07\0\b\0\b\0\t\0\t\0\n\0\x0b\0\x0b\0\f\0\r\0\x19\0\x1f\0#\0$\0$\0\x06\0*\0\x1a\0\x07\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0 \0!\0%\0\r\0-\0 \0!\0,\0%\0+\0+\0.\0/\0,\x001\x006\x007\x009\0;\0 \0!\0:\0:\0=\0;\0>\0?\0A\0\"\0)\x000\x000\x000\x000\x000\x000\x000\x000\x000\x000\x000\x002\0\f\x008\0@\0@\0@\0@\0@\0@\0@\0@\0@\0@\0@\0B\0D\0E\0F\0G\0H\0I\0J\0K\0L\0M\0\0\0\0\0\0\0\x18\0N\0k\0s\0u\0w\0z\0z\x000\0|\0\x8b\0\0\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0@\0\x9f\0\xa1\0\xa2\0\xa3\0\xa3\0\0\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\xa0\0\xa7\0\xa8\0\xab\0\x18\0\xa0\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x1b\0&\0\xa4\0\xaa\0&\0&\0\xa9\0\xa9\0\xa4\0\xaa\0\x1b\0\xac\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\xad\0\xaf\0&\0\xb0\0\xb3\0\xb4\0\xb5\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\xb6\0\xb7\0\xb7\0\xba\0\x1b\0\xbb\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1c\0\xb8\0\xbc\0\xbe\0\xbf\0\xc1\0\xc2\0\xb8\0\xc3\0\xc4\0\x1c\0\xc5\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\xc6\0\xc7\0\xc8\0\xc9\0\xca\0\xcd\0\xce\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\xcf\0\xcf\0\xd2\0\xd3\0\x1c\0\xd4\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\x005\0\xd0\0\xd6\x005\x005\0<\0\xd7\0\xd0\0<\0<\0a\0a\0a\0a\0a\0a\0a\0a\0a\0a\0\xf0\0\x1c\x01\x19\x015\0\x19\x01&\x01'\x01\x9a\0<\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0(\x01(\x01%\x01)\x01&\0&\0&\0%\x01.\x01)\x015\0/\x010\x010\x012\x01<\x003\x014\x01&\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\x006\x017\x01S\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0X\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\x1f\x015\0I\x01I\x01Y\x01`\x01<\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0W\x01Z\x01Z\x01m\0S\0W\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0a\x01b\x01b\x01d\x01m\0e\x01m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0f\x015\x005\x005\x001\x01[\x01<\0<\0<\0T\x001\x01[\x01h\x01c\x01i\x015\0T\0\x88\x01T\0c\x01<\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0\x8c\x01\x8d\x01\x17\x01\x8e\x01\x94\x01\x8c\x01\x95\x01T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0\x98\x01\x17\x01\x8f\x01\x8f\x01T\0\x99\x01T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0U\0\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x96\x01\x96\x01\x9a\x01U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0@\x01\x9c\x01\x9d\x01@\x01\xa5\x01\x1f\x01\xff\xffU\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0\xff\xff\xff\xff\xff\xff\xff\xffU\0\xff\xffU\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0V\0\b\x01\b\x01\b\x01\b\x01\b\x01\b\x01\b\x01\b\x01\xff\xff\xff\xff\xff\xffV\0V\0V\0V\0V\0V\0V\0V\0V\0V\0\x90\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x90\x01V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0\xff\xff\xff\xff\xff\xff\xff\xffV\0\xff\xffV\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0W\0\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x17\x01W\0\xff\xffW\0W\0W\0W\0W\0W\0W\0W\0W\0W\0\x97\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x97\x01W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0\xff\xff\xff\xff\xff\xff\xff\xffW\0\xff\xffW\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0X\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff@\x01\xff\xff\xff\xff\xff\xff\xff\xffX\0X\0X\0X\0X\0X\0X\0X\0X\0X\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffX\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0\xff\xff\xff\xff\xff\xff\xff\xffX\0\xff\xffX\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0Y\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffY\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffY\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0\xff\xff\xff\xff\xff\xff\xff\xffY\0\xff\xffY\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Z\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffZ\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffZ\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0\xff\xff\xff\xff\xff\xff\xff\xffZ\0\xff\xffZ\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0[\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\xff\xff\xff\xff\xff\xff\xff\xff[\0\xff\xff[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\\\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\xff\xff\xff\xff\xff\xff\xff\xff\\\0\xff\xff\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0]\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\xff\xff\xff\xff\xff\xff\xff\xff]\0\xff\xff]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0^\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff^\0\xff\xff^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0\xff\xff\xff\xff\xff\xff\xff\xff^\0\xff\xff^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0_\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\xff\xff\xff\xff\xff\xff\xff\xff_\0\xff\xff_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0`\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff`\0\xff\xff`\0\xff\xff\xff\xff`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0\xff\xff\xff\xff\xff\xff\xff\xff`\0\xff\xff`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0b\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffb\0b\0b\0b\0b\0b\0b\0b\0b\0b\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffb\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0\xff\xff\xff\xff\xff\xff\xff\xffb\0\xff\xffb\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0c\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffc\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffc\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\xff\xff\xff\xff\xff\xff\xff\xffc\0\xff\xffc\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0d\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffd\0d\0d\0d\0d\0d\0d\0d\0d\0d\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffd\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0\xff\xff\xff\xff\xff\xff\xff\xffd\0\xff\xffd\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0e\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffe\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffe\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\xff\xff\xff\xff\xff\xff\xff\xffe\0\xff\xffe\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0f\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfff\0f\0f\0f\0f\0f\0f\0f\0f\0f\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfff\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0\xff\xff\xff\xff\xff\xff\xff\xfff\0\xff\xfff\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0g\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffg\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffg\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\xff\xff\xff\xff\xff\xff\xff\xffg\0\xff\xffg\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0h\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffh\0h\0h\0h\0h\0h\0h\0h\0h\0h\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffh\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0\xff\xff\xff\xff\xff\xff\xff\xffh\0\xff\xffh\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0i\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffi\0i\0i\0i\0i\0i\0i\0i\0i\0i\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffi\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0\xff\xff\xff\xff\xff\xff\xff\xffi\0\xff\xffi\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0j\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffF\x01F\x01\xff\xff\xff\xff\xff\xff\xff\xffj\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffj\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\xff\xff\xff\xff\xff\xff\xff\xffj\0\xff\xffj\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\x85\0\x85\0\xff\xff\x85\0\x85\0\xff\xff\xff\xff\xff\xff\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xff\xff\xff\xff\xff\xff\xff\xff\x85\0\xff\xff\x85\0\xff\xff\x85\0\xff\xff\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\xae\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0F\x01\x85\0\xff\xff\x85\0\xff\xff\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x98\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xff\xff\xff\xff\xff\xff\xff\xff\x98\0\xff\xff\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0{\x01{\x01{\x01{\x01{\x01{\x01{\x01{\x01{\x01{\x01\xff\xff\xff\xff\x85\0\x85\0\x85\0\x99\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x85\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x85\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\xff\xff\xff\xff{\x01\xff\xff\x99\0\xff\xff\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x9b\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x9b\0\xff\xff\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\xff\xff\xff\xff\xff\xff\xff\xff\x9b\0\xff\xff\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9c\0\xa5\0\xff\xff\xff\xff\xa5\0\xa5\0\xff\xff\xff\xff\xff\xff\xff\xff\x9c\0\xff\xff\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\xff\xff\xff\xff\xa5\0\xff\xff\xff\xff\xff\xff\xff\xff\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\xff\xff\xff\xff\xff\xff\xff\xff\x9c\0\xff\xff\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9d\0\xff\xff\xf7\0\x9d\0\x9d\0\xb2\0\xff\xff\xff\xff\xb2\0\xb2\0\xb9\0\xff\xff\xff\xff\xb9\0\xb9\0*\x01\xff\xff\xff\xff*\x01*\x01\xff\xff\xff\xff\xff\xff\x9d\0\xff\xff\xff\xff\xf7\0\xff\xff\xb2\0\xff\xff\xff\xff\xf7\0\xff\xff\xb9\0\xff\xff\xff\xff\xff\xff\x9d\0*\x01\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\xd1\0\xff\xff\xff\xff\xd1\0\xd1\0\xb2\0\xff\xff\xff\xff\xff\xff\xff\xff\xb9\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xff\xff\xd1\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xf7\0\xff\xff\xd1\0\xff\xff\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xb2\0l\x01\xff\xff\xff\xffl\x01\xb9\0\xff\xff\xff\xff\xff\xff\xbd\0|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01\xff\xff|\x01\xd5\0\xd8\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xa5\0\xa5\0\xa5\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xff\xff\xa5\0\xff\xff\xff\xff\xff\xff\xff\xffl\x01\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xff\xff\xff\xff\xff\xff\xff\xff\xd8\0\xff\xff\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xff\xff\xff\xff\xff\xff\xff\xffl\x01\xff\xff\xff\xff\xff\xff\x9d\0\x9d\0\x9d\0\xff\xff\xff\xff\xb2\0\xb2\0\xb2\0\xff\xff\xff\xff\xb9\0\xb9\0\xb9\0\xff\xff\x9d\0*\x01*\x01*\x01\xff\xff\xb2\0\xff\xff\xff\xff\xff\xff\xff\xff\xb9\0\xff\xff\xff\xff\xff\xff\xff\xff*\x01\xff\xff\xff\xff\xf7\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xd9\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xd1\0\xd1\0\xd1\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xff\xff\xd1\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xff\xff\xff\xff\xff\xff\xff\xff\xd9\0\xff\xff\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xda\0\xff\xffl\x01\xff\xff\xff\xff\xff\xff\xff\xff\xda\0\xff\xff\xda\0\xff\xff\xff\xff\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xff\xff\xff\xff\xff\xff\xff\xff\xda\0\xff\xff\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xdb\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xff\xff\xff\xff\xff\xff\xff\xff\xdb\0\xff\xff\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdd\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xff\xff\xff\xff\xff\xff\xff\xff\xdd\0\xff\xff\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xde\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xff\xff\xff\xff\xff\xff\xff\xff\xde\0\xff\xff\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xdf\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xff\xff\xff\xff\xff\xff\xff\xff\xdf\0\xff\xff\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xe0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xff\xff\xff\xff\xff\xff\xff\xff\xe0\0\xff\xff\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe1\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xff\xff\xff\xff\xff\xff\xff\xff\xe1\0\xff\xff\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe2\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe2\0\xff\xff\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xff\xff\xff\xff\xff\xff\xff\xff\xe2\0\xff\xff\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe3\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xff\xff\xff\xff\xff\xff\xff\xff\xe3\0\xff\xff\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe4\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xff\xff\xff\xff\xff\xff\xff\xff\xe4\0\xff\xff\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe5\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xff\xff\xff\xff\xff\xff\xff\xff\xe5\0\xff\xff\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe6\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xff\xff\xff\xff\xff\xff\xff\xff\xe6\0\xff\xff\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe7\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xff\xff\xff\xff\xff\xff\xff\xff\xe7\0\xff\xff\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe8\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xff\xff\xff\xff\xff\xff\xff\xff\xe8\0\xff\xff\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe9\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe9\0\xff\xff\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xff\xff\xff\xff\xff\xff\xff\xff\xe9\0\xff\xff\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xea\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xff\xff\xff\xff\xff\xff\xff\xff\xea\0\xff\xff\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xeb\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xeb\0\xff\xff\xeb\0\xff\xff\xff\xff\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xff\xff\xff\xff\xff\xff\xff\xff\xeb\0\xff\xff\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xec\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xff\xff\xff\xff\xff\xff\xff\xff\xec\0\xff\xff\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xed\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xff\xff\xff\xff\xff\xff\xff\xff\xed\0\xff\xff\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xee\0\xff\xff\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xef\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xff\xff\xff\xff\xff\xff\xff\xff\xef\0\xff\xff\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xf2\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xff\xff\xff\xff\xff\xff\xff\xff\xf2\0\xff\xff\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf3\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xff\xff\xff\xff\xff\xff\xff\xff\xf3\0\xff\xff\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf4\0+\x01+\x01\xff\xff+\x01+\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xff\xff\xff\xff+\x01\xff\xff\xff\xff\xff\xff\xff\xff\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xff\xff\xff\xff\xff\xff\xff\xff\xf4\0\xff\xff\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf5\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xfc\0\xff\xff\xff\xff\xfc\0\xf5\0\xff\xff\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfc\0\xfc\0\xfc\0\xfc\0\xfc\0\xfc\0\xfc\0\xfc\x005\x015\x015\x015\x015\x015\x015\x015\x015\x015\x015\x01\xff\xff\xff\xff\xff\xff:\x01\xff\xff\xff\xff:\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff+\x01+\x01+\x01\xff\xff\xff\xff\xff\xff\xff\xff\xfc\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfc\0+\x01\xff\xff\xff\xff\xfc\x005\x01\xff\xff\xff\xff\xff\xff:\x01\xff\xff\xff\xff\xfc\0\xff\xff\xff\xff\xff\xff\xfc\0\xff\xff\xfc\0\xfc\0\xfc\0\xff\xff\xfc\0\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff:\x01:\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\xff\xff\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\xff\xff\xff\xff\\\x01\xff\xff\0\x01\\\x01\\\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\xff\xff\xff\xff\xff\xff\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\xff\xff\xff\xff\xff\xff\\\x01\xff\xff\xff\xff\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\xff\xff\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\xff\xff\xa0\x01\xff\xff\xfc\0\xa0\x01\xff\xff\xff\xff\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\xff\xff\xff\xff\xff\xff\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xa0\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\xff\xff\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01#\x01#\x01:\x01#\x01#\x01\xff\xff\xff\xff\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\xff\xff\xff\xff\xff\xff\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\xa0\x01#\x01\xff\xff\xff\xff\xa0\x01\xff\xff\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01#\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\xff\xffg\x01g\x01g\x01g\x01g\x01g\x01g\x01g\x01g\x01g\x01g\x01\xff\xff\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\xff\xff\xff\xff\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01?\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\\\x01\\\x01\\\x01g\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x01\xff\xff\xff\xff\xff\xff\xff\xff\\\x01\x9b\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01\xff\xff\xff\xff\xff\xff\xff\xff?\x01\xff\xff?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01\xff\xff}\x01}\x01}\x01}\x01}\x01}\x01}\x01}\x01}\x01}\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xa0\x01\xff\xff}\x01}\x01}\x01}\x01}\x01}\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff#\x01#\x01#\x01E\x01}\x01}\x01}\x01}\x01}\x01}\x01\xff\xff\xff\xff\xff\xff\xff\xff#\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff#\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01\xff\xff\xff\xff\xff\xff\xff\xffE\x01\xff\xffE\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01M\x01M\x01\xff\xffM\x01M\x01]\x01]\x01\xff\xff]\x01]\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffM\x01\xff\xffM\x01\xff\xffM\x01]\x01\xff\xffM\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffM\x01M\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffM\x01\xff\xffM\x01M\x01M\x01\xff\xff\xff\xffM\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01\xff\xff\xff\xff\xff\xff\xff\xffM\x01\xff\xffM\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01P\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffP\x01\xff\xff\xff\xffP\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffP\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\xff\xff\xff\xff\xff\xff\xff\xffP\x01\xff\xffP\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\xff\xffs\x01\xff\xff\xff\xffs\x01\xff\xff\xff\xff\xff\xff\x91\x01\xff\xff\xff\xff\x91\x01\x91\x01\xff\xffM\x01M\x01M\x01\xff\xff\xff\xff]\x01]\x01]\x01u\x01u\x01\xff\xffs\x01\xff\xff\xff\xffM\x01s\x01s\x01\x91\x01\xff\xff]\x01\xff\xffu\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01M\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffs\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01\xff\xff\xff\xff\xff\xff\xff\xffu\x01\xff\xffu\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01z\x01\xff\xff\xff\xff\xff\xffs\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffz\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffz\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01\xff\xff\xff\xff\xff\xff\xff\xffz\x01\xff\xffz\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01\xff\xff\xff\xff~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01\xff\xff~\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff~\x01~\x01~\x01~\x01~\x01~\x01\xff\xff\xff\xff\x91\x01\x91\x01\x91\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x91\x01\xff\xff\xff\xff\xff\xff\x81\x01\xff\xff\xff\xff\xff\xff\xff\xffs\x01~\x01~\x01~\x01~\x01~\x01~\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\xff\xff\x81\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\xff\xff\xff\xff\xff\xff\xff\xff\x81\x01\xff\xff\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x83\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\xff\xff\x83\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\xff\xff\xff\xff\xff\xff\xff\xff\x83\x01\xff\xff\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x84\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\xff\xff\x84\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\xff\xff\xff\xff\xff\xff\xff\xff\x84\x01\xff\xff\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x85\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\xff\xff\x85\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\xff\xff\xff\xff\xff\xff\xff\xff\x85\x01\xff\xff\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x86\x01\x92\x01\x92\x01\xff\xff\x92\x01\x92\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\xff\xff\x86\x01\x92\x01\xff\xff\xff\xff\xff\xff\xff\xff\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\xff\xff\xff\xff\xff\xff\xff\xff\x86\x01\xff\xff\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x87\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\xff\xff\x87\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\xff\xff\xff\xff\xff\xff\xff\xff\x87\x01\xff\xff\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x89\x01\x89\x01\xff\xff\x89\x01\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\x92\x01\x92\x01\x92\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x92\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01\x89\x01\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01",
  /* lex_base_code */"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\n\0\x16\0\"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x02\0\0\0\0\0\0\0\x01\0\f\0\0\0\f\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0,\x006\0_\0B\0v\0L\0N\0\0\0\x81\0\0\0\x98\0\0\0\xa2\0\xac\0\xb6\0\0\0\xc0\0\0\0\xca\0\0\0\xe1\0\xeb\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x04\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x0e\x01\x1a\x01&\x01W\x01\0\0\0\0\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\t\0\x0b\0\r\0\x0f\0\xe5\0\x1a\0\b\0h\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0H\x01\0\0\0\0\0\0\0\0y\x01\r\0\x1c\0\x10\0\x1a\x01\x1d\0E\0\x83\x01\0\0\x8d\x01\x9a\x01\xa4\x01\xae\x01\0\0\0\0\xb8\x01\xc2\x01\xdb\x01\xe5\x01\x89\0\x8b\0\0\0\xf9\x01\0\0\x03\x02\0\0\r\x02\x17\x02\0\0!\x02\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  /* lex_backtrk_code */"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\f\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x0f\0\x0f\0\0\0\x0f\0\0\0\x0f\0\x0f\0\0\0#\0\0\0&\0)\0)\0)\0\0\0)\0)\0\0\0,\0\0\0/\0\0\0\0\0,\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0W\0W\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0h\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0W\0k\0k\0s\0\0\0s\0v\0v\0W\0k\0~\0k\0k\0&\0\x8f\0/\0\x94\0\x99\0\x99\0\x99\0\x99\0\x99\0\x9e\0\xa1\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  /* lex_default_code */"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  /* lex_trans_code */"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\t\0\0\0\t\0\t\0\t\0\t\0\t\0e\0\0\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\t\0\0\0\t\0\0\0\0\0\0\0\0\0e\0\0\0e\0\t\0e\0\0\0\0\0\0\0\0\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\0\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\0\0\x04\0\x04\0\x04\0\x04\0\x04\0\x04\0\x04\0\x04\0\x01\0\x01\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\0\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x01\0\x01\0 \0 \0 \0 \0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0e\0\t\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0e\0e\x002\x002\x002\0\0\0\t\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0e\x002\0\t\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x8c\0\x8c\0\x8c\0\x8c\0\0\0\0\0\t\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x01\0e\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\x002\0\0\0\0\0\0\0\0\0\0\0\0\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\0\0\0\0\0\0\0\0\0\0\0\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\x002\0\0\0\0\0M\0M\0M\0M\0M\0M\0M\0M\0M\0M\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0\0\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0M\0\0\0`\0`\0`\0`\0`\0`\0`\0`\0R\0R\x002\0\0\0\0\x002\x002\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x002\0M\0M\0M\0M\0M\0M\0M\0M\0M\0M\x002\0\0\0\0\x002\x002\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0\0\0\0\0\0\0e\0\0\0\0\0\0\0\0\x002\x002\x002\x002\x002\x002\x002\x002\x002\x002\x002\x002\0\0\0\0\0\0\0\0\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0\0\0\0\x002\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0{\0{\0{\0{\0{\0{\0{\0{\0{\0{\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0{\0{\0{\0{\0{\0{\0R\0\0\0\x81\0\x81\0\x81\0\x81\0\x81\0\x81\0\x81\0\x81\0\x86\0\x86\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0\0\0\0\0\0\0\0\0\0\0\0\0{\0{\0{\0{\0{\0{\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0R\0\0\0\x86\0\x86\0\x86\0\x86\0\x86\0\x86\0\x86\0\x86\0\x86\0\x86\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0{\0{\0{\0{\0{\0{\0{\0{\0{\0{\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0{\0{\0{\0{\0{\0{\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0{\0{\0{\0{\0{\0{\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  /* lex_check_code */"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff5\0\xff\xff<\x005\x005\0<\0<\0\xb2\0\xff\xff\xb9\0\xb2\0\xb2\0\xb9\0\xb9\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff5\0\xff\xff<\0\xff\xff\xff\xff\xff\xff\xff\xff\xb2\0\xff\xff\xb9\0!\0\xa0\0\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1b\0\xff\xff\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1c\0\xff\xff\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0W\0\xff\xffW\0W\0W\0W\0W\0W\0W\0W\0W\0W\0Y\0Y\0Z\0Z\0>\0@\0@\0@\0@\0@\0@\0@\0@\0@\0@\0@\0A\0\xbb\0=\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0\xba\0\xbe\0\xd2\0\xd3\0\xd6\0\xff\xff?\0V\0V\0V\0V\0V\0V\0X\0X\0X\0X\0X\0X\0X\0X\0\xbc\0\xd4\0@\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\xe4\0\xe4\0\xe5\0\xe5\0\xff\xff\xff\xffB\0V\0V\0V\0V\0V\0V\0^\0\xbf\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0a\0a\0a\0a\0a\0a\0a\0a\0a\0a\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0\xd7\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfff\0f\0f\0f\0f\0f\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfff\0f\0f\0f\0f\0f\0\x85\0\xff\xff\xff\xff\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9b\0\xff\xff\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9c\0\xff\xff\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9d\0\xff\xff\xff\xff\x9d\0\x9d\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x9d\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xd1\0\xff\xff\xff\xff\xd1\0\xd1\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\xff\xff\xff\xff\xff\xff\xbd\0\xff\xff\xff\xff\xff\xff\xff\xff\xd1\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xff\xff\xff\xff\xff\xff\xff\xff\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xff\xff\xff\xff\xd5\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe2\0\xff\xff\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe9\0\xff\xff\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
  /* lex_code */"\xff\x01\xff\xff\x03\xff\x01\xff\xff\x02\xff\xff\0\x02\xff\0\x01\xff\x06\xff\xff\x07\xff\xff\x01\xff\x03\xff\xff\x05\xff\xff\x04\xff\xff\0\x04\xff\0\x05\xff\0\x03\xff\0\x06\xff\0\x07\xff\x11\xff\x10\xff\x0e\xff\r\xff\f\xff\x0b\xff\n\xff\t\xff\b\xff\x07\xff\x06\xff\x05\xff\x04\xff\xff\x13\xff\x12\xff\xff\x12\xff\x13\xff\xff\x03\x11\x02\x12\x01\x0f\0\x10\xff\x16\xff\x13\xff\xff\x14\xff\xff\0\x14\xff\x01\x13\0\x0e\xff\x15\xff\xff\0\r\xff\x01\x15\0\f\xff\x19\xff\xff\0\t\xff\x13\xff\x16\xff\xff\x13\xff\xff\x18\xff\xff\x17\xff\xff\x01\x17\0\x04\xff\x01\x18\0\x06\xff\x01\x16\0\b\xff\0\x0b\xff\x01\x19\0\n\xff"
];

function token(env, lexbuf) {
  lexbuf[/* lex_mem */9] = Caml_array.caml_make_vect(8, -1);
  var env$1 = env;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 0;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.new_engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    var exit = 0;
    switch (__ocaml_lex_state$1) {
      case 0 : 
          Lexing.new_line(lexbuf$1);
          return token(env$1, lexbuf$1);
      case 1 : 
          var env$2 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* UnexpectedToken */Block.__(1, ["ILLEGAL"]));
          return token(env$2, lexbuf$1);
      case 2 : 
          unicode_fix_cols(lexbuf$1);
          return token(env$1, lexbuf$1);
      case 3 : 
          var start = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var buf = $$Buffer.create(127);
          var match = comment(env$1, buf, lexbuf$1);
          var env$3 = save_comment(match[0], start, match[1], buf, true);
          return token(env$3, lexbuf$1);
      case 4 : 
          var sp = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4] + 2 | 0, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var escape_type = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          var pattern = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_curr_pos */5]);
          if (env$1[/* lex_enable_comment_syntax */3]) {
            var env$4;
            if (env$1[/* lex_in_comment_syntax */2]) {
              var loc = from_lb(env$1[/* lex_source */0], lexbuf$1);
              env$4 = unexpected_error(env$1, loc, pattern);
            } else {
              env$4 = env$1;
            }
            var env$5 = in_comment_syntax(true, env$4);
            if (escape_type === ":") {
              return /* tuple */[
                      env$5,
                      /* T_COLON */77
                    ];
            } else {
              return token(env$5, lexbuf$1);
            }
          } else {
            var start$1 = from_lb(env$1[/* lex_source */0], lexbuf$1);
            var buf$1 = $$Buffer.create(127);
            $$Buffer.add_string(buf$1, sp);
            $$Buffer.add_string(buf$1, escape_type);
            var match$1 = comment(env$1, buf$1, lexbuf$1);
            var env$6 = save_comment(match$1[0], start$1, match$1[1], buf$1, true);
            return token(env$6, lexbuf$1);
          }
      case 5 : 
          if (env$1[/* lex_in_comment_syntax */2]) {
            var env$7 = in_comment_syntax(false, env$1);
            return token(env$7, lexbuf$1);
          } else {
            yyback(1, lexbuf$1);
            return /* tuple */[
                    env$1,
                    /* T_MULT */97
                  ];
          }
      case 6 : 
          var start$2 = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var buf$2 = $$Buffer.create(127);
          var match$2 = line_comment(env$1, buf$2, lexbuf$1);
          var env$8 = save_comment(match$2[0], start$2, match$2[1], buf$2, false);
          return token(env$8, lexbuf$1);
      case 7 : 
          if (lexbuf$1[/* lex_start_pos */4] === 0) {
            var match$3 = line_comment(env$1, $$Buffer.create(127), lexbuf$1);
            return token(match$3[0], lexbuf$1);
          } else {
            return /* tuple */[
                    env$1,
                    /* T_ERROR */104
                  ];
          }
      case 8 : 
          var quote = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          var start$3 = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var buf$3 = $$Buffer.create(127);
          var raw = $$Buffer.create(127);
          $$Buffer.add_char(raw, quote);
          var match$4 = string_quote(env$1, quote, buf$3, raw, false, lexbuf$1);
          return /* tuple */[
                  match$4[0],
                  /* T_STRING */Block.__(1, [/* tuple */[
                        btwn(start$3, match$4[1]),
                        $$Buffer.contents(buf$3),
                        $$Buffer.contents(raw),
                        match$4[2]
                      ]])
                ];
      case 9 : 
          var cooked = $$Buffer.create(127);
          var raw$1 = $$Buffer.create(127);
          var literal = $$Buffer.create(127);
          $$Buffer.add_string(literal, Lexing.lexeme(lexbuf$1));
          var start$4 = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var match$5 = template_part(env$1, start$4, cooked, raw$1, literal, lexbuf$1);
          return /* tuple */[
                  match$5[0],
                  /* T_TEMPLATE_PART */Block.__(2, [/* tuple */[
                        match$5[1],
                        /* record */[
                          /* cooked */$$Buffer.contents(cooked),
                          /* raw */$$Buffer.contents(raw$1),
                          /* literal */$$Buffer.contents(literal)
                        ],
                        match$5[2]
                      ]])
                ];
      case 10 : 
          var w = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w, /* T_NUMBER */Block.__(0, [/* BINARY */0]));
      case 11 : 
          return /* tuple */[
                  env$1,
                  /* T_NUMBER */Block.__(0, [/* BINARY */0])
                ];
      case 12 : 
          var w$1 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$1, /* T_NUMBER */Block.__(0, [/* OCTAL */2]));
      case 13 : 
          return /* tuple */[
                  env$1,
                  /* T_NUMBER */Block.__(0, [/* OCTAL */2])
                ];
      case 14 : 
          var w$2 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$2, /* T_NUMBER */Block.__(0, [/* LEGACY_OCTAL */1]));
      case 15 : 
          return /* tuple */[
                  env$1,
                  /* T_NUMBER */Block.__(0, [/* LEGACY_OCTAL */1])
                ];
      case 16 : 
      case 18 : 
      case 20 : 
          exit = 1;
          break;
      case 17 : 
      case 19 : 
      case 21 : 
          return /* tuple */[
                  env$1,
                  /* T_NUMBER */Block.__(0, [/* NORMAL */3])
                ];
      case 22 : 
          var word = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_curr_pos */5]);
          unicode_fix_cols(lexbuf$1);
          try {
            return /* tuple */[
                    env$1,
                    Hashtbl.find(keywords, word)
                  ];
          }
          catch (exn){
            if (exn === Caml_builtin_exceptions.not_found) {
              return /* tuple */[
                      env$1,
                      /* T_IDENTIFIER */0
                    ];
            } else {
              throw exn;
            }
          }
      case 23 : 
          return /* tuple */[
                  env$1,
                  /* T_LCURLY */1
                ];
      case 24 : 
          return /* tuple */[
                  env$1,
                  /* T_RCURLY */2
                ];
      case 25 : 
          return /* tuple */[
                  env$1,
                  /* T_LPAREN */3
                ];
      case 26 : 
          return /* tuple */[
                  env$1,
                  /* T_RPAREN */4
                ];
      case 27 : 
          return /* tuple */[
                  env$1,
                  /* T_LBRACKET */5
                ];
      case 28 : 
          return /* tuple */[
                  env$1,
                  /* T_RBRACKET */6
                ];
      case 29 : 
          return /* tuple */[
                  env$1,
                  /* T_ELLIPSIS */11
                ];
      case 30 : 
          return /* tuple */[
                  env$1,
                  /* T_PERIOD */9
                ];
      case 31 : 
          return /* tuple */[
                  env$1,
                  /* T_SEMICOLON */7
                ];
      case 32 : 
          return /* tuple */[
                  env$1,
                  /* T_COMMA */8
                ];
      case 33 : 
          return /* tuple */[
                  env$1,
                  /* T_COLON */77
                ];
      case 34 : 
          return /* tuple */[
                  env$1,
                  /* T_PLING */76
                ];
      case 35 : 
          return /* tuple */[
                  env$1,
                  /* T_AND */79
                ];
      case 36 : 
          return /* tuple */[
                  env$1,
                  /* T_OR */78
                ];
      case 37 : 
          return /* tuple */[
                  env$1,
                  /* T_STRICT_EQUAL */85
                ];
      case 38 : 
          return /* tuple */[
                  env$1,
                  /* T_STRICT_NOT_EQUAL */86
                ];
      case 39 : 
          return /* tuple */[
                  env$1,
                  /* T_LESS_THAN_EQUAL */87
                ];
      case 40 : 
          return /* tuple */[
                  env$1,
                  /* T_GREATER_THAN_EQUAL */88
                ];
      case 41 : 
          return /* tuple */[
                  env$1,
                  /* T_EQUAL */83
                ];
      case 42 : 
          return /* tuple */[
                  env$1,
                  /* T_NOT_EQUAL */84
                ];
      case 43 : 
          return /* tuple */[
                  env$1,
                  /* T_INCR */102
                ];
      case 44 : 
          return /* tuple */[
                  env$1,
                  /* T_DECR */103
                ];
      case 45 : 
          return /* tuple */[
                  env$1,
                  /* T_LSHIFT_ASSIGN */65
                ];
      case 46 : 
          return /* tuple */[
                  env$1,
                  /* T_LSHIFT */91
                ];
      case 47 : 
          return /* tuple */[
                  env$1,
                  /* T_RSHIFT_ASSIGN */64
                ];
      case 48 : 
          return /* tuple */[
                  env$1,
                  /* T_RSHIFT3_ASSIGN */63
                ];
      case 49 : 
          return /* tuple */[
                  env$1,
                  /* T_RSHIFT3 */93
                ];
      case 50 : 
          return /* tuple */[
                  env$1,
                  /* T_RSHIFT */92
                ];
      case 51 : 
          return /* tuple */[
                  env$1,
                  /* T_PLUS_ASSIGN */74
                ];
      case 52 : 
          return /* tuple */[
                  env$1,
                  /* T_MINUS_ASSIGN */73
                ];
      case 53 : 
          return /* tuple */[
                  env$1,
                  /* T_MULT_ASSIGN */71
                ];
      case 54 : 
          return /* tuple */[
                  env$1,
                  /* T_EXP_ASSIGN */72
                ];
      case 55 : 
          return /* tuple */[
                  env$1,
                  /* T_MOD_ASSIGN */69
                ];
      case 56 : 
          return /* tuple */[
                  env$1,
                  /* T_BIT_AND_ASSIGN */68
                ];
      case 57 : 
          return /* tuple */[
                  env$1,
                  /* T_BIT_OR_ASSIGN */67
                ];
      case 58 : 
          return /* tuple */[
                  env$1,
                  /* T_BIT_XOR_ASSIGN */66
                ];
      case 59 : 
          return /* tuple */[
                  env$1,
                  /* T_LESS_THAN */89
                ];
      case 60 : 
          return /* tuple */[
                  env$1,
                  /* T_GREATER_THAN */90
                ];
      case 61 : 
          return /* tuple */[
                  env$1,
                  /* T_PLUS */94
                ];
      case 62 : 
          return /* tuple */[
                  env$1,
                  /* T_MINUS */95
                ];
      case 63 : 
          return /* tuple */[
                  env$1,
                  /* T_MULT */97
                ];
      case 64 : 
          return /* tuple */[
                  env$1,
                  /* T_EXP */98
                ];
      case 65 : 
          return /* tuple */[
                  env$1,
                  /* T_MOD */99
                ];
      case 66 : 
          return /* tuple */[
                  env$1,
                  /* T_BIT_OR */80
                ];
      case 67 : 
          return /* tuple */[
                  env$1,
                  /* T_BIT_AND */82
                ];
      case 68 : 
          return /* tuple */[
                  env$1,
                  /* T_BIT_XOR */81
                ];
      case 69 : 
          return /* tuple */[
                  env$1,
                  /* T_NOT */100
                ];
      case 70 : 
          return /* tuple */[
                  env$1,
                  /* T_BIT_NOT */101
                ];
      case 71 : 
          return /* tuple */[
                  env$1,
                  /* T_ASSIGN */75
                ];
      case 72 : 
          return /* tuple */[
                  env$1,
                  /* T_ARROW */10
                ];
      case 73 : 
          return /* tuple */[
                  env$1,
                  /* T_DIV_ASSIGN */70
                ];
      case 74 : 
          return /* tuple */[
                  env$1,
                  /* T_DIV */96
                ];
      case 75 : 
          return /* tuple */[
                  env$1,
                  /* T_AT */12
                ];
      case 76 : 
          var env$9;
          if (env$1[/* lex_in_comment_syntax */2]) {
            var loc$1 = from_lb(env$1[/* lex_source */0], lexbuf$1);
            env$9 = lex_error(env$1, loc$1, /* UnexpectedEOS */4);
          } else {
            env$9 = env$1;
          }
          return /* tuple */[
                  env$9,
                  /* T_EOF */105
                ];
      case 77 : 
          var env$10 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* UnexpectedToken */Block.__(1, ["ILLEGAL"]));
          return /* tuple */[
                  env$10,
                  /* T_ERROR */104
                ];
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
    if (exit === 1) {
      var w$3 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
      return illegal_number(env$1, lexbuf$1, w$3, /* T_NUMBER */Block.__(0, [/* NORMAL */3]));
    }
    
  };
}

function __ocaml_lex_regexp_rec(_env, lexbuf, ___ocaml_lex_state) {
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var env = _env;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          return /* tuple */[
                  env,
                  /* T_EOF */105
                ];
      case 1 : 
          Lexing.new_line(lexbuf);
          ___ocaml_lex_state = 291;
          continue ;
      case 2 : 
          unicode_fix_cols(lexbuf);
          ___ocaml_lex_state = 291;
          continue ;
      case 3 : 
          var start = from_lb(env[/* lex_source */0], lexbuf);
          var buf = $$Buffer.create(127);
          var match = line_comment(env, buf, lexbuf);
          var env$1 = save_comment(match[0], start, match[1], buf, true);
          ___ocaml_lex_state = 291;
          _env = env$1;
          continue ;
      case 4 : 
          var start$1 = from_lb(env[/* lex_source */0], lexbuf);
          var buf$1 = $$Buffer.create(127);
          var match$1 = comment(env, buf$1, lexbuf);
          var env$2 = save_comment(match$1[0], start$1, match$1[1], buf$1, true);
          ___ocaml_lex_state = 291;
          _env = env$2;
          continue ;
      case 5 : 
          var start$2 = from_lb(env[/* lex_source */0], lexbuf);
          var buf$2 = $$Buffer.create(127);
          var match$2 = regexp_body(env, buf$2, lexbuf);
          var env$3 = match$2[0];
          var end_ = from_lb(env$3[/* lex_source */0], lexbuf);
          var loc = btwn(start$2, end_);
          return /* tuple */[
                  env$3,
                  /* T_REGEXP */Block.__(3, [/* tuple */[
                        loc,
                        $$Buffer.contents(buf$2),
                        match$2[1]
                      ]])
                ];
      case 6 : 
          var env$4 = lex_error(env, from_lb(env[/* lex_source */0], lexbuf), /* UnexpectedToken */Block.__(1, ["ILLEGAL"]));
          return /* tuple */[
                  env$4,
                  /* T_ERROR */104
                ];
      default:
        Curry._1(lexbuf[/* refill_buff */0], lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function line_comment(env, buf, lexbuf) {
  var env$1 = env;
  var buf$1 = buf;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 287;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          return /* tuple */[
                  env$1,
                  from_lb(env$1[/* lex_source */0], lexbuf$1)
                ];
      case 1 : 
          var match = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var match$1 = match[/* _end */2];
          Lexing.new_line(lexbuf$1);
          var _end_000 = /* line */match$1[/* line */0];
          var _end_001 = /* column */match$1[/* column */1] - 1 | 0;
          var _end_002 = /* offset */match$1[/* offset */2] - 1 | 0;
          var _end = /* record */[
            _end_000,
            _end_001,
            _end_002
          ];
          return /* tuple */[
                  env$1,
                  /* record */[
                    /* source */match[/* source */0],
                    /* start */match[/* start */1],
                    /* _end */_end
                  ]
                ];
      case 2 : 
          var c = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(buf$1, c);
          return line_comment(env$1, buf$1, lexbuf$1);
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function comment(env, buf, lexbuf) {
  var env$1 = env;
  var buf$1 = buf;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 279;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          var env$2 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* UnexpectedToken */Block.__(1, ["ILLEGAL"]));
          return /* tuple */[
                  env$2,
                  from_lb(env$2[/* lex_source */0], lexbuf$1)
                ];
      case 1 : 
          Lexing.new_line(lexbuf$1);
          $$Buffer.add_char(buf$1, /* "\n" */10);
          return comment(env$1, buf$1, lexbuf$1);
      case 2 : 
          var loc = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var env$3 = env$1[/* lex_in_comment_syntax */2] ? unexpected_error_w_suggest(env$1, loc, "*/", "*-/") : env$1;
          return /* tuple */[
                  env$3,
                  loc
                ];
      case 3 : 
          if (env$1[/* lex_in_comment_syntax */2]) {
            return /* tuple */[
                    env$1,
                    from_lb(env$1[/* lex_source */0], lexbuf$1)
                  ];
          } else {
            $$Buffer.add_string(buf$1, "*-/");
            return comment(env$1, buf$1, lexbuf$1);
          }
      case 4 : 
          var c = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(buf$1, c);
          return comment(env$1, buf$1, lexbuf$1);
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function template_part(env, start, cooked, raw, literal, lexbuf) {
  var env$1 = env;
  var start$1 = start;
  var cooked$1 = cooked;
  var raw$1 = raw;
  var literal$1 = literal;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 416;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          var env$2 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* UnexpectedToken */Block.__(1, ["ILLEGAL"]));
          return /* tuple */[
                  env$2,
                  btwn(start$1, from_lb(env$2[/* lex_source */0], lexbuf$1)),
                  true
                ];
      case 1 : 
          $$Buffer.add_char(literal$1, /* "`" */96);
          return /* tuple */[
                  env$1,
                  btwn(start$1, from_lb(env$1[/* lex_source */0], lexbuf$1)),
                  true
                ];
      case 2 : 
          $$Buffer.add_string(literal$1, "${");
          return /* tuple */[
                  env$1,
                  btwn(start$1, from_lb(env$1[/* lex_source */0], lexbuf$1)),
                  false
                ];
      case 3 : 
          $$Buffer.add_char(raw$1, /* "\\" */92);
          $$Buffer.add_char(literal$1, /* "\\" */92);
          var match = string_escape(env$1, cooked$1, lexbuf$1);
          var str = Lexing.lexeme(lexbuf$1);
          $$Buffer.add_string(raw$1, str);
          $$Buffer.add_string(literal$1, str);
          return template_part(match[0], start$1, cooked$1, raw$1, literal$1, lexbuf$1);
      case 4 : 
          var lf = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_start_pos */4] + 2 | 0);
          $$Buffer.add_string(raw$1, lf);
          $$Buffer.add_string(literal$1, lf);
          $$Buffer.add_string(cooked$1, "\n");
          Lexing.new_line(lexbuf$1);
          return template_part(env$1, start$1, cooked$1, raw$1, literal$1, lexbuf$1);
      case 5 : 
          var lf$1 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(raw$1, lf$1);
          $$Buffer.add_char(literal$1, lf$1);
          $$Buffer.add_char(cooked$1, /* "\n" */10);
          Lexing.new_line(lexbuf$1);
          return template_part(env$1, start$1, cooked$1, raw$1, literal$1, lexbuf$1);
      case 6 : 
          var c = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(raw$1, c);
          $$Buffer.add_char(literal$1, c);
          $$Buffer.add_char(cooked$1, c);
          return template_part(env$1, start$1, cooked$1, raw$1, literal$1, lexbuf$1);
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function __ocaml_lex_template_tail_rec(_env, lexbuf, ___ocaml_lex_state) {
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var env = _env;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          Lexing.new_line(lexbuf);
          ___ocaml_lex_state = 393;
          continue ;
      case 1 : 
          unicode_fix_cols(lexbuf);
          ___ocaml_lex_state = 393;
          continue ;
      case 2 : 
          var start = from_lb(env[/* lex_source */0], lexbuf);
          var buf = $$Buffer.create(127);
          var match = line_comment(env, buf, lexbuf);
          var env$1 = save_comment(match[0], start, match[1], buf, true);
          ___ocaml_lex_state = 393;
          _env = env$1;
          continue ;
      case 3 : 
          var start$1 = from_lb(env[/* lex_source */0], lexbuf);
          var buf$1 = $$Buffer.create(127);
          var match$1 = comment(env, buf$1, lexbuf);
          var env$2 = save_comment(match$1[0], start$1, match$1[1], buf$1, true);
          ___ocaml_lex_state = 393;
          _env = env$2;
          continue ;
      case 4 : 
          var start$2 = from_lb(env[/* lex_source */0], lexbuf);
          var cooked = $$Buffer.create(127);
          var raw = $$Buffer.create(127);
          var literal = $$Buffer.create(127);
          $$Buffer.add_string(literal, "}");
          var match$2 = template_part(env, start$2, cooked, raw, literal, lexbuf);
          return /* tuple */[
                  match$2[0],
                  /* T_TEMPLATE_PART */Block.__(2, [/* tuple */[
                        match$2[1],
                        /* record */[
                          /* cooked */$$Buffer.contents(cooked),
                          /* raw */$$Buffer.contents(raw),
                          /* literal */$$Buffer.contents(literal)
                        ],
                        match$2[2]
                      ]])
                ];
      case 5 : 
          var env$3 = lex_error(env, from_lb(env[/* lex_source */0], lexbuf), /* UnexpectedToken */Block.__(1, ["ILLEGAL"]));
          return /* tuple */[
                  env$3,
                  /* T_TEMPLATE_PART */Block.__(2, [/* tuple */[
                        from_lb(env$3[/* lex_source */0], lexbuf),
                        /* record */[
                          /* cooked */"",
                          /* raw */"",
                          /* literal */""
                        ],
                        true
                      ]])
                ];
      default:
        Curry._1(lexbuf[/* refill_buff */0], lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function __ocaml_lex_jsx_tag_rec(_env, lexbuf, ___ocaml_lex_state) {
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var env = _env;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          return /* tuple */[
                  env,
                  /* T_EOF */105
                ];
      case 1 : 
          Lexing.new_line(lexbuf);
          ___ocaml_lex_state = 333;
          continue ;
      case 2 : 
          unicode_fix_cols(lexbuf);
          ___ocaml_lex_state = 333;
          continue ;
      case 3 : 
          var start = from_lb(env[/* lex_source */0], lexbuf);
          var buf = $$Buffer.create(127);
          var match = line_comment(env, buf, lexbuf);
          var env$1 = save_comment(match[0], start, match[1], buf, true);
          ___ocaml_lex_state = 333;
          _env = env$1;
          continue ;
      case 4 : 
          var start$1 = from_lb(env[/* lex_source */0], lexbuf);
          var buf$1 = $$Buffer.create(127);
          var match$1 = comment(env, buf$1, lexbuf);
          var env$2 = save_comment(match$1[0], start$1, match$1[1], buf$1, true);
          ___ocaml_lex_state = 333;
          _env = env$2;
          continue ;
      case 5 : 
          return /* tuple */[
                  env,
                  /* T_LESS_THAN */89
                ];
      case 6 : 
          return /* tuple */[
                  env,
                  /* T_DIV */96
                ];
      case 7 : 
          return /* tuple */[
                  env,
                  /* T_GREATER_THAN */90
                ];
      case 8 : 
          return /* tuple */[
                  env,
                  /* T_LCURLY */1
                ];
      case 9 : 
          return /* tuple */[
                  env,
                  /* T_COLON */77
                ];
      case 10 : 
          return /* tuple */[
                  env,
                  /* T_PERIOD */9
                ];
      case 11 : 
          return /* tuple */[
                  env,
                  /* T_ASSIGN */75
                ];
      case 12 : 
          unicode_fix_cols(lexbuf);
          return /* tuple */[
                  env,
                  /* T_JSX_IDENTIFIER */106
                ];
      case 13 : 
          var quote = Caml_bytes.get(lexbuf[/* lex_buffer */1], lexbuf[/* lex_start_pos */4]);
          var start$2 = from_lb(env[/* lex_source */0], lexbuf);
          var buf$2 = $$Buffer.create(127);
          var raw = $$Buffer.create(127);
          $$Buffer.add_char(raw, quote);
          var mode = quote === /* "'" */39 ? /* JSX_SINGLE_QUOTED_TEXT */0 : /* JSX_DOUBLE_QUOTED_TEXT */1;
          var match$2 = jsx_text(env, mode, buf$2, raw, lexbuf);
          $$Buffer.add_char(raw, quote);
          var value = $$Buffer.contents(buf$2);
          var raw$1 = $$Buffer.contents(raw);
          return /* tuple */[
                  match$2[0],
                  /* T_JSX_TEXT */Block.__(4, [/* tuple */[
                        btwn(start$2, match$2[1]),
                        value,
                        raw$1
                      ]])
                ];
      case 14 : 
          return /* tuple */[
                  env,
                  /* T_ERROR */104
                ];
      default:
        Curry._1(lexbuf[/* refill_buff */0], lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function jsx_text(env, mode, buf, raw, lexbuf) {
  var env$1 = env;
  var mode$1 = mode;
  var buf$1 = buf;
  var raw$1 = raw;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 371;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          var c = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          var exit = 0;
          switch (mode$1) {
            case 0 : 
                if (c !== 39) {
                  exit = 1;
                } else {
                  return /* tuple */[
                          env$1,
                          from_lb(env$1[/* lex_source */0], lexbuf$1)
                        ];
                }
                break;
            case 1 : 
                if (c !== 34) {
                  exit = 1;
                } else {
                  return /* tuple */[
                          env$1,
                          from_lb(env$1[/* lex_source */0], lexbuf$1)
                        ];
                }
                break;
            case 2 : 
                var exit$1 = 0;
                if (c !== 60 && c !== 123) {
                  exit = 1;
                } else {
                  exit$1 = 2;
                }
                if (exit$1 === 2) {
                  back(lexbuf$1);
                  return /* tuple */[
                          env$1,
                          from_lb(env$1[/* lex_source */0], lexbuf$1)
                        ];
                }
                break;
            
          }
          if (exit === 1) {
            $$Buffer.add_char(raw$1, c);
            $$Buffer.add_char(buf$1, c);
            return jsx_text(env$1, mode$1, buf$1, raw$1, lexbuf$1);
          }
          break;
      case 1 : 
          var env$2 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* UnexpectedToken */Block.__(1, ["ILLEGAL"]));
          return /* tuple */[
                  env$2,
                  from_lb(env$2[/* lex_source */0], lexbuf$1)
                ];
      case 2 : 
          var lt = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_curr_pos */5]);
          $$Buffer.add_string(raw$1, lt);
          $$Buffer.add_string(buf$1, lt);
          Lexing.new_line(lexbuf$1);
          return jsx_text(env$1, mode$1, buf$1, raw$1, lexbuf$1);
      case 3 : 
          var n = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4] + 3 | 0, lexbuf$1[/* lex_curr_pos */5] - 1 | 0);
          var s = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_curr_pos */5]);
          $$Buffer.add_string(raw$1, s);
          var code = Caml_format.caml_int_of_string("0x" + n);
          List.iter((function (param) {
                  return $$Buffer.add_char(buf$1, param);
                }), utf16to8(code));
          return jsx_text(env$1, mode$1, buf$1, raw$1, lexbuf$1);
      case 4 : 
          var n$1 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4] + 2 | 0, lexbuf$1[/* lex_curr_pos */5] - 1 | 0);
          var s$1 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_curr_pos */5]);
          $$Buffer.add_string(raw$1, s$1);
          var code$1 = Caml_format.caml_int_of_string(n$1);
          List.iter((function (param) {
                  return $$Buffer.add_char(buf$1, param);
                }), utf16to8(code$1));
          return jsx_text(env$1, mode$1, buf$1, raw$1, lexbuf$1);
      case 5 : 
          var entity = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4] + 1 | 0, lexbuf$1[/* lex_curr_pos */5] - 1 | 0);
          var s$2 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_curr_pos */5]);
          $$Buffer.add_string(raw$1, s$2);
          var code$2;
          switch (entity) {
            case "'int'" : 
                code$2 = 8747;
                break;
            case "AElig" : 
                code$2 = 198;
                break;
            case "Aacute" : 
                code$2 = 193;
                break;
            case "Acirc" : 
                code$2 = 194;
                break;
            case "Agrave" : 
                code$2 = 192;
                break;
            case "Alpha" : 
                code$2 = 913;
                break;
            case "Aring" : 
                code$2 = 197;
                break;
            case "Atilde" : 
                code$2 = 195;
                break;
            case "Auml" : 
                code$2 = 196;
                break;
            case "Beta" : 
                code$2 = 914;
                break;
            case "Ccedil" : 
                code$2 = 199;
                break;
            case "Chi" : 
                code$2 = 935;
                break;
            case "Dagger" : 
                code$2 = 8225;
                break;
            case "Delta" : 
                code$2 = 916;
                break;
            case "ETH" : 
                code$2 = 208;
                break;
            case "Eacute" : 
                code$2 = 201;
                break;
            case "Ecirc" : 
                code$2 = 202;
                break;
            case "Egrave" : 
                code$2 = 200;
                break;
            case "Epsilon" : 
                code$2 = 917;
                break;
            case "Eta" : 
                code$2 = 919;
                break;
            case "Euml" : 
                code$2 = 203;
                break;
            case "Gamma" : 
                code$2 = 915;
                break;
            case "Iacute" : 
                code$2 = 205;
                break;
            case "Icirc" : 
                code$2 = 206;
                break;
            case "Igrave" : 
                code$2 = 204;
                break;
            case "Iota" : 
                code$2 = 921;
                break;
            case "Iuml" : 
                code$2 = 207;
                break;
            case "Kappa" : 
                code$2 = 922;
                break;
            case "Lambda" : 
                code$2 = 923;
                break;
            case "Mu" : 
                code$2 = 924;
                break;
            case "Ntilde" : 
                code$2 = 209;
                break;
            case "Nu" : 
                code$2 = 925;
                break;
            case "OElig" : 
                code$2 = 338;
                break;
            case "Oacute" : 
                code$2 = 211;
                break;
            case "Ocirc" : 
                code$2 = 212;
                break;
            case "Ograve" : 
                code$2 = 210;
                break;
            case "Omega" : 
                code$2 = 937;
                break;
            case "Omicron" : 
                code$2 = 927;
                break;
            case "Oslash" : 
                code$2 = 216;
                break;
            case "Otilde" : 
                code$2 = 213;
                break;
            case "Ouml" : 
                code$2 = 214;
                break;
            case "Phi" : 
                code$2 = 934;
                break;
            case "Pi" : 
                code$2 = 928;
                break;
            case "Prime" : 
                code$2 = 8243;
                break;
            case "Psi" : 
                code$2 = 936;
                break;
            case "Rho" : 
                code$2 = 929;
                break;
            case "Scaron" : 
                code$2 = 352;
                break;
            case "Sigma" : 
                code$2 = 931;
                break;
            case "THORN" : 
                code$2 = 222;
                break;
            case "Tau" : 
                code$2 = 932;
                break;
            case "Theta" : 
                code$2 = 920;
                break;
            case "Uacute" : 
                code$2 = 218;
                break;
            case "Ucirc" : 
                code$2 = 219;
                break;
            case "Ugrave" : 
                code$2 = 217;
                break;
            case "Upsilon" : 
                code$2 = 933;
                break;
            case "Uuml" : 
                code$2 = 220;
                break;
            case "Xi" : 
                code$2 = 926;
                break;
            case "Yacute" : 
                code$2 = 221;
                break;
            case "Yuml" : 
                code$2 = 376;
                break;
            case "Zeta" : 
                code$2 = 918;
                break;
            case "aacute" : 
                code$2 = 225;
                break;
            case "acirc" : 
                code$2 = 226;
                break;
            case "acute" : 
                code$2 = 180;
                break;
            case "aelig" : 
                code$2 = 230;
                break;
            case "agrave" : 
                code$2 = 224;
                break;
            case "alefsym" : 
                code$2 = 8501;
                break;
            case "alpha" : 
                code$2 = 945;
                break;
            case "amp" : 
                code$2 = 38;
                break;
            case "and" : 
                code$2 = 8743;
                break;
            case "ang" : 
                code$2 = 8736;
                break;
            case "apos" : 
                code$2 = 39;
                break;
            case "aring" : 
                code$2 = 229;
                break;
            case "asymp" : 
                code$2 = 8776;
                break;
            case "atilde" : 
                code$2 = 227;
                break;
            case "auml" : 
                code$2 = 228;
                break;
            case "bdquo" : 
                code$2 = 8222;
                break;
            case "beta" : 
                code$2 = 946;
                break;
            case "brvbar" : 
                code$2 = 166;
                break;
            case "bull" : 
                code$2 = 8226;
                break;
            case "cap" : 
                code$2 = 8745;
                break;
            case "ccedil" : 
                code$2 = 231;
                break;
            case "cedil" : 
                code$2 = 184;
                break;
            case "cent" : 
                code$2 = 162;
                break;
            case "chi" : 
                code$2 = 967;
                break;
            case "circ" : 
                code$2 = 710;
                break;
            case "clubs" : 
                code$2 = 9827;
                break;
            case "cong" : 
                code$2 = 8773;
                break;
            case "copy" : 
                code$2 = 169;
                break;
            case "crarr" : 
                code$2 = 8629;
                break;
            case "cup" : 
                code$2 = 8746;
                break;
            case "curren" : 
                code$2 = 164;
                break;
            case "dArr" : 
                code$2 = 8659;
                break;
            case "dagger" : 
                code$2 = 8224;
                break;
            case "darr" : 
                code$2 = 8595;
                break;
            case "deg" : 
                code$2 = 176;
                break;
            case "delta" : 
                code$2 = 948;
                break;
            case "diams" : 
                code$2 = 9830;
                break;
            case "divide" : 
                code$2 = 247;
                break;
            case "eacute" : 
                code$2 = 233;
                break;
            case "ecirc" : 
                code$2 = 234;
                break;
            case "egrave" : 
                code$2 = 232;
                break;
            case "empty" : 
                code$2 = 8709;
                break;
            case "emsp" : 
                code$2 = 8195;
                break;
            case "ensp" : 
                code$2 = 8194;
                break;
            case "epsilon" : 
                code$2 = 949;
                break;
            case "equiv" : 
                code$2 = 8801;
                break;
            case "eta" : 
                code$2 = 951;
                break;
            case "eth" : 
                code$2 = 240;
                break;
            case "euml" : 
                code$2 = 235;
                break;
            case "euro" : 
                code$2 = 8364;
                break;
            case "exist" : 
                code$2 = 8707;
                break;
            case "fnof" : 
                code$2 = 402;
                break;
            case "forall" : 
                code$2 = 8704;
                break;
            case "frac12" : 
                code$2 = 189;
                break;
            case "frac14" : 
                code$2 = 188;
                break;
            case "frac34" : 
                code$2 = 190;
                break;
            case "frasl" : 
                code$2 = 8260;
                break;
            case "gamma" : 
                code$2 = 947;
                break;
            case "ge" : 
                code$2 = 8805;
                break;
            case "gt" : 
                code$2 = 62;
                break;
            case "hArr" : 
                code$2 = 8660;
                break;
            case "harr" : 
                code$2 = 8596;
                break;
            case "hearts" : 
                code$2 = 9829;
                break;
            case "hellip" : 
                code$2 = 8230;
                break;
            case "iacute" : 
                code$2 = 237;
                break;
            case "icirc" : 
                code$2 = 238;
                break;
            case "iexcl" : 
                code$2 = 161;
                break;
            case "igrave" : 
                code$2 = 236;
                break;
            case "image" : 
                code$2 = 8465;
                break;
            case "infin" : 
                code$2 = 8734;
                break;
            case "iota" : 
                code$2 = 953;
                break;
            case "iquest" : 
                code$2 = 191;
                break;
            case "isin" : 
                code$2 = 8712;
                break;
            case "iuml" : 
                code$2 = 239;
                break;
            case "kappa" : 
                code$2 = 954;
                break;
            case "lArr" : 
                code$2 = 8656;
                break;
            case "lambda" : 
                code$2 = 955;
                break;
            case "lang" : 
                code$2 = 10216;
                break;
            case "laquo" : 
                code$2 = 171;
                break;
            case "larr" : 
                code$2 = 8592;
                break;
            case "lceil" : 
                code$2 = 8968;
                break;
            case "ldquo" : 
                code$2 = 8220;
                break;
            case "le" : 
                code$2 = 8804;
                break;
            case "lfloor" : 
                code$2 = 8970;
                break;
            case "lowast" : 
                code$2 = 8727;
                break;
            case "loz" : 
                code$2 = 9674;
                break;
            case "lrm" : 
                code$2 = 8206;
                break;
            case "lsaquo" : 
                code$2 = 8249;
                break;
            case "lsquo" : 
                code$2 = 8216;
                break;
            case "lt" : 
                code$2 = 60;
                break;
            case "macr" : 
                code$2 = 175;
                break;
            case "mdash" : 
                code$2 = 8212;
                break;
            case "micro" : 
                code$2 = 181;
                break;
            case "middot" : 
                code$2 = 183;
                break;
            case "minus" : 
                code$2 = 8722;
                break;
            case "mu" : 
                code$2 = 956;
                break;
            case "nabla" : 
                code$2 = 8711;
                break;
            case "nbsp" : 
                code$2 = 160;
                break;
            case "ndash" : 
                code$2 = 8211;
                break;
            case "ne" : 
                code$2 = 8800;
                break;
            case "ni" : 
                code$2 = 8715;
                break;
            case "not" : 
                code$2 = 172;
                break;
            case "notin" : 
                code$2 = 8713;
                break;
            case "nsub" : 
                code$2 = 8836;
                break;
            case "ntilde" : 
                code$2 = 241;
                break;
            case "nu" : 
                code$2 = 957;
                break;
            case "oacute" : 
                code$2 = 243;
                break;
            case "ocirc" : 
                code$2 = 244;
                break;
            case "oelig" : 
                code$2 = 339;
                break;
            case "ograve" : 
                code$2 = 242;
                break;
            case "oline" : 
                code$2 = 8254;
                break;
            case "omega" : 
                code$2 = 969;
                break;
            case "omicron" : 
                code$2 = 959;
                break;
            case "oplus" : 
                code$2 = 8853;
                break;
            case "or" : 
                code$2 = 8744;
                break;
            case "ordf" : 
                code$2 = 170;
                break;
            case "ordm" : 
                code$2 = 186;
                break;
            case "oslash" : 
                code$2 = 248;
                break;
            case "otilde" : 
                code$2 = 245;
                break;
            case "otimes" : 
                code$2 = 8855;
                break;
            case "ouml" : 
                code$2 = 246;
                break;
            case "para" : 
                code$2 = 182;
                break;
            case "part" : 
                code$2 = 8706;
                break;
            case "permil" : 
                code$2 = 8240;
                break;
            case "perp" : 
                code$2 = 8869;
                break;
            case "phi" : 
                code$2 = 966;
                break;
            case "pi" : 
                code$2 = 960;
                break;
            case "piv" : 
                code$2 = 982;
                break;
            case "plusmn" : 
                code$2 = 177;
                break;
            case "pound" : 
                code$2 = 163;
                break;
            case "prime" : 
                code$2 = 8242;
                break;
            case "prod" : 
                code$2 = 8719;
                break;
            case "prop" : 
                code$2 = 8733;
                break;
            case "psi" : 
                code$2 = 968;
                break;
            case "quot" : 
                code$2 = 34;
                break;
            case "rArr" : 
                code$2 = 8658;
                break;
            case "radic" : 
                code$2 = 8730;
                break;
            case "rang" : 
                code$2 = 10217;
                break;
            case "raquo" : 
                code$2 = 187;
                break;
            case "rarr" : 
                code$2 = 8594;
                break;
            case "rceil" : 
                code$2 = 8969;
                break;
            case "rdquo" : 
                code$2 = 8221;
                break;
            case "real" : 
                code$2 = 8476;
                break;
            case "reg" : 
                code$2 = 174;
                break;
            case "rfloor" : 
                code$2 = 8971;
                break;
            case "rho" : 
                code$2 = 961;
                break;
            case "rlm" : 
                code$2 = 8207;
                break;
            case "rsaquo" : 
                code$2 = 8250;
                break;
            case "rsquo" : 
                code$2 = 8217;
                break;
            case "sbquo" : 
                code$2 = 8218;
                break;
            case "scaron" : 
                code$2 = 353;
                break;
            case "sdot" : 
                code$2 = 8901;
                break;
            case "sect" : 
                code$2 = 167;
                break;
            case "shy" : 
                code$2 = 173;
                break;
            case "sigma" : 
                code$2 = 963;
                break;
            case "sigmaf" : 
                code$2 = 962;
                break;
            case "sim" : 
                code$2 = 8764;
                break;
            case "spades" : 
                code$2 = 9824;
                break;
            case "sub" : 
                code$2 = 8834;
                break;
            case "sube" : 
                code$2 = 8838;
                break;
            case "sum" : 
                code$2 = 8721;
                break;
            case "sup" : 
                code$2 = 8835;
                break;
            case "sup1" : 
                code$2 = 185;
                break;
            case "sup2" : 
                code$2 = 178;
                break;
            case "sup3" : 
                code$2 = 179;
                break;
            case "supe" : 
                code$2 = 8839;
                break;
            case "szlig" : 
                code$2 = 223;
                break;
            case "tau" : 
                code$2 = 964;
                break;
            case "there4" : 
                code$2 = 8756;
                break;
            case "theta" : 
                code$2 = 952;
                break;
            case "thetasym" : 
                code$2 = 977;
                break;
            case "thinsp" : 
                code$2 = 8201;
                break;
            case "thorn" : 
                code$2 = 254;
                break;
            case "tilde" : 
                code$2 = 732;
                break;
            case "times" : 
                code$2 = 215;
                break;
            case "trade" : 
                code$2 = 8482;
                break;
            case "uArr" : 
                code$2 = 8657;
                break;
            case "uacute" : 
                code$2 = 250;
                break;
            case "uarr" : 
                code$2 = 8593;
                break;
            case "ucirc" : 
                code$2 = 251;
                break;
            case "ugrave" : 
                code$2 = 249;
                break;
            case "uml" : 
                code$2 = 168;
                break;
            case "upsih" : 
                code$2 = 978;
                break;
            case "upsilon" : 
                code$2 = 965;
                break;
            case "uuml" : 
                code$2 = 252;
                break;
            case "weierp" : 
                code$2 = 8472;
                break;
            case "xi" : 
                code$2 = 958;
                break;
            case "yacute" : 
                code$2 = 253;
                break;
            case "yen" : 
                code$2 = 165;
                break;
            case "yuml" : 
                code$2 = 255;
                break;
            case "zeta" : 
                code$2 = 950;
                break;
            case "zwj" : 
                code$2 = 8205;
                break;
            case "zwnj" : 
                code$2 = 8204;
                break;
            default:
              code$2 = undefined;
          }
          if (code$2 !== undefined) {
            List.iter((function (param) {
                    return $$Buffer.add_char(buf$1, param);
                  }), utf16to8(code$2));
          } else {
            $$Buffer.add_string(buf$1, "&" + (entity + ";"));
          }
          return jsx_text(env$1, mode$1, buf$1, raw$1, lexbuf$1);
      case 6 : 
          var c$1 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(raw$1, c$1);
          $$Buffer.add_char(buf$1, c$1);
          return jsx_text(env$1, mode$1, buf$1, raw$1, lexbuf$1);
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function regexp_body(env, buf, lexbuf) {
  var env$1 = env;
  var buf$1 = buf;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 314;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          var loc = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var env$2 = lex_error(env$1, loc, /* UnterminatedRegExp */13);
          return /* tuple */[
                  env$2,
                  ""
                ];
      case 1 : 
          var loc$1 = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var env$3 = lex_error(env$1, loc$1, /* UnterminatedRegExp */13);
          return /* tuple */[
                  env$3,
                  ""
                ];
      case 2 : 
          var s = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_start_pos */4] + 2 | 0);
          $$Buffer.add_string(buf$1, s);
          return regexp_body(env$1, buf$1, lexbuf$1);
      case 3 : 
          var flags = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4] + 1 | 0, lexbuf$1[/* lex_curr_pos */5]);
          return /* tuple */[
                  env$1,
                  flags
                ];
      case 4 : 
          return /* tuple */[
                  env$1,
                  ""
                ];
      case 5 : 
          var c = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(buf$1, c);
          var env$4 = regexp_class(env$1, buf$1, lexbuf$1);
          return regexp_body(env$4, buf$1, lexbuf$1);
      case 6 : 
          var loc$2 = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var env$5 = lex_error(env$1, loc$2, /* UnterminatedRegExp */13);
          return /* tuple */[
                  env$5,
                  ""
                ];
      case 7 : 
          var c$1 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(buf$1, c$1);
          return regexp_body(env$1, buf$1, lexbuf$1);
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function regexp_class(env, buf, lexbuf) {
  var env$1 = env;
  var buf$1 = buf;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 326;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    var exit = 0;
    switch (__ocaml_lex_state$1) {
      case 0 : 
          return env$1;
      case 1 : 
      case 2 : 
          exit = 1;
          break;
      case 3 : 
          var c = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(buf$1, c);
          return env$1;
      case 4 : 
          var c$1 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(buf$1, c$1);
          return regexp_class(env$1, buf$1, lexbuf$1);
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
    if (exit === 1) {
      var s = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_start_pos */4] + 2 | 0);
      $$Buffer.add_string(buf$1, s);
      return regexp_class(env$1, buf$1, lexbuf$1);
    }
    
  };
}

function string_escape(env, buf, lexbuf) {
  var env$1 = env;
  var buf$1 = buf;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 252;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          return /* tuple */[
                  env$1,
                  false
                ];
      case 1 : 
          $$Buffer.add_string(buf$1, "\\");
          return /* tuple */[
                  env$1,
                  false
                ];
      case 2 : 
          var a = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4] + 1 | 0);
          var b = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4] + 2 | 0);
          var code = (hexa_to_int(a) << 4) + hexa_to_int(b) | 0;
          List.iter((function (param) {
                  return $$Buffer.add_char(buf$1, param);
                }), utf16to8(code));
          return /* tuple */[
                  env$1,
                  false
                ];
      case 3 : 
          var a$1 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          var b$1 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4] + 1 | 0);
          var c = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4] + 2 | 0);
          var code$1 = ((oct_to_int(a$1) << 6) + (oct_to_int(b$1) << 3) | 0) + oct_to_int(c) | 0;
          if (code$1 < 256) {
            List.iter((function (param) {
                    return $$Buffer.add_char(buf$1, param);
                  }), utf16to8(code$1));
          } else {
            var code$2 = (oct_to_int(a$1) << 3) + oct_to_int(b$1) | 0;
            List.iter((function (param) {
                    return $$Buffer.add_char(buf$1, param);
                  }), utf16to8(code$2));
            $$Buffer.add_char(buf$1, c);
          }
          return /* tuple */[
                  env$1,
                  true
                ];
      case 4 : 
          var a$2 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          var b$2 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4] + 1 | 0);
          var code$3 = (oct_to_int(a$2) << 3) + oct_to_int(b$2) | 0;
          List.iter((function (param) {
                  return $$Buffer.add_char(buf$1, param);
                }), utf16to8(code$3));
          return /* tuple */[
                  env$1,
                  true
                ];
      case 5 : 
          $$Buffer.add_char(buf$1, Char.chr(0));
          return /* tuple */[
                  env$1,
                  false
                ];
      case 6 : 
          $$Buffer.add_char(buf$1, Char.chr(8));
          return /* tuple */[
                  env$1,
                  false
                ];
      case 7 : 
          $$Buffer.add_char(buf$1, Char.chr(12));
          return /* tuple */[
                  env$1,
                  false
                ];
      case 8 : 
          $$Buffer.add_char(buf$1, Char.chr(10));
          return /* tuple */[
                  env$1,
                  false
                ];
      case 9 : 
          $$Buffer.add_char(buf$1, Char.chr(13));
          return /* tuple */[
                  env$1,
                  false
                ];
      case 10 : 
          $$Buffer.add_char(buf$1, Char.chr(9));
          return /* tuple */[
                  env$1,
                  false
                ];
      case 11 : 
          $$Buffer.add_char(buf$1, Char.chr(11));
          return /* tuple */[
                  env$1,
                  false
                ];
      case 12 : 
          var a$3 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          var code$4 = oct_to_int(a$3);
          List.iter((function (param) {
                  return $$Buffer.add_char(buf$1, param);
                }), utf16to8(code$4));
          return /* tuple */[
                  env$1,
                  true
                ];
      case 13 : 
          var a$4 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4] + 1 | 0);
          var b$3 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4] + 2 | 0);
          var c$1 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4] + 3 | 0);
          var d = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4] + 4 | 0);
          var code$5 = (((hexa_to_int(a$4) << 12) + (hexa_to_int(b$3) << 8) | 0) + (hexa_to_int(c$1) << 4) | 0) + hexa_to_int(d) | 0;
          List.iter((function (param) {
                  return $$Buffer.add_char(buf$1, param);
                }), utf16to8(code$5));
          return /* tuple */[
                  env$1,
                  false
                ];
      case 14 : 
          var hex_code = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4] + 2 | 0, lexbuf$1[/* lex_curr_pos */5] - 1 | 0);
          var code$6 = Caml_format.caml_int_of_string("0x" + hex_code);
          var env$2 = code$6 > 1114111 ? lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* UnexpectedToken */Block.__(1, ["ILLEGAL"])) : env$1;
          List.iter((function (param) {
                  return $$Buffer.add_char(buf$1, param);
                }), utf16to8(code$6));
          return /* tuple */[
                  env$2,
                  false
                ];
      case 15 : 
          var c$2 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          var env$3 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* UnexpectedToken */Block.__(1, ["ILLEGAL"]));
          $$Buffer.add_char(buf$1, c$2);
          return /* tuple */[
                  env$3,
                  false
                ];
      case 16 : 
          Lexing.new_line(lexbuf$1);
          return /* tuple */[
                  env$1,
                  false
                ];
      case 17 : 
          var c$3 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(buf$1, c$3);
          return /* tuple */[
                  env$1,
                  false
                ];
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function string_quote(env, q, buf, raw, octal, lexbuf) {
  var env$1 = env;
  var q$1 = q;
  var buf$1 = buf;
  var raw$1 = raw;
  var octal$1 = octal;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 247;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          var q$prime = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(raw$1, q$prime);
          if (q$1 === q$prime) {
            return /* tuple */[
                    env$1,
                    from_lb(env$1[/* lex_source */0], lexbuf$1),
                    octal$1
                  ];
          } else {
            $$Buffer.add_char(buf$1, q$prime);
            return string_quote(env$1, q$1, buf$1, raw$1, octal$1, lexbuf$1);
          }
      case 1 : 
          var e = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(raw$1, e);
          var match = string_escape(env$1, buf$1, lexbuf$1);
          var octal$2 = match[1] || octal$1;
          $$Buffer.add_string(raw$1, Lexing.lexeme(lexbuf$1));
          return string_quote(match[0], q$1, buf$1, raw$1, octal$2, lexbuf$1);
      case 2 : 
          var x = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_curr_pos */5]);
          $$Buffer.add_string(raw$1, x);
          var env$2 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* UnexpectedToken */Block.__(1, ["ILLEGAL"]));
          $$Buffer.add_string(buf$1, x);
          return /* tuple */[
                  env$2,
                  from_lb(env$2[/* lex_source */0], lexbuf$1),
                  octal$1
                ];
      case 3 : 
          var x$1 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(raw$1, x$1);
          $$Buffer.add_char(buf$1, x$1);
          return string_quote(env$1, q$1, buf$1, raw$1, octal$1, lexbuf$1);
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function type_token(env, lexbuf) {
  lexbuf[/* lex_mem */9] = Caml_array.caml_make_vect(26, -1);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 17, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 16, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 15, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 14, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 13, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 12, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 11, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 10, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 9, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 8, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 7, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 6, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 5, lexbuf[/* lex_curr_pos */5]);
  Caml_array.caml_array_set(lexbuf[/* lex_mem */9], 4, lexbuf[/* lex_curr_pos */5]);
  var env$1 = env;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 133;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.new_engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          Lexing.new_line(lexbuf$1);
          return type_token(env$1, lexbuf$1);
      case 1 : 
          unicode_fix_cols(lexbuf$1);
          return type_token(env$1, lexbuf$1);
      case 2 : 
          var start = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var buf = $$Buffer.create(127);
          var match = comment(env$1, buf, lexbuf$1);
          var env$2 = save_comment(match[0], start, match[1], buf, true);
          return type_token(env$2, lexbuf$1);
      case 3 : 
          var sp = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4] + 2 | 0, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var escape_type = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          var pattern = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_curr_pos */5]);
          if (env$1[/* lex_enable_comment_syntax */3]) {
            var env$3;
            if (env$1[/* lex_in_comment_syntax */2]) {
              var loc = from_lb(env$1[/* lex_source */0], lexbuf$1);
              env$3 = unexpected_error(env$1, loc, pattern);
            } else {
              env$3 = env$1;
            }
            var env$4 = in_comment_syntax(true, env$3);
            if (escape_type === ":") {
              return /* tuple */[
                      env$4,
                      /* T_COLON */77
                    ];
            } else {
              return type_token(env$4, lexbuf$1);
            }
          } else {
            var start$1 = from_lb(env$1[/* lex_source */0], lexbuf$1);
            var buf$1 = $$Buffer.create(127);
            $$Buffer.add_string(buf$1, sp);
            $$Buffer.add_string(buf$1, escape_type);
            var match$1 = comment(env$1, buf$1, lexbuf$1);
            var env$5 = save_comment(match$1[0], start$1, match$1[1], buf$1, true);
            return type_token(env$5, lexbuf$1);
          }
      case 4 : 
          if (env$1[/* lex_in_comment_syntax */2]) {
            var env$6 = in_comment_syntax(false, env$1);
            return type_token(env$6, lexbuf$1);
          } else {
            yyback(1, lexbuf$1);
            return /* tuple */[
                    env$1,
                    /* T_MULT */97
                  ];
          }
      case 5 : 
          var start$2 = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var buf$2 = $$Buffer.create(127);
          var match$2 = line_comment(env$1, buf$2, lexbuf$1);
          var env$7 = save_comment(match$2[0], start$2, match$2[1], buf$2, true);
          return type_token(env$7, lexbuf$1);
      case 6 : 
          var quote = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          var start$3 = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var buf$3 = $$Buffer.create(127);
          var raw = $$Buffer.create(127);
          $$Buffer.add_char(raw, quote);
          var match$3 = string_quote(env$1, quote, buf$3, raw, false, lexbuf$1);
          return /* tuple */[
                  match$3[0],
                  /* T_STRING */Block.__(1, [/* tuple */[
                        btwn(start$3, match$3[1]),
                        $$Buffer.contents(buf$3),
                        $$Buffer.contents(raw),
                        match$3[2]
                      ]])
                ];
      case 7 : 
          var neg = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w, mk_num_singleton(/* BINARY */0, num, neg));
      case 8 : 
          var neg$1 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$1 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return /* tuple */[
                  env$1,
                  mk_num_singleton(/* BINARY */0, num$1, neg$1)
                ];
      case 9 : 
          var neg$2 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$2 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w$1 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$1, mk_num_singleton(/* OCTAL */2, num$2, neg$2));
      case 10 : 
          var neg$3 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$3 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return /* tuple */[
                  env$1,
                  mk_num_singleton(/* OCTAL */2, num$3, neg$3)
                ];
      case 11 : 
          var neg$4 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$4 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w$2 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$2, mk_num_singleton(/* LEGACY_OCTAL */1, num$4, neg$4));
      case 12 : 
          var neg$5 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$5 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return /* tuple */[
                  env$1,
                  mk_num_singleton(/* LEGACY_OCTAL */1, num$5, neg$5)
                ];
      case 13 : 
          var neg$6 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$6 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w$3 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          var match$4;
          try {
            match$4 = /* tuple */[
              env$1,
              mk_num_singleton(/* NORMAL */3, num$6, neg$6)
            ];
          }
          catch (exn){
            if (Sys.win32) {
              var loc$1 = from_lb(env$1[/* lex_source */0], lexbuf$1);
              var env$8 = lex_error(env$1, loc$1, /* WindowsFloatOfString */59);
              match$4 = /* tuple */[
                env$8,
                /* T_NUMBER_SINGLETON_TYPE */Block.__(5, [
                    /* NORMAL */3,
                    789.0
                  ])
              ];
            } else {
              throw exn;
            }
          }
          return illegal_number(match$4[0], lexbuf$1, w$3, match$4[1]);
      case 14 : 
          var neg$7 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$7 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          try {
            return /* tuple */[
                    env$1,
                    mk_num_singleton(/* NORMAL */3, num$7, neg$7)
                  ];
          }
          catch (exn$1){
            if (Sys.win32) {
              var loc$2 = from_lb(env$1[/* lex_source */0], lexbuf$1);
              var env$9 = lex_error(env$1, loc$2, /* WindowsFloatOfString */59);
              return /* tuple */[
                      env$9,
                      /* T_NUMBER_SINGLETON_TYPE */Block.__(5, [
                          /* NORMAL */3,
                          789.0
                        ])
                    ];
            } else {
              throw exn$1;
            }
          }
      case 15 : 
          var neg$8 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$8 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w$4 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$4, mk_num_singleton(/* NORMAL */3, num$8, neg$8));
      case 16 : 
          var neg$9 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$9 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return /* tuple */[
                  env$1,
                  mk_num_singleton(/* NORMAL */3, num$9, neg$9)
                ];
      case 17 : 
          var neg$10 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$10 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w$5 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$5, mk_num_singleton(/* NORMAL */3, num$10, neg$10));
      case 18 : 
          var neg$11 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$11 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 3), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 2));
          return /* tuple */[
                  env$1,
                  mk_num_singleton(/* NORMAL */3, num$11, neg$11)
                ];
      case 19 : 
          var word = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_curr_pos */5]);
          unicode_fix_cols(lexbuf$1);
          try {
            return /* tuple */[
                    env$1,
                    Hashtbl.find(type_keywords, word)
                  ];
          }
          catch (exn$2){
            if (exn$2 === Caml_builtin_exceptions.not_found) {
              return /* tuple */[
                      env$1,
                      /* T_IDENTIFIER */0
                    ];
            } else {
              throw exn$2;
            }
          }
      case 22 : 
          return /* tuple */[
                  env$1,
                  /* T_LCURLY */1
                ];
      case 23 : 
          return /* tuple */[
                  env$1,
                  /* T_RCURLY */2
                ];
      case 24 : 
          return /* tuple */[
                  env$1,
                  /* T_LPAREN */3
                ];
      case 25 : 
          return /* tuple */[
                  env$1,
                  /* T_RPAREN */4
                ];
      case 26 : 
          return /* tuple */[
                  env$1,
                  /* T_ELLIPSIS */11
                ];
      case 27 : 
          return /* tuple */[
                  env$1,
                  /* T_PERIOD */9
                ];
      case 28 : 
          return /* tuple */[
                  env$1,
                  /* T_SEMICOLON */7
                ];
      case 29 : 
          return /* tuple */[
                  env$1,
                  /* T_COMMA */8
                ];
      case 20 : 
      case 32 : 
          return /* tuple */[
                  env$1,
                  /* T_LBRACKET */5
                ];
      case 21 : 
      case 33 : 
          return /* tuple */[
                  env$1,
                  /* T_RBRACKET */6
                ];
      case 34 : 
          return /* tuple */[
                  env$1,
                  /* T_LESS_THAN */89
                ];
      case 35 : 
          return /* tuple */[
                  env$1,
                  /* T_GREATER_THAN */90
                ];
      case 31 : 
      case 37 : 
          return /* tuple */[
                  env$1,
                  /* T_PLING */76
                ];
      case 38 : 
          return /* tuple */[
                  env$1,
                  /* T_MULT */97
                ];
      case 30 : 
      case 39 : 
          return /* tuple */[
                  env$1,
                  /* T_COLON */77
                ];
      case 40 : 
          return /* tuple */[
                  env$1,
                  /* T_BIT_OR */80
                ];
      case 41 : 
          return /* tuple */[
                  env$1,
                  /* T_BIT_AND */82
                ];
      case 42 : 
          return /* tuple */[
                  env$1,
                  /* T_TYPEOF */44
                ];
      case 43 : 
          return /* tuple */[
                  env$1,
                  /* T_ARROW */10
                ];
      case 36 : 
      case 44 : 
          return /* tuple */[
                  env$1,
                  /* T_ASSIGN */75
                ];
      case 45 : 
          return /* tuple */[
                  env$1,
                  /* T_PLUS */94
                ];
      case 46 : 
          return /* tuple */[
                  env$1,
                  /* T_MINUS */95
                ];
      case 47 : 
          var env$10;
          if (env$1[/* lex_in_comment_syntax */2]) {
            var loc$3 = from_lb(env$1[/* lex_source */0], lexbuf$1);
            env$10 = lex_error(env$1, loc$3, /* UnexpectedEOS */4);
          } else {
            env$10 = env$1;
          }
          return /* tuple */[
                  env$10,
                  /* T_EOF */105
                ];
      case 48 : 
          return /* tuple */[
                  env$1,
                  /* T_ERROR */104
                ];
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function jsx_child(env, start, buf, raw, lexbuf) {
  var env$1 = env;
  var start$1 = start;
  var buf$1 = buf;
  var raw$1 = raw;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 364;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    switch (__ocaml_lex_state$1) {
      case 0 : 
          var lt = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_curr_pos */5]);
          $$Buffer.add_string(raw$1, lt);
          $$Buffer.add_string(buf$1, lt);
          Lexing.new_line(lexbuf$1);
          var match = jsx_text(env$1, /* JSX_CHILD_TEXT */2, buf$1, raw$1, lexbuf$1);
          var value = $$Buffer.contents(buf$1);
          var raw$2 = $$Buffer.contents(raw$1);
          return /* tuple */[
                  match[0],
                  /* T_JSX_TEXT */Block.__(4, [/* tuple */[
                        btwn(start$1, match[1]),
                        value,
                        raw$2
                      ]])
                ];
      case 1 : 
          return /* tuple */[
                  env$1,
                  /* T_EOF */105
                ];
      case 2 : 
          return /* tuple */[
                  env$1,
                  /* T_LESS_THAN */89
                ];
      case 3 : 
          return /* tuple */[
                  env$1,
                  /* T_LCURLY */1
                ];
      case 4 : 
          var c = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(raw$1, c);
          $$Buffer.add_char(buf$1, c);
          var match$1 = jsx_text(env$1, /* JSX_CHILD_TEXT */2, buf$1, raw$1, lexbuf$1);
          var value$1 = $$Buffer.contents(buf$1);
          var raw$3 = $$Buffer.contents(raw$1);
          return /* tuple */[
                  match$1[0],
                  /* T_JSX_TEXT */Block.__(4, [/* tuple */[
                        btwn(start$1, match$1[1]),
                        value$1,
                        raw$3
                      ]])
                ];
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function regexp(env) {
  return get_result_and_clear_state(__ocaml_lex_regexp_rec(env, env[/* lex_lb */1], 291));
}

function jsx_child$1(env) {
  var start = from_curr_lb(env[/* lex_source */0], env[/* lex_lb */1]);
  var buf = $$Buffer.create(127);
  var raw = $$Buffer.create(127);
  var match = jsx_child(env, start, buf, raw, env[/* lex_lb */1]);
  return get_result_and_clear_state(/* tuple */[
              match[0],
              match[1]
            ]);
}

function jsx_tag(env) {
  return get_result_and_clear_state(__ocaml_lex_jsx_tag_rec(env, env[/* lex_lb */1], 333));
}

function template_tail(env) {
  return get_result_and_clear_state(__ocaml_lex_template_tail_rec(env, env[/* lex_lb */1], 393));
}

function type_token$1(env) {
  return get_result_and_clear_state(type_token(env, env[/* lex_lb */1]));
}

function token$1(env) {
  return get_result_and_clear_state(token(env, env[/* lex_lb */1]));
}

function height(param) {
  if (param) {
    return param[/* h */3];
  } else {
    return 0;
  }
}

function create(l, v, r) {
  var hl = l ? l[/* h */3] : 0;
  var hr = r ? r[/* h */3] : 0;
  return /* Node */[
          /* l */l,
          /* v */v,
          /* r */r,
          /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal(l, v, r) {
  var hl = l ? l[/* h */3] : 0;
  var hr = r ? r[/* h */3] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[/* r */2];
      var lv = l[/* v */1];
      var ll = l[/* l */0];
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      } else if (lr) {
        return create(create(ll, lv, lr[/* l */0]), lr[/* v */1], create(lr[/* r */2], v, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Set.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Set.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r) {
      var rr = r[/* r */2];
      var rv = r[/* v */1];
      var rl = r[/* l */0];
      if (height(rr) >= height(rl)) {
        return create(create(l, v, rl), rv, rr);
      } else if (rl) {
        return create(create(l, v, rl[/* l */0]), rl[/* v */1], create(rl[/* r */2], rv, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Set.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Set.bal"
          ];
    }
  } else {
    return /* Node */[
            /* l */l,
            /* v */v,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function add(x, t) {
  if (t) {
    var r = t[/* r */2];
    var v = t[/* v */1];
    var l = t[/* l */0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return t;
    } else if (c < 0) {
      var ll = add(x, l);
      if (l === ll) {
        return t;
      } else {
        return bal(ll, v, r);
      }
    } else {
      var rr = add(x, r);
      if (r === rr) {
        return t;
      } else {
        return bal(l, v, rr);
      }
    }
  } else {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* r : Empty */0,
            /* h */1
          ];
  }
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param[/* v */1]);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param[/* l */0] : param[/* r */2];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function create$1(lex_env, mode) {
  var lexbuf = lex_env[/* lex_lb */1];
  var lexbuf$1 = /* record */[
    /* refill_buff */lexbuf[/* refill_buff */0],
    /* lex_buffer */lexbuf[/* lex_buffer */1],
    /* lex_buffer_len */lexbuf[/* lex_buffer_len */2],
    /* lex_abs_pos */lexbuf[/* lex_abs_pos */3],
    /* lex_start_pos */lexbuf[/* lex_start_pos */4],
    /* lex_curr_pos */lexbuf[/* lex_curr_pos */5],
    /* lex_last_pos */lexbuf[/* lex_last_pos */6],
    /* lex_last_action */lexbuf[/* lex_last_action */7],
    /* lex_eof_reached */lexbuf[/* lex_eof_reached */8],
    /* lex_mem */lexbuf[/* lex_mem */9],
    /* lex_start_p */lexbuf[/* lex_start_p */10],
    /* lex_curr_p */lexbuf[/* lex_curr_p */11]
  ];
  var lex_env$1 = with_lexbuf(lexbuf$1, lex_env);
  return /* record */[
          /* la_results : array */[],
          /* la_num_lexed */0,
          /* la_lex_mode */mode,
          /* la_lex_env */lex_env$1
        ];
}

function next_power_of_two(n) {
  var _i = 1;
  while(true) {
    var i = _i;
    if (i >= n) {
      return i;
    } else {
      _i = (i << 1);
      continue ;
    }
  };
}

function grow(t, n) {
  if (t[/* la_results */0].length < n) {
    var new_size = next_power_of_two(n);
    var filler = function (i) {
      if (i < t[/* la_results */0].length) {
        return Caml_array.caml_array_get(t[/* la_results */0], i);
      }
      
    };
    var new_arr = $$Array.init(new_size, filler);
    t[/* la_results */0] = new_arr;
    return /* () */0;
  } else {
    return 0;
  }
}

function lex(t) {
  var lex_env = t[/* la_lex_env */3];
  var match = t[/* la_lex_mode */2];
  var match$1;
  switch (match) {
    case 1 : 
        match$1 = type_token$1(lex_env);
        break;
    case 2 : 
        match$1 = jsx_tag(lex_env);
        break;
    case 3 : 
        match$1 = jsx_child$1(lex_env);
        break;
    case 4 : 
        match$1 = template_tail(lex_env);
        break;
    case 5 : 
        match$1 = regexp(lex_env);
        break;
    case 0 : 
    case 6 : 
        match$1 = token$1(lex_env);
        break;
    
  }
  var lex_env$1 = match$1[0];
  var lexbuf = lex_env$1[/* lex_lb */1];
  var lexbuf$1 = /* record */[
    /* refill_buff */lexbuf[/* refill_buff */0],
    /* lex_buffer */lexbuf[/* lex_buffer */1],
    /* lex_buffer_len */lexbuf[/* lex_buffer_len */2],
    /* lex_abs_pos */lexbuf[/* lex_abs_pos */3],
    /* lex_start_pos */lexbuf[/* lex_start_pos */4],
    /* lex_curr_pos */lexbuf[/* lex_curr_pos */5],
    /* lex_last_pos */lexbuf[/* lex_last_pos */6],
    /* lex_last_action */lexbuf[/* lex_last_action */7],
    /* lex_eof_reached */lexbuf[/* lex_eof_reached */8],
    /* lex_mem */lexbuf[/* lex_mem */9],
    /* lex_start_p */lexbuf[/* lex_start_p */10],
    /* lex_curr_p */lexbuf[/* lex_curr_p */11]
  ];
  var cloned_env = with_lexbuf(lexbuf$1, lex_env$1);
  t[/* la_lex_env */3] = lex_env$1;
  Caml_array.caml_array_set(t[/* la_results */0], t[/* la_num_lexed */1], /* tuple */[
        cloned_env,
        match$1[1]
      ]);
  t[/* la_num_lexed */1] = t[/* la_num_lexed */1] + 1 | 0;
  return /* () */0;
}

function lex_until(t, i) {
  grow(t, i + 1 | 0);
  while(t[/* la_num_lexed */1] <= i) {
    lex(t);
  };
  return /* () */0;
}

var default_parse_options = /* record */[
  /* esproposal_class_instance_fields */false,
  /* esproposal_class_static_fields */false,
  /* esproposal_decorators */false,
  /* esproposal_export_star_as */false,
  /* types */true,
  /* use_strict */false
];

function init_env($staropt$star, $staropt$star$1, source, content) {
  var token_sink = $staropt$star !== undefined ? Caml_option.valFromOption($staropt$star) : undefined;
  var parse_options = $staropt$star$1 !== undefined ? Caml_option.valFromOption($staropt$star$1) : undefined;
  var lb = Lexing.from_string(content);
  if (source !== undefined) {
    var match = source;
    if (typeof match !== "number") {
      var init = lb[/* lex_curr_p */11];
      lb[/* lex_curr_p */11] = /* record */[
        /* pos_fname */match[0],
        /* pos_lnum */init[/* pos_lnum */1],
        /* pos_bol */init[/* pos_bol */2],
        /* pos_cnum */init[/* pos_cnum */3]
      ];
    }
    
  }
  var parse_options$1 = parse_options !== undefined ? parse_options : default_parse_options;
  var enable_types_in_comments = parse_options$1[/* types */4];
  var lex_env = new_lex_env(source, lb, enable_types_in_comments);
  return /* record */[
          /* errors : record */[/* contents : [] */0],
          /* comments : record */[/* contents : [] */0],
          /* labels : Empty */0,
          /* exports : record */[/* contents : Empty */0],
          /* last_loc : record */[/* contents */undefined],
          /* in_strict_mode */parse_options$1[/* use_strict */5],
          /* in_export */false,
          /* in_loop */false,
          /* in_switch */false,
          /* in_function */false,
          /* no_in */false,
          /* no_call */false,
          /* no_let */false,
          /* allow_yield */true,
          /* allow_await */false,
          /* error_callback */undefined,
          /* lex_mode_stack : record */[/* contents : :: */[
              /* NORMAL */0,
              /* [] */0
            ]],
          /* lex_env : record */[/* contents */lex_env],
          /* lookahead : record */[/* contents */create$1(lex_env, /* NORMAL */0)],
          /* token_sink : record */[/* contents */token_sink],
          /* parse_options */parse_options$1,
          /* source */source
        ];
}

function error_at(env, param) {
  var e = param[1];
  env[/* errors */0][0] = /* :: */[
    /* tuple */[
      param[0],
      e
    ],
    env[/* errors */0][0]
  ];
  var match = env[/* error_callback */15];
  if (match !== undefined) {
    return Curry._2(match, env, e);
  } else {
    return /* () */0;
  }
}

function comment_list(env) {
  return (function (param) {
      return List.iter((function (c) {
                    env[/* comments */1][0] = /* :: */[
                      c,
                      env[/* comments */1][0]
                    ];
                    return /* () */0;
                  }), param);
    });
}

function record_export(env, param) {
  var export_name = param[1];
  var $$exports = env[/* exports */3][0];
  if (mem(export_name, $$exports)) {
    return error_at(env, /* tuple */[
                param[0],
                /* DuplicateExport */Block.__(7, [export_name])
              ]);
  } else {
    env[/* exports */3][0] = add(export_name, env[/* exports */3][0]);
    return /* () */0;
  }
}

function lookahead($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  if (i >= 2) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "parser_env.ml",
            288,
            2
          ]
        ];
  }
  var t = env[/* lookahead */18][0];
  var i$1 = i;
  lex_until(t, i$1);
  var match = Caml_array.caml_array_get(t[/* la_results */0], i$1);
  if (match !== undefined) {
    return match[1];
  } else {
    throw [
          Caml_builtin_exceptions.failure,
          "Lookahead.peek failed"
        ];
  }
}

function with_strict(in_strict_mode, env) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* in_strict_mode */5] = in_strict_mode;
  return newrecord;
}

function with_in_function(in_function, env) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* in_function */9] = in_function;
  return newrecord;
}

function with_allow_yield(allow_yield, env) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* allow_yield */13] = allow_yield;
  return newrecord;
}

function with_no_let(no_let, env) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* no_let */12] = no_let;
  return newrecord;
}

function with_in_loop(in_loop, env) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* in_loop */7] = in_loop;
  return newrecord;
}

function with_no_in(no_in, env) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* no_in */10] = no_in;
  return newrecord;
}

function with_in_switch(in_switch, env) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* in_switch */8] = in_switch;
  return newrecord;
}

function with_in_export(in_export, env) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* in_export */6] = in_export;
  return newrecord;
}

function with_no_call(no_call, env) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* no_call */11] = no_call;
  return newrecord;
}

function with_error_callback(error_callback, env) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* error_callback */15] = error_callback;
  return newrecord;
}

function error_list(env) {
  return (function (param) {
      return List.iter((function (param) {
                    return error_at(env, param);
                  }), param);
    });
}

function without_error_callback(env) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* error_callback */15] = undefined;
  return newrecord;
}

function add_label(env, label) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* labels */2] = add(label, env[/* labels */2]);
  return newrecord;
}

function enter_function(env, async, generator) {
  var newrecord = Caml_array.caml_array_dup(env);
  newrecord[/* allow_await */14] = async;
  newrecord[/* allow_yield */13] = generator;
  newrecord[/* in_function */9] = true;
  newrecord[/* in_switch */8] = false;
  newrecord[/* in_loop */7] = false;
  newrecord[/* labels */2] = /* Empty */0;
  return newrecord;
}

function is_future_reserved(param) {
  if (param === "enum") {
    return true;
  } else {
    return false;
  }
}

function is_strict_reserved(param) {
  switch (param) {
    case "implements" : 
    case "interface" : 
    case "package" : 
    case "private" : 
    case "protected" : 
    case "public" : 
    case "static" : 
    case "yield" : 
        return true;
    default:
      return false;
  }
}

function is_restricted(param) {
  switch (param) {
    case "arguments" : 
    case "eval" : 
        return true;
    default:
      return false;
  }
}

function token$2($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  return lookahead(i, env)[/* lex_token */0];
}

function value($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  return lookahead(i, env)[/* lex_value */2];
}

function loc($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  return lookahead(i, env)[/* lex_loc */1];
}

function errors($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  return lookahead(i, env)[/* lex_errors */3];
}

function comments($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  return lookahead(i, env)[/* lex_comments */4];
}

function lex_env($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  var t = env[/* lookahead */18][0];
  var i$1 = i;
  lex_until(t, i$1);
  var match = Caml_array.caml_array_get(t[/* la_results */0], i$1);
  if (match !== undefined) {
    return match[0];
  } else {
    throw [
          Caml_builtin_exceptions.failure,
          "Lookahead.peek failed"
        ];
  }
}

function is_line_terminator(env) {
  var match = env[/* last_loc */4][0];
  if (match !== undefined) {
    return loc(undefined, env)[/* start */1][/* line */0] > match[/* start */1][/* line */0];
  } else {
    return false;
  }
}

function is_implicit_semicolon(env) {
  var match = token$2(undefined, env);
  if (typeof match === "number") {
    var switcher = match - 3 | 0;
    if (switcher > 101 || switcher < 0) {
      if ((switcher + 1 >>> 0) > 103) {
        return is_line_terminator(env);
      } else {
        return true;
      }
    } else if (switcher !== 4) {
      return is_line_terminator(env);
    } else {
      return false;
    }
  } else {
    return is_line_terminator(env);
  }
}

function semicolon_loc($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  if (token$2(i, env) === /* T_SEMICOLON */7) {
    return loc(i, env);
  }
  
}

function is_identifier($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  var name = value(i, env);
  var match = token$2(i, env);
  if (is_strict_reserved(name) || is_restricted(name) || is_future_reserved(name)) {
    return true;
  } else if (typeof match === "number") {
    var switcher = match - 1 | 0;
    if (switcher > 56 || switcher < 0) {
      return switcher < 62;
    } else {
      return switcher === 25;
    }
  } else {
    return false;
  }
}

function is_function($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  if (token$2(i, env) === /* T_FUNCTION */13) {
    return true;
  } else if (token$2(i, env) === /* T_ASYNC */61) {
    return token$2(i + 1 | 0, env) === /* T_FUNCTION */13;
  } else {
    return false;
  }
}

function is_class($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  var match = token$2(i, env);
  if (typeof match === "number") {
    if (match !== 12) {
      return match === 38;
    } else {
      return true;
    }
  } else {
    return false;
  }
}

function error$1(env, e) {
  var loc$1 = loc(undefined, env);
  return error_at(env, /* tuple */[
              loc$1,
              e
            ]);
}

function get_unexpected_error(param) {
  var exit = 0;
  var tmp = param[0];
  if (typeof tmp === "number") {
    switch (tmp) {
      case 0 : 
          return /* UnexpectedIdentifier */2;
      case 105 : 
          return /* UnexpectedEOS */4;
      default:
        exit = 1;
    }
  } else {
    switch (tmp.tag | 0) {
      case 0 : 
          return /* UnexpectedNumber */0;
      case 1 : 
      case 4 : 
          return /* UnexpectedString */1;
      default:
        exit = 1;
    }
  }
  if (exit === 1) {
    var word = param[1];
    if (is_future_reserved(word)) {
      return /* UnexpectedReserved */3;
    } else if (is_strict_reserved(word)) {
      return /* StrictReservedWord */39;
    } else {
      return /* UnexpectedToken */Block.__(1, [word]);
    }
  }
  
}

function error_unexpected(env) {
  error_list(env)(errors(undefined, env));
  return error$1(env, get_unexpected_error(/* tuple */[
                  token$2(undefined, env),
                  value(undefined, env)
                ]));
}

function error_on_decorators(env) {
  return (function (param) {
      return List.iter((function (decorator) {
                    return error_at(env, /* tuple */[
                                decorator[0],
                                /* UnsupportedDecorator */57
                              ]);
                  }), param);
    });
}

function strict_error(env, e) {
  if (env[/* in_strict_mode */5]) {
    return error$1(env, e);
  } else {
    return 0;
  }
}

function strict_error_at(env, param) {
  if (env[/* in_strict_mode */5]) {
    return error_at(env, /* tuple */[
                param[0],
                param[1]
              ]);
  } else {
    return 0;
  }
}

function token$3(env) {
  var match = env[/* token_sink */19][0];
  if (match !== undefined) {
    var token_loc = loc(undefined, env);
    var token$4 = token$2(undefined, env);
    var token_value = value(undefined, env);
    Curry._1(match, /* record */[
          /* token_loc */token_loc,
          /* token */token$4,
          /* token_context */List.hd(env[/* lex_mode_stack */16][0]),
          /* token_value */token_value
        ]);
  }
  env[/* lex_env */17][0] = lex_env(undefined, env);
  error_list(env)(errors(undefined, env));
  comment_list(env)(comments(undefined, env));
  env[/* last_loc */4][0] = loc(undefined, env);
  var t = env[/* lookahead */18][0];
  lex_until(t, 0);
  if (t[/* la_num_lexed */1] > 1) {
    $$Array.blit(t[/* la_results */0], 1, t[/* la_results */0], 0, t[/* la_num_lexed */1] - 1 | 0);
  }
  Caml_array.caml_array_set(t[/* la_results */0], t[/* la_num_lexed */1] - 1 | 0, undefined);
  t[/* la_num_lexed */1] = t[/* la_num_lexed */1] - 1 | 0;
  return /* () */0;
}

function push_lex_mode(env, mode) {
  env[/* lex_mode_stack */16][0] = /* :: */[
    mode,
    env[/* lex_mode_stack */16][0]
  ];
  env[/* lookahead */18][0] = create$1(env[/* lex_env */17][0], List.hd(env[/* lex_mode_stack */16][0]));
  return /* () */0;
}

function pop_lex_mode(env) {
  var match = env[/* lex_mode_stack */16][0];
  var new_stack;
  if (match) {
    new_stack = match[1];
  } else {
    throw [
          Caml_builtin_exceptions.failure,
          "Popping lex mode from empty stack"
        ];
  }
  env[/* lex_mode_stack */16][0] = new_stack;
  env[/* lookahead */18][0] = create$1(env[/* lex_env */17][0], List.hd(env[/* lex_mode_stack */16][0]));
  return /* () */0;
}

function double_pop_lex_mode(env) {
  var match = env[/* lex_mode_stack */16][0];
  var new_stack;
  if (match) {
    var match$1 = match[1];
    if (match$1) {
      new_stack = match$1[1];
    } else {
      throw [
            Caml_builtin_exceptions.failure,
            "Popping lex mode from empty stack"
          ];
    }
  } else {
    throw [
          Caml_builtin_exceptions.failure,
          "Popping lex mode from empty stack"
        ];
  }
  env[/* lex_mode_stack */16][0] = new_stack;
  env[/* lookahead */18][0] = create$1(env[/* lex_env */17][0], List.hd(env[/* lex_mode_stack */16][0]));
  return /* () */0;
}

function semicolon(env) {
  if (is_implicit_semicolon(env)) {
    return 0;
  } else if (token$2(undefined, env) === /* T_SEMICOLON */7) {
    return token$3(env);
  } else {
    return error_unexpected(env);
  }
}

function token$4(env, t) {
  if (Caml_obj.caml_notequal(token$2(undefined, env), t)) {
    error_unexpected(env);
  }
  return token$3(env);
}

function maybe(env, t) {
  if (Caml_obj.caml_equal(token$2(undefined, env), t)) {
    token$3(env);
    return true;
  } else {
    return false;
  }
}

function contextual(env, str) {
  if (value(undefined, env) !== str) {
    error_unexpected(env);
  }
  return token$3(env);
}

var Rollback = Caml_exceptions.create("Flow_parser_reg_test.Parser_env.Try.Rollback");

function save_state(env) {
  var match = env[/* token_sink */19][0];
  var token_buffer;
  if (match !== undefined) {
    var buffer = /* record */[
      /* length */0,
      /* first : Nil */0,
      /* last : Nil */0
    ];
    env[/* token_sink */19][0] = (function (token_data) {
        return Queue.add(token_data, buffer);
      });
    token_buffer = /* tuple */[
      match,
      buffer
    ];
  } else {
    token_buffer = undefined;
  }
  return /* record */[
          /* saved_errors */env[/* errors */0][0],
          /* saved_comments */env[/* comments */1][0],
          /* saved_last_loc */env[/* last_loc */4][0],
          /* saved_lex_mode_stack */env[/* lex_mode_stack */16][0],
          /* saved_lex_env */env[/* lex_env */17][0],
          /* token_buffer */token_buffer
        ];
}

function reset_token_sink(flush, env, token_buffer_info) {
  if (token_buffer_info !== undefined) {
    var match = token_buffer_info;
    var orig_token_sink = match[0];
    env[/* token_sink */19][0] = orig_token_sink;
    if (flush) {
      return Queue.iter(orig_token_sink, match[1]);
    } else {
      return 0;
    }
  } else {
    return /* () */0;
  }
}

function to_parse(env, parse) {
  var saved_state = save_state(env);
  try {
    var env$1 = env;
    var saved_state$1 = saved_state;
    var result = Curry._1(parse, env);
    reset_token_sink(true, env$1, saved_state$1[/* token_buffer */5]);
    return /* ParsedSuccessfully */[result];
  }
  catch (exn){
    if (exn === Rollback) {
      var env$2 = env;
      var saved_state$2 = saved_state;
      reset_token_sink(false, env$2, saved_state$2[/* token_buffer */5]);
      env$2[/* errors */0][0] = saved_state$2[/* saved_errors */0];
      env$2[/* comments */1][0] = saved_state$2[/* saved_comments */1];
      env$2[/* last_loc */4][0] = saved_state$2[/* saved_last_loc */2];
      env$2[/* lex_mode_stack */16][0] = saved_state$2[/* saved_lex_mode_stack */3];
      env$2[/* lex_env */17][0] = saved_state$2[/* saved_lex_env */4];
      env$2[/* lookahead */18][0] = create$1(env$2[/* lex_env */17][0], List.hd(env$2[/* lex_mode_stack */16][0]));
      return /* FailedToParse */0;
    } else {
      throw exn;
    }
  }
}

var Parser_env_048 = /* Peek */[
  token$2,
  value,
  loc,
  errors,
  comments,
  is_line_terminator,
  is_implicit_semicolon,
  semicolon_loc,
  is_identifier,
  is_function,
  is_class
];

var Parser_env_051 = /* Try */[
  Rollback,
  to_parse
];

function height$1(param) {
  if (param) {
    return param[/* h */3];
  } else {
    return 0;
  }
}

function create$2(l, v, r) {
  var hl = l ? l[/* h */3] : 0;
  var hr = r ? r[/* h */3] : 0;
  return /* Node */[
          /* l */l,
          /* v */v,
          /* r */r,
          /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal$1(l, v, r) {
  var hl = l ? l[/* h */3] : 0;
  var hr = r ? r[/* h */3] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[/* r */2];
      var lv = l[/* v */1];
      var ll = l[/* l */0];
      if (height$1(ll) >= height$1(lr)) {
        return create$2(ll, lv, create$2(lr, v, r));
      } else if (lr) {
        return create$2(create$2(ll, lv, lr[/* l */0]), lr[/* v */1], create$2(lr[/* r */2], v, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Set.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Set.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r) {
      var rr = r[/* r */2];
      var rv = r[/* v */1];
      var rl = r[/* l */0];
      if (height$1(rr) >= height$1(rl)) {
        return create$2(create$2(l, v, rl), rv, rr);
      } else if (rl) {
        return create$2(create$2(l, v, rl[/* l */0]), rl[/* v */1], create$2(rl[/* r */2], rv, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Set.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Set.bal"
          ];
    }
  } else {
    return /* Node */[
            /* l */l,
            /* v */v,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function add$1(x, t) {
  if (t) {
    var r = t[/* r */2];
    var v = t[/* v */1];
    var l = t[/* l */0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return t;
    } else if (c < 0) {
      var ll = add$1(x, l);
      if (l === ll) {
        return t;
      } else {
        return bal$1(ll, v, r);
      }
    } else {
      var rr = add$1(x, r);
      if (r === rr) {
        return t;
      } else {
        return bal$1(l, v, rr);
      }
    }
  } else {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* r : Empty */0,
            /* h */1
          ];
  }
}

function mem$1(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param[/* v */1]);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param[/* l */0] : param[/* r */2];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function height$2(param) {
  if (param) {
    return param[/* h */4];
  } else {
    return 0;
  }
}

function create$3(l, x, d, r) {
  var hl = height$2(l);
  var hr = height$2(r);
  return /* Node */[
          /* l */l,
          /* v */x,
          /* d */d,
          /* r */r,
          /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal$2(l, x, d, r) {
  var hl = l ? l[/* h */4] : 0;
  var hr = r ? r[/* h */4] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[/* r */3];
      var ld = l[/* d */2];
      var lv = l[/* v */1];
      var ll = l[/* l */0];
      if (height$2(ll) >= height$2(lr)) {
        return create$3(ll, lv, ld, create$3(lr, x, d, r));
      } else if (lr) {
        return create$3(create$3(ll, lv, ld, lr[/* l */0]), lr[/* v */1], lr[/* d */2], create$3(lr[/* r */3], x, d, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r) {
      var rr = r[/* r */3];
      var rd = r[/* d */2];
      var rv = r[/* v */1];
      var rl = r[/* l */0];
      if (height$2(rr) >= height$2(rl)) {
        return create$3(create$3(l, x, d, rl), rv, rd, rr);
      } else if (rl) {
        return create$3(create$3(l, x, d, rl[/* l */0]), rl[/* v */1], rl[/* d */2], create$3(rl[/* r */3], rv, rd, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Map.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Map.bal"
          ];
    }
  } else {
    return /* Node */[
            /* l */l,
            /* v */x,
            /* d */d,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function add$2(x, data, m) {
  if (m) {
    var r = m[/* r */3];
    var d = m[/* d */2];
    var v = m[/* v */1];
    var l = m[/* l */0];
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      if (d === data) {
        return m;
      } else {
        return /* Node */[
                /* l */l,
                /* v */x,
                /* d */data,
                /* r */r,
                /* h */m[/* h */4]
              ];
      }
    } else if (c < 0) {
      var ll = add$2(x, data, l);
      if (l === ll) {
        return m;
      } else {
        return bal$2(ll, v, d, r);
      }
    } else {
      var rr = add$2(x, data, r);
      if (r === rr) {
        return m;
      } else {
        return bal$2(l, v, d, rr);
      }
    }
  } else {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* d */data,
            /* r : Empty */0,
            /* h */1
          ];
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param[/* v */1]);
      if (c === 0) {
        return param[/* d */2];
      } else {
        _param = c < 0 ? param[/* l */0] : param[/* r */3];
        continue ;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function compare$1(param, param$1) {
  var loc = compare(param[0], param$1[0]);
  if (loc === 0) {
    return Caml_obj.caml_compare(param[1], param$1[1]);
  } else {
    return loc;
  }
}

function height$3(param) {
  if (param) {
    return param[/* h */3];
  } else {
    return 0;
  }
}

function create$4(l, v, r) {
  var hl = l ? l[/* h */3] : 0;
  var hr = r ? r[/* h */3] : 0;
  return /* Node */[
          /* l */l,
          /* v */v,
          /* r */r,
          /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        ];
}

function bal$3(l, v, r) {
  var hl = l ? l[/* h */3] : 0;
  var hr = r ? r[/* h */3] : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l[/* r */2];
      var lv = l[/* v */1];
      var ll = l[/* l */0];
      if (height$3(ll) >= height$3(lr)) {
        return create$4(ll, lv, create$4(lr, v, r));
      } else if (lr) {
        return create$4(create$4(ll, lv, lr[/* l */0]), lr[/* v */1], create$4(lr[/* r */2], v, r));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Set.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Set.bal"
          ];
    }
  } else if (hr > (hl + 2 | 0)) {
    if (r) {
      var rr = r[/* r */2];
      var rv = r[/* v */1];
      var rl = r[/* l */0];
      if (height$3(rr) >= height$3(rl)) {
        return create$4(create$4(l, v, rl), rv, rr);
      } else if (rl) {
        return create$4(create$4(l, v, rl[/* l */0]), rl[/* v */1], create$4(rl[/* r */2], rv, rr));
      } else {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "Set.bal"
            ];
      }
    } else {
      throw [
            Caml_builtin_exceptions.invalid_argument,
            "Set.bal"
          ];
    }
  } else {
    return /* Node */[
            /* l */l,
            /* v */v,
            /* r */r,
            /* h */hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          ];
  }
}

function add$3(x, t) {
  if (t) {
    var r = t[/* r */2];
    var v = t[/* v */1];
    var l = t[/* l */0];
    var c = compare$1(x, v);
    if (c === 0) {
      return t;
    } else if (c < 0) {
      var ll = add$3(x, l);
      if (l === ll) {
        return t;
      } else {
        return bal$3(ll, v, r);
      }
    } else {
      var rr = add$3(x, r);
      if (r === rr) {
        return t;
      } else {
        return bal$3(l, v, rr);
      }
    }
  } else {
    return /* Node */[
            /* l : Empty */0,
            /* v */x,
            /* r : Empty */0,
            /* h */1
          ];
  }
}

function mem$2(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = compare$1(x, param[/* v */1]);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param[/* l */0] : param[/* r */2];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function filter_duplicate_errors(errs) {
  var errs$1 = List.rev(errs);
  var match = List.fold_left((function (param, err) {
          var deduped = param[1];
          var set = param[0];
          if (mem$2(err, set)) {
            return /* tuple */[
                    set,
                    deduped
                  ];
          } else {
            return /* tuple */[
                    add$3(err, set),
                    /* :: */[
                      err,
                      deduped
                    ]
                  ];
          }
        }), /* tuple */[
        /* Empty */0,
        /* [] */0
      ], errs$1);
  return List.rev(match[1]);
}

function with_loc(fn, env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var result = Curry._1(fn, env);
  var match = env[/* last_loc */4][0];
  var end_loc = match !== undefined ? match : (error$1(env, /* Assertion */Block.__(0, ["did not consume any tokens"])), Curry._2(Parser_env_048[/* loc */2], undefined, env));
  return /* tuple */[
          btwn(start_loc, end_loc),
          result
        ];
}

var Parse = Caml_module.init_mod([
      "parser_flow.ml",
      95,
      6
    ], [[
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ]]);

function intersection(env) {
  maybe(env, /* T_BIT_AND */82);
  var left = prefix(env);
  return Curry._2(intersection_with, env, left);
}

function union(env) {
  maybe(env, /* T_BIT_OR */80);
  var left = intersection(env);
  return Curry._2(union_with, env, left);
}

function function_param_with_id(env, name) {
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, /* UnexpectedTypeAnnotation */6);
  }
  var optional = maybe(env, /* T_PLING */76);
  token$4(env, /* T_COLON */77);
  var typeAnnotation = union(env);
  return /* tuple */[
          btwn(name[0], typeAnnotation[0]),
          /* record */[
            /* name */name,
            /* typeAnnotation */typeAnnotation,
            /* optional */optional
          ]
        ];
}

function postfix_with(env, _t) {
  while(true) {
    var t = _t;
    if (!Curry._1(Parser_env_048[/* is_line_terminator */5], env) && maybe(env, /* T_LBRACKET */5)) {
      var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      token$4(env, /* T_RBRACKET */6);
      var loc = btwn(t[0], end_loc);
      var t_001 = /* Array */Block.__(3, [t]);
      var t$1 = /* tuple */[
        loc,
        t_001
      ];
      _t = t$1;
      continue ;
    } else {
      return t;
    }
  };
}

function rev_nonempty_acc(acc) {
  var end_loc;
  if (acc) {
    end_loc = acc[0][0];
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "parser_flow.ml",
            127,
            13
          ]
        ];
  }
  var acc$1 = List.rev(acc);
  var start_loc;
  if (acc$1) {
    start_loc = acc$1[0][0];
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "parser_flow.ml",
            131,
            13
          ]
        ];
  }
  return /* tuple */[
          btwn(start_loc, end_loc),
          acc$1
        ];
}

function primary(env) {
  var loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var token$5 = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  if (typeof token$5 === "number") {
    switch (token$5) {
      case 0 : 
          var match = generic(env);
          return /* tuple */[
                  match[0],
                  /* Generic */Block.__(4, [match[1]])
                ];
      case 1 : 
          var match$1 = Curry._2(_object, undefined, env);
          return /* tuple */[
                  match$1[0],
                  /* Object */Block.__(2, [match$1[1]])
                ];
      case 3 : 
          var env$1 = env;
          var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$1);
          var match$2 = param_list_or_type(env$1);
          if (match$2.tag) {
            return match$2[0];
          } else {
            var match$3 = match$2[0];
            token$4(env$1, /* T_ARROW */10);
            var returnType = union(env$1);
            var end_loc = returnType[0];
            return /* tuple */[
                    btwn(start_loc, end_loc),
                    /* Function */Block.__(1, [/* record */[
                          /* params */match$3[1],
                          /* returnType */returnType,
                          /* rest */match$3[0],
                          /* typeParameters */undefined
                        ]])
                  ];
          }
      case 5 : 
          var env$2 = env;
          var start_loc$1 = Curry._2(Parser_env_048[/* loc */2], undefined, env$2);
          token$4(env$2, /* T_LBRACKET */5);
          var tl = types(env$2, /* [] */0);
          var end_loc$1 = Curry._2(Parser_env_048[/* loc */2], undefined, env$2);
          token$4(env$2, /* T_RBRACKET */6);
          return /* tuple */[
                  btwn(start_loc$1, end_loc$1),
                  /* Tuple */Block.__(8, [tl])
                ];
      case 28 : 
      case 29 : 
          exit = 2;
          break;
      case 44 : 
          var start_loc$2 = Curry._2(Parser_env_048[/* loc */2], undefined, env);
          token$4(env, /* T_TYPEOF */44);
          var t = primary(env);
          return /* tuple */[
                  btwn(start_loc$2, t[0]),
                  /* Typeof */Block.__(7, [t])
                ];
      case 89 : 
          var env$3 = env;
          var start_loc$3 = Curry._2(Parser_env_048[/* loc */2], undefined, env$3);
          var typeParameters = Curry._2(type_parameter_declaration, false, env$3);
          var match$4 = function_param_list(env$3);
          token$4(env$3, /* T_ARROW */10);
          var returnType$1 = union(env$3);
          var end_loc$2 = returnType$1[0];
          return /* tuple */[
                  btwn(start_loc$3, end_loc$2),
                  /* Function */Block.__(1, [/* record */[
                        /* params */match$4[1],
                        /* returnType */returnType$1,
                        /* rest */match$4[0],
                        /* typeParameters */typeParameters
                      ]])
                ];
      case 97 : 
          token$4(env, /* T_MULT */97);
          return /* tuple */[
                  loc,
                  /* Exists */6
                ];
      default:
        exit = 1;
    }
  } else {
    switch (token$5.tag | 0) {
      case 1 : 
          var match$5 = token$5[0];
          var octal = match$5[3];
          var raw = match$5[2];
          var value = match$5[1];
          var loc$1 = match$5[0];
          if (octal) {
            strict_error(env, /* StrictOctalLiteral */31);
          }
          token$4(env, /* T_STRING */Block.__(1, [/* tuple */[
                    loc$1,
                    value,
                    raw,
                    octal
                  ]]));
          return /* tuple */[
                  loc$1,
                  /* StringLiteral */Block.__(9, [/* record */[
                        /* value */value,
                        /* raw */raw
                      ]])
                ];
      case 5 : 
          var value$1 = token$5[1];
          var number_type = token$5[0];
          var raw$1 = Curry._2(Parser_env_048[/* value */1], undefined, env);
          token$4(env, /* T_NUMBER_SINGLETON_TYPE */Block.__(5, [
                  number_type,
                  value$1
                ]));
          if (number_type === /* LEGACY_OCTAL */1) {
            strict_error(env, /* StrictOctalLiteral */31);
          }
          return /* tuple */[
                  loc,
                  /* NumberLiteral */Block.__(10, [/* record */[
                        /* value */value$1,
                        /* raw */raw$1
                      ]])
                ];
      default:
        exit = 1;
    }
  }
  switch (exit) {
    case 1 : 
        var match$6 = primitive(token$5);
        if (match$6 !== undefined) {
          token$4(env, token$5);
          return /* tuple */[
                  loc,
                  match$6
                ];
        } else {
          error_unexpected(env);
          return /* tuple */[
                  loc,
                  /* Any */0
                ];
        }
    case 2 : 
        var raw$2 = Curry._2(Parser_env_048[/* value */1], undefined, env);
        token$4(env, token$5);
        var value$2 = token$5 === /* T_TRUE */29;
        return /* tuple */[
                loc,
                /* BooleanLiteral */Block.__(11, [/* record */[
                      /* value */value$2,
                      /* raw */raw$2
                    ]])
              ];
    
  }
}

function prefix(env) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  if (typeof match === "number" && match === 76) {
    var loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
    token$4(env, /* T_PLING */76);
    var t = prefix(env);
    return /* tuple */[
            btwn(loc, t[0]),
            /* Nullable */Block.__(0, [t])
          ];
  } else {
    var env$1 = env;
    var t$1 = primary(env$1);
    return postfix_with(env$1, t$1);
  }
}

function param_list_or_type(env) {
  token$4(env, /* T_LPAREN */3);
  var token$5 = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var ret;
  var exit = 0;
  if (typeof token$5 === "number") {
    if (token$5 !== 105) {
      if (token$5 >= 12) {
        exit = 1;
      } else {
        switch (token$5) {
          case 0 : 
              ret = function_param_or_generic_type(env);
              break;
          case 4 : 
              ret = /* ParamList */Block.__(0, [/* tuple */[
                    undefined,
                    /* [] */0
                  ]]);
              break;
          case 1 : 
          case 2 : 
          case 3 : 
          case 5 : 
          case 6 : 
          case 7 : 
          case 8 : 
          case 9 : 
          case 10 : 
              exit = 1;
              break;
          case 11 : 
              ret = /* ParamList */Block.__(0, [Curry._2(function_param_list_without_parens, env, /* [] */0)]);
              break;
          
        }
      }
    } else {
      ret = /* ParamList */Block.__(0, [Curry._2(function_param_list_without_parens, env, /* [] */0)]);
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var match = primitive(token$5);
    if (match !== undefined) {
      var match$1 = Curry._2(Parser_env_048[/* token */0], 1, env);
      if (typeof match$1 === "number" && (match$1 === 77 || match$1 === 76)) {
        var match$2 = Curry._1(Parse[/* identifier_or_reserved_keyword */11], env);
        var name = match$2[0];
        if (!env[/* parse_options */20][/* types */4]) {
          error$1(env, /* UnexpectedTypeAnnotation */6);
        }
        var optional = maybe(env, /* T_PLING */76);
        token$4(env, /* T_COLON */77);
        var typeAnnotation = union(env);
        if (Curry._2(Parser_env_048[/* token */0], undefined, env) !== /* T_RPAREN */4) {
          token$4(env, /* T_COMMA */8);
        }
        var param_000 = btwn(name[0], typeAnnotation[0]);
        var param_001 = /* record */[
          /* name */name,
          /* typeAnnotation */typeAnnotation,
          /* optional */optional
        ];
        var param = /* tuple */[
          param_000,
          param_001
        ];
        ret = /* ParamList */Block.__(0, [Curry._2(function_param_list_without_parens, env, /* :: */[
                  param,
                  /* [] */0
                ])]);
      } else {
        ret = /* Type */Block.__(1, [union(env)]);
      }
    } else {
      ret = /* Type */Block.__(1, [union(env)]);
    }
  }
  token$4(env, /* T_RPAREN */4);
  return ret;
}

function function_param_or_generic_type(env) {
  var id = Curry._2(Parse[/* identifier */10], undefined, env);
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  if (typeof match === "number" && (match === 77 || match === 76)) {
    var param = function_param_with_id(env, id);
    maybe(env, /* T_COMMA */8);
    return /* ParamList */Block.__(0, [Curry._2(function_param_list_without_parens, env, /* :: */[
                    param,
                    /* [] */0
                  ])]);
  } else {
    exit = 1;
  }
  if (exit === 1) {
    return /* Type */Block.__(1, [Curry._2(union_with, env, Curry._2(intersection_with, env, postfix_with(env, generic_type_with_identifier(env, id))))]);
  }
  
}

function primitive(param) {
  if (typeof param === "number") {
    if (param !== 27) {
      if (param >= 107) {
        switch (param - 107 | 0) {
          case 0 : 
              return /* Any */0;
          case 1 : 
              return /* Boolean */5;
          case 2 : 
              return /* Number */3;
          case 3 : 
              return /* String */4;
          case 4 : 
              return /* Void */1;
          
        }
      } else {
        return undefined;
      }
    } else {
      return /* Null */2;
    }
  }
  
}

function generic_type_with_identifier(env, id) {
  var match = Curry._2(raw_generic_with_identifier, env, id);
  return /* tuple */[
          match[0],
          /* Generic */Block.__(4, [match[1]])
        ];
}

function function_param_list(env) {
  token$4(env, /* T_LPAREN */3);
  var ret = Curry._2(function_param_list_without_parens, env, /* [] */0);
  token$4(env, /* T_RPAREN */4);
  return ret;
}

function generic(env) {
  return Curry._2(raw_generic_with_identifier, env, Curry._2(Parse[/* identifier */10], undefined, env));
}

function identifier(env, _param) {
  while(true) {
    var param = _param;
    var qualification = param[1];
    var q_loc = param[0];
    if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_PERIOD */9) {
      token$4(env, /* T_PERIOD */9);
      var id = Curry._2(Parse[/* identifier */10], undefined, env);
      var loc = btwn(q_loc, id[0]);
      var qualification$1 = /* Qualified */Block.__(1, [/* tuple */[
            loc,
            /* record */[
              /* qualification */qualification,
              /* id */id
            ]
          ]]);
      _param = /* tuple */[
        loc,
        qualification$1
      ];
      continue ;
    } else {
      return /* tuple */[
              q_loc,
              qualification
            ];
    }
  };
}

function raw_generic_with_identifier(env, id) {
  var id_000 = id[0];
  var id_001 = /* Unqualified */Block.__(0, [id]);
  var id$1 = /* tuple */[
    id_000,
    id_001
  ];
  var match = identifier(env, id$1);
  var id_loc = match[0];
  var typeParameters = Curry._1(type_parameter_instantiation, env);
  var loc = typeParameters !== undefined ? btwn(id_loc, typeParameters[0]) : id_loc;
  return /* tuple */[
          loc,
          /* record */[
            /* id */match[1],
            /* typeParameters */typeParameters
          ]
        ];
}

function param(env) {
  var match = Curry._1(Parse[/* identifier_or_reserved_keyword */11], env);
  return function_param_with_id(env, match[0]);
}

function function_param_list_without_parens(env) {
  return (function (param$1) {
      var env$1 = env;
      var _acc = param$1;
      while(true) {
        var acc = _acc;
        var t = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
        var exit = 0;
        if (typeof t === "number") {
          var switcher = t - 4 | 0;
          exit = switcher > 7 || switcher < 0 ? (
              switcher !== 101 ? 1 : 2
            ) : (
              switcher > 6 || switcher < 1 ? 2 : 1
            );
        } else {
          exit = 1;
        }
        switch (exit) {
          case 1 : 
              var acc_000 = param(env$1);
              var acc$1 = /* :: */[
                acc_000,
                acc
              ];
              if (Curry._2(Parser_env_048[/* token */0], undefined, env$1) !== /* T_RPAREN */4) {
                token$4(env$1, /* T_COMMA */8);
              }
              _acc = acc$1;
              continue ;
          case 2 : 
              var rest = t === /* T_ELLIPSIS */11 ? (token$4(env$1, /* T_ELLIPSIS */11), param(env$1)) : undefined;
              return /* tuple */[
                      rest,
                      List.rev(acc)
                    ];
          
        }
      };
    });
}

function union_with(env, left) {
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_BIT_OR */80) {
    var env$1 = env;
    var _acc = /* :: */[
      left,
      /* [] */0
    ];
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
      var exit = 0;
      if (typeof match === "number" && match === 80) {
        token$4(env$1, /* T_BIT_OR */80);
        _acc = /* :: */[
          intersection(env$1),
          acc
        ];
        continue ;
      } else {
        exit = 1;
      }
      if (exit === 1) {
        var match$1 = rev_nonempty_acc(acc);
        return /* tuple */[
                match$1[0],
                /* Union */Block.__(5, [match$1[1]])
              ];
      }
      
    };
  } else {
    return left;
  }
}

function intersection_with(env, left) {
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_BIT_AND */82) {
    var env$1 = env;
    var _acc = /* :: */[
      left,
      /* [] */0
    ];
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
      var exit = 0;
      if (typeof match === "number" && match === 82) {
        token$4(env$1, /* T_BIT_AND */82);
        _acc = /* :: */[
          prefix(env$1),
          acc
        ];
        continue ;
      } else {
        exit = 1;
      }
      if (exit === 1) {
        var match$1 = rev_nonempty_acc(acc);
        return /* tuple */[
                match$1[0],
                /* Intersection */Block.__(6, [match$1[1]])
              ];
      }
      
    };
  } else {
    return left;
  }
}

function params(env, allow_default, _require_default, _acc) {
  while(true) {
    var acc = _acc;
    var require_default = _require_default;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var variance = typeof match === "number" ? (
        match !== 94 ? (
            match !== 95 ? undefined : (token$3(env), /* Minus */1)
          ) : (token$3(env), /* Plus */0)
      ) : undefined;
    var match$1 = Curry._2(Parse[/* identifier_with_type */12], env, /* StrictParamName */28);
    var id = match$1[1];
    var loc = match$1[0];
    var match$2 = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var match$3;
    if (allow_default) {
      var exit = 0;
      if (typeof match$2 === "number" && match$2 === 75) {
        token$3(env);
        match$3 = /* tuple */[
          union(env),
          true
        ];
      } else {
        exit = 1;
      }
      if (exit === 1) {
        if (require_default) {
          error_at(env, /* tuple */[
                loc,
                /* MissingTypeParamDefault */58
              ]);
        }
        match$3 = /* tuple */[
          undefined,
          require_default
        ];
      }
      
    } else {
      match$3 = /* tuple */[
        undefined,
        false
      ];
    }
    var param_001 = /* record */[
      /* name */id[/* name */0],
      /* bound */id[/* typeAnnotation */1],
      /* variance */variance,
      /* default */match$3[0]
    ];
    var param = /* tuple */[
      loc,
      param_001
    ];
    var acc$1 = /* :: */[
      param,
      acc
    ];
    var match$4 = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit$1 = 0;
    if (typeof match$4 === "number" && !(match$4 !== 90 && match$4 !== 105)) {
      return List.rev(acc$1);
    } else {
      exit$1 = 1;
    }
    if (exit$1 === 1) {
      token$4(env, /* T_COMMA */8);
      if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_GREATER_THAN */90) {
        return List.rev(acc$1);
      } else {
        _acc = acc$1;
        _require_default = match$3[1];
        continue ;
      }
    }
    
  };
}

function type_parameter_declaration(allow_default, env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_LESS_THAN */89) {
    if (!env[/* parse_options */20][/* types */4]) {
      error$1(env, /* UnexpectedTypeAnnotation */6);
    }
    token$4(env, /* T_LESS_THAN */89);
    var params$1 = params(env, allow_default, false, /* [] */0);
    var loc = btwn(start_loc, Curry._2(Parser_env_048[/* loc */2], undefined, env));
    token$4(env, /* T_GREATER_THAN */90);
    return /* tuple */[
            loc,
            /* record */[/* params */params$1]
          ];
  }
  
}

function params$1(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number" && !(match !== 90 && match !== 105)) {
      return List.rev(acc);
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var acc_000 = union(env);
      var acc$1 = /* :: */[
        acc_000,
        acc
      ];
      if (Curry._2(Parser_env_048[/* token */0], undefined, env) !== /* T_GREATER_THAN */90) {
        token$4(env, /* T_COMMA */8);
      }
      _acc = acc$1;
      continue ;
    }
    
  };
}

function type_parameter_instantiation(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_LESS_THAN */89) {
    token$4(env, /* T_LESS_THAN */89);
    var params$2 = params$1(env, /* [] */0);
    var loc = btwn(start_loc, Curry._2(Parser_env_048[/* loc */2], undefined, env));
    token$4(env, /* T_GREATER_THAN */90);
    return /* tuple */[
            loc,
            /* record */[/* params */params$2]
          ];
  }
  
}

function types(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number" && !(match !== 6 && match !== 105)) {
      return List.rev(acc);
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var acc_000 = union(env);
      var acc$1 = /* :: */[
        acc_000,
        acc
      ];
      if (Curry._2(Parser_env_048[/* token */0], undefined, env) !== /* T_RBRACKET */6) {
        token$4(env, /* T_COMMA */8);
      }
      _acc = acc$1;
      continue ;
    }
    
  };
}

function methodish(env, start_loc) {
  var typeParameters = Curry._2(type_parameter_declaration, false, env);
  var match = function_param_list(env);
  token$4(env, /* T_COLON */77);
  var returnType = union(env);
  var loc = btwn(start_loc, returnType[0]);
  return /* tuple */[
          loc,
          /* record */[
            /* params */match[1],
            /* returnType */returnType,
            /* rest */match[0],
            /* typeParameters */typeParameters
          ]
        ];
}

function method_property(env, start_loc, $$static, key) {
  var value = methodish(env, start_loc);
  var value_000 = value[0];
  var value_001 = /* Function */Block.__(1, [value[1]]);
  var value$1 = /* tuple */[
    value_000,
    value_001
  ];
  return /* tuple */[
          value_000,
          /* record */[
            /* key */key,
            /* value */value$1,
            /* optional */false,
            /* static */$$static,
            /* _method */true
          ]
        ];
}

function call_property(env, start_loc, $$static) {
  var value = methodish(env, Curry._2(Parser_env_048[/* loc */2], undefined, env));
  return /* tuple */[
          btwn(start_loc, value[0]),
          /* record */[
            /* value */value,
            /* static */$$static
          ]
        ];
}

function property(env, start_loc, $$static, key) {
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, /* UnexpectedTypeAnnotation */6);
  }
  var optional = maybe(env, /* T_PLING */76);
  token$4(env, /* T_COLON */77);
  var value = union(env);
  return /* tuple */[
          btwn(start_loc, value[0]),
          /* record */[
            /* key */key,
            /* value */value,
            /* optional */optional,
            /* static */$$static,
            /* _method */false
          ]
        ];
}

function indexer_property(env, start_loc, $$static) {
  token$4(env, /* T_LBRACKET */5);
  var match = Curry._1(Parse[/* identifier_or_reserved_keyword */11], env);
  token$4(env, /* T_COLON */77);
  var key = union(env);
  token$4(env, /* T_RBRACKET */6);
  token$4(env, /* T_COLON */77);
  var value = union(env);
  return /* tuple */[
          btwn(start_loc, value[0]),
          /* record */[
            /* id */match[0],
            /* key */key,
            /* value */value,
            /* static */$$static
          ]
        ];
}

function semicolon$1(env) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  if (typeof match === "number") {
    if (match >= 7) {
      if (match >= 9) {
        return error_unexpected(env);
      } else {
        return token$3(env);
      }
    } else if (match !== 2) {
      return error_unexpected(env);
    } else {
      return /* () */0;
    }
  } else {
    return error_unexpected(env);
  }
}

function properties(allow_static, env, _param) {
  while(true) {
    var param = _param;
    var callProperties = param[2];
    var indexers = param[1];
    var acc = param[0];
    var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
    var $$static = allow_static && maybe(env, /* T_STATIC */40);
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number") {
      if (match !== 89) {
        if (match !== 105) {
          if (match >= 6) {
            exit = 1;
          } else {
            switch (match) {
              case 2 : 
                  exit = 2;
                  break;
              case 3 : 
                  exit = 3;
                  break;
              case 0 : 
              case 1 : 
              case 4 : 
                  exit = 1;
                  break;
              case 5 : 
                  var indexer = indexer_property(env, start_loc, $$static);
                  semicolon$1(env);
                  _param = /* tuple */[
                    acc,
                    /* :: */[
                      indexer,
                      indexers
                    ],
                    callProperties
                  ];
                  continue ;
              
            }
          }
        } else {
          exit = 2;
        }
      } else {
        exit = 3;
      }
    } else {
      exit = 1;
    }
    switch (exit) {
      case 1 : 
          var match$1 = Curry._2(Parser_env_048[/* token */0], undefined, env);
          var match$2;
          var exit$1 = 0;
          if ($$static && typeof match$1 === "number" && match$1 === 77) {
            strict_error_at(env, /* tuple */[
                  start_loc,
                  /* StrictReservedWord */39
                ]);
            var static_key_001 = /* Identifier */Block.__(1, [/* tuple */[
                  start_loc,
                  /* record */[
                    /* name */"static",
                    /* typeAnnotation */undefined,
                    /* optional */false
                  ]
                ]]);
            var static_key = /* tuple */[
              start_loc,
              static_key_001
            ];
            match$2 = /* tuple */[
              false,
              static_key
            ];
          } else {
            exit$1 = 4;
          }
          if (exit$1 === 4) {
            push_lex_mode(env, /* NORMAL */0);
            var key = Curry._1(Parse[/* object_key */18], env);
            pop_lex_mode(env);
            match$2 = /* tuple */[
              $$static,
              key
            ];
          }
          var key$1 = match$2[1][1];
          var $$static$1 = match$2[0];
          var match$3 = Curry._2(Parser_env_048[/* token */0], undefined, env);
          var property$1 = typeof match$3 === "number" && !(match$3 !== 3 && match$3 !== 89) ? method_property(env, start_loc, $$static$1, key$1) : property(env, start_loc, $$static$1, key$1);
          semicolon$1(env);
          _param = /* tuple */[
            /* :: */[
              property$1,
              acc
            ],
            indexers,
            callProperties
          ];
          continue ;
      case 2 : 
          return /* tuple */[
                  List.rev(acc),
                  List.rev(indexers),
                  List.rev(callProperties)
                ];
      case 3 : 
          var call_prop = call_property(env, start_loc, $$static);
          semicolon$1(env);
          _param = /* tuple */[
            acc,
            indexers,
            /* :: */[
              call_prop,
              callProperties
            ]
          ];
          continue ;
      
    }
  };
}

function _object($staropt$star, env) {
  var allow_static = $staropt$star !== undefined ? $staropt$star : false;
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_LCURLY */1);
  var match = properties(allow_static, env, /* tuple */[
        /* [] */0,
        /* [] */0,
        /* [] */0
      ]);
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_RCURLY */2);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[
            /* properties */match[0],
            /* indexers */match[1],
            /* callProperties */match[2]
          ]
        ];
}

var _type = union;

function annotation(env) {
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, /* UnexpectedTypeAnnotation */6);
  }
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_COLON */77);
  var typeAnnotation = union(env);
  var match = env[/* last_loc */4][0];
  var end_loc;
  if (match !== undefined) {
    end_loc = match;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "parser_flow.ml",
            121,
            16
          ]
        ];
  }
  return /* tuple */[
          btwn(start_loc, end_loc),
          typeAnnotation
        ];
}

function annotation_opt(env) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  if (typeof match === "number" && match === 77) {
    return annotation(env);
  }
  
}

function wrap(f, env) {
  var env$1 = with_strict(true, env);
  push_lex_mode(env$1, /* TYPE */1);
  var ret = Curry._1(f, env$1);
  pop_lex_mode(env$1);
  return ret;
}

var partial_arg = Curry._1(type_parameter_declaration, true);

function type_parameter_declaration_with_defaults(param) {
  return wrap(partial_arg, param);
}

var partial_arg$1 = Curry._1(type_parameter_declaration, false);

function type_parameter_declaration$1(param) {
  return wrap(partial_arg$1, param);
}

function _object$1($staropt$star, env) {
  var allow_static = $staropt$star !== undefined ? $staropt$star : false;
  return wrap(Curry._1(_object, allow_static), env);
}

function pattern(check_env, _param) {
  while(true) {
    var param = _param;
    var p = param[1];
    switch (p.tag | 0) {
      case 0 : 
          var check_env$1 = check_env;
          var o = p[0];
          return List.fold_left(object_property, check_env$1, o[/* properties */0]);
      case 1 : 
          var check_env$2 = check_env;
          var arr = p[0];
          return List.fold_left(array_element, check_env$2, arr[/* elements */0]);
      case 2 : 
          _param = p[0][/* left */0];
          continue ;
      case 3 : 
          var param$1 = check_env;
          var id = p[0];
          var name = id[1][/* name */0];
          var param_names = param$1[1];
          var env = param$1[0];
          if (mem$1(name, param_names)) {
            error_at(env, /* tuple */[
                  id[0],
                  /* StrictParamDupe */29
                ]);
          }
          var match = identifier_no_dupe_check(/* tuple */[
                env,
                param_names
              ], id);
          return /* tuple */[
                  match[0],
                  add$1(name, match[1])
                ];
      case 4 : 
          error_at(check_env[0], /* tuple */[
                param[0],
                /* ExpectedPatternFoundExpression */18
              ]);
          return check_env;
      
    }
  };
}

function object_property(check_env, param) {
  if (param.tag) {
    return pattern(check_env, param[0][1][/* argument */0]);
  } else {
    var property = param[0][1];
    var match = property[/* key */0];
    var check_env$1;
    switch (match.tag | 0) {
      case 1 : 
          check_env$1 = identifier_no_dupe_check(check_env, match[0]);
          break;
      case 0 : 
      case 2 : 
          check_env$1 = check_env;
          break;
      
    }
    return pattern(check_env$1, property[/* pattern */1]);
  }
}

function array_element(check_env, param) {
  if (param !== undefined) {
    var match = param;
    if (match.tag) {
      return pattern(check_env, match[0][1][/* argument */0]);
    } else {
      return pattern(check_env, match[0]);
    }
  } else {
    return check_env;
  }
}

function identifier_no_dupe_check(param, param$1) {
  var name = param$1[1][/* name */0];
  var loc = param$1[0];
  var env = param[0];
  if (is_restricted(name)) {
    strict_error_at(env, /* tuple */[
          loc,
          /* StrictParamName */28
        ]);
  }
  if (is_future_reserved(name) || is_strict_reserved(name)) {
    strict_error_at(env, /* tuple */[
          loc,
          /* StrictReservedWord */39
        ]);
  }
  return /* tuple */[
          env,
          param[1]
        ];
}

function strict_post_check(env, strict, simple, id, params) {
  if (strict || !simple) {
    var env$1 = strict ? with_strict(!env[/* in_strict_mode */5], env) : env;
    if (id !== undefined) {
      var match = id;
      var name = match[1][/* name */0];
      var loc = match[0];
      if (is_restricted(name)) {
        strict_error_at(env$1, /* tuple */[
              loc,
              /* StrictFunctionName */30
            ]);
      }
      if (is_future_reserved(name) || is_strict_reserved(name)) {
        strict_error_at(env$1, /* tuple */[
              loc,
              /* StrictReservedWord */39
            ]);
      }
      
    }
    List.fold_left(pattern, /* tuple */[
          env$1,
          /* Empty */0
        ], params);
    return /* () */0;
  } else {
    return 0;
  }
}

function param$1(env) {
  var id = Curry._2(Parse[/* pattern */16], env, /* StrictParamName */28);
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_ASSIGN */75) {
    token$4(env, /* T_ASSIGN */75);
    var $$default = Curry._1(Parse[/* assignment */7], env);
    return /* tuple */[
            id,
            $$default
          ];
  } else {
    return /* tuple */[
            id,
            undefined
          ];
  }
}

function param_list(env, _param) {
  while(true) {
    var param$2 = _param;
    var has_default = param$2[2];
    var defaults = param$2[1];
    var params = param$2[0];
    var t = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof t === "number") {
      var switcher = t - 4 | 0;
      exit = switcher > 7 || switcher < 0 ? (
          switcher !== 101 ? 1 : 2
        ) : (
          switcher > 6 || switcher < 1 ? 2 : 1
        );
    } else {
      exit = 1;
    }
    switch (exit) {
      case 1 : 
          var match = param$1(env);
          var $$default = match[1];
          var has_default$1 = has_default || $$default !== undefined;
          if (Curry._2(Parser_env_048[/* token */0], undefined, env) !== /* T_RPAREN */4) {
            token$4(env, /* T_COMMA */8);
          }
          _param = /* tuple */[
            /* :: */[
              match[0],
              params
            ],
            /* :: */[
              $$default,
              defaults
            ],
            has_default$1
          ];
          continue ;
      case 2 : 
          var rest = t === /* T_ELLIPSIS */11 ? (token$4(env, /* T_ELLIPSIS */11), Curry._2(Parse[/* identifier_with_type */12], env, /* StrictParamName */28)) : undefined;
          if (Curry._2(Parser_env_048[/* token */0], undefined, env) !== /* T_RPAREN */4) {
            error$1(env, /* ParameterAfterRestParameter */47);
          }
          return /* tuple */[
                  List.rev(params),
                  has_default ? List.rev(defaults) : /* [] */0,
                  rest
                ];
      
    }
  };
}

function function_params(env) {
  token$4(env, /* T_LPAREN */3);
  var match = param_list(env, /* tuple */[
        /* [] */0,
        /* [] */0,
        false
      ]);
  token$4(env, /* T_RPAREN */4);
  return /* tuple */[
          match[0],
          match[1],
          match[2]
        ];
}

function function_body(env, async, generator) {
  var env$1 = enter_function(env, async, generator);
  var match = Curry._1(Parse[/* function_block_body */14], env$1);
  var loc = match[0];
  return /* tuple */[
          loc,
          /* BodyBlock */Block.__(0, [/* tuple */[
                loc,
                match[1]
              ]]),
          match[2]
        ];
}

function generator(env, is_async) {
  var match = maybe(env, /* T_MULT */97);
  if (is_async && match) {
    error$1(env, /* AsyncGenerator */48);
    return true;
  } else {
    return match;
  }
}

function is_simple_param(param) {
  if (param[1].tag === 3) {
    return true;
  } else {
    return false;
  }
}

function is_simple_function_params(params, defaults, rest) {
  if (defaults === /* [] */0 && rest === undefined) {
    return List.for_all(is_simple_param, params);
  } else {
    return false;
  }
}

function _function(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var async = maybe(env, /* T_ASYNC */61);
  token$4(env, /* T_FUNCTION */13);
  var generator$1 = generator(env, async);
  var match = env[/* in_export */6];
  var match$1 = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var match$2;
  var exit = 0;
  if (match && typeof match$1 === "number") {
    if (match$1 !== 3) {
      if (match$1 !== 89) {
        exit = 1;
      } else {
        var typeParams = Curry._1(type_parameter_declaration$1, env);
        var id = Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_LPAREN */3 ? undefined : Curry._2(Parse[/* identifier */10], /* StrictFunctionName */30, env);
        match$2 = /* tuple */[
          typeParams,
          id
        ];
      }
    } else {
      match$2 = /* tuple */[
        undefined,
        undefined
      ];
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var id$1 = Curry._2(Parse[/* identifier */10], /* StrictFunctionName */30, env);
    match$2 = /* tuple */[
      Curry._1(type_parameter_declaration$1, env),
      id$1
    ];
  }
  var id$2 = match$2[1];
  var match$3 = function_params(env);
  var rest = match$3[2];
  var defaults = match$3[1];
  var params = match$3[0];
  var returnType = wrap(annotation_opt, env);
  var predicate = Curry._1(Parse[/* predicate */22], env);
  var match$4 = function_body(env, async, generator$1);
  var body = match$4[1];
  var simple = is_simple_function_params(params, defaults, rest);
  strict_post_check(env, match$4[2], simple, id$2, params);
  var match$5;
  match$5 = body.tag ? /* tuple */[
      body[0][0],
      true
    ] : /* tuple */[
      body[0][0],
      false
    ];
  return /* tuple */[
          btwn(start_loc, match$5[0]),
          /* FunctionDeclaration */Block.__(18, [/* record */[
                /* id */id$2,
                /* params */params,
                /* defaults */defaults,
                /* rest */rest,
                /* body */body,
                /* async */async,
                /* generator */generator$1,
                /* predicate */predicate,
                /* expression */match$5[1],
                /* returnType */returnType,
                /* typeParameters */match$2[0]
              ]])
        ];
}

function variable_declaration(env) {
  var id = Curry._2(Parse[/* pattern */16], env, /* StrictVarName */27);
  var match;
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_ASSIGN */75) {
    token$4(env, /* T_ASSIGN */75);
    match = /* tuple */[
      Curry._1(Parse[/* assignment */7], env),
      /* [] */0
    ];
  } else {
    match = id[1].tag === 3 ? /* tuple */[
        undefined,
        /* [] */0
      ] : /* tuple */[
        undefined,
        /* :: */[
          /* tuple */[
            id[0],
            /* NoUninitializedDestructuring */43
          ],
          /* [] */0
        ]
      ];
  }
  var init = match[0];
  var end_loc = init !== undefined ? init[0] : id[0];
  return /* tuple */[
          /* tuple */[
            btwn(id[0], end_loc),
            /* record */[
              /* id */id,
              /* init */init
            ]
          ],
          match[1]
        ];
}

function helper(env, _decls, _errs) {
  while(true) {
    var errs = _errs;
    var decls = _decls;
    var match = variable_declaration(env);
    var decl = match[0];
    var decls$1 = /* :: */[
      decl,
      decls
    ];
    var errs$1 = Pervasives.$at(match[1], errs);
    if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_COMMA */8) {
      token$4(env, /* T_COMMA */8);
      _errs = errs$1;
      _decls = decls$1;
      continue ;
    } else {
      var end_loc = decl[0];
      var declarations = List.rev(decls$1);
      var start_loc = decl[0];
      return /* tuple */[
              btwn(start_loc, end_loc),
              declarations,
              List.rev(errs$1)
            ];
    }
  };
}

function declarations(token$5, kind, env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, token$5);
  var match = helper(env, /* [] */0, /* [] */0);
  return /* tuple */[
          /* tuple */[
            btwn(start_loc, match[0]),
            /* record */[
              /* declarations */match[1],
              /* kind */kind
            ]
          ],
          match[2]
        ];
}

function $$const(env) {
  var env$1 = with_no_let(true, env);
  var match = declarations(/* T_CONST */25, /* Const */2, env$1);
  var match$1 = match[0];
  var variable = match$1[1];
  var errs = List.fold_left((function (errs, decl) {
          if (decl[1][/* init */1] !== undefined) {
            return errs;
          } else {
            return /* :: */[
                    /* tuple */[
                      decl[0],
                      /* NoUninitializedConst */42
                    ],
                    errs
                  ];
          }
        }), match[1], variable[/* declarations */0]);
  return /* tuple */[
          /* tuple */[
            match$1[0],
            variable
          ],
          List.rev(errs)
        ];
}

function _let(env) {
  var env$1 = with_no_let(true, env);
  return declarations(/* T_LET */26, /* Let */1, env$1);
}

function variable(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var match$1;
  if (typeof match === "number") {
    switch (match) {
      case 22 : 
          match$1 = declarations(/* T_VAR */22, /* Var */0, env);
          break;
      case 23 : 
      case 24 : 
          error_unexpected(env);
          match$1 = declarations(/* T_VAR */22, /* Var */0, env);
          break;
      case 25 : 
          match$1 = $$const(env);
          break;
      case 26 : 
          match$1 = _let(env);
          break;
      default:
        error_unexpected(env);
        match$1 = declarations(/* T_VAR */22, /* Var */0, env);
    }
  } else {
    error_unexpected(env);
    match$1 = declarations(/* T_VAR */22, /* Var */0, env);
  }
  var match$2 = match$1[0];
  return /* tuple */[
          /* tuple */[
            btwn(start_loc, match$2[0]),
            /* VariableDeclaration */Block.__(19, [match$2[1]])
          ],
          match$1[1]
        ];
}

function is_tighter(a, b) {
  var a_prec;
  a_prec = a.tag ? a[0] - 1 | 0 : a[0];
  return a_prec >= b[0];
}

function is_lhs(param) {
  var tmp = param[1];
  if (typeof tmp === "number") {
    return false;
  } else {
    switch (tmp.tag | 0) {
      case 13 : 
      case 18 : 
          return true;
      default:
        return false;
    }
  }
}

function is_assignable_lhs(param) {
  var tmp = param[1];
  if (typeof tmp === "number") {
    return false;
  } else {
    switch (tmp.tag | 0) {
      case 0 : 
      case 1 : 
      case 13 : 
      case 18 : 
          return true;
      default:
        return false;
    }
  }
}

function assignment_op(env) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var op;
  if (typeof match === "number") {
    switch (match) {
      case 63 : 
          op = /* RShift3Assign */9;
          break;
      case 64 : 
          op = /* RShiftAssign */8;
          break;
      case 65 : 
          op = /* LShiftAssign */7;
          break;
      case 66 : 
          op = /* BitXorAssign */11;
          break;
      case 67 : 
          op = /* BitOrAssign */10;
          break;
      case 68 : 
          op = /* BitAndAssign */12;
          break;
      case 69 : 
          op = /* ModAssign */6;
          break;
      case 70 : 
          op = /* DivAssign */5;
          break;
      case 71 : 
          op = /* MultAssign */3;
          break;
      case 72 : 
          op = /* ExpAssign */4;
          break;
      case 73 : 
          op = /* MinusAssign */2;
          break;
      case 74 : 
          op = /* PlusAssign */1;
          break;
      case 75 : 
          op = /* Assign */0;
          break;
      default:
        op = undefined;
    }
  } else {
    op = undefined;
  }
  if (op !== undefined) {
    token$3(env);
  }
  return op;
}

function conditional(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var expr = Curry._1(logical, env);
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_PLING */76) {
    token$4(env, /* T_PLING */76);
    var env$prime = with_no_in(false, env);
    var consequent = Curry._1(assignment, env$prime);
    token$4(env, /* T_COLON */77);
    var match = with_loc(assignment, env);
    var loc = btwn(start_loc, match[0]);
    return /* tuple */[
            loc,
            /* Conditional */Block.__(10, [/* record */[
                  /* test */expr,
                  /* consequent */consequent,
                  /* alternate */match[1]
                ]])
          ];
  } else {
    return expr;
  }
}

function peek_unary_op(env) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  if (typeof match === "number") {
    if (match >= 46) {
      if (match >= 94) {
        if (match >= 102) {
          return undefined;
        } else {
          switch (match - 94 | 0) {
            case 0 : 
                return /* Plus */1;
            case 1 : 
                return /* Minus */0;
            case 2 : 
            case 3 : 
            case 4 : 
            case 5 : 
                return undefined;
            case 6 : 
                return /* Not */2;
            case 7 : 
                return /* BitNot */3;
            
          }
        }
      } else if (match !== 62 || !env[/* allow_await */14]) {
        return undefined;
      } else {
        return /* Await */7;
      }
    } else if (match >= 43) {
      switch (match - 43 | 0) {
        case 0 : 
            return /* Delete */6;
        case 1 : 
            return /* Typeof */4;
        case 2 : 
            return /* Void */5;
        
      }
    } else {
      return undefined;
    }
  }
  
}

function unary(env) {
  var begin_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var op = peek_unary_op(env);
  if (op !== undefined) {
    var operator = op;
    token$3(env);
    var argument = unary(env);
    var loc = btwn(begin_loc, argument[0]);
    if (operator === 6) {
      var tmp = argument[1];
      if (typeof tmp !== "number") {
        if (tmp.tag === 18) {
          strict_error_at(env, /* tuple */[
                loc,
                /* StrictDelete */32
              ]);
        }
        
      }
      
    }
    return /* tuple */[
            loc,
            /* Unary */Block.__(5, [/* record */[
                  /* operator */operator,
                  /* prefix */true,
                  /* argument */argument
                ]])
          ];
  } else {
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var op$1 = typeof match === "number" ? (
        match !== 102 ? (
            match !== 103 ? undefined : /* Decrement */1
          ) : /* Increment */0
      ) : undefined;
    if (op$1 !== undefined) {
      token$3(env);
      var argument$1 = unary(env);
      if (!is_lhs(argument$1)) {
        error_at(env, /* tuple */[
              argument$1[0],
              /* InvalidLHSInAssignment */14
            ]);
      }
      var match$1 = argument$1[1];
      if (typeof match$1 !== "number") {
        if (match$1.tag === 18) {
          if (is_restricted(match$1[0][1][/* name */0])) {
            strict_error(env, /* StrictLHSPrefix */38);
          }
          
        }
        
      }
      return /* tuple */[
              btwn(begin_loc, argument$1[0]),
              /* Update */Block.__(8, [/* record */[
                    /* operator */op$1,
                    /* argument */argument$1,
                    /* prefix */true
                  ]])
            ];
    } else {
      var env$1 = env;
      var argument$2 = left_hand_side(env$1);
      if (Curry._1(Parser_env_048[/* is_line_terminator */5], env$1)) {
        return argument$2;
      } else {
        var match$2 = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
        var op$2 = typeof match$2 === "number" ? (
            match$2 !== 102 ? (
                match$2 !== 103 ? undefined : /* Decrement */1
              ) : /* Increment */0
          ) : undefined;
        if (op$2 !== undefined) {
          if (!is_lhs(argument$2)) {
            error_at(env$1, /* tuple */[
                  argument$2[0],
                  /* InvalidLHSInAssignment */14
                ]);
          }
          var match$3 = argument$2[1];
          if (typeof match$3 !== "number") {
            if (match$3.tag === 18) {
              if (is_restricted(match$3[0][1][/* name */0])) {
                strict_error(env$1, /* StrictLHSPostfix */37);
              }
              
            }
            
          }
          var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$1);
          token$3(env$1);
          return /* tuple */[
                  btwn(argument$2[0], end_loc),
                  /* Update */Block.__(8, [/* record */[
                        /* operator */op$2,
                        /* argument */argument$2,
                        /* prefix */false
                      ]])
                ];
        } else {
          return argument$2;
        }
      }
    }
  }
}

function left_hand_side(env) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var expr;
  var exit = 0;
  if (typeof match === "number" && match === 42) {
    expr = _new(env, (function (new_expr, _args) {
            return new_expr;
          }));
  } else {
    exit = 1;
  }
  if (exit === 1) {
    expr = Curry._2(Parser_env_048[/* is_function */9], undefined, env) ? _function$1(env) : primary$1(env);
  }
  var expr$1 = member(env, expr);
  var match$1 = Curry._2(Parser_env_048[/* token */0], undefined, env);
  if (typeof match$1 === "number") {
    if (match$1 === 3) {
      return call(env, expr$1);
    } else {
      return expr$1;
    }
  } else if (match$1.tag === 2) {
    return member(env, tagged_template(env, expr$1, match$1[0]));
  } else {
    return expr$1;
  }
}

function call(env, _left) {
  while(true) {
    var left = _left;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    if (typeof match === "number") {
      switch (match) {
        case 3 : 
            if (env[/* no_call */11]) {
              return left;
            } else {
              var match$1 = Curry._1($$arguments, env);
              _left = /* tuple */[
                btwn(left[0], match$1[0]),
                /* Call */Block.__(12, [/* record */[
                      /* callee */left,
                      /* arguments */match$1[1]
                    ]])
              ];
              continue ;
            }
        case 5 : 
            token$4(env, /* T_LBRACKET */5);
            var expr = Curry._1(Parse[/* expression */6], env);
            var last_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
            var loc = btwn(left[0], last_loc);
            token$4(env, /* T_RBRACKET */6);
            _left = /* tuple */[
              loc,
              /* Member */Block.__(13, [/* record */[
                    /* _object */left,
                    /* property : PropertyExpression */Block.__(1, [expr]),
                    /* computed */true
                  ]])
            ];
            continue ;
        case 9 : 
            token$4(env, /* T_PERIOD */9);
            var match$2 = identifier_or_reserved_keyword(env);
            var id = match$2[0];
            _left = /* tuple */[
              btwn(left[0], id[0]),
              /* Member */Block.__(13, [/* record */[
                    /* _object */left,
                    /* property : PropertyIdentifier */Block.__(0, [id]),
                    /* computed */false
                  ]])
            ];
            continue ;
        default:
          return left;
      }
    } else if (match.tag === 2) {
      return tagged_template(env, left, match[0]);
    } else {
      return left;
    }
  };
}

function _new(env, _finish_fn) {
  while(true) {
    var finish_fn = _finish_fn;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number" && match === 42) {
      var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      token$4(env, /* T_NEW */42);
      var finish_fn$prime = (function(finish_fn,start_loc){
      return function finish_fn$prime(callee, args) {
        var match;
        if (args !== undefined) {
          var match$1 = args;
          match = /* tuple */[
            match$1[0],
            match$1[1]
          ];
        } else {
          match = /* tuple */[
            callee[0],
            /* [] */0
          ];
        }
        var callee$prime_000 = btwn(start_loc, match[0]);
        var callee$prime_001 = /* New */Block.__(11, [/* record */[
              /* callee */callee,
              /* arguments */match[1]
            ]]);
        var callee$prime = /* tuple */[
          callee$prime_000,
          callee$prime_001
        ];
        return Curry._2(finish_fn, callee$prime, undefined);
      }
      }(finish_fn,start_loc));
      _finish_fn = finish_fn$prime;
      continue ;
    } else {
      exit = 1;
    }
    if (exit === 1) {
      Curry._2(Parser_env_048[/* token */0], undefined, env);
      var expr = Curry._2(Parser_env_048[/* is_function */9], undefined, env) ? _function$1(env) : primary$1(env);
      var callee = member(with_no_call(true, env), expr);
      var match$1 = Curry._2(Parser_env_048[/* token */0], undefined, env);
      var callee$1;
      callee$1 = typeof match$1 === "number" || match$1.tag !== 2 ? callee : tagged_template(env, callee, match$1[0]);
      var match$2 = Curry._2(Parser_env_048[/* token */0], undefined, env);
      var args = typeof match$2 === "number" && match$2 === 3 ? Curry._1($$arguments, env) : undefined;
      return Curry._2(finish_fn, callee$1, args);
    }
    
  };
}

function member(env, left) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  if (typeof match === "number") {
    if (match !== 5) {
      if (match !== 9) {
        return left;
      } else {
        token$4(env, /* T_PERIOD */9);
        var match$1 = identifier_or_reserved_keyword(env);
        var id = match$1[0];
        return call(env, /* tuple */[
                    btwn(left[0], id[0]),
                    /* Member */Block.__(13, [/* record */[
                          /* _object */left,
                          /* property : PropertyIdentifier */Block.__(0, [id]),
                          /* computed */false
                        ]])
                  ]);
      }
    } else {
      token$4(env, /* T_LBRACKET */5);
      var expr = Curry._1(Parse[/* expression */6], with_no_call(false, env));
      var last_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      token$4(env, /* T_RBRACKET */6);
      return call(env, /* tuple */[
                  btwn(left[0], last_loc),
                  /* Member */Block.__(13, [/* record */[
                        /* _object */left,
                        /* property : PropertyExpression */Block.__(1, [expr]),
                        /* computed */true
                      ]])
                ]);
    }
  } else {
    return left;
  }
}

function _function$1(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var async = maybe(env, /* T_ASYNC */61);
  token$4(env, /* T_FUNCTION */13);
  var generator$1 = generator(env, async);
  var match;
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_LPAREN */3) {
    match = /* tuple */[
      undefined,
      undefined
    ];
  } else {
    var match$1 = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var id = typeof match$1 === "number" && match$1 === 89 ? undefined : Curry._2(Parse[/* identifier */10], /* StrictFunctionName */30, env);
    match = /* tuple */[
      id,
      Curry._1(type_parameter_declaration$1, env)
    ];
  }
  var id$1 = match[0];
  var match$2 = function_params(env);
  var rest = match$2[2];
  var defaults = match$2[1];
  var params = match$2[0];
  var returnType = wrap(annotation_opt, env);
  var predicate = Curry._1(Parse[/* predicate */22], env);
  var match$3 = function_body(env, async, generator$1);
  var body = match$3[1];
  var simple = is_simple_function_params(params, defaults, rest);
  strict_post_check(env, match$3[2], simple, id$1, params);
  var expression;
  expression = body.tag ? true : false;
  return /* tuple */[
          btwn(start_loc, match$3[0]),
          /* Function */Block.__(2, [/* record */[
                /* id */id$1,
                /* params */params,
                /* defaults */defaults,
                /* rest */rest,
                /* body */body,
                /* async */async,
                /* generator */generator$1,
                /* predicate */predicate,
                /* expression */expression,
                /* returnType */returnType,
                /* typeParameters */match[1]
              ]])
        ];
}

function number(env, number_type) {
  var value = Curry._2(Parser_env_048[/* value */1], undefined, env);
  var value$1;
  if (number_type !== 0) {
    switch (number_type - 1 | 0) {
      case 0 : 
          strict_error(env, /* StrictOctalLiteral */31);
          value$1 = Caml_format.caml_int_of_string("0o" + value);
          break;
      case 1 : 
          value$1 = Caml_format.caml_int_of_string(value);
          break;
      case 2 : 
          try {
            value$1 = float_of_string(value);
          }
          catch (exn){
            if (Sys.win32) {
              error$1(env, /* WindowsFloatOfString */59);
              value$1 = 789.0;
            } else {
              throw exn;
            }
          }
          break;
      
    }
  } else {
    value$1 = Caml_format.caml_int_of_string(value);
  }
  token$4(env, /* T_NUMBER */Block.__(0, [number_type]));
  return value$1;
}

function primary$1(env) {
  var loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var token$5 = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  if (typeof token$5 === "number") {
    switch (token$5) {
      case 1 : 
          var env$1 = env;
          var match = Curry._1(Parse[/* object_initializer */8], env$1);
          return /* tuple */[
                  match[0],
                  /* Object */Block.__(1, [match[1]])
                ];
      case 3 : 
          var env$2 = env;
          token$4(env$2, /* T_LPAREN */3);
          var expression = Curry._1(assignment, env$2);
          var match$1 = Curry._2(Parser_env_048[/* token */0], undefined, env$2);
          var ret;
          if (typeof match$1 === "number") {
            if (match$1 !== 8) {
              if (match$1 !== 77) {
                ret = expression;
              } else {
                var typeAnnotation = wrap(annotation, env$2);
                ret = /* tuple */[
                  btwn(expression[0], typeAnnotation[0]),
                  /* TypeCast */Block.__(24, [/* record */[
                        /* expression */expression,
                        /* typeAnnotation */typeAnnotation
                      ]])
                ];
              }
            } else {
              ret = sequence(env$2, /* :: */[
                    expression,
                    /* [] */0
                  ]);
            }
          } else {
            ret = expression;
          }
          token$4(env$2, /* T_RPAREN */4);
          return ret;
      case 5 : 
          var match$2 = Curry._1(array_initializer, env);
          return /* tuple */[
                  match$2[0],
                  /* Array */Block.__(0, [match$2[1]])
                ];
      case 19 : 
          token$4(env, /* T_THIS */19);
          return /* tuple */[
                  loc,
                  /* This */0
                ];
      case 27 : 
          var raw = Curry._2(Parser_env_048[/* value */1], undefined, env);
          token$4(env, /* T_NULL */27);
          return /* tuple */[
                  loc,
                  /* Literal */Block.__(19, [/* record */[
                        /* value : Null */0,
                        /* raw */raw
                      ]])
                ];
      case 28 : 
      case 29 : 
          exit = 2;
          break;
      case 38 : 
          return Curry._1(Parse[/* class_expression */20], env);
      case 49 : 
          var loc$1 = Curry._2(Parser_env_048[/* loc */2], undefined, env);
          token$4(env, /* T_SUPER */49);
          var id_001 = /* record */[
            /* name */"super",
            /* typeAnnotation */undefined,
            /* optional */false
          ];
          var id = /* tuple */[
            loc$1,
            id_001
          ];
          return /* tuple */[
                  loc$1,
                  /* Identifier */Block.__(18, [id])
                ];
      case 89 : 
          var match$3 = Curry._1(Parse[/* jsx_element */15], env);
          return /* tuple */[
                  match$3[0],
                  /* JSXElement */Block.__(22, [match$3[1]])
                ];
      case 70 : 
      case 96 : 
          var env$3 = env;
          push_lex_mode(env$3, /* REGEXP */5);
          var loc$2 = Curry._2(Parser_env_048[/* loc */2], undefined, env$3);
          var match$4 = Curry._2(Parser_env_048[/* token */0], undefined, env$3);
          var match$5;
          if (typeof match$4 === "number") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "parser_flow.ml",
                    1699,
                    15
                  ]
                ];
          } else if (match$4.tag === 3) {
            var match$6 = match$4[0];
            var raw$1 = Curry._2(Parser_env_048[/* value */1], undefined, env$3);
            token$3(env$3);
            match$5 = /* tuple */[
              raw$1,
              match$6[1],
              match$6[2]
            ];
          } else {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "parser_flow.ml",
                    1699,
                    15
                  ]
                ];
          }
          var raw_flags = match$5[2];
          pop_lex_mode(env$3);
          var filtered_flags = $$Buffer.create(raw_flags.length);
          $$String.iter((function (c) {
                  if (c >= 110) {
                    if (c !== 121) {
                      return /* () */0;
                    } else {
                      return $$Buffer.add_char(filtered_flags, c);
                    }
                  } else if (c >= 103) {
                    switch (c - 103 | 0) {
                      case 1 : 
                      case 3 : 
                      case 4 : 
                      case 5 : 
                          return /* () */0;
                      case 0 : 
                      case 2 : 
                      case 6 : 
                          return $$Buffer.add_char(filtered_flags, c);
                      
                    }
                  } else {
                    return /* () */0;
                  }
                }), raw_flags);
          var flags = $$Buffer.contents(filtered_flags);
          if (flags !== raw_flags) {
            error$1(env$3, /* InvalidRegExpFlags */Block.__(3, [raw_flags]));
          }
          var value = /* RegExp */Block.__(3, [/* record */[
                /* pattern */match$5[1],
                /* flags */flags
              ]]);
          return /* tuple */[
                  loc$2,
                  /* Literal */Block.__(19, [/* record */[
                        /* value */value,
                        /* raw */match$5[0]
                      ]])
                ];
      default:
        exit = 1;
    }
  } else {
    switch (token$5.tag | 0) {
      case 0 : 
          var raw$2 = Curry._2(Parser_env_048[/* value */1], undefined, env);
          var value$1 = /* Number */Block.__(2, [number(env, token$5[0])]);
          return /* tuple */[
                  loc,
                  /* Literal */Block.__(19, [/* record */[
                        /* value */value$1,
                        /* raw */raw$2
                      ]])
                ];
      case 1 : 
          var match$7 = token$5[0];
          var octal = match$7[3];
          var raw$3 = match$7[2];
          var value$2 = match$7[1];
          var loc$3 = match$7[0];
          if (octal) {
            strict_error(env, /* StrictOctalLiteral */31);
          }
          token$4(env, /* T_STRING */Block.__(1, [/* tuple */[
                    loc$3,
                    value$2,
                    raw$3,
                    octal
                  ]]));
          var value$3 = /* String */Block.__(0, [value$2]);
          return /* tuple */[
                  loc$3,
                  /* Literal */Block.__(19, [/* record */[
                        /* value */value$3,
                        /* raw */raw$3
                      ]])
                ];
      case 2 : 
          var match$8 = Curry._2(template_literal, env, token$5[0]);
          return /* tuple */[
                  match$8[0],
                  /* TemplateLiteral */Block.__(20, [match$8[1]])
                ];
      default:
        exit = 1;
    }
  }
  switch (exit) {
    case 1 : 
        if (Curry._2(Parser_env_048[/* is_identifier */8], undefined, env)) {
          var id$1 = Curry._2(Parse[/* identifier */10], undefined, env);
          return /* tuple */[
                  id$1[0],
                  /* Identifier */Block.__(18, [id$1])
                ];
        } else {
          error_unexpected(env);
          if (token$5 === /* T_ERROR */104) {
            token$3(env);
          }
          return /* tuple */[
                  loc,
                  /* Literal */Block.__(19, [/* record */[
                        /* value : Null */0,
                        /* raw */"null"
                      ]])
                ];
        }
    case 2 : 
        var raw$4 = Curry._2(Parser_env_048[/* value */1], undefined, env);
        token$4(env, token$5);
        var value$4 = /* Boolean */Block.__(1, [token$5 === /* T_TRUE */29]);
        return /* tuple */[
                loc,
                /* Literal */Block.__(19, [/* record */[
                      /* value */value$4,
                      /* raw */raw$4
                    ]])
              ];
    
  }
}

function tagged_template(env, tag, part) {
  var quasi = Curry._2(template_literal, env, part);
  return /* tuple */[
          btwn(tag[0], quasi[0]),
          /* TaggedTemplate */Block.__(21, [/* record */[
                /* tag */tag,
                /* quasi */quasi
              ]])
        ];
}

function sequence(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number" && match === 8) {
      token$4(env, /* T_COMMA */8);
      var expr = Curry._1(assignment, env);
      _acc = /* :: */[
        expr,
        acc
      ];
      continue ;
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var last_loc = acc ? acc[0][0] : none;
      var expressions = List.rev(acc);
      var first_loc = expressions ? expressions[0][0] : none;
      return /* tuple */[
              btwn(first_loc, last_loc),
              /* Sequence */Block.__(4, [/* record */[/* expressions */expressions]])
            ];
    }
    
  };
}

function identifier_or_reserved_keyword(env) {
  var lex_token = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var lex_value = Curry._2(Parser_env_048[/* value */1], undefined, env);
  var lex_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var exit = 0;
  if (typeof lex_token === "number") {
    if (lex_token >= 58) {
      if (lex_token >= 62) {
        exit = 1;
      } else {
        return /* tuple */[
                Curry._2(Parse[/* identifier */10], undefined, env),
                undefined
              ];
      }
    } else if (lex_token !== 0) {
      exit = 1;
    } else {
      return /* tuple */[
              Curry._2(Parse[/* identifier */10], undefined, env),
              undefined
            ];
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var err;
    var exit$1 = 0;
    if (typeof lex_token === "number") {
      var switcher = lex_token - 58 | 0;
      if (switcher > 48 || switcher < 0) {
        if (switcher >= -45) {
          exit$1 = 2;
        } else {
          error_unexpected(env);
          err = undefined;
        }
      } else if (switcher !== 4) {
        error_unexpected(env);
        err = undefined;
      } else {
        exit$1 = 2;
      }
    } else {
      error_unexpected(env);
      err = undefined;
    }
    if (exit$1 === 2) {
      err = /* tuple */[
        lex_loc,
        get_unexpected_error(/* tuple */[
              lex_token,
              lex_value
            ])
      ];
    }
    token$3(env);
    return /* tuple */[
            /* tuple */[
              lex_loc,
              /* record */[
                /* name */lex_value,
                /* typeAnnotation */undefined,
                /* optional */false
              ]
            ],
            err
          ];
  }
  
}

function assignment_but_not_arrow_function(env) {
  var expr = conditional(env);
  var match = assignment_op(env);
  if (match !== undefined) {
    if (!is_assignable_lhs(expr)) {
      error_at(env, /* tuple */[
            expr[0],
            /* InvalidLHSInAssignment */14
          ]);
    }
    var match$1 = expr[1];
    if (typeof match$1 !== "number") {
      if (match$1.tag === 18) {
        if (is_restricted(match$1[0][1][/* name */0])) {
          strict_error_at(env, /* tuple */[
                expr[0],
                /* StrictLHSAssignment */36
              ]);
        }
        
      }
      
    }
    var left = Curry._2(Parse[/* pattern_from_expr */17], env, expr);
    var right = Curry._1(assignment, env);
    var loc = btwn(left[0], right[0]);
    return /* tuple */[
            loc,
            /* Assignment */Block.__(7, [/* record */[
                  /* operator */match,
                  /* left */left,
                  /* right */right
                ]])
          ];
  } else {
    return expr;
  }
}

function error_callback(param, param$1) {
  throw Parser_env_051[/* Rollback */0];
}

function try_assignment_but_not_arrow_function(env) {
  var env$1 = with_error_callback(error_callback, env);
  var ret = assignment_but_not_arrow_function(env$1);
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
  var exit = 0;
  if (typeof match === "number") {
    if (match !== 10) {
      if (match !== 77) {
        exit = 1;
      } else {
        throw Parser_env_051[/* Rollback */0];
      }
    } else {
      throw Parser_env_051[/* Rollback */0];
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    if (Curry._2(Parser_env_048[/* is_identifier */8], undefined, env$1)) {
      if (Curry._2(Parser_env_048[/* value */1], undefined, env$1) === "checks") {
        throw Parser_env_051[/* Rollback */0];
      }
      var match$1 = ret[1];
      if (typeof match$1 === "number" || !(match$1.tag === 18 && match$1[0][1][/* name */0] === "async")) {
        return ret;
      } else {
        if (!Curry._1(Parser_env_048[/* is_line_terminator */5], env$1)) {
          throw Parser_env_051[/* Rollback */0];
        }
        return ret;
      }
    } else {
      return ret;
    }
  }
  
}

function assignment(env) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var match$1 = Curry._2(Parser_env_048[/* is_identifier */8], undefined, env);
  var exit = 0;
  var exit$1 = 0;
  if (typeof match === "number") {
    var switcher = match - 4 | 0;
    if (switcher > 84 || switcher < 0) {
      if ((switcher + 1 >>> 0) > 86) {
        exit$1 = 2;
      } else {
        exit = 1;
      }
    } else if (switcher !== 52 || !env[/* allow_yield */13]) {
      exit$1 = 2;
    } else {
      var env$1 = env;
      var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$1);
      token$4(env$1, /* T_YIELD */56);
      if (!env$1[/* allow_yield */13]) {
        error$1(env$1, /* IllegalYield */24);
      }
      var delegate = maybe(env$1, /* T_MULT */97);
      var has_argument = !(Curry._2(Parser_env_048[/* token */0], undefined, env$1) === /* T_SEMICOLON */7 || Curry._1(Parser_env_048[/* is_implicit_semicolon */6], env$1));
      var argument = delegate || has_argument ? Curry._1(assignment, env$1) : undefined;
      var end_loc;
      if (argument !== undefined) {
        end_loc = argument[0];
      } else {
        var match$2 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$1);
        var end_loc$1 = match$2 !== undefined ? match$2 : start_loc;
        semicolon(env$1);
        end_loc = end_loc$1;
      }
      return /* tuple */[
              btwn(start_loc, end_loc),
              /* Yield */Block.__(14, [/* record */[
                    /* argument */argument,
                    /* delegate */delegate
                  ]])
            ];
    }
  } else {
    exit$1 = 2;
  }
  if (exit$1 === 2) {
    if (match$1) {
      exit = 1;
    } else {
      return assignment_but_not_arrow_function(env);
    }
  }
  if (exit === 1) {
    var match$3 = Curry._2(Parser_env_051[/* to_parse */1], env, try_assignment_but_not_arrow_function);
    if (match$3) {
      return match$3[0];
    } else {
      var match$4 = Curry._2(Parser_env_051[/* to_parse */1], env, try_arrow_function);
      if (match$4) {
        return match$4[0];
      } else {
        return assignment_but_not_arrow_function(env);
      }
    }
  }
  
}

function make_logical(left, right, operator, loc) {
  return /* tuple */[
          loc,
          /* Logical */Block.__(9, [/* record */[
                /* operator */operator,
                /* left */left,
                /* right */right
              ]])
        ];
}

function logical_and(env, _left, _lloc) {
  while(true) {
    var lloc = _lloc;
    var left = _left;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    if (typeof match === "number" && match === 79) {
      token$4(env, /* T_AND */79);
      var match$1 = with_loc(binary, env);
      var loc = btwn(lloc, match$1[0]);
      _lloc = loc;
      _left = make_logical(left, match$1[1], /* And */1, loc);
      continue ;
    } else {
      return /* tuple */[
              lloc,
              left
            ];
    }
  };
}

function logical_or(env, _left, _lloc) {
  while(true) {
    var lloc = _lloc;
    var left = _left;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    if (typeof match === "number" && match === 78) {
      token$4(env, /* T_OR */78);
      var match$1 = with_loc(binary, env);
      var match$2 = logical_and(env, match$1[1], match$1[0]);
      var loc = btwn(lloc, match$2[0]);
      _lloc = loc;
      _left = make_logical(left, match$2[1], /* Or */0, loc);
      continue ;
    } else {
      return /* tuple */[
              lloc,
              left
            ];
    }
  };
}

function logical(env) {
  var match = with_loc(binary, env);
  var match$1 = logical_and(env, match[1], match[0]);
  return logical_or(env, match$1[1], match$1[0])[1];
}

function binary_op(env) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var ret;
  if (typeof match === "number") {
    var switcher = match - 15 | 0;
    if (switcher === 0 || switcher === 1) {
      ret = switcher !== 0 ? /* tuple */[
          /* Instanceof */21,
          /* Left_assoc */Block.__(0, [6])
        ] : (
          env[/* no_in */10] ? undefined : /* tuple */[
              /* In */20,
              /* Left_assoc */Block.__(0, [6])
            ]
        );
    } else if (switcher >= 65) {
      switch (switcher - 65 | 0) {
        case 0 : 
            ret = /* tuple */[
              /* BitOr */17,
              /* Left_assoc */Block.__(0, [2])
            ];
            break;
        case 1 : 
            ret = /* tuple */[
              /* Xor */18,
              /* Left_assoc */Block.__(0, [3])
            ];
            break;
        case 2 : 
            ret = /* tuple */[
              /* BitAnd */19,
              /* Left_assoc */Block.__(0, [4])
            ];
            break;
        case 3 : 
            ret = /* tuple */[
              /* Equal */0,
              /* Left_assoc */Block.__(0, [5])
            ];
            break;
        case 4 : 
            ret = /* tuple */[
              /* NotEqual */1,
              /* Left_assoc */Block.__(0, [5])
            ];
            break;
        case 5 : 
            ret = /* tuple */[
              /* StrictEqual */2,
              /* Left_assoc */Block.__(0, [5])
            ];
            break;
        case 6 : 
            ret = /* tuple */[
              /* StrictNotEqual */3,
              /* Left_assoc */Block.__(0, [5])
            ];
            break;
        case 7 : 
            ret = /* tuple */[
              /* LessThanEqual */5,
              /* Left_assoc */Block.__(0, [6])
            ];
            break;
        case 8 : 
            ret = /* tuple */[
              /* GreaterThanEqual */7,
              /* Left_assoc */Block.__(0, [6])
            ];
            break;
        case 9 : 
            ret = /* tuple */[
              /* LessThan */4,
              /* Left_assoc */Block.__(0, [6])
            ];
            break;
        case 10 : 
            ret = /* tuple */[
              /* GreaterThan */6,
              /* Left_assoc */Block.__(0, [6])
            ];
            break;
        case 11 : 
            ret = /* tuple */[
              /* LShift */8,
              /* Left_assoc */Block.__(0, [7])
            ];
            break;
        case 12 : 
            ret = /* tuple */[
              /* RShift */9,
              /* Left_assoc */Block.__(0, [7])
            ];
            break;
        case 13 : 
            ret = /* tuple */[
              /* RShift3 */10,
              /* Left_assoc */Block.__(0, [7])
            ];
            break;
        case 14 : 
            ret = /* tuple */[
              /* Plus */11,
              /* Left_assoc */Block.__(0, [8])
            ];
            break;
        case 15 : 
            ret = /* tuple */[
              /* Minus */12,
              /* Left_assoc */Block.__(0, [8])
            ];
            break;
        case 16 : 
            ret = /* tuple */[
              /* Div */15,
              /* Left_assoc */Block.__(0, [9])
            ];
            break;
        case 17 : 
            ret = /* tuple */[
              /* Mult */13,
              /* Left_assoc */Block.__(0, [9])
            ];
            break;
        case 18 : 
            ret = /* tuple */[
              /* Exp */14,
              /* Right_assoc */Block.__(1, [10])
            ];
            break;
        case 19 : 
            ret = /* tuple */[
              /* Mod */16,
              /* Left_assoc */Block.__(0, [9])
            ];
            break;
        case 20 : 
        case 21 : 
        case 22 : 
        case 23 : 
        case 24 : 
        case 25 : 
        case 26 : 
        case 27 : 
        case 28 : 
        case 29 : 
        case 30 : 
        case 31 : 
            ret = undefined;
            break;
        
      }
    } else {
      ret = undefined;
    }
  } else {
    ret = undefined;
  }
  if (ret !== undefined) {
    token$3(env);
  }
  return ret;
}

function make_binary(left, right, operator, loc) {
  return /* tuple */[
          loc,
          /* Binary */Block.__(6, [/* record */[
                /* operator */operator,
                /* left */left,
                /* right */right
              ]])
        ];
}

function add_to_stack(_right, _param, _rloc, _stack) {
  while(true) {
    var param = _param;
    var stack = _stack;
    var rloc = _rloc;
    var right = _right;
    var rpri = param[1];
    var rop = param[0];
    var exit = 0;
    if (stack) {
      var match = stack[0];
      var match$1 = match[1];
      if (is_tighter(match$1[1], rpri)) {
        var loc = btwn(match[2], rloc);
        var right$1 = make_binary(match[0], right, match$1[0], loc);
        _stack = stack[1];
        _rloc = loc;
        _param = /* tuple */[
          rop,
          rpri
        ];
        _right = right$1;
        continue ;
      } else {
        exit = 1;
      }
    } else {
      exit = 1;
    }
    if (exit === 1) {
      return /* :: */[
              /* tuple */[
                right,
                /* tuple */[
                  rop,
                  rpri
                ],
                rloc
              ],
              stack
            ];
    }
    
  };
}

function binary(env) {
  var env$1 = env;
  var _stack = /* [] */0;
  while(true) {
    var stack = _stack;
    var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$1);
    var is_unary = peek_unary_op(env$1) !== undefined;
    var right = unary(with_no_in(false, env$1));
    var match = env$1[/* last_loc */4][0];
    var end_loc = match !== undefined ? match : right[0];
    var right_loc = btwn(start_loc, end_loc);
    if (Curry._2(Parser_env_048[/* token */0], undefined, env$1) === /* T_LESS_THAN */89) {
      var tmp = right[1];
      if (typeof tmp !== "number") {
        if (tmp.tag === 22) {
          error$1(env$1, /* AdjacentJSXElements */46);
        }
        
      }
      
    }
    var match$1 = binary_op(env$1);
    if (match$1 !== undefined) {
      var match$2 = match$1;
      var rop = match$2[0];
      if (is_unary && rop === /* Exp */14) {
        error_at(env$1, /* tuple */[
              right_loc,
              /* InvalidLHSInExponentiation */15
            ]);
      }
      _stack = add_to_stack(right, /* tuple */[
            rop,
            match$2[1]
          ], right_loc, stack);
      continue ;
    } else {
      var _right = right;
      var _rloc = right_loc;
      var _param = stack;
      while(true) {
        var param = _param;
        var rloc = _rloc;
        var right$1 = _right;
        if (param) {
          var match$3 = param[0];
          var loc = btwn(match$3[2], rloc);
          _param = param[1];
          _rloc = loc;
          _right = make_binary(match$3[0], right$1, match$3[1][0], loc);
          continue ;
        } else {
          return right$1;
        }
      };
    }
  };
}

function argument(env) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  if (typeof match === "number" && match === 11) {
    var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
    token$4(env, /* T_ELLIPSIS */11);
    var argument$1 = Curry._1(assignment, env);
    var loc = btwn(start_loc, argument$1[0]);
    return /* Spread */Block.__(1, [/* tuple */[
                loc,
                /* record */[/* argument */argument$1]
              ]]);
  } else {
    return /* Expression */Block.__(0, [Curry._1(assignment, env)]);
  }
}

function arguments$prime(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number" && !(match !== 4 && match !== 105)) {
      return List.rev(acc);
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var acc_000 = argument(env);
      var acc$1 = /* :: */[
        acc_000,
        acc
      ];
      if (Curry._2(Parser_env_048[/* token */0], undefined, env) !== /* T_RPAREN */4) {
        token$4(env, /* T_COMMA */8);
      }
      _acc = acc$1;
      continue ;
    }
    
  };
}

function $$arguments(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_LPAREN */3);
  var args = arguments$prime(env, /* [] */0);
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_RPAREN */4);
  return /* tuple */[
          btwn(start_loc, end_loc),
          args
        ];
}

function template_parts(env, _quasis, _expressions) {
  while(true) {
    var expressions = _expressions;
    var quasis = _quasis;
    var expr = Curry._1(Parse[/* expression */6], env);
    var expressions$1 = /* :: */[
      expr,
      expressions
    ];
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number" && match === 2) {
      push_lex_mode(env, /* TEMPLATE */4);
      var match$1 = Curry._2(Parser_env_048[/* token */0], undefined, env);
      var match$2;
      if (typeof match$1 === "number") {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "parser_flow.ml",
                1602,
                19
              ]
            ];
      } else if (match$1.tag === 2) {
        var match$3 = match$1[0];
        var tail = match$3[2];
        var match$4 = match$3[1];
        token$3(env);
        match$2 = /* tuple */[
          match$3[0],
          /* record */[
            /* value : record */[
              /* raw */match$4[/* raw */1],
              /* cooked */match$4[/* cooked */0]
            ],
            /* tail */tail
          ],
          tail
        ];
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "parser_flow.ml",
                1602,
                19
              ]
            ];
      }
      var loc = match$2[0];
      pop_lex_mode(env);
      var quasis_000 = /* tuple */[
        loc,
        match$2[1]
      ];
      var quasis$1 = /* :: */[
        quasis_000,
        quasis
      ];
      if (match$2[2]) {
        return /* tuple */[
                loc,
                List.rev(quasis$1),
                List.rev(expressions$1)
              ];
      } else {
        _expressions = expressions$1;
        _quasis = quasis$1;
        continue ;
      }
    } else {
      exit = 1;
    }
    if (exit === 1) {
      error_unexpected(env);
      var imaginary_quasi_000 = expr[0];
      var imaginary_quasi_001 = /* record */[
        /* value : record */[
          /* raw */"",
          /* cooked */""
        ],
        /* tail */true
      ];
      var imaginary_quasi = /* tuple */[
        imaginary_quasi_000,
        imaginary_quasi_001
      ];
      return /* tuple */[
              expr[0],
              List.rev(/* :: */[
                    imaginary_quasi,
                    quasis
                  ]),
              List.rev(expressions$1)
            ];
    }
    
  };
}

function template_literal(env, part) {
  var is_tail = part[2];
  var match = part[1];
  var start_loc = part[0];
  token$4(env, /* T_TEMPLATE_PART */Block.__(2, [part]));
  var head_001 = /* record */[
    /* value : record */[
      /* raw */match[/* raw */1],
      /* cooked */match[/* cooked */0]
    ],
    /* tail */is_tail
  ];
  var head = /* tuple */[
    start_loc,
    head_001
  ];
  var match$1 = is_tail ? /* tuple */[
      start_loc,
      /* :: */[
        head,
        /* [] */0
      ],
      /* [] */0
    ] : template_parts(env, /* :: */[
          head,
          /* [] */0
        ], /* [] */0);
  var loc = btwn(start_loc, match$1[0]);
  return /* tuple */[
          loc,
          /* record */[
            /* quasis */match$1[1],
            /* expressions */match$1[2]
          ]
        ];
}

function elements(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number") {
      if (match !== 105) {
        if (match >= 12) {
          exit = 1;
        } else {
          switch (match) {
            case 6 : 
                return List.rev(acc);
            case 8 : 
                token$4(env, /* T_COMMA */8);
                _acc = /* :: */[
                  undefined,
                  acc
                ];
                continue ;
            case 0 : 
            case 1 : 
            case 2 : 
            case 3 : 
            case 4 : 
            case 5 : 
            case 7 : 
            case 9 : 
            case 10 : 
                exit = 1;
                break;
            case 11 : 
                var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
                token$4(env, /* T_ELLIPSIS */11);
                var argument = Curry._1(assignment, env);
                var loc = btwn(start_loc, argument[0]);
                var elem = /* Spread */Block.__(1, [/* tuple */[
                      loc,
                      /* record */[/* argument */argument]
                    ]]);
                _acc = /* :: */[
                  elem,
                  acc
                ];
                continue ;
            
          }
        }
      } else {
        return List.rev(acc);
      }
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var elem$1 = /* Expression */Block.__(0, [Curry._1(assignment, env)]);
      if (Curry._2(Parser_env_048[/* token */0], undefined, env) !== /* T_RBRACKET */6) {
        token$4(env, /* T_COMMA */8);
      }
      _acc = /* :: */[
        elem$1,
        acc
      ];
      continue ;
    }
    
  };
}

function array_initializer(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_LBRACKET */5);
  var elements$1 = elements(env, /* [] */0);
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_RBRACKET */6);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* elements */elements$1]
        ];
}

function error_callback$1(param, param$1) {
  if (typeof param$1 === "number") {
    var switcher = param$1 - 28 | 0;
    if (switcher > 16 || switcher < 0) {
      if (switcher !== 19) {
        throw Parser_env_051[/* Rollback */0];
      } else {
        return /* () */0;
      }
    } else if (switcher > 15 || switcher < 1) {
      return /* () */0;
    } else {
      throw Parser_env_051[/* Rollback */0];
    }
  } else {
    throw Parser_env_051[/* Rollback */0];
  }
}

function try_arrow_function(env) {
  var env$1 = with_error_callback(error_callback$1, env);
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$1);
  var async = Curry._2(Parser_env_048[/* token */0], 1, env$1) !== /* T_ARROW */10 && maybe(env$1, /* T_ASYNC */61);
  var typeParameters = Curry._1(type_parameter_declaration$1, env$1);
  var match;
  if (Curry._2(Parser_env_048[/* is_identifier */8], undefined, env$1) && typeParameters === undefined) {
    var id = Curry._2(Parse[/* identifier */10], /* StrictParamName */28, env$1);
    var param_000 = id[0];
    var param_001 = /* Identifier */Block.__(3, [id]);
    var param = /* tuple */[
      param_000,
      param_001
    ];
    match = /* tuple */[
      /* :: */[
        param,
        /* [] */0
      ],
      /* [] */0,
      undefined,
      undefined
    ];
  } else {
    var match$1 = function_params(env$1);
    match = /* tuple */[
      match$1[0],
      match$1[1],
      match$1[2],
      wrap(annotation_opt, env$1)
    ];
  }
  var rest = match[2];
  var defaults = match[1];
  var params = match[0];
  var predicate = Curry._1(Parse[/* predicate */22], env$1);
  var env$2 = params === /* [] */0 || rest !== undefined ? without_error_callback(env$1) : env$1;
  if (Curry._1(Parser_env_048[/* is_line_terminator */5], env$2) && Curry._2(Parser_env_048[/* token */0], undefined, env$2) === /* T_ARROW */10) {
    error$1(env$2, /* NewlineBeforeArrow */44);
  }
  token$4(env$2, /* T_ARROW */10);
  var env$3 = without_error_callback(env$2);
  var match$2 = with_loc((function (param) {
          var env = param;
          var async$1 = async;
          var generator = false;
          var env$1 = with_in_function(true, env);
          var match = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
          var exit = 0;
          if (typeof match === "number" && match === 1) {
            var match$1 = function_body(env$1, async$1, generator);
            return /* tuple */[
                    match$1[1],
                    match$1[2]
                  ];
          } else {
            exit = 1;
          }
          if (exit === 1) {
            var env$2 = enter_function(env$1, async$1, generator);
            var expr = Curry._1(Parse[/* assignment */7], env$2);
            return /* tuple */[
                    /* BodyExpression */Block.__(1, [expr]),
                    env$2[/* in_strict_mode */5]
                  ];
          }
          
        }), env$3);
  var match$3 = match$2[1];
  var body = match$3[0];
  var simple = is_simple_function_params(params, defaults, rest);
  strict_post_check(env$3, match$3[1], simple, undefined, params);
  var expression;
  expression = body.tag ? true : false;
  var loc = btwn(start_loc, match$2[0]);
  return /* tuple */[
          loc,
          /* ArrowFunction */Block.__(3, [/* record */[
                /* id */undefined,
                /* params */params,
                /* defaults */defaults,
                /* rest */rest,
                /* body */body,
                /* async */async,
                /* generator */false,
                /* predicate */predicate,
                /* expression */expression,
                /* returnType */match[3],
                /* typeParameters */typeParameters
              ]])
        ];
}

function decorator_list_helper(env, _decorators) {
  while(true) {
    var decorators = _decorators;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    if (typeof match === "number" && match === 12) {
      token$3(env);
      _decorators = /* :: */[
        left_hand_side(env),
        decorators
      ];
      continue ;
    } else {
      return decorators;
    }
  };
}

function decorator_list(env) {
  if (env[/* parse_options */20][/* esproposal_decorators */2]) {
    return List.rev(decorator_list_helper(env, /* [] */0));
  } else {
    return /* [] */0;
  }
}

function key(env) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  if (typeof match === "number") {
    if (match === 5) {
      var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      token$4(env, /* T_LBRACKET */5);
      var expr = Curry._1(Parse[/* assignment */7], with_no_in(false, env));
      var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      token$4(env, /* T_RBRACKET */6);
      return /* tuple */[
              btwn(start_loc, end_loc),
              /* Computed */Block.__(2, [expr])
            ];
    } else {
      exit = 1;
    }
  } else {
    switch (match.tag | 0) {
      case 0 : 
          var raw = Curry._2(Parser_env_048[/* value */1], undefined, env);
          var loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
          var value = number(env, match[0]);
          var value$1 = /* Number */Block.__(2, [value]);
          return /* tuple */[
                  loc,
                  /* Literal */Block.__(0, [/* tuple */[
                        loc,
                        /* record */[
                          /* value */value$1,
                          /* raw */raw
                        ]
                      ]])
                ];
      case 1 : 
          var match$1 = match[0];
          var octal = match$1[3];
          var raw$1 = match$1[2];
          var value$2 = match$1[1];
          var loc$1 = match$1[0];
          if (octal) {
            strict_error(env, /* StrictOctalLiteral */31);
          }
          token$4(env, /* T_STRING */Block.__(1, [/* tuple */[
                    loc$1,
                    value$2,
                    raw$1,
                    octal
                  ]]));
          var value$3 = /* String */Block.__(0, [value$2]);
          return /* tuple */[
                  loc$1,
                  /* Literal */Block.__(0, [/* tuple */[
                        loc$1,
                        /* record */[
                          /* value */value$3,
                          /* raw */raw$1
                        ]
                      ]])
                ];
      default:
        exit = 1;
    }
  }
  if (exit === 1) {
    var match$2 = identifier_or_reserved_keyword(env);
    var id = match$2[0];
    return /* tuple */[
            id[0],
            /* Identifier */Block.__(1, [id])
          ];
  }
  
}

function _method(env, kind) {
  var generator$1 = generator(env, false);
  var match = key(env);
  var typeParameters = kind !== 0 ? undefined : Curry._1(type_parameter_declaration$1, env);
  token$4(env, /* T_LPAREN */3);
  var params;
  switch (kind) {
    case 0 : 
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "parser_flow.ml",
                1954,
                16
              ]
            ];
    case 1 : 
        params = /* [] */0;
        break;
    case 2 : 
        var param = Curry._2(Parse[/* identifier_with_type */12], env, /* StrictParamName */28);
        params = /* :: */[
          /* tuple */[
            param[0],
            /* Identifier */Block.__(3, [param])
          ],
          /* [] */0
        ];
        break;
    
  }
  token$4(env, /* T_RPAREN */4);
  var returnType = wrap(annotation_opt, env);
  var match$1 = function_body(env, false, generator$1);
  var body = match$1[1];
  var simple = is_simple_function_params(params, /* [] */0, undefined);
  strict_post_check(env, match$1[2], simple, undefined, params);
  var match$2;
  match$2 = body.tag ? /* tuple */[
      body[0][0],
      true
    ] : /* tuple */[
      body[0][0],
      false
    ];
  var value_000 = match$2[0];
  var value_001 = /* record */[
    /* id */undefined,
    /* params */params,
    /* defaults : [] */0,
    /* rest */undefined,
    /* body */body,
    /* async */false,
    /* generator */generator$1,
    /* predicate */undefined,
    /* expression */match$2[1],
    /* returnType */returnType,
    /* typeParameters */typeParameters
  ];
  var value = /* tuple */[
    value_000,
    value_001
  ];
  return /* tuple */[
          match[1],
          value
        ];
}

function property$1(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_ELLIPSIS */11) {
    token$4(env, /* T_ELLIPSIS */11);
    var argument = Curry._1(Parse[/* assignment */7], env);
    return /* SpreadProperty */Block.__(1, [/* tuple */[
                btwn(start_loc, argument[0]),
                /* record */[/* argument */argument]
              ]]);
  } else {
    var async = Curry._2(Parser_env_048[/* is_identifier */8], 1, env) && maybe(env, /* T_ASYNC */61);
    var match = generator(env, async);
    var match$1 = key(env);
    var tmp;
    var exit = 0;
    if (async || match) {
      exit = 1;
    } else {
      var key$1 = match$1[1];
      switch (key$1.tag | 0) {
        case 1 : 
            switch (key$1[0][1][/* name */0]) {
              case "get" : 
                  var match$2 = Curry._2(Parser_env_048[/* token */0], undefined, env);
                  if (typeof match$2 === "number") {
                    var switcher = match$2 - 3 | 0;
                    tmp = switcher > 74 || switcher < 0 ? (
                        switcher !== 86 ? get(env, start_loc) : init(env, start_loc, key$1, false, false)
                      ) : (
                        switcher > 73 || switcher < 1 ? init(env, start_loc, key$1, false, false) : get(env, start_loc)
                      );
                  } else {
                    tmp = get(env, start_loc);
                  }
                  break;
              case "set" : 
                  var match$3 = Curry._2(Parser_env_048[/* token */0], undefined, env);
                  if (typeof match$3 === "number") {
                    var switcher$1 = match$3 - 3 | 0;
                    tmp = switcher$1 > 74 || switcher$1 < 0 ? (
                        switcher$1 !== 86 ? set(env, start_loc) : init(env, start_loc, key$1, false, false)
                      ) : (
                        switcher$1 > 73 || switcher$1 < 1 ? init(env, start_loc, key$1, false, false) : set(env, start_loc)
                      );
                  } else {
                    tmp = set(env, start_loc);
                  }
                  break;
              default:
                exit = 1;
            }
            break;
        case 0 : 
        case 2 : 
            exit = 1;
            break;
        
      }
    }
    if (exit === 1) {
      tmp = init(env, start_loc, match$1[1], async, match);
    }
    return /* Property */Block.__(0, [tmp]);
  }
}

function get(env, start_loc) {
  var match = _method(env, /* Get */1);
  var match$1 = match[1];
  var end_loc = match$1[0];
  var value_001 = /* Function */Block.__(2, [match$1[1]]);
  var value = /* tuple */[
    end_loc,
    value_001
  ];
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[
            /* key */match[0],
            /* value */value,
            /* kind : Get */1,
            /* _method */false,
            /* shorthand */false
          ]
        ];
}

function set(env, start_loc) {
  var match = _method(env, /* Set */2);
  var match$1 = match[1];
  var end_loc = match$1[0];
  var value_001 = /* Function */Block.__(2, [match$1[1]]);
  var value = /* tuple */[
    end_loc,
    value_001
  ];
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[
            /* key */match[0],
            /* value */value,
            /* kind : Set */2,
            /* _method */false,
            /* shorthand */false
          ]
        ];
}

function init(env, start_loc, key, async, generator) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var match$1;
  var exit = 0;
  if (typeof match === "number") {
    if (match !== 89) {
      if (match >= 9) {
        exit = 1;
      } else {
        switch (match) {
          case 3 : 
              exit = 3;
              break;
          case 0 : 
          case 1 : 
          case 4 : 
          case 5 : 
          case 6 : 
          case 7 : 
              exit = 1;
              break;
          case 2 : 
          case 8 : 
              exit = 2;
              break;
          
        }
      }
    } else {
      exit = 3;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 : 
        token$4(env, /* T_COLON */77);
        match$1 = /* tuple */[
          Curry._1(Parse[/* assignment */7], env),
          false,
          false
        ];
        break;
    case 2 : 
        var tmp;
        switch (key.tag | 0) {
          case 0 : 
              var lit = key[0];
              tmp = /* tuple */[
                lit[0],
                /* Literal */Block.__(19, [lit[1]])
              ];
              break;
          case 1 : 
              var id = key[0];
              tmp = /* tuple */[
                id[0],
                /* Identifier */Block.__(18, [id])
              ];
              break;
          case 2 : 
              tmp = key[0];
              break;
          
        }
        match$1 = /* tuple */[
          tmp,
          true,
          false
        ];
        break;
    case 3 : 
        var typeParameters = Curry._1(type_parameter_declaration$1, env);
        var match$2 = function_params(env);
        var rest = match$2[2];
        var defaults = match$2[1];
        var params = match$2[0];
        var returnType = wrap(annotation_opt, env);
        var match$3 = function_body(env, async, generator);
        var body = match$3[1];
        var simple = is_simple_function_params(params, defaults, rest);
        strict_post_check(env, match$3[2], simple, undefined, params);
        var match$4;
        match$4 = body.tag ? /* tuple */[
            body[0][0],
            true
          ] : /* tuple */[
            body[0][0],
            false
          ];
        var value_000 = match$4[0];
        var value_001 = /* Function */Block.__(2, [/* record */[
              /* id */undefined,
              /* params */params,
              /* defaults */defaults,
              /* rest */rest,
              /* body */body,
              /* async */async,
              /* generator */generator,
              /* predicate */undefined,
              /* expression */match$4[1],
              /* returnType */returnType,
              /* typeParameters */typeParameters
            ]]);
        var value = /* tuple */[
          value_000,
          value_001
        ];
        match$1 = /* tuple */[
          value,
          false,
          true
        ];
        break;
    
  }
  var value$1 = match$1[0];
  return /* tuple */[
          btwn(start_loc, value$1[0]),
          /* record */[
            /* key */key,
            /* value */value$1,
            /* kind : Init */0,
            /* _method */match$1[2],
            /* shorthand */match$1[1]
          ]
        ];
}

function check_property(env, prop_map, prop) {
  if (prop.tag) {
    return prop_map;
  } else {
    var match = prop[0];
    var prop$1 = match[1];
    var prop_loc = match[0];
    var exit = 0;
    switch (prop$1[/* key */0].tag | 0) {
      case 0 : 
      case 1 : 
          exit = 1;
          break;
      case 2 : 
          return prop_map;
      
    }
    if (exit === 1) {
      var match$1 = prop$1[/* key */0];
      var key;
      switch (match$1.tag | 0) {
        case 0 : 
            var match$2 = match$1[0][1][/* value */0];
            if (typeof match$2 === "number") {
              key = "null";
            } else {
              switch (match$2.tag | 0) {
                case 0 : 
                    key = match$2[0];
                    break;
                case 1 : 
                    var b = match$2[0];
                    key = b ? "true" : "false";
                    break;
                case 2 : 
                    key = Pervasives.string_of_float(match$2[0]);
                    break;
                case 3 : 
                    throw [
                          Caml_builtin_exceptions.failure,
                          "RegExp cannot be property key"
                        ];
                
              }
            }
            break;
        case 1 : 
            key = match$1[0][1][/* name */0];
            break;
        case 2 : 
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "parser_flow.ml",
                    2103,
                    30
                  ]
                ];
        
      }
      var prev_kinds;
      try {
        prev_kinds = find(key, prop_map);
      }
      catch (exn){
        if (exn === Caml_builtin_exceptions.not_found) {
          prev_kinds = /* Empty */0;
        } else {
          throw exn;
        }
      }
      var match$3 = prop$1[/* kind */2];
      var kind_string;
      switch (match$3) {
        case 0 : 
            kind_string = "Init";
            break;
        case 1 : 
            kind_string = "Get";
            break;
        case 2 : 
            kind_string = "Set";
            break;
        
      }
      var exit$1 = 0;
      switch (kind_string) {
        case "Init" : 
            if (mem$1("Init", prev_kinds)) {
              strict_error_at(env, /* tuple */[
                    prop_loc,
                    /* StrictDuplicateProperty */33
                  ]);
            } else if (mem$1("Set", prev_kinds) || mem$1("Get", prev_kinds)) {
              error_at(env, /* tuple */[
                    prop_loc,
                    /* AccessorDataProperty */34
                  ]);
            }
            break;
        case "Get" : 
        case "Set" : 
            exit$1 = 2;
            break;
        default:
          
      }
      if (exit$1 === 2) {
        if (mem$1("Init", prev_kinds)) {
          error_at(env, /* tuple */[
                prop_loc,
                /* AccessorDataProperty */34
              ]);
        } else if (mem$1(kind_string, prev_kinds)) {
          error_at(env, /* tuple */[
                prop_loc,
                /* AccessorGetSet */35
              ]);
        }
        
      }
      var kinds = add$1(kind_string, prev_kinds);
      return add$2(key, kinds, prop_map);
    }
    
  }
}

function properties$1(env, _param) {
  while(true) {
    var param = _param;
    var acc = param[1];
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number" && !(match !== 2 && match !== 105)) {
      return List.rev(acc);
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var prop = property$1(env);
      var prop_map = check_property(env, param[0], prop);
      if (Curry._2(Parser_env_048[/* token */0], undefined, env) !== /* T_RCURLY */2) {
        token$4(env, /* T_COMMA */8);
      }
      _param = /* tuple */[
        prop_map,
        /* :: */[
          prop,
          acc
        ]
      ];
      continue ;
    }
    
  };
}

function _initializer(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_LCURLY */1);
  var props = properties$1(env, /* tuple */[
        /* Empty */0,
        /* [] */0
      ]);
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_RCURLY */2);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* properties */props]
        ];
}

function class_implements(env, _acc) {
  while(true) {
    var acc = _acc;
    var id = Curry._2(Parse[/* identifier */10], undefined, env);
    var typeParameters = wrap(type_parameter_instantiation, env);
    var loc = typeParameters !== undefined ? btwn(id[0], typeParameters[0]) : id[0];
    var implement_001 = /* record */[
      /* id */id,
      /* typeParameters */typeParameters
    ];
    var implement = /* tuple */[
      loc,
      implement_001
    ];
    var acc$1 = /* :: */[
      implement,
      acc
    ];
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    if (typeof match === "number" && match === 8) {
      token$4(env, /* T_COMMA */8);
      _acc = acc$1;
      continue ;
    } else {
      return List.rev(acc$1);
    }
  };
}

function init$1(env, start_loc, decorators, key, async, generator, $$static) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  var exit$1 = 0;
  if (typeof match === "number") {
    var switcher = match - 75 | 0;
    if (switcher > 2 || switcher < 0) {
      if (switcher !== -68) {
        exit = 1;
      } else {
        exit$1 = 2;
      }
    } else if (switcher !== 1) {
      exit$1 = 2;
    } else {
      exit = 1;
    }
  } else {
    exit = 1;
  }
  if (exit$1 === 2) {
    if (!async && !generator) {
      var typeAnnotation = wrap(annotation_opt, env);
      var options = env[/* parse_options */20];
      var value = Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_ASSIGN */75 && ($$static && options[/* esproposal_class_static_fields */1] || !$$static && options[/* esproposal_class_instance_fields */0]) ? (token$4(env, /* T_ASSIGN */75), Curry._1(Parse[/* expression */6], env)) : undefined;
      var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      if (!maybe(env, /* T_SEMICOLON */7)) {
        if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_LBRACKET */5 || Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_LPAREN */3) {
          error_unexpected(env);
        }
        
      }
      var loc = btwn(start_loc, end_loc);
      return /* Property */Block.__(1, [/* tuple */[
                  loc,
                  /* record */[
                    /* key */key,
                    /* value */value,
                    /* typeAnnotation */typeAnnotation,
                    /* static */$$static
                  ]
                ]]);
    } else {
      exit = 1;
    }
  }
  if (exit === 1) {
    var typeParameters = Curry._1(type_parameter_declaration$1, env);
    var match$1 = function_params(env);
    var rest = match$1[2];
    var defaults = match$1[1];
    var params = match$1[0];
    var returnType = wrap(annotation_opt, env);
    var match$2 = function_body(env, async, generator);
    var body = match$2[1];
    var simple = is_simple_function_params(params, defaults, rest);
    strict_post_check(env, match$2[2], simple, undefined, params);
    var match$3;
    match$3 = body.tag ? /* tuple */[
        body[0][0],
        true
      ] : /* tuple */[
        body[0][0],
        false
      ];
    var end_loc$1 = match$3[0];
    var value_001 = /* record */[
      /* id */undefined,
      /* params */params,
      /* defaults */defaults,
      /* rest */rest,
      /* body */body,
      /* async */async,
      /* generator */generator,
      /* predicate */undefined,
      /* expression */match$3[1],
      /* returnType */returnType,
      /* typeParameters */typeParameters
    ];
    var value$1 = /* tuple */[
      end_loc$1,
      value_001
    ];
    var kind;
    switch (key.tag | 0) {
      case 0 : 
          var match$4 = key[0][1][/* value */0];
          kind = typeof match$4 === "number" || match$4.tag || match$4[0] !== "constructor" ? /* Method */1 : /* Constructor */0;
          break;
      case 1 : 
          kind = key[0][1][/* name */0] === "constructor" ? /* Constructor */0 : /* Method */1;
          break;
      case 2 : 
          kind = /* Method */1;
          break;
      
    }
    return /* Method */Block.__(0, [/* tuple */[
                btwn(start_loc, end_loc$1),
                /* record */[
                  /* kind */kind,
                  /* key */key,
                  /* value */value$1,
                  /* static */$$static,
                  /* decorators */decorators
                ]
              ]]);
  }
  
}

function class_element(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var decorators = decorator_list(env);
  var $$static = maybe(env, /* T_STATIC */40);
  var async = Curry._2(Parser_env_048[/* token */0], 1, env) !== /* T_LPAREN */3 && Curry._2(Parser_env_048[/* token */0], 1, env) !== /* T_COLON */77 && maybe(env, /* T_ASYNC */61);
  var generator$1 = generator(env, async);
  var match = key(env);
  var exit = 0;
  if (async || generator$1) {
    exit = 1;
  } else {
    var key$1 = match[1];
    switch (key$1.tag | 0) {
      case 1 : 
          switch (key$1[0][1][/* name */0]) {
            case "get" : 
                var match$1 = Curry._2(Parser_env_048[/* token */0], undefined, env);
                var exit$1 = 0;
                exit$1 = typeof match$1 === "number" ? (
                    match$1 >= 75 ? (
                        match$1 >= 78 ? (
                            match$1 !== 89 ? 2 : 3
                          ) : (
                            match$1 !== 76 ? 3 : 2
                          )
                      ) : (
                        match$1 !== 3 && match$1 !== 7 ? 2 : 3
                      )
                  ) : 2;
                switch (exit$1) {
                  case 2 : 
                      var env$1 = env;
                      var start_loc$1 = start_loc;
                      var decorators$1 = decorators;
                      var $$static$1 = $$static;
                      var match$2 = _method(env$1, /* Get */1);
                      var value = match$2[1];
                      return /* Method */Block.__(0, [/* tuple */[
                                  btwn(start_loc$1, value[0]),
                                  /* record */[
                                    /* kind : Get */2,
                                    /* key */match$2[0],
                                    /* value */value,
                                    /* static */$$static$1,
                                    /* decorators */decorators$1
                                  ]
                                ]]);
                  case 3 : 
                      return init$1(env, start_loc, decorators, key$1, async, generator$1, $$static);
                  
                }
                break;
            case "set" : 
                var match$3 = Curry._2(Parser_env_048[/* token */0], undefined, env);
                var exit$2 = 0;
                exit$2 = typeof match$3 === "number" ? (
                    match$3 >= 75 ? (
                        match$3 >= 78 ? (
                            match$3 !== 89 ? 2 : 3
                          ) : (
                            match$3 !== 76 ? 3 : 2
                          )
                      ) : (
                        match$3 !== 3 && match$3 !== 7 ? 2 : 3
                      )
                  ) : 2;
                switch (exit$2) {
                  case 2 : 
                      var env$2 = env;
                      var start_loc$2 = start_loc;
                      var decorators$2 = decorators;
                      var $$static$2 = $$static;
                      var match$4 = _method(env$2, /* Set */2);
                      var value$1 = match$4[1];
                      return /* Method */Block.__(0, [/* tuple */[
                                  btwn(start_loc$2, value$1[0]),
                                  /* record */[
                                    /* kind : Set */3,
                                    /* key */match$4[0],
                                    /* value */value$1,
                                    /* static */$$static$2,
                                    /* decorators */decorators$2
                                  ]
                                ]]);
                  case 3 : 
                      return init$1(env, start_loc, decorators, key$1, async, generator$1, $$static);
                  
                }
                break;
            default:
              exit = 1;
          }
          break;
      case 0 : 
      case 2 : 
          exit = 1;
          break;
      
    }
  }
  if (exit === 1) {
    return init$1(env, start_loc, decorators, match[1], async, generator$1, $$static);
  }
  
}

function elements$1(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number") {
      var switcher = match - 3 | 0;
      if (switcher > 101 || switcher < 0) {
        if ((switcher + 1 >>> 0) > 103) {
          exit = 1;
        } else {
          return List.rev(acc);
        }
      } else if (switcher !== 4) {
        exit = 1;
      } else {
        token$4(env, /* T_SEMICOLON */7);
        continue ;
      }
    } else {
      exit = 1;
    }
    if (exit === 1) {
      _acc = /* :: */[
        Curry._1(class_element, env),
        acc
      ];
      continue ;
    }
    
  };
}

function class_body(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_LCURLY */1);
  var body = elements$1(env, /* [] */0);
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_RCURLY */2);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* body */body]
        ];
}

function _class(env) {
  var match;
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_EXTENDS */39) {
    token$4(env, /* T_EXTENDS */39);
    var superClass = left_hand_side(with_allow_yield(false, env));
    var superTypeParameters = wrap(type_parameter_instantiation, env);
    match = /* tuple */[
      superClass,
      superTypeParameters
    ];
  } else {
    match = /* tuple */[
      undefined,
      undefined
    ];
  }
  var $$implements;
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_IMPLEMENTS */50) {
    if (!env[/* parse_options */20][/* types */4]) {
      error$1(env, /* UnexpectedTypeInterface */10);
    }
    token$4(env, /* T_IMPLEMENTS */50);
    $$implements = class_implements(env, /* [] */0);
  } else {
    $$implements = /* [] */0;
  }
  var body = Curry._1(class_body, env);
  return /* tuple */[
          body,
          match[0],
          match[1],
          $$implements
        ];
}

function class_declaration(env, decorators) {
  var env$1 = with_strict(true, env);
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$1);
  var decorators$1 = Pervasives.$at(decorators, decorator_list(env$1));
  token$4(env$1, /* T_CLASS */38);
  var tmp_env = with_no_let(true, env$1);
  var match = env$1[/* in_export */6];
  var match$1 = Curry._2(Parser_env_048[/* is_identifier */8], undefined, tmp_env);
  var id = match && !match$1 ? undefined : Curry._2(Parse[/* identifier */10], undefined, tmp_env);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env$1);
  var match$2 = _class(env$1);
  var body = match$2[0];
  var loc = btwn(start_loc, body[0]);
  return /* tuple */[
          loc,
          /* ClassDeclaration */Block.__(20, [/* record */[
                /* id */id,
                /* body */body,
                /* superClass */match$2[1],
                /* typeParameters */typeParameters,
                /* superTypeParameters */match$2[2],
                /* implements */match$2[3],
                /* classDecorators */decorators$1
              ]])
        ];
}

function class_expression(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var decorators = decorator_list(env);
  token$4(env, /* T_CLASS */38);
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var match$1;
  var exit = 0;
  if (typeof match === "number") {
    var switcher = match - 1 | 0;
    if (switcher > 38 || switcher < 0) {
      if (switcher !== 88) {
        exit = 1;
      } else {
        match$1 = /* tuple */[
          undefined,
          undefined
        ];
      }
    } else if (switcher > 37 || switcher < 1) {
      match$1 = /* tuple */[
        undefined,
        undefined
      ];
    } else {
      exit = 1;
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var id = Curry._2(Parse[/* identifier */10], undefined, env);
    var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env);
    match$1 = /* tuple */[
      id,
      typeParameters
    ];
  }
  var match$2 = _class(env);
  var body = match$2[0];
  var loc = btwn(start_loc, body[0]);
  return /* tuple */[
          loc,
          /* Class */Block.__(23, [/* record */[
                /* id */match$1[0],
                /* body */body,
                /* superClass */match$2[1],
                /* typeParameters */match$1[1],
                /* superTypeParameters */match$2[2],
                /* implements */match$2[3],
                /* classDecorators */decorators
              ]])
        ];
}

function expression(env) {
  var expression$1 = Curry._1(Parse[/* expression */6], env);
  var match = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env);
  var end_loc = match !== undefined ? match : expression$1[0];
  semicolon(env);
  return /* tuple */[
          btwn(expression$1[0], end_loc),
          /* Expression */Block.__(1, [/* record */[/* expression */expression$1]])
        ];
}

function declare_var(env, start_loc) {
  token$4(env, /* T_VAR */22);
  var id = Curry._2(Parse[/* identifier_with_type */12], env, /* StrictVarName */27);
  var match = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env);
  var end_loc = match !== undefined ? match : id[0];
  var loc = btwn(start_loc, end_loc);
  semicolon(env);
  return /* tuple */[
          loc,
          /* record */[/* id */id]
        ];
}

function declare_function(env, start_loc) {
  token$4(env, /* T_FUNCTION */13);
  var id = Curry._2(Parse[/* identifier */10], undefined, env);
  var start_sig_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var typeParameters = Curry._1(type_parameter_declaration$1, env);
  var match = wrap(function_param_list, env);
  token$4(env, /* T_COLON */77);
  var returnType = wrap(_type, env);
  var end_loc = returnType[0];
  var loc = btwn(start_sig_loc, end_loc);
  var value_001 = /* Function */Block.__(1, [/* record */[
        /* params */match[1],
        /* returnType */returnType,
        /* rest */match[0],
        /* typeParameters */typeParameters
      ]]);
  var value = /* tuple */[
    loc,
    value_001
  ];
  var typeAnnotation = /* tuple */[
    loc,
    value
  ];
  var init = id[1];
  var id_000 = btwn(id[0], end_loc);
  var id_001 = /* record */[
    /* name */init[/* name */0],
    /* typeAnnotation */typeAnnotation,
    /* optional */init[/* optional */2]
  ];
  var id$1 = /* tuple */[
    id_000,
    id_001
  ];
  var match$1 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env);
  var end_loc$1 = match$1 !== undefined ? match$1 : end_loc;
  var predicate = Curry._1(Parse[/* predicate */22], env);
  semicolon(env);
  var loc$1 = btwn(start_loc, end_loc$1);
  return /* tuple */[
          loc$1,
          /* record */[
            /* id */id$1,
            /* predicate */predicate
          ]
        ];
}

function declare($staropt$star, env) {
  var in_module = $staropt$star !== undefined ? $staropt$star : false;
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, /* UnexpectedTypeDeclaration */7);
  }
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var match = Curry._2(Parser_env_048[/* token */0], 1, env);
  var exit = 0;
  if (typeof match === "number") {
    if (match >= 22) {
      if (match >= 38) {
        if (match >= 62) {
          exit = 1;
        } else {
          switch (match - 38 | 0) {
            case 0 : 
                token$4(env, /* T_DECLARE */58);
                var env$1 = env;
                var start_loc$1 = start_loc;
                var match$1 = Curry._2(declare_class, env$1, start_loc$1);
                return /* tuple */[
                        match$1[0],
                        /* DeclareClass */Block.__(24, [match$1[1]])
                      ];
            case 9 : 
                if (in_module) {
                  return declare_export_declaration(in_module, env);
                } else {
                  exit = 1;
                }
                break;
            case 13 : 
                token$4(env, /* T_DECLARE */58);
                return $$interface(env);
            case 21 : 
                token$4(env, /* T_DECLARE */58);
                return type_alias(env);
            case 1 : 
            case 2 : 
            case 3 : 
            case 4 : 
            case 5 : 
            case 6 : 
            case 7 : 
            case 8 : 
            case 10 : 
            case 11 : 
            case 12 : 
            case 14 : 
            case 15 : 
            case 16 : 
            case 17 : 
            case 18 : 
            case 19 : 
            case 20 : 
            case 22 : 
                exit = 1;
                break;
            case 23 : 
                token$4(env, /* T_DECLARE */58);
                error$1(env, /* DeclareAsync */49);
                token$4(env, /* T_ASYNC */61);
                return declare_function_statement(env, start_loc);
            
          }
        }
      } else if (match >= 23) {
        exit = 1;
      } else {
        token$4(env, /* T_DECLARE */58);
        return declare_var_statement(env, start_loc);
      }
    } else if (match !== 13) {
      if (match !== 0 || Curry._2(Parser_env_048[/* value */1], 1, env) !== "module") {
        exit = 1;
      } else {
        token$4(env, /* T_DECLARE */58);
        contextual(env, "module");
        if (in_module || Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_PERIOD */9) {
          var env$2 = env;
          var start_loc$2 = start_loc;
          token$4(env$2, /* T_PERIOD */9);
          contextual(env$2, "exports");
          var type_annot = wrap(annotation, env$2);
          var match$2 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$2);
          var end_loc = match$2 !== undefined ? match$2 : type_annot[0];
          semicolon(env$2);
          var loc = btwn(start_loc$2, end_loc);
          return /* tuple */[
                  loc,
                  /* DeclareModuleExports */Block.__(26, [type_annot])
                ];
        } else {
          var env$3 = env;
          var start_loc$3 = start_loc;
          var match$3 = Curry._2(Parser_env_048[/* token */0], undefined, env$3);
          var id;
          if (typeof match$3 === "number" || match$3.tag !== 1) {
            id = /* Identifier */Block.__(0, [Curry._2(Parse[/* identifier */10], undefined, env$3)]);
          } else {
            var match$4 = match$3[0];
            var octal = match$4[3];
            var raw = match$4[2];
            var value = match$4[1];
            var loc$1 = match$4[0];
            if (octal) {
              strict_error(env$3, /* StrictOctalLiteral */31);
            }
            token$4(env$3, /* T_STRING */Block.__(1, [/* tuple */[
                      loc$1,
                      value,
                      raw,
                      octal
                    ]]));
            var value$1 = /* String */Block.__(0, [value]);
            id = /* Literal */Block.__(1, [/* tuple */[
                  loc$1,
                  /* record */[
                    /* value */value$1,
                    /* raw */raw
                  ]
                ]]);
          }
          var body_start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$3);
          token$4(env$3, /* T_LCURLY */1);
          var match$5 = module_items(env$3, undefined, /* [] */0);
          var module_kind = match$5[0];
          token$4(env$3, /* T_RCURLY */2);
          var body_end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$3);
          var body_loc = btwn(body_start_loc, body_end_loc);
          var body_001 = /* record */[/* body */match$5[1]];
          var body = /* tuple */[
            body_loc,
            body_001
          ];
          var loc$2 = btwn(start_loc$3, body_loc);
          var kind = module_kind !== undefined ? module_kind : /* CommonJS */Block.__(0, [loc$2]);
          return /* tuple */[
                  loc$2,
                  /* DeclareModule */Block.__(25, [/* record */[
                        /* id */id,
                        /* body */body,
                        /* kind */kind
                      ]])
                ];
        }
      }
    } else {
      token$4(env, /* T_DECLARE */58);
      return declare_function_statement(env, start_loc);
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    if (in_module) {
      token$4(env, /* T_DECLARE */58);
      return declare_var_statement(env, start_loc);
    } else {
      return Curry._1(Parse[/* statement */1], env);
    }
  }
  
}

function extract_ident_name(param) {
  return param[1][/* name */0];
}

function export_specifiers_and_errs(env, _specifiers, _errs) {
  while(true) {
    var errs = _errs;
    var specifiers = _specifiers;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number" && !(match !== 2 && match !== 105)) {
      return /* tuple */[
              List.rev(specifiers),
              List.rev(errs)
            ];
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var match$1 = Curry._1(Parse[/* identifier_or_reserved_keyword */11], env);
      var id = match$1[0];
      var match$2;
      if (Curry._2(Parser_env_048[/* value */1], undefined, env) === "as") {
        contextual(env, "as");
        var match$3 = Curry._1(Parse[/* identifier_or_reserved_keyword */11], env);
        var name = match$3[0];
        record_export(env, /* tuple */[
              name[0],
              extract_ident_name(name)
            ]);
        match$2 = /* tuple */[
          name,
          undefined,
          name[0]
        ];
      } else {
        var loc = id[0];
        record_export(env, /* tuple */[
              loc,
              extract_ident_name(id)
            ]);
        match$2 = /* tuple */[
          undefined,
          match$1[1],
          loc
        ];
      }
      var err = match$2[1];
      var loc$1 = btwn(id[0], match$2[2]);
      var specifier_001 = /* record */[
        /* id */id,
        /* name */match$2[0]
      ];
      var specifier = /* tuple */[
        loc$1,
        specifier_001
      ];
      if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_COMMA */8) {
        token$4(env, /* T_COMMA */8);
      }
      var errs$1 = err !== undefined ? /* :: */[
          err,
          errs
        ] : errs;
      _errs = errs$1;
      _specifiers = /* :: */[
        specifier,
        specifiers
      ];
      continue ;
    }
    
  };
}

function type_alias_helper(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, /* UnexpectedTypeAlias */5);
  }
  token$4(env, /* T_TYPE */59);
  push_lex_mode(env, /* TYPE */1);
  var id = Curry._2(Parse[/* identifier */10], undefined, env);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env);
  token$4(env, /* T_ASSIGN */75);
  var right = wrap(_type, env);
  var match = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env);
  var end_loc = match !== undefined ? match : right[0];
  semicolon(env);
  pop_lex_mode(env);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[
            /* id */id,
            /* typeParameters */typeParameters,
            /* right */right
          ]
        ];
}

function export_source(env) {
  contextual(env, "from");
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  if (typeof match === "number" || match.tag !== 1) {
    exit = 1;
  } else {
    var match$1 = match[0];
    var octal = match$1[3];
    var raw = match$1[2];
    var value = match$1[1];
    var loc = match$1[0];
    if (octal) {
      strict_error(env, /* StrictOctalLiteral */31);
    }
    token$4(env, /* T_STRING */Block.__(1, [/* tuple */[
              loc,
              value,
              raw,
              octal
            ]]));
    var value$1 = /* String */Block.__(0, [value]);
    return /* tuple */[
            loc,
            /* record */[
              /* value */value$1,
              /* raw */raw
            ]
          ];
  }
  if (exit === 1) {
    var raw$1 = Curry._2(Parser_env_048[/* value */1], undefined, env);
    var value$2 = /* String */Block.__(0, [raw$1]);
    var ret_000 = Curry._2(Parser_env_048[/* loc */2], undefined, env);
    var ret_001 = /* record */[
      /* value */value$2,
      /* raw */raw$1
    ];
    var ret = /* tuple */[
      ret_000,
      ret_001
    ];
    error_unexpected(env);
    return ret;
  }
  
}

function $$interface(env) {
  if (Curry._2(Parser_env_048[/* is_identifier */8], 1, env)) {
    var match = Curry._1(interface_helper, env);
    return /* tuple */[
            match[0],
            /* InterfaceDeclaration */Block.__(21, [match[1]])
          ];
  } else {
    return expression(env);
  }
}

function declare_var_statement(env, start_loc) {
  var match = declare_var(env, start_loc);
  return /* tuple */[
          match[0],
          /* DeclareVariable */Block.__(22, [match[1]])
        ];
}

function declare_export_declaration($staropt$star, env) {
  var allow_export_type = $staropt$star !== undefined ? $staropt$star : false;
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, /* UnexpectedTypeDeclaration */7);
  }
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_DECLARE */58);
  var env$1 = with_in_export(true, with_strict(true, env));
  token$4(env$1, /* T_EXPORT */47);
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
  var exit = 0;
  if (typeof match === "number") {
    if (match >= 52) {
      if (match !== 59) {
        if (match !== 97) {
          exit = 1;
        } else {
          var loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$1);
          token$4(env$1, /* T_MULT */97);
          var parse_export_star_as = env$1[/* parse_options */20][/* esproposal_export_star_as */3];
          var local_name = Curry._2(Parser_env_048[/* value */1], undefined, env$1) === "as" ? (contextual(env$1, "as"), parse_export_star_as ? Curry._2(Parse[/* identifier */10], undefined, env$1) : (error$1(env$1, /* UnexpectedTypeDeclaration */7), undefined)) : undefined;
          var specifiers = /* ExportBatchSpecifier */Block.__(1, [
              loc,
              local_name
            ]);
          var source = export_source(env$1);
          var match$1 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$1);
          var end_loc = match$1 !== undefined ? match$1 : source[0];
          var source$1 = source;
          semicolon(env$1);
          return /* tuple */[
                  btwn(start_loc, end_loc),
                  /* DeclareExportDeclaration */Block.__(27, [/* record */[
                        /* default */false,
                        /* declaration */undefined,
                        /* specifiers */specifiers,
                        /* source */source$1
                      ]])
                ];
        }
      } else if (allow_export_type) {
        var match$2 = type_alias_helper(env$1);
        var alias_loc = match$2[0];
        var loc$1 = btwn(start_loc, alias_loc);
        return /* tuple */[
                loc$1,
                /* DeclareExportDeclaration */Block.__(27, [/* record */[
                      /* default */false,
                      /* declaration *//* NamedType */Block.__(4, [/* tuple */[
                            alias_loc,
                            match$2[1]
                          ]]),
                      /* specifiers */undefined,
                      /* source */undefined
                    ]])
              ];
      } else {
        exit = 1;
      }
    } else if (match >= 39) {
      if (match >= 51 && allow_export_type) {
        var match$3 = Curry._1(interface_helper, env$1);
        var iface_loc = match$3[0];
        var loc$2 = btwn(start_loc, iface_loc);
        return /* tuple */[
                loc$2,
                /* DeclareExportDeclaration */Block.__(27, [/* record */[
                      /* default */false,
                      /* declaration *//* Interface */Block.__(5, [/* tuple */[
                            iface_loc,
                            match$3[1]
                          ]]),
                      /* specifiers */undefined,
                      /* source */undefined
                    ]])
              ];
      } else {
        exit = 1;
      }
    } else if (match >= 13) {
      switch (match - 13 | 0) {
        case 21 : 
            token$4(env$1, /* T_DEFAULT */34);
            var match$4 = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
            var match$5;
            var exit$1 = 0;
            if (typeof match$4 === "number") {
              if (match$4 !== 13) {
                if (match$4 !== 38) {
                  exit$1 = 3;
                } else {
                  var _class = Curry._2(declare_class, env$1, start_loc);
                  match$5 = /* tuple */[
                    _class[0],
                    /* Class */Block.__(2, [_class])
                  ];
                }
              } else {
                var fn = declare_function(env$1, start_loc);
                match$5 = /* tuple */[
                  fn[0],
                  /* Function */Block.__(1, [fn])
                ];
              }
            } else {
              exit$1 = 3;
            }
            if (exit$1 === 3) {
              var _type$1 = wrap(_type, env$1);
              var match$6 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$1);
              var end_loc$1 = match$6 !== undefined ? match$6 : _type$1[0];
              semicolon(env$1);
              match$5 = /* tuple */[
                end_loc$1,
                /* DefaultType */Block.__(3, [_type$1])
              ];
            }
            return /* tuple */[
                    btwn(start_loc, match$5[0]),
                    /* DeclareExportDeclaration */Block.__(27, [/* record */[
                          /* default */true,
                          /* declaration */match$5[1],
                          /* specifiers */undefined,
                          /* source */undefined
                        ]])
                  ];
        case 1 : 
        case 2 : 
        case 3 : 
        case 4 : 
        case 5 : 
        case 6 : 
        case 7 : 
        case 8 : 
        case 10 : 
        case 11 : 
        case 14 : 
        case 15 : 
        case 16 : 
        case 17 : 
        case 18 : 
        case 19 : 
        case 20 : 
        case 22 : 
        case 23 : 
        case 24 : 
            exit = 1;
            break;
        case 0 : 
        case 9 : 
        case 12 : 
        case 13 : 
        case 25 : 
            exit = 2;
            break;
        
      }
    } else {
      exit = 1;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 : 
        var match$7 = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
        if (typeof match$7 === "number") {
          if (match$7 !== 51) {
            if (match$7 !== 59) {
              
            } else {
              error$1(env$1, /* DeclareExportType */52);
            }
          } else {
            error$1(env$1, /* DeclareExportInterface */53);
          }
        }
        token$4(env$1, /* T_LCURLY */1);
        var match$8 = export_specifiers_and_errs(env$1, /* [] */0, /* [] */0);
        var specifiers$1 = /* ExportSpecifiers */Block.__(0, [match$8[0]]);
        var end_loc$2 = Curry._2(Parser_env_048[/* loc */2], undefined, env$1);
        token$4(env$1, /* T_RCURLY */2);
        var source$2 = Curry._2(Parser_env_048[/* value */1], undefined, env$1) === "from" ? export_source(env$1) : (List.iter((function (param) {
                    return error_at(env$1, param);
                  }), match$8[1]), undefined);
        var match$9 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$1);
        var end_loc$3 = match$9 !== undefined ? match$9 : (
            source$2 !== undefined ? source$2[0] : end_loc$2
          );
        semicolon(env$1);
        return /* tuple */[
                btwn(start_loc, end_loc$3),
                /* DeclareExportDeclaration */Block.__(27, [/* record */[
                      /* default */false,
                      /* declaration */undefined,
                      /* specifiers */specifiers$1,
                      /* source */source$2
                    ]])
              ];
    case 2 : 
        var token$5 = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
        var match$10;
        var exit$2 = 0;
        if (typeof token$5 === "number") {
          if (token$5 >= 23) {
            if (token$5 >= 27) {
              if (token$5 !== 38) {
                exit$2 = 3;
              } else {
                var _class$1 = Curry._2(declare_class, env$1, start_loc);
                match$10 = /* tuple */[
                  _class$1[0],
                  /* Class */Block.__(2, [_class$1])
                ];
              }
            } else {
              exit$2 = token$5 >= 25 ? 4 : 3;
            }
          } else if (token$5 !== 13) {
            exit$2 = token$5 >= 22 ? 4 : 3;
          } else {
            var fn$1 = declare_function(env$1, start_loc);
            match$10 = /* tuple */[
              fn$1[0],
              /* Function */Block.__(1, [fn$1])
            ];
          }
        } else {
          exit$2 = 3;
        }
        switch (exit$2) {
          case 3 : 
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    /* tuple */[
                      "parser_flow.ml",
                      3480,
                      17
                    ]
                  ];
          case 4 : 
              if (typeof token$5 === "number") {
                if (token$5 !== 25) {
                  if (token$5 !== 26) {
                    
                  } else {
                    error$1(env$1, /* DeclareExportLet */50);
                  }
                } else {
                  error$1(env$1, /* DeclareExportConst */51);
                }
              }
              var $$var = declare_var(env$1, start_loc);
              match$10 = /* tuple */[
                $$var[0],
                /* Variable */Block.__(0, [$$var])
              ];
              break;
          
        }
        return /* tuple */[
                btwn(start_loc, match$10[0]),
                /* DeclareExportDeclaration */Block.__(27, [/* record */[
                      /* default */false,
                      /* declaration */match$10[1],
                      /* specifiers */undefined,
                      /* source */undefined
                    ]])
              ];
    
  }
}

function type_alias(env) {
  if (Curry._2(Parser_env_048[/* is_identifier */8], 1, env)) {
    var match = type_alias_helper(env);
    return /* tuple */[
            match[0],
            /* TypeAlias */Block.__(7, [match[1]])
          ];
  } else {
    return Curry._1(Parse[/* statement */1], env);
  }
}

function declare_function_statement(env, start_loc) {
  var match = declare_function(env, start_loc);
  return /* tuple */[
          match[0],
          /* DeclareFunction */Block.__(23, [match[1]])
        ];
}

function supers(env, _acc) {
  while(true) {
    var acc = _acc;
    var $$super = wrap(generic, env);
    var acc$1 = /* :: */[
      $$super,
      acc
    ];
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    if (typeof match === "number" && match === 8) {
      token$4(env, /* T_COMMA */8);
      _acc = acc$1;
      continue ;
    } else {
      return List.rev(acc$1);
    }
  };
}

function interface_helper(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, /* UnexpectedTypeInterface */10);
  }
  token$4(env, /* T_INTERFACE */51);
  var id = Curry._2(Parse[/* identifier */10], undefined, env);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env);
  var $$extends = Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_EXTENDS */39 ? (token$4(env, /* T_EXTENDS */39), supers(env, /* [] */0)) : /* [] */0;
  var body = _object$1(true, env);
  var loc = btwn(start_loc, body[0]);
  return /* tuple */[
          loc,
          /* record */[
            /* id */id,
            /* typeParameters */typeParameters,
            /* body */body,
            /* extends */$$extends,
            /* mixins : [] */0
          ]
        ];
}

function supers$1(env, _acc) {
  while(true) {
    var acc = _acc;
    var $$super = wrap(generic, env);
    var acc$1 = /* :: */[
      $$super,
      acc
    ];
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    if (typeof match === "number" && match === 8) {
      token$4(env, /* T_COMMA */8);
      _acc = acc$1;
      continue ;
    } else {
      return List.rev(acc$1);
    }
  };
}

function declare_class(env, start_loc) {
  var env$1 = with_strict(true, env);
  token$4(env$1, /* T_CLASS */38);
  var id = Curry._2(Parse[/* identifier */10], undefined, env$1);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env$1);
  var $$extends = Curry._2(Parser_env_048[/* token */0], undefined, env$1) === /* T_EXTENDS */39 ? (token$4(env$1, /* T_EXTENDS */39), supers$1(env$1, /* [] */0)) : /* [] */0;
  var mixins = Curry._2(Parser_env_048[/* value */1], undefined, env$1) === "mixins" ? (contextual(env$1, "mixins"), supers$1(env$1, /* [] */0)) : /* [] */0;
  var body = _object$1(true, env$1);
  var loc = btwn(start_loc, body[0]);
  return /* tuple */[
          loc,
          /* record */[
            /* id */id,
            /* typeParameters */typeParameters,
            /* body */body,
            /* extends */$$extends,
            /* mixins */mixins
          ]
        ];
}

function module_items(env, _module_kind, _acc) {
  while(true) {
    var acc = _acc;
    var module_kind = _module_kind;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number" && !(match !== 2 && match !== 105)) {
      return /* tuple */[
              module_kind,
              List.rev(acc)
            ];
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var stmt = declare(true, env);
      var stmt$1 = stmt[1];
      var loc = stmt[0];
      var module_kind$1;
      if (module_kind !== undefined) {
        if (module_kind.tag) {
          if (typeof stmt$1 === "number" || stmt$1.tag !== 26) {
            module_kind$1 = module_kind;
          } else {
            error$1(env, /* AmbiguousDeclareModuleKind */61);
            module_kind$1 = module_kind;
          }
        } else if (typeof stmt$1 === "number") {
          module_kind$1 = module_kind;
        } else {
          switch (stmt$1.tag | 0) {
            case 26 : 
                error$1(env, /* DuplicateDeclareModuleExports */60);
                module_kind$1 = module_kind;
                break;
            case 27 : 
                var declaration = stmt$1[0][/* declaration */1];
                if (declaration !== undefined) {
                  switch (declaration.tag | 0) {
                    case 4 : 
                    case 5 : 
                        break;
                    default:
                      error$1(env, /* AmbiguousDeclareModuleKind */61);
                  }
                } else {
                  error$1(env, /* AmbiguousDeclareModuleKind */61);
                }
                module_kind$1 = module_kind;
                break;
            default:
              module_kind$1 = module_kind;
          }
        }
      } else if (typeof stmt$1 === "number") {
        module_kind$1 = module_kind;
      } else {
        switch (stmt$1.tag | 0) {
          case 26 : 
              module_kind$1 = /* CommonJS */Block.__(0, [loc]);
              break;
          case 27 : 
              var declaration$1 = stmt$1[0][/* declaration */1];
              if (declaration$1 !== undefined) {
                switch (declaration$1.tag | 0) {
                  case 4 : 
                  case 5 : 
                      module_kind$1 = module_kind;
                      break;
                  default:
                    module_kind$1 = /* ES */Block.__(1, [loc]);
                }
              } else {
                module_kind$1 = /* ES */Block.__(1, [loc]);
              }
              break;
          default:
            module_kind$1 = module_kind;
        }
      }
      _acc = /* :: */[
        stmt,
        acc
      ];
      _module_kind = module_kind$1;
      continue ;
    }
    
  };
}

function fold(acc, _param) {
  while(true) {
    var param = _param;
    var match = param[1];
    switch (match.tag | 0) {
      case 0 : 
          return List.fold_left((function (acc, prop) {
                        if (prop.tag) {
                          return fold(acc, prop[0][1][/* argument */0]);
                        } else {
                          return fold(acc, prop[0][1][/* pattern */1]);
                        }
                      }), acc, match[0][/* properties */0]);
      case 1 : 
          return List.fold_left((function (acc, elem) {
                        if (elem !== undefined) {
                          var match = elem;
                          if (match.tag) {
                            return fold(acc, match[0][1][/* argument */0]);
                          } else {
                            return fold(acc, match[0]);
                          }
                        } else {
                          return acc;
                        }
                      }), acc, match[0][/* elements */0]);
      case 2 : 
          _param = match[0][/* left */0];
          continue ;
      case 3 : 
          var match$1 = match[0];
          return /* :: */[
                  /* tuple */[
                    match$1[0],
                    match$1[1][/* name */0]
                  ],
                  acc
                ];
      case 4 : 
          throw [
                Caml_builtin_exceptions.failure,
                "Parser error: No such thing as an expression pattern!"
              ];
      
    }
  };
}

function assert_can_be_forin_or_forof(env, err, param) {
  if (param !== undefined) {
    var match = param;
    if (match.tag) {
      var match$1 = match[0];
      var loc = match$1[0];
      if (Curry._1(Parse[/* is_assignable_lhs */21], /* tuple */[
              loc,
              match$1[1]
            ])) {
        return 0;
      } else {
        return error_at(env, /* tuple */[
                    loc,
                    err
                  ]);
      }
    } else {
      var match$2 = match[0];
      var declarations = match$2[1][/* declarations */0];
      var exit = 0;
      if (declarations && !(declarations[0][1][/* init */1] !== undefined || declarations[1])) {
        return /* () */0;
      } else {
        exit = 1;
      }
      if (exit === 1) {
        return error_at(env, /* tuple */[
                    match$2[0],
                    err
                  ]);
      }
      
    }
  } else {
    return error$1(env, err);
  }
}

function _if(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_IF */14);
  token$4(env, /* T_LPAREN */3);
  var test = Curry._1(Parse[/* expression */6], env);
  token$4(env, /* T_RPAREN */4);
  Curry._2(Parser_env_048[/* token */0], undefined, env);
  var consequent = Curry._2(Parser_env_048[/* is_function */9], undefined, env) ? (strict_error(env, /* StrictFunctionStatement */45), _function(env)) : Curry._1(Parse[/* statement */1], env);
  var alternate = Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_ELSE */41 ? (token$4(env, /* T_ELSE */41), Curry._1(Parse[/* statement */1], env)) : undefined;
  var end_loc = alternate !== undefined ? alternate[0] : consequent[0];
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* If */Block.__(2, [/* record */[
                /* test */test,
                /* consequent */consequent,
                /* alternate */alternate
              ]])
        ];
}

function case_list(env, _param) {
  while(true) {
    var param = _param;
    var acc = param[1];
    var seen_default = param[0];
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number" && !(match !== 2 && match !== 105)) {
      return List.rev(acc);
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      var match$1 = Curry._2(Parser_env_048[/* token */0], undefined, env);
      var test;
      var exit$1 = 0;
      if (typeof match$1 === "number" && match$1 === 34) {
        if (seen_default) {
          error$1(env, /* MultipleDefaultsInSwitch */19);
        }
        token$4(env, /* T_DEFAULT */34);
        test = undefined;
      } else {
        exit$1 = 2;
      }
      if (exit$1 === 2) {
        token$4(env, /* T_CASE */31);
        test = Curry._1(Parse[/* expression */6], env);
      }
      var seen_default$1 = seen_default || test === undefined;
      var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      token$4(env, /* T_COLON */77);
      var term_fn = function (param) {
        if (typeof param === "number") {
          var switcher = param - 2 | 0;
          if (switcher > 29 || switcher < 0) {
            return switcher === 32;
          } else {
            return switcher > 28 || switcher < 1;
          }
        } else {
          return false;
        }
      };
      var consequent = Curry._2(Parse[/* statement_list */3], term_fn, with_in_switch(true, env));
      var match$2 = List.rev(consequent);
      var end_loc$1 = match$2 ? match$2[0][0] : end_loc;
      var acc_000 = /* tuple */[
        btwn(start_loc, end_loc$1),
        /* record */[
          /* test */test,
          /* consequent */consequent
        ]
      ];
      var acc$1 = /* :: */[
        acc_000,
        acc
      ];
      _param = /* tuple */[
        seen_default$1,
        acc$1
      ];
      continue ;
    }
    
  };
}

function var_or_const(env) {
  var match = variable(env);
  var match$1 = match[0];
  var start_loc = match$1[0];
  var match$2 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env);
  var end_loc = match$2 !== undefined ? match$2 : start_loc;
  semicolon(env);
  List.iter((function (param) {
          return error_at(env, param);
        }), match[1]);
  return /* tuple */[
          btwn(start_loc, end_loc),
          match$1[1]
        ];
}

function source(env) {
  contextual(env, "from");
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  if (typeof match === "number" || match.tag !== 1) {
    exit = 1;
  } else {
    var match$1 = match[0];
    var octal = match$1[3];
    var raw = match$1[2];
    var value = match$1[1];
    var loc = match$1[0];
    if (octal) {
      strict_error(env, /* StrictOctalLiteral */31);
    }
    token$4(env, /* T_STRING */Block.__(1, [/* tuple */[
              loc,
              value,
              raw,
              octal
            ]]));
    var value$1 = /* String */Block.__(0, [value]);
    return /* tuple */[
            loc,
            /* record */[
              /* value */value$1,
              /* raw */raw
            ]
          ];
  }
  if (exit === 1) {
    var raw$1 = Curry._2(Parser_env_048[/* value */1], undefined, env);
    var value$2 = /* String */Block.__(0, [raw$1]);
    var ret_000 = Curry._2(Parser_env_048[/* loc */2], undefined, env);
    var ret_001 = /* record */[
      /* value */value$2,
      /* raw */raw$1
    ];
    var ret = /* tuple */[
      ret_000,
      ret_001
    ];
    error_unexpected(env);
    return ret;
  }
  
}

function specifier_list(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number" && !(match !== 2 && match !== 105)) {
      return List.rev(acc);
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var match$1 = Curry._1(Parse[/* identifier_or_reserved_keyword */11], env);
      var err = match$1[1];
      var remote = match$1[0];
      var specifier;
      if (Curry._2(Parser_env_048[/* value */1], undefined, env) === "as") {
        contextual(env, "as");
        var local = Curry._2(Parse[/* identifier */10], undefined, env);
        specifier = /* ImportNamedSpecifier */Block.__(0, [/* record */[
              /* local */local,
              /* remote */remote
            ]]);
      } else {
        if (err !== undefined) {
          error_at(env, err);
        }
        specifier = /* ImportNamedSpecifier */Block.__(0, [/* record */[
              /* local */undefined,
              /* remote */remote
            ]]);
      }
      if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_COMMA */8) {
        token$4(env, /* T_COMMA */8);
      }
      _acc = /* :: */[
        specifier,
        acc
      ];
      continue ;
    }
    
  };
}

function named_or_namespace_specifier(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  if (typeof match === "number" && match === 97) {
    token$4(env, /* T_MULT */97);
    contextual(env, "as");
    var id = Curry._2(Parse[/* identifier */10], undefined, env);
    return /* :: */[
            /* ImportNamespaceSpecifier */Block.__(2, [/* tuple */[
                  btwn(start_loc, id[0]),
                  id
                ]]),
            /* [] */0
          ];
  } else {
    exit = 1;
  }
  if (exit === 1) {
    token$4(env, /* T_LCURLY */1);
    var specifiers = specifier_list(env, /* [] */0);
    token$4(env, /* T_RCURLY */2);
    return specifiers;
  }
  
}

function from_expr(env, param) {
  var expr = param[1];
  var loc = param[0];
  var exit = 0;
  if (typeof expr === "number") {
    exit = 1;
  } else {
    switch (expr.tag | 0) {
      case 0 : 
          var env$1 = env;
          var param$1 = /* tuple */[
            loc,
            expr[0]
          ];
          var elements = List.map((function (param) {
                  var env$2 = env$1;
                  var param$1 = param;
                  if (param$1 !== undefined) {
                    var match = param$1;
                    if (match.tag) {
                      var match$1 = match[0];
                      var argument = Curry._2(Parse[/* pattern_from_expr */17], env$2, match$1[1][/* argument */0]);
                      return /* Spread */Block.__(1, [/* tuple */[
                                  match$1[0],
                                  /* record */[/* argument */argument]
                                ]]);
                    } else {
                      var match$2 = match[0];
                      return /* Element */Block.__(0, [Curry._2(Parse[/* pattern_from_expr */17], env$2, /* tuple */[
                                      match$2[0],
                                      match$2[1]
                                    ])]);
                    }
                  }
                  
                }), param$1[1][/* elements */0]);
          return /* tuple */[
                  param$1[0],
                  /* Array */Block.__(1, [/* record */[
                        /* elements */elements,
                        /* typeAnnotation */undefined
                      ]])
                ];
      case 1 : 
          var env$2 = env;
          var param$2 = /* tuple */[
            loc,
            expr[0]
          ];
          var properties = List.map((function (param) {
                  var env$3 = env$2;
                  var prop = param;
                  if (prop.tag) {
                    var match = prop[0];
                    var argument = Curry._2(Parse[/* pattern_from_expr */17], env$3, match[1][/* argument */0]);
                    return /* SpreadProperty */Block.__(1, [/* tuple */[
                                match[0],
                                /* record */[/* argument */argument]
                              ]]);
                  } else {
                    var match$1 = prop[0];
                    var match$2 = match$1[1];
                    var key = match$2[/* key */0];
                    var key$1;
                    switch (key.tag | 0) {
                      case 0 : 
                          key$1 = /* Literal */Block.__(0, [key[0]]);
                          break;
                      case 1 : 
                          key$1 = /* Identifier */Block.__(1, [key[0]]);
                          break;
                      case 2 : 
                          key$1 = /* Computed */Block.__(2, [key[0]]);
                          break;
                      
                    }
                    var pattern = Curry._2(Parse[/* pattern_from_expr */17], env$3, match$2[/* value */1]);
                    return /* Property */Block.__(0, [/* tuple */[
                                match$1[0],
                                /* record */[
                                  /* key */key$1,
                                  /* pattern */pattern,
                                  /* shorthand */match$2[/* shorthand */4]
                                ]
                              ]]);
                  }
                }), param$2[1][/* properties */0]);
          return /* tuple */[
                  param$2[0],
                  /* Object */Block.__(0, [/* record */[
                        /* properties */properties,
                        /* typeAnnotation */undefined
                      ]])
                ];
      case 7 : 
          var match = expr[0];
          if (match[/* operator */0] !== 0) {
            exit = 1;
          } else {
            return /* tuple */[
                    loc,
                    /* Assignment */Block.__(2, [/* record */[
                          /* left */match[/* left */1],
                          /* right */match[/* right */2]
                        ]])
                  ];
          }
          break;
      case 18 : 
          return /* tuple */[
                  loc,
                  /* Identifier */Block.__(3, [expr[0]])
                ];
      default:
        exit = 1;
    }
  }
  if (exit === 1) {
    return /* tuple */[
            loc,
            /* Expression */Block.__(4, [/* tuple */[
                  loc,
                  expr
                ]])
          ];
  }
  
}

function _object$2(restricted_error) {
  var property = function (env) {
    var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
    if (maybe(env, /* T_ELLIPSIS */11)) {
      var argument = pattern$1(env, restricted_error);
      var loc = btwn(start_loc, argument[0]);
      return /* SpreadProperty */Block.__(1, [/* tuple */[
                  loc,
                  /* record */[/* argument */argument]
                ]]);
    } else {
      var match = Curry._1(Parse[/* object_key */18], env);
      var match$1 = match[1];
      var key;
      switch (match$1.tag | 0) {
        case 0 : 
            key = /* Literal */Block.__(0, [match$1[0]]);
            break;
        case 1 : 
            key = /* Identifier */Block.__(1, [match$1[0]]);
            break;
        case 2 : 
            key = /* Computed */Block.__(2, [match$1[0]]);
            break;
        
      }
      var match$2 = Curry._2(Parser_env_048[/* token */0], undefined, env);
      var prop;
      var exit = 0;
      if (typeof match$2 === "number" && match$2 === 77) {
        token$4(env, /* T_COLON */77);
        prop = /* tuple */[
          pattern$1(env, restricted_error),
          false
        ];
      } else {
        exit = 1;
      }
      if (exit === 1) {
        switch (key.tag | 0) {
          case 1 : 
              var id = key[0];
              var pattern_000 = id[0];
              var pattern_001 = /* Identifier */Block.__(3, [id]);
              var pattern$2 = /* tuple */[
                pattern_000,
                pattern_001
              ];
              prop = /* tuple */[
                pattern$2,
                true
              ];
              break;
          case 0 : 
          case 2 : 
              error_unexpected(env);
              prop = undefined;
              break;
          
        }
      }
      if (prop !== undefined) {
        var match$3 = prop;
        var pattern$3 = match$3[0];
        var match$4 = Curry._2(Parser_env_048[/* token */0], undefined, env);
        var pattern$4;
        if (typeof match$4 === "number" && match$4 === 75) {
          token$4(env, /* T_ASSIGN */75);
          var $$default = Curry._1(Parse[/* assignment */7], env);
          var loc$1 = btwn(pattern$3[0], $$default[0]);
          pattern$4 = /* tuple */[
            loc$1,
            /* Assignment */Block.__(2, [/* record */[
                  /* left */pattern$3,
                  /* right */$$default
                ]])
          ];
        } else {
          pattern$4 = pattern$3;
        }
        var loc$2 = btwn(start_loc, pattern$4[0]);
        return /* Property */Block.__(0, [/* tuple */[
                    loc$2,
                    /* record */[
                      /* key */key,
                      /* pattern */pattern$4,
                      /* shorthand */match$3[1]
                    ]
                  ]]);
      } else {
        return undefined;
      }
    }
  };
  var properties = function (env, _acc) {
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
      var exit = 0;
      if (typeof match === "number" && !(match !== 2 && match !== 105)) {
        return List.rev(acc);
      } else {
        exit = 1;
      }
      if (exit === 1) {
        var match$1 = property(env);
        if (match$1 !== undefined) {
          if (Curry._2(Parser_env_048[/* token */0], undefined, env) !== /* T_RCURLY */2) {
            token$4(env, /* T_COMMA */8);
          }
          _acc = /* :: */[
            match$1,
            acc
          ];
          continue ;
        } else {
          continue ;
        }
      }
      
    };
  };
  return (function (env) {
      var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      token$4(env, /* T_LCURLY */1);
      var properties$1 = properties(env, /* [] */0);
      var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      token$4(env, /* T_RCURLY */2);
      var match;
      if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_COLON */77) {
        var typeAnnotation = wrap(annotation, env);
        match = /* tuple */[
          typeAnnotation[0],
          typeAnnotation
        ];
      } else {
        match = /* tuple */[
          end_loc,
          undefined
        ];
      }
      return /* tuple */[
              btwn(start_loc, match[0]),
              /* Object */Block.__(0, [/* record */[
                    /* properties */properties$1,
                    /* typeAnnotation */match[1]
                  ]])
            ];
    });
}

function _array(restricted_error) {
  var elements = function (env, _acc) {
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
      var exit = 0;
      if (typeof match === "number") {
        if (match !== 105) {
          if (match >= 12) {
            exit = 1;
          } else {
            switch (match) {
              case 6 : 
                  return List.rev(acc);
              case 8 : 
                  token$4(env, /* T_COMMA */8);
                  _acc = /* :: */[
                    undefined,
                    acc
                  ];
                  continue ;
              case 0 : 
              case 1 : 
              case 2 : 
              case 3 : 
              case 4 : 
              case 5 : 
              case 7 : 
              case 9 : 
              case 10 : 
                  exit = 1;
                  break;
              case 11 : 
                  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
                  token$4(env, /* T_ELLIPSIS */11);
                  var argument = pattern$1(env, restricted_error);
                  var loc = btwn(start_loc, argument[0]);
                  var element = /* Spread */Block.__(1, [/* tuple */[
                        loc,
                        /* record */[/* argument */argument]
                      ]]);
                  _acc = /* :: */[
                    element,
                    acc
                  ];
                  continue ;
              
            }
          }
        } else {
          return List.rev(acc);
        }
      } else {
        exit = 1;
      }
      if (exit === 1) {
        var pattern$2 = pattern$1(env, restricted_error);
        var match$1 = Curry._2(Parser_env_048[/* token */0], undefined, env);
        var pattern$3;
        if (typeof match$1 === "number" && match$1 === 75) {
          token$4(env, /* T_ASSIGN */75);
          var $$default = Curry._1(Parse[/* expression */6], env);
          var loc$1 = btwn(pattern$2[0], $$default[0]);
          pattern$3 = /* tuple */[
            loc$1,
            /* Assignment */Block.__(2, [/* record */[
                  /* left */pattern$2,
                  /* right */$$default
                ]])
          ];
        } else {
          pattern$3 = pattern$2;
        }
        var element$1 = /* Element */Block.__(0, [pattern$3]);
        if (Curry._2(Parser_env_048[/* token */0], undefined, env) !== /* T_RBRACKET */6) {
          token$4(env, /* T_COMMA */8);
        }
        _acc = /* :: */[
          element$1,
          acc
        ];
        continue ;
      }
      
    };
  };
  return (function (env) {
      var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      token$4(env, /* T_LBRACKET */5);
      var elements$1 = elements(env, /* [] */0);
      var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      token$4(env, /* T_RBRACKET */6);
      var match;
      if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_COLON */77) {
        var typeAnnotation = wrap(annotation, env);
        match = /* tuple */[
          typeAnnotation[0],
          typeAnnotation
        ];
      } else {
        match = /* tuple */[
          end_loc,
          undefined
        ];
      }
      return /* tuple */[
              btwn(start_loc, match[0]),
              /* Array */Block.__(1, [/* record */[
                    /* elements */elements$1,
                    /* typeAnnotation */match[1]
                  ]])
            ];
    });
}

function pattern$1(env, restricted_error) {
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  if (typeof match === "number") {
    if (match !== 1) {
      if (match !== 5) {
        exit = 1;
      } else {
        return _array(restricted_error)(env);
      }
    } else {
      return _object$2(restricted_error)(env);
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var id = Curry._2(Parse[/* identifier_with_type */12], env, restricted_error);
    return /* tuple */[
            id[0],
            /* Identifier */Block.__(3, [id])
          ];
  }
  
}

function spread_attribute(env) {
  push_lex_mode(env, /* NORMAL */0);
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_LCURLY */1);
  token$4(env, /* T_ELLIPSIS */11);
  var argument = Curry._1(assignment, env);
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_RCURLY */2);
  pop_lex_mode(env);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* argument */argument]
        ];
}

function expression_container(env) {
  push_lex_mode(env, /* NORMAL */0);
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_LCURLY */1);
  var expression;
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_RCURLY */2) {
    var empty_loc = btwn_exclusive(start_loc, Curry._2(Parser_env_048[/* loc */2], undefined, env));
    expression = /* EmptyExpression */Block.__(1, [empty_loc]);
  } else {
    expression = /* Expression */Block.__(0, [Curry._1(Parse[/* expression */6], env)]);
  }
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_RCURLY */2);
  pop_lex_mode(env);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* expression */expression]
        ];
}

function identifier$1(env) {
  var loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var name = Curry._2(Parser_env_048[/* value */1], undefined, env);
  token$4(env, /* T_JSX_IDENTIFIER */106);
  return /* tuple */[
          loc,
          /* record */[/* name */name]
        ];
}

function member_expression(env, _member) {
  while(true) {
    var member = _member;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    if (typeof match === "number" && match === 9) {
      var _object = /* MemberExpression */Block.__(1, [member]);
      token$4(env, /* T_PERIOD */9);
      var property = identifier$1(env);
      var loc = btwn(member[0], property[0]);
      var member_001 = /* record */[
        /* _object */_object,
        /* property */property
      ];
      var member$1 = /* tuple */[
        loc,
        member_001
      ];
      _member = member$1;
      continue ;
    } else {
      return member;
    }
  };
}

function name(env) {
  var name$1 = identifier$1(env);
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  if (typeof match === "number") {
    if (match !== 9) {
      if (match !== 77) {
        return /* Identifier */Block.__(0, [name$1]);
      } else {
        token$4(env, /* T_COLON */77);
        var name$2 = identifier$1(env);
        var loc = btwn(name$1[0], name$2[0]);
        return /* NamespacedName */Block.__(1, [/* tuple */[
                    loc,
                    /* record */[
                      /* namespace */name$1,
                      /* name */name$2
                    ]
                  ]]);
      }
    } else {
      var _object = /* Identifier */Block.__(0, [name$1]);
      token$4(env, /* T_PERIOD */9);
      var property = identifier$1(env);
      var loc$1 = btwn(name$1[0], property[0]);
      var member_001 = /* record */[
        /* _object */_object,
        /* property */property
      ];
      var member = /* tuple */[
        loc$1,
        member_001
      ];
      return /* MemberExpression */Block.__(2, [member_expression(env, member)]);
    }
  } else {
    return /* Identifier */Block.__(0, [name$1]);
  }
}

function attribute(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var name = identifier$1(env);
  var match;
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_COLON */77) {
    token$4(env, /* T_COLON */77);
    var name$1 = identifier$1(env);
    var loc = btwn(name[0], name$1[0]);
    match = /* tuple */[
      loc,
      /* NamespacedName */Block.__(1, [/* tuple */[
            loc,
            /* record */[
              /* namespace */name,
              /* name */name$1
            ]
          ]])
    ];
  } else {
    match = /* tuple */[
      name[0],
      /* Identifier */Block.__(0, [name])
    ];
  }
  var match$1;
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_ASSIGN */75) {
    token$4(env, /* T_ASSIGN */75);
    var token$5 = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof token$5 === "number") {
      if (token$5 === 1) {
        var match$2 = expression_container(env);
        var expression_container$1 = match$2[1];
        var loc$1 = match$2[0];
        var match$3 = expression_container$1[/* expression */0];
        if (match$3.tag) {
          error$1(env, /* JSXAttributeValueEmptyExpression */40);
        }
        match$1 = /* tuple */[
          loc$1,
          /* ExpressionContainer */Block.__(1, [
              loc$1,
              expression_container$1
            ])
        ];
      } else {
        exit = 1;
      }
    } else if (token$5.tag === 4) {
      var match$4 = token$5[0];
      var loc$2 = match$4[0];
      token$4(env, token$5);
      var value = /* String */Block.__(0, [match$4[1]]);
      match$1 = /* tuple */[
        loc$2,
        /* Literal */Block.__(0, [
            loc$2,
            /* record */[
              /* value */value,
              /* raw */match$4[2]
            ]
          ])
      ];
    } else {
      exit = 1;
    }
    if (exit === 1) {
      error$1(env, /* InvalidJSXAttributeValue */41);
      var loc$3 = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      match$1 = /* tuple */[
        loc$3,
        /* Literal */Block.__(0, [
            loc$3,
            /* record */[
              /* value : String */Block.__(0, [""]),
              /* raw */""
            ]
          ])
      ];
    }
    
  } else {
    match$1 = /* tuple */[
      match[0],
      undefined
    ];
  }
  return /* tuple */[
          btwn(start_loc, match$1[0]),
          /* record */[
            /* name */match[1],
            /* value */match$1[1]
          ]
        ];
}

function attributes(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof match === "number") {
      if (match >= 91) {
        if (match !== 96 && match !== 105) {
          exit = 1;
        } else {
          return List.rev(acc);
        }
      } else if (match !== 1) {
        if (match >= 90) {
          return List.rev(acc);
        } else {
          exit = 1;
        }
      } else {
        var attribute$1 = /* SpreadAttribute */Block.__(1, [spread_attribute(env)]);
        _acc = /* :: */[
          attribute$1,
          acc
        ];
        continue ;
      }
    } else {
      exit = 1;
    }
    if (exit === 1) {
      var attribute$2 = /* Attribute */Block.__(0, [attribute(env)]);
      _acc = /* :: */[
        attribute$2,
        acc
      ];
      continue ;
    }
    
  };
}

function opening_element_without_lt(env, start_loc) {
  var name$1 = name(env);
  var attributes$1 = attributes(env, /* [] */0);
  var selfClosing = Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_DIV */96;
  if (selfClosing) {
    token$4(env, /* T_DIV */96);
  }
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_GREATER_THAN */90);
  pop_lex_mode(env);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[
            /* name */name$1,
            /* selfClosing */selfClosing,
            /* attributes */attributes$1
          ]
        ];
}

function closing_element_without_lt(env, start_loc) {
  token$4(env, /* T_DIV */96);
  var name$1 = name(env);
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_GREATER_THAN */90);
  double_pop_lex_mode(env);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* name */name$1]
        ];
}

function child(env) {
  var token$5 = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  if (typeof token$5 === "number") {
    if (token$5 === 1) {
      var expression_container$1 = expression_container(env);
      return /* tuple */[
              expression_container$1[0],
              /* ExpressionContainer */Block.__(1, [expression_container$1[1]])
            ];
    } else {
      exit = 1;
    }
  } else if (token$5.tag === 4) {
    var match = token$5[0];
    token$4(env, token$5);
    return /* tuple */[
            match[0],
            /* Text */Block.__(2, [/* record */[
                  /* value */match[1],
                  /* raw */match[2]
                ]])
          ];
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var element$1 = element(env);
    return /* tuple */[
            element$1[0],
            /* Element */Block.__(0, [element$1[1]])
          ];
  }
  
}

function element(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  push_lex_mode(env, /* JSX_TAG */2);
  token$4(env, /* T_LESS_THAN */89);
  return Curry._2(element_without_lt, env, start_loc);
}

function element_or_closing(env) {
  push_lex_mode(env, /* JSX_TAG */2);
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_LESS_THAN */89);
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  if (typeof match === "number" && !(match !== 96 && match !== 105)) {
    return /* Closing */Block.__(0, [closing_element_without_lt(env, start_loc)]);
  } else {
    return /* ChildElement */Block.__(1, [Curry._2(element_without_lt, env, start_loc)]);
  }
}

function children_and_closing(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    if (typeof match === "number") {
      if (match !== 89) {
        if (match !== 105) {
          _acc = /* :: */[
            child(env),
            acc
          ];
          continue ;
        } else {
          error_unexpected(env);
          return /* tuple */[
                  List.rev(acc),
                  undefined
                ];
        }
      } else {
        var match$1 = element_or_closing(env);
        if (match$1.tag) {
          var element = match$1[0];
          var element_000 = element[0];
          var element_001 = /* Element */Block.__(0, [element[1]]);
          var element$1 = /* tuple */[
            element_000,
            element_001
          ];
          _acc = /* :: */[
            element$1,
            acc
          ];
          continue ;
        } else {
          return /* tuple */[
                  List.rev(acc),
                  match$1[0]
                ];
        }
      }
    } else {
      _acc = /* :: */[
        child(env),
        acc
      ];
      continue ;
    }
  };
}

function normalize(name) {
  switch (name.tag | 0) {
    case 0 : 
        return name[0][1][/* name */0];
    case 1 : 
        var match = name[0][1];
        return match[/* namespace */0][1][/* name */0] + (":" + match[/* name */1][1][/* name */0]);
    case 2 : 
        var match$1 = name[0][1];
        var _object = match$1[/* _object */0];
        var _object$1;
        _object$1 = _object.tag ? normalize(/* MemberExpression */Block.__(2, [_object[0]])) : _object[0][1][/* name */0];
        return _object$1 + ("." + match$1[/* property */1][1][/* name */0]);
    
  }
}

function element_without_lt(env, start_loc) {
  var openingElement = opening_element_without_lt(env, start_loc);
  var match = openingElement[1][/* selfClosing */1] ? /* tuple */[
      /* [] */0,
      undefined
    ] : (push_lex_mode(env, /* JSX_CHILD */3), children_and_closing(env, /* [] */0));
  var closingElement = match[1];
  var end_loc;
  if (closingElement !== undefined) {
    var match$1 = closingElement;
    var opening_name = normalize(openingElement[1][/* name */0]);
    if (normalize(match$1[1][/* name */0]) !== opening_name) {
      error$1(env, /* ExpectedJSXClosingTag */Block.__(6, [opening_name]));
    }
    end_loc = match$1[0];
  } else {
    end_loc = openingElement[0];
  }
  return /* tuple */[
          btwn(openingElement[0], end_loc),
          /* record */[
            /* openingElement */openingElement,
            /* closingElement */closingElement,
            /* children */match[0]
          ]
        ];
}

function statement(env) {
  while(true) {
    var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    var exit$1 = 0;
    if (typeof match === "number") {
      if (match !== 105) {
        if (match >= 58) {
          exit$1 = 2;
        } else {
          switch (match) {
            case 1 : 
                var env$1 = env;
                var match$1 = Curry._1(Parse[/* block_body */13], env$1);
                return /* tuple */[
                        match$1[0],
                        /* Block */Block.__(0, [match$1[1]])
                      ];
            case 7 : 
                var env$2 = env;
                var loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$2);
                token$4(env$2, /* T_SEMICOLON */7);
                return /* tuple */[
                        loc,
                        /* Empty */0
                      ];
            case 14 : 
                return _if(env);
            case 17 : 
                var env$3 = env;
                if (!env$3[/* in_function */9]) {
                  error$1(env$3, /* IllegalReturn */23);
                }
                var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$3);
                token$4(env$3, /* T_RETURN */17);
                var argument = Curry._2(Parser_env_048[/* token */0], undefined, env$3) === /* T_SEMICOLON */7 || Curry._1(Parser_env_048[/* is_implicit_semicolon */6], env$3) ? undefined : Curry._1(Parse[/* expression */6], env$3);
                var match$2 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$3);
                var end_loc = match$2 !== undefined ? match$2 : (
                    argument !== undefined ? argument[0] : start_loc
                  );
                semicolon(env$3);
                return /* tuple */[
                        btwn(start_loc, end_loc),
                        /* Return */Block.__(9, [/* record */[/* argument */argument]])
                      ];
            case 18 : 
                var env$4 = env;
                var start_loc$1 = Curry._2(Parser_env_048[/* loc */2], undefined, env$4);
                token$4(env$4, /* T_SWITCH */18);
                token$4(env$4, /* T_LPAREN */3);
                var discriminant = Curry._1(Parse[/* expression */6], env$4);
                token$4(env$4, /* T_RPAREN */4);
                token$4(env$4, /* T_LCURLY */1);
                var cases = case_list(env$4, /* tuple */[
                      false,
                      /* [] */0
                    ]);
                var end_loc$1 = Curry._2(Parser_env_048[/* loc */2], undefined, env$4);
                token$4(env$4, /* T_RCURLY */2);
                return /* tuple */[
                        btwn(start_loc$1, end_loc$1),
                        /* Switch */Block.__(8, [/* record */[
                              /* discriminant */discriminant,
                              /* cases */cases,
                              /* lexical */false
                            ]])
                      ];
            case 20 : 
                var env$5 = env;
                var start_loc$2 = Curry._2(Parser_env_048[/* loc */2], undefined, env$5);
                token$4(env$5, /* T_THROW */20);
                if (Curry._1(Parser_env_048[/* is_line_terminator */5], env$5)) {
                  error_at(env$5, /* tuple */[
                        start_loc$2,
                        /* NewlineAfterThrow */11
                      ]);
                }
                var argument$1 = Curry._1(Parse[/* expression */6], env$5);
                var match$3 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$5);
                var end_loc$2 = match$3 !== undefined ? match$3 : argument$1[0];
                semicolon(env$5);
                return /* tuple */[
                        btwn(start_loc$2, end_loc$2),
                        /* Throw */Block.__(10, [/* record */[/* argument */argument$1]])
                      ];
            case 21 : 
                var env$6 = env;
                var start_loc$3 = Curry._2(Parser_env_048[/* loc */2], undefined, env$6);
                token$4(env$6, /* T_TRY */21);
                var block = Curry._1(Parse[/* block_body */13], env$6);
                var match$4 = Curry._2(Parser_env_048[/* token */0], undefined, env$6);
                var handler;
                if (typeof match$4 === "number" && match$4 === 32) {
                  var start_loc$4 = Curry._2(Parser_env_048[/* loc */2], undefined, env$6);
                  token$4(env$6, /* T_CATCH */32);
                  token$4(env$6, /* T_LPAREN */3);
                  var id = Curry._2(Parse[/* identifier */10], /* StrictCatchVariable */26, env$6);
                  var param_000 = id[0];
                  var param_001 = /* Identifier */Block.__(3, [id]);
                  var param = /* tuple */[
                    param_000,
                    param_001
                  ];
                  token$4(env$6, /* T_RPAREN */4);
                  var body = Curry._1(Parse[/* block_body */13], env$6);
                  var loc$1 = btwn(start_loc$4, body[0]);
                  handler = /* tuple */[
                    loc$1,
                    /* record */[
                      /* param */param,
                      /* guard */undefined,
                      /* body */body
                    ]
                  ];
                } else {
                  handler = undefined;
                }
                var match$5 = Curry._2(Parser_env_048[/* token */0], undefined, env$6);
                var finalizer = typeof match$5 === "number" && match$5 === 36 ? (token$4(env$6, /* T_FINALLY */36), Curry._1(Parse[/* block_body */13], env$6)) : undefined;
                var end_loc$3 = finalizer !== undefined ? finalizer[0] : (
                    handler !== undefined ? handler[0] : (error_at(env$6, /* tuple */[
                              block[0],
                              /* NoCatchOrFinally */20
                            ]), block[0])
                  );
                return /* tuple */[
                        btwn(start_loc$3, end_loc$3),
                        /* Try */Block.__(11, [/* record */[
                              /* block */block,
                              /* handler */handler,
                              /* guardedHandlers : [] */0,
                              /* finalizer */finalizer
                            ]])
                      ];
            case 22 : 
                return var_or_const(env);
            case 23 : 
                var env$7 = env;
                var start_loc$5 = Curry._2(Parser_env_048[/* loc */2], undefined, env$7);
                token$4(env$7, /* T_WHILE */23);
                token$4(env$7, /* T_LPAREN */3);
                var test = Curry._1(Parse[/* expression */6], env$7);
                token$4(env$7, /* T_RPAREN */4);
                var body$1 = Curry._1(Parse[/* statement */1], with_in_loop(true, env$7));
                return /* tuple */[
                        btwn(start_loc$5, body$1[0]),
                        /* While */Block.__(12, [/* record */[
                              /* test */test,
                              /* body */body$1
                            ]])
                      ];
            case 24 : 
                var env$8 = env;
                var start_loc$6 = Curry._2(Parser_env_048[/* loc */2], undefined, env$8);
                token$4(env$8, /* T_WITH */24);
                token$4(env$8, /* T_LPAREN */3);
                var _object = Curry._1(Parse[/* expression */6], env$8);
                token$4(env$8, /* T_RPAREN */4);
                var body$2 = Curry._1(Parse[/* statement */1], env$8);
                var loc$2 = btwn(start_loc$6, body$2[0]);
                strict_error_at(env$8, /* tuple */[
                      loc$2,
                      /* StrictModeWith */25
                    ]);
                return /* tuple */[
                        loc$2,
                        /* With */Block.__(6, [/* record */[
                              /* _object */_object,
                              /* body */body$2
                            ]])
                      ];
            case 30 : 
                var env$9 = env;
                var start_loc$7 = Curry._2(Parser_env_048[/* loc */2], undefined, env$9);
                token$4(env$9, /* T_BREAK */30);
                var label;
                if (Curry._2(Parser_env_048[/* token */0], undefined, env$9) === /* T_SEMICOLON */7 || Curry._1(Parser_env_048[/* is_implicit_semicolon */6], env$9)) {
                  label = undefined;
                } else {
                  var label$1 = Curry._2(Parse[/* identifier */10], undefined, env$9);
                  var name = label$1[1][/* name */0];
                  if (!mem$1(name, env$9[/* labels */2])) {
                    error$1(env$9, /* UnknownLabel */Block.__(4, [name]));
                  }
                  label = label$1;
                }
                var match$6 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$9);
                var end_loc$4 = match$6 !== undefined ? match$6 : (
                    label !== undefined ? label[0] : start_loc$7
                  );
                var loc$3 = btwn(start_loc$7, end_loc$4);
                if (label === undefined && !(env$9[/* in_loop */7] || env$9[/* in_switch */8])) {
                  error_at(env$9, /* tuple */[
                        loc$3,
                        /* IllegalBreak */22
                      ]);
                }
                semicolon(env$9);
                return /* tuple */[
                        loc$3,
                        /* Break */Block.__(4, [/* record */[/* label */label]])
                      ];
            case 33 : 
                var env$10 = env;
                var start_loc$8 = Curry._2(Parser_env_048[/* loc */2], undefined, env$10);
                token$4(env$10, /* T_CONTINUE */33);
                var label$2;
                if (Curry._2(Parser_env_048[/* token */0], undefined, env$10) === /* T_SEMICOLON */7 || Curry._1(Parser_env_048[/* is_implicit_semicolon */6], env$10)) {
                  label$2 = undefined;
                } else {
                  var label$3 = Curry._2(Parse[/* identifier */10], undefined, env$10);
                  var name$1 = label$3[1][/* name */0];
                  if (!mem$1(name$1, env$10[/* labels */2])) {
                    error$1(env$10, /* UnknownLabel */Block.__(4, [name$1]));
                  }
                  label$2 = label$3;
                }
                var match$7 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$10);
                var end_loc$5 = match$7 !== undefined ? match$7 : (
                    label$2 !== undefined ? label$2[0] : start_loc$8
                  );
                var loc$4 = btwn(start_loc$8, end_loc$5);
                if (!env$10[/* in_loop */7]) {
                  error_at(env$10, /* tuple */[
                        loc$4,
                        /* IllegalContinue */21
                      ]);
                }
                semicolon(env$10);
                return /* tuple */[
                        loc$4,
                        /* Continue */Block.__(5, [/* record */[/* label */label$2]])
                      ];
            case 35 : 
                var env$11 = env;
                var start_loc$9 = Curry._2(Parser_env_048[/* loc */2], undefined, env$11);
                token$4(env$11, /* T_DO */35);
                var body$3 = Curry._1(Parse[/* statement */1], with_in_loop(true, env$11));
                token$4(env$11, /* T_WHILE */23);
                token$4(env$11, /* T_LPAREN */3);
                var test$1 = Curry._1(Parse[/* expression */6], env$11);
                var end_loc$6 = Curry._2(Parser_env_048[/* loc */2], undefined, env$11);
                token$4(env$11, /* T_RPAREN */4);
                var match$8 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$11);
                var end_loc$7 = match$8 !== undefined ? match$8 : end_loc$6;
                if (Curry._2(Parser_env_048[/* token */0], undefined, env$11) === /* T_SEMICOLON */7) {
                  semicolon(env$11);
                }
                return /* tuple */[
                        btwn(start_loc$9, end_loc$7),
                        /* DoWhile */Block.__(13, [/* record */[
                              /* body */body$3,
                              /* test */test$1
                            ]])
                      ];
            case 37 : 
                var env$12 = env;
                var start_loc$10 = Curry._2(Parser_env_048[/* loc */2], undefined, env$12);
                token$4(env$12, /* T_FOR */37);
                token$4(env$12, /* T_LPAREN */3);
                var match$9 = Curry._2(Parser_env_048[/* token */0], undefined, env$12);
                var match$10;
                var exit$2 = 0;
                if (typeof match$9 === "number") {
                  if (match$9 >= 22) {
                    if (match$9 >= 27) {
                      exit$2 = 1;
                    } else {
                      switch (match$9 - 22 | 0) {
                        case 0 : 
                            var match$11 = declarations(/* T_VAR */22, /* Var */0, with_no_in(true, env$12));
                            match$10 = /* tuple */[
                              /* InitDeclaration */Block.__(0, [match$11[0]]),
                              match$11[1]
                            ];
                            break;
                        case 1 : 
                        case 2 : 
                            exit$2 = 1;
                            break;
                        case 3 : 
                            var match$12 = $$const(with_no_in(true, env$12));
                            match$10 = /* tuple */[
                              /* InitDeclaration */Block.__(0, [match$12[0]]),
                              match$12[1]
                            ];
                            break;
                        case 4 : 
                            var match$13 = _let(with_no_in(true, env$12));
                            match$10 = /* tuple */[
                              /* InitDeclaration */Block.__(0, [match$13[0]]),
                              match$13[1]
                            ];
                            break;
                        
                      }
                    }
                  } else if (match$9 !== 7) {
                    exit$2 = 1;
                  } else {
                    match$10 = /* tuple */[
                      undefined,
                      /* [] */0
                    ];
                  }
                } else {
                  exit$2 = 1;
                }
                if (exit$2 === 1) {
                  var expr = Curry._1(Parse[/* expression */6], with_no_let(true, with_no_in(true, env$12)));
                  match$10 = /* tuple */[
                    /* InitExpression */Block.__(1, [expr]),
                    /* [] */0
                  ];
                }
                var init = match$10[0];
                var match$14 = Curry._2(Parser_env_048[/* token */0], undefined, env$12);
                var exit$3 = 0;
                if (typeof match$14 === "number") {
                  if (match$14 !== 15) {
                    if (match$14 !== 60) {
                      exit$3 = 1;
                    } else {
                      assert_can_be_forin_or_forof(env$12, /* InvalidLHSInForOf */17, init);
                      var left;
                      if (init !== undefined) {
                        var match$15 = init;
                        left = match$15.tag ? /* LeftExpression */Block.__(1, [match$15[0]]) : /* LeftDeclaration */Block.__(0, [match$15[0]]);
                      } else {
                        throw [
                              Caml_builtin_exceptions.assert_failure,
                              /* tuple */[
                                "parser_flow.ml",
                                2573,
                                22
                              ]
                            ];
                      }
                      token$4(env$12, /* T_OF */60);
                      var right = Curry._1(Parse[/* assignment */7], env$12);
                      token$4(env$12, /* T_RPAREN */4);
                      var body$4 = Curry._1(Parse[/* statement */1], with_in_loop(true, env$12));
                      return /* tuple */[
                              btwn(start_loc$10, body$4[0]),
                              /* ForOf */Block.__(16, [/* record */[
                                    /* left */left,
                                    /* right */right,
                                    /* body */body$4
                                  ]])
                            ];
                    }
                  } else {
                    assert_can_be_forin_or_forof(env$12, /* InvalidLHSInForIn */16, init);
                    var left$1;
                    if (init !== undefined) {
                      var match$16 = init;
                      left$1 = match$16.tag ? /* LeftExpression */Block.__(1, [match$16[0]]) : /* LeftDeclaration */Block.__(0, [match$16[0]]);
                    } else {
                      throw [
                            Caml_builtin_exceptions.assert_failure,
                            /* tuple */[
                              "parser_flow.ml",
                              2556,
                              22
                            ]
                          ];
                    }
                    token$4(env$12, /* T_IN */15);
                    var right$1 = Curry._1(Parse[/* expression */6], env$12);
                    token$4(env$12, /* T_RPAREN */4);
                    var body$5 = Curry._1(Parse[/* statement */1], with_in_loop(true, env$12));
                    return /* tuple */[
                            btwn(start_loc$10, body$5[0]),
                            /* ForIn */Block.__(15, [/* record */[
                                  /* left */left$1,
                                  /* right */right$1,
                                  /* body */body$5,
                                  /* each */false
                                ]])
                          ];
                  }
                } else {
                  exit$3 = 1;
                }
                if (exit$3 === 1) {
                  List.iter((function(env$12){
                      return function (param) {
                        return error_at(env$12, param);
                      }
                      }(env$12)), match$10[1]);
                  token$4(env$12, /* T_SEMICOLON */7);
                  var match$17 = Curry._2(Parser_env_048[/* token */0], undefined, env$12);
                  var test$2 = typeof match$17 === "number" && match$17 === 7 ? undefined : Curry._1(Parse[/* expression */6], env$12);
                  token$4(env$12, /* T_SEMICOLON */7);
                  var match$18 = Curry._2(Parser_env_048[/* token */0], undefined, env$12);
                  var update = typeof match$18 === "number" && match$18 === 4 ? undefined : Curry._1(Parse[/* expression */6], env$12);
                  token$4(env$12, /* T_RPAREN */4);
                  var body$6 = Curry._1(Parse[/* statement */1], with_in_loop(true, env$12));
                  return /* tuple */[
                          btwn(start_loc$10, body$6[0]),
                          /* For */Block.__(14, [/* record */[
                                /* init */init,
                                /* test */test$2,
                                /* update */update,
                                /* body */body$6
                              ]])
                        ];
                }
                case 0 : 
            case 2 : 
            case 3 : 
            case 4 : 
            case 5 : 
            case 6 : 
            case 8 : 
            case 9 : 
            case 10 : 
            case 11 : 
            case 12 : 
            case 13 : 
            case 15 : 
            case 16 : 
            case 19 : 
            case 25 : 
            case 26 : 
            case 27 : 
            case 28 : 
            case 29 : 
            case 31 : 
            case 32 : 
            case 34 : 
            case 36 : 
            case 38 : 
            case 39 : 
            case 40 : 
            case 41 : 
            case 42 : 
            case 43 : 
            case 44 : 
            case 45 : 
            case 46 : 
            case 47 : 
            case 48 : 
            case 49 : 
            case 50 : 
            case 51 : 
            case 52 : 
            case 53 : 
            case 54 : 
            case 55 : 
            case 56 : 
                exit$1 = 2;
                break;
            case 57 : 
                var env$13 = env;
                var start_loc$11 = Curry._2(Parser_env_048[/* loc */2], undefined, env$13);
                token$4(env$13, /* T_DEBUGGER */57);
                var match$19 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$13);
                var end_loc$8 = match$19 !== undefined ? match$19 : start_loc$11;
                semicolon(env$13);
                return /* tuple */[
                        btwn(start_loc$11, end_loc$8),
                        /* Debugger */1
                      ];
            
          }
        }
      } else {
        error_unexpected(env);
        return /* tuple */[
                Curry._2(Parser_env_048[/* loc */2], undefined, env),
                /* Empty */0
              ];
      }
    } else {
      exit$1 = 2;
    }
    if (exit$1 === 2) {
      if (Curry._2(Parser_env_048[/* is_identifier */8], undefined, env)) {
        var env$14 = env;
        var expr$1 = Curry._1(Parse[/* expression */6], env$14);
        var match$20 = Curry._2(Parser_env_048[/* token */0], undefined, env$14);
        var match$21 = expr$1[1];
        var loc$5 = expr$1[0];
        var exit$4 = 0;
        if (typeof match$21 === "number" || !(match$21.tag === 18 && typeof match$20 === "number" && match$20 === 77)) {
          exit$4 = 1;
        } else {
          var label$4 = match$21[0];
          var match$22 = label$4[1];
          var name$2 = match$22[/* name */0];
          token$4(env$14, /* T_COLON */77);
          if (mem$1(name$2, env$14[/* labels */2])) {
            error_at(env$14, /* tuple */[
                  loc$5,
                  /* Redeclaration */Block.__(5, [
                      "Label",
                      name$2
                    ])
                ]);
          }
          var env$15 = add_label(env$14, name$2);
          var labeled_stmt = Curry._1(Parse[/* statement */1], env$15);
          return /* tuple */[
                  btwn(loc$5, labeled_stmt[0]),
                  /* Labeled */Block.__(3, [/* record */[
                        /* label */label$4,
                        /* body */labeled_stmt
                      ]])
                ];
        }
        if (exit$4 === 1) {
          var match$23 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$14);
          var end_loc$9 = match$23 !== undefined ? match$23 : expr$1[0];
          semicolon(env$14);
          return /* tuple */[
                  btwn(expr$1[0], end_loc$9),
                  /* Expression */Block.__(1, [/* record */[/* expression */expr$1]])
                ];
        }
        
      } else if (typeof match === "number") {
        if (match !== 77) {
          if (match >= 49) {
            return expression(env);
          } else {
            switch (match) {
              case 41 : 
                  return _if(env);
              case 0 : 
              case 1 : 
              case 3 : 
              case 5 : 
              case 7 : 
              case 12 : 
              case 13 : 
              case 14 : 
              case 17 : 
              case 18 : 
              case 19 : 
              case 20 : 
              case 21 : 
              case 22 : 
              case 23 : 
              case 24 : 
              case 25 : 
              case 26 : 
              case 27 : 
              case 28 : 
              case 29 : 
              case 30 : 
              case 33 : 
              case 35 : 
              case 37 : 
              case 38 : 
              case 42 : 
              case 43 : 
              case 44 : 
              case 45 : 
              case 46 : 
                  return expression(env);
              case 2 : 
              case 4 : 
              case 6 : 
              case 8 : 
              case 9 : 
              case 10 : 
              case 11 : 
              case 15 : 
              case 16 : 
              case 31 : 
              case 32 : 
              case 34 : 
              case 36 : 
              case 39 : 
              case 40 : 
              case 47 : 
              case 48 : 
                  exit = 1;
                  break;
              
            }
          }
        } else {
          exit = 1;
        }
      } else {
        return expression(env);
      }
    }
    if (exit === 1) {
      error_unexpected(env);
      token$3(env);
      continue ;
    }
    
  };
}

function statement_list_item($staropt$star, env) {
  var decorators = $staropt$star !== undefined ? $staropt$star : /* [] */0;
  if (!Curry._2(Parser_env_048[/* is_class */10], undefined, env)) {
    error_on_decorators(env)(decorators);
  }
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  if (typeof match === "number") {
    if (match !== 25) {
      if (match !== 26) {
        exit = 1;
      } else {
        var env$1 = env;
        var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$1);
        token$4(env$1, /* T_LET */26);
        if (Curry._2(Parser_env_048[/* token */0], undefined, env$1) === /* T_LPAREN */3) {
          token$4(env$1, /* T_LPAREN */3);
          var match$1 = helper(with_no_let(true, env$1), /* [] */0, /* [] */0);
          var head = List.map((function (param) {
                  var match = param[1];
                  return /* record */[
                          /* id */match[/* id */0],
                          /* init */match[/* init */1]
                        ];
                }), match$1[1]);
          token$4(env$1, /* T_RPAREN */4);
          var body = Curry._1(Parse[/* statement */1], env$1);
          var match$2 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$1);
          var end_loc = match$2 !== undefined ? match$2 : match$1[0];
          semicolon(env$1);
          List.iter((function (param) {
                  return error_at(env$1, param);
                }), match$1[2]);
          return /* tuple */[
                  btwn(start_loc, end_loc),
                  /* Let */Block.__(17, [/* record */[
                        /* head */head,
                        /* body */body
                      ]])
                ];
        } else {
          var match$3 = helper(with_no_let(true, env$1), /* [] */0, /* [] */0);
          var declaration = /* VariableDeclaration */Block.__(19, [/* record */[
                /* declarations */match$3[1],
                /* kind : Let */1
              ]]);
          var match$4 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$1);
          var end_loc$1 = match$4 !== undefined ? match$4 : match$3[0];
          semicolon(env$1);
          List.iter((function (param) {
                  return error_at(env$1, param);
                }), match$3[2]);
          return /* tuple */[
                  btwn(start_loc, end_loc$1),
                  declaration
                ];
        }
      }
    } else {
      return var_or_const(env);
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    if (Curry._2(Parser_env_048[/* is_function */9], undefined, env)) {
      return _function(env);
    } else if (Curry._2(Parser_env_048[/* is_class */10], undefined, env)) {
      return class_declaration$1(env, decorators);
    } else if (typeof match === "number") {
      switch (match) {
        case 51 : 
            return $$interface(env);
        case 52 : 
        case 53 : 
        case 54 : 
        case 55 : 
        case 56 : 
        case 57 : 
            return statement(env);
        case 58 : 
            return declare(undefined, env);
        case 59 : 
            return type_alias(env);
        default:
          return statement(env);
      }
    } else {
      return statement(env);
    }
  }
  
}

function module_item(env) {
  var decorators = decorator_list(env);
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  if (typeof match === "number") {
    switch (match) {
      case 47 : 
          var env$1 = env;
          var decorators$1 = decorators;
          var env$2 = with_in_export(true, with_strict(true, env$1));
          var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$2);
          token$4(env$2, /* T_EXPORT */47);
          var match$1 = Curry._2(Parser_env_048[/* token */0], undefined, env$2);
          var exit = 0;
          if (typeof match$1 === "number") {
            if (match$1 >= 51) {
              if (match$1 !== 97) {
                if (match$1 >= 62) {
                  exit = 1;
                } else {
                  switch (match$1 - 51 | 0) {
                    case 0 : 
                        if (!env$2[/* parse_options */20][/* types */4]) {
                          error$1(env$2, /* UnexpectedTypeExport */9);
                        }
                        var $$interface$1 = $$interface(env$2);
                        var match$2 = $$interface$1[1];
                        if (typeof match$2 === "number") {
                          throw [
                                Caml_builtin_exceptions.failure,
                                "Internal Flow Error! Parsed `export interface` into something other than an interface declaration!"
                              ];
                        } else if (match$2.tag === 21) {
                          record_export(env$2, /* tuple */[
                                $$interface$1[0],
                                extract_ident_name(match$2[0][/* id */0])
                              ]);
                        } else {
                          throw [
                                Caml_builtin_exceptions.failure,
                                "Internal Flow Error! Parsed `export interface` into something other than an interface declaration!"
                              ];
                        }
                        var end_loc = $$interface$1[0];
                        return /* tuple */[
                                btwn(start_loc, end_loc),
                                /* ExportDeclaration */Block.__(28, [/* record */[
                                      /* default */false,
                                      /* declaration *//* Declaration */Block.__(0, [$$interface$1]),
                                      /* specifiers */undefined,
                                      /* source */undefined,
                                      /* exportKind : ExportType */0
                                    ]])
                              ];
                    case 8 : 
                        if (Curry._2(Parser_env_048[/* token */0], 1, env$2) !== /* T_LCURLY */1) {
                          if (!env$2[/* parse_options */20][/* types */4]) {
                            error$1(env$2, /* UnexpectedTypeExport */9);
                          }
                          var type_alias$1 = type_alias(env$2);
                          var match$3 = type_alias$1[1];
                          if (typeof match$3 === "number") {
                            throw [
                                  Caml_builtin_exceptions.failure,
                                  "Internal Flow Error! Parsed `export type` into something other than a type alias!"
                                ];
                          } else if (match$3.tag === 7) {
                            record_export(env$2, /* tuple */[
                                  type_alias$1[0],
                                  extract_ident_name(match$3[0][/* id */0])
                                ]);
                          } else {
                            throw [
                                  Caml_builtin_exceptions.failure,
                                  "Internal Flow Error! Parsed `export type` into something other than a type alias!"
                                ];
                          }
                          var end_loc$1 = type_alias$1[0];
                          return /* tuple */[
                                  btwn(start_loc, end_loc$1),
                                  /* ExportDeclaration */Block.__(28, [/* record */[
                                        /* default */false,
                                        /* declaration *//* Declaration */Block.__(0, [type_alias$1]),
                                        /* specifiers */undefined,
                                        /* source */undefined,
                                        /* exportKind : ExportType */0
                                      ]])
                                ];
                        } else {
                          exit = 1;
                        }
                        break;
                    case 1 : 
                    case 2 : 
                    case 3 : 
                    case 4 : 
                    case 5 : 
                    case 6 : 
                    case 7 : 
                    case 9 : 
                        exit = 1;
                        break;
                    case 10 : 
                        exit = 2;
                        break;
                    
                  }
                }
              } else {
                var loc = Curry._2(Parser_env_048[/* loc */2], undefined, env$2);
                token$4(env$2, /* T_MULT */97);
                var parse_export_star_as = env$2[/* parse_options */20][/* esproposal_export_star_as */3];
                var local_name = Curry._2(Parser_env_048[/* value */1], undefined, env$2) === "as" ? (contextual(env$2, "as"), parse_export_star_as ? Curry._2(Parse[/* identifier */10], undefined, env$2) : (error$1(env$2, /* UnexpectedTypeDeclaration */7), undefined)) : undefined;
                var specifiers = /* ExportBatchSpecifier */Block.__(1, [
                    loc,
                    local_name
                  ]);
                var source$1 = export_source(env$2);
                var match$4 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$2);
                var end_loc$2 = match$4 !== undefined ? match$4 : source$1[0];
                var source$2 = source$1;
                semicolon(env$2);
                return /* tuple */[
                        btwn(start_loc, end_loc$2),
                        /* ExportDeclaration */Block.__(28, [/* record */[
                              /* default */false,
                              /* declaration */undefined,
                              /* specifiers */specifiers,
                              /* source */source$2,
                              /* exportKind : ExportValue */1
                            ]])
                      ];
              }
            } else {
              switch (match$1) {
                case 34 : 
                    token$4(env$2, /* T_DEFAULT */34);
                    record_export(env$2, /* tuple */[
                          btwn(start_loc, Curry._2(Parser_env_048[/* loc */2], undefined, env$2)),
                          "default"
                        ]);
                    var match$5 = Curry._2(Parser_env_048[/* token */0], undefined, env$2);
                    var match$6;
                    var exit$1 = 0;
                    if (typeof match$5 === "number" && match$5 === 13) {
                      var fn = _function(env$2);
                      match$6 = /* tuple */[
                        fn[0],
                        /* Declaration */Block.__(0, [fn])
                      ];
                    } else {
                      exit$1 = 3;
                    }
                    if (exit$1 === 3) {
                      if (Curry._2(Parser_env_048[/* is_class */10], undefined, env$2)) {
                        var _class = class_declaration(env$2, decorators$1);
                        match$6 = /* tuple */[
                          _class[0],
                          /* Declaration */Block.__(0, [_class])
                        ];
                      } else {
                        var expr = Curry._1(Parse[/* assignment */7], env$2);
                        var match$7 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$2);
                        var end_loc$3 = match$7 !== undefined ? match$7 : expr[0];
                        semicolon(env$2);
                        match$6 = /* tuple */[
                          end_loc$3,
                          /* Expression */Block.__(1, [expr])
                        ];
                      }
                    }
                    return /* tuple */[
                            btwn(start_loc, match$6[0]),
                            /* ExportDeclaration */Block.__(28, [/* record */[
                                  /* default */true,
                                  /* declaration */match$6[1],
                                  /* specifiers */undefined,
                                  /* source */undefined,
                                  /* exportKind : ExportValue */1
                                ]])
                          ];
                case 14 : 
                case 15 : 
                case 16 : 
                case 17 : 
                case 18 : 
                case 19 : 
                case 20 : 
                case 21 : 
                case 23 : 
                case 24 : 
                case 27 : 
                case 28 : 
                case 29 : 
                case 30 : 
                case 31 : 
                case 32 : 
                case 33 : 
                case 35 : 
                case 36 : 
                case 37 : 
                    exit = 1;
                    break;
                case 12 : 
                case 13 : 
                case 22 : 
                case 25 : 
                case 26 : 
                case 38 : 
                    exit = 2;
                    break;
                default:
                  exit = 1;
              }
            }
          } else {
            exit = 1;
          }
          switch (exit) {
            case 1 : 
                var match$8 = Curry._2(Parser_env_048[/* token */0], undefined, env$2);
                var exportKind = typeof match$8 === "number" && match$8 === 59 ? (token$3(env$2), /* ExportType */0) : /* ExportValue */1;
                token$4(env$2, /* T_LCURLY */1);
                var match$9 = export_specifiers_and_errs(env$2, /* [] */0, /* [] */0);
                var specifiers$1 = /* ExportSpecifiers */Block.__(0, [match$9[0]]);
                var end_loc$4 = Curry._2(Parser_env_048[/* loc */2], undefined, env$2);
                token$4(env$2, /* T_RCURLY */2);
                var source$3 = Curry._2(Parser_env_048[/* value */1], undefined, env$2) === "from" ? export_source(env$2) : (List.iter((function (param) {
                            return error_at(env$2, param);
                          }), match$9[1]), undefined);
                var match$10 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$2);
                var end_loc$5 = match$10 !== undefined ? match$10 : (
                    source$3 !== undefined ? source$3[0] : end_loc$4
                  );
                semicolon(env$2);
                return /* tuple */[
                        btwn(start_loc, end_loc$5),
                        /* ExportDeclaration */Block.__(28, [/* record */[
                              /* default */false,
                              /* declaration */undefined,
                              /* specifiers */specifiers$1,
                              /* source */source$3,
                              /* exportKind */exportKind
                            ]])
                      ];
            case 2 : 
                var stmt = Curry._2(Parse[/* statement_list_item */2], decorators$1, env$2);
                var match$11 = stmt[1];
                var loc$1 = stmt[0];
                var names;
                if (typeof match$11 === "number") {
                  throw [
                        Caml_builtin_exceptions.failure,
                        "Internal Flow Error! Unexpected export statement declaration!"
                      ];
                } else {
                  switch (match$11.tag | 0) {
                    case 18 : 
                        var match$12 = match$11[0][/* id */0];
                        if (match$12 !== undefined) {
                          names = /* :: */[
                            /* tuple */[
                              loc$1,
                              extract_ident_name(match$12)
                            ],
                            /* [] */0
                          ];
                        } else {
                          error_at(env$2, /* tuple */[
                                loc$1,
                                /* ExportNamelessFunction */56
                              ]);
                          names = /* [] */0;
                        }
                        break;
                    case 19 : 
                        names = List.fold_left((function (names, param) {
                                var id = param[1][/* id */0];
                                var param$1 = names;
                                var param$2 = /* :: */[
                                  id,
                                  /* [] */0
                                ];
                                return List.fold_left(fold, param$1, param$2);
                              }), /* [] */0, match$11[0][/* declarations */0]);
                        break;
                    case 20 : 
                        var match$13 = match$11[0][/* id */0];
                        if (match$13 !== undefined) {
                          names = /* :: */[
                            /* tuple */[
                              loc$1,
                              extract_ident_name(match$13)
                            ],
                            /* [] */0
                          ];
                        } else {
                          error_at(env$2, /* tuple */[
                                loc$1,
                                /* ExportNamelessClass */55
                              ]);
                          names = /* [] */0;
                        }
                        break;
                    default:
                      throw [
                            Caml_builtin_exceptions.failure,
                            "Internal Flow Error! Unexpected export statement declaration!"
                          ];
                  }
                }
                List.iter((function (param) {
                        return record_export(env$2, param);
                      }), names);
                var declaration = /* Declaration */Block.__(0, [stmt]);
                return /* tuple */[
                        btwn(start_loc, stmt[0]),
                        /* ExportDeclaration */Block.__(28, [/* record */[
                              /* default */false,
                              /* declaration */declaration,
                              /* specifiers */undefined,
                              /* source */undefined,
                              /* exportKind : ExportValue */1
                            ]])
                      ];
            
          }
      case 48 : 
          error_on_decorators(env)(decorators);
          var env$3 = env;
          var env$4 = with_strict(true, env$3);
          var start_loc$1 = Curry._2(Parser_env_048[/* loc */2], undefined, env$4);
          token$4(env$4, /* T_IMPORT */48);
          var match$14 = Curry._2(Parser_env_048[/* token */0], undefined, env$4);
          var match$15;
          if (typeof match$14 === "number") {
            if (match$14 !== 44) {
              if (match$14 !== 59) {
                match$15 = /* tuple */[
                  /* ImportValue */2,
                  undefined
                ];
              } else {
                if (!env$4[/* parse_options */20][/* types */4]) {
                  error$1(env$4, /* UnexpectedTypeImport */8);
                }
                match$15 = /* tuple */[
                  /* ImportType */0,
                  Curry._2(Parse[/* identifier */10], undefined, env$4)
                ];
              }
            } else {
              if (!env$4[/* parse_options */20][/* types */4]) {
                error$1(env$4, /* UnexpectedTypeImport */8);
              }
              token$4(env$4, /* T_TYPEOF */44);
              match$15 = /* tuple */[
                /* ImportTypeof */1,
                undefined
              ];
            }
          } else {
            match$15 = /* tuple */[
              /* ImportValue */2,
              undefined
            ];
          }
          var type_ident = match$15[1];
          var importKind = match$15[0];
          var match$16 = Curry._2(Parser_env_048[/* token */0], undefined, env$4);
          var match$17 = Curry._2(Parser_env_048[/* is_identifier */8], undefined, env$4);
          var exit$2 = 0;
          var exit$3 = 0;
          if (typeof match$16 === "number") {
            if (match$16 === 8) {
              exit$2 = 1;
            } else {
              exit$3 = 2;
            }
          } else if (match$16.tag === 1 && importKind === /* ImportValue */2) {
            var match$18 = match$16[0];
            var octal = match$18[3];
            var raw = match$18[2];
            var value = match$18[1];
            var str_loc = match$18[0];
            if (octal) {
              strict_error(env$4, /* StrictOctalLiteral */31);
            }
            token$4(env$4, /* T_STRING */Block.__(1, [/* tuple */[
                      str_loc,
                      value,
                      raw,
                      octal
                    ]]));
            var value$1 = /* String */Block.__(0, [value]);
            var source_001 = /* record */[
              /* value */value$1,
              /* raw */raw
            ];
            var source$4 = /* tuple */[
              str_loc,
              source_001
            ];
            var match$19 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$4);
            var end_loc$6 = match$19 !== undefined ? match$19 : str_loc;
            semicolon(env$4);
            return /* tuple */[
                    btwn(start_loc$1, end_loc$6),
                    /* ImportDeclaration */Block.__(29, [/* record */[
                          /* importKind */importKind,
                          /* source */source$4,
                          /* specifiers : [] */0
                        ]])
                  ];
          } else {
            exit$3 = 2;
          }
          if (exit$3 === 2) {
            if (match$17) {
              exit$2 = 1;
            } else {
              var specifiers$2 = named_or_namespace_specifier(env$4);
              var source$5 = source(env$4);
              var match$20 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$4);
              var end_loc$7 = match$20 !== undefined ? match$20 : source$5[0];
              semicolon(env$4);
              return /* tuple */[
                      btwn(start_loc$1, end_loc$7),
                      /* ImportDeclaration */Block.__(29, [/* record */[
                            /* importKind */importKind,
                            /* source */source$5,
                            /* specifiers */specifiers$2
                          ]])
                    ];
            }
          }
          if (exit$2 === 1) {
            var match$21 = Curry._2(Parser_env_048[/* token */0], undefined, env$4);
            var match$22 = Curry._2(Parser_env_048[/* value */1], undefined, env$4);
            var match$23;
            var exit$4 = 0;
            if (type_ident !== undefined && typeof match$21 === "number") {
              var type_ident$1 = type_ident;
              if (match$21 !== 8 && (match$21 !== 0 || match$22 !== "from")) {
                exit$4 = 2;
              } else {
                match$23 = /* tuple */[
                  /* ImportValue */2,
                  /* ImportDefaultSpecifier */Block.__(1, [type_ident$1])
                ];
              }
            } else {
              exit$4 = 2;
            }
            if (exit$4 === 2) {
              match$23 = /* tuple */[
                importKind,
                /* ImportDefaultSpecifier */Block.__(1, [Curry._2(Parse[/* identifier */10], undefined, env$4)])
              ];
            }
            var match$24 = Curry._2(Parser_env_048[/* token */0], undefined, env$4);
            var additional_specifiers = typeof match$24 === "number" && match$24 === 8 ? (token$4(env$4, /* T_COMMA */8), named_or_namespace_specifier(env$4)) : /* [] */0;
            var source$6 = source(env$4);
            var match$25 = Curry._2(Parser_env_048[/* semicolon_loc */7], undefined, env$4);
            var end_loc$8 = match$25 !== undefined ? match$25 : source$6[0];
            semicolon(env$4);
            return /* tuple */[
                    btwn(start_loc$1, end_loc$8),
                    /* ImportDeclaration */Block.__(29, [/* record */[
                          /* importKind */match$23[0],
                          /* source */source$6,
                          /* specifiers : :: */[
                            match$23[1],
                            additional_specifiers
                          ]
                        ]])
                  ];
          }
          case 49 : 
      case 50 : 
      case 51 : 
      case 52 : 
      case 53 : 
      case 54 : 
      case 55 : 
      case 56 : 
      case 57 : 
          return statement_list_item(decorators, env);
      case 58 : 
          if (Curry._2(Parser_env_048[/* token */0], 1, env) === /* T_EXPORT */47) {
            error_on_decorators(env)(decorators);
            return declare_export_declaration(undefined, env);
          } else {
            return statement_list_item(decorators, env);
          }
      default:
        return statement_list_item(decorators, env);
    }
  } else {
    return statement_list_item(decorators, env);
  }
}

function statement_list(term_fn, env) {
  var env$1 = env;
  var term_fn$1 = term_fn;
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var t = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
    var exit = 0;
    if (typeof t === "number" && t === 105) {
      return List.rev(acc);
    } else {
      exit = 1;
    }
    if (exit === 1) {
      if (Curry._1(term_fn$1, t)) {
        return List.rev(acc);
      } else {
        _acc = /* :: */[
          statement_list_item(undefined, env$1),
          acc
        ];
        continue ;
      }
    }
    
  };
}

function statement_list$1(_env, term_fn, item_fn, _param) {
  while(true) {
    var param = _param;
    var env = _env;
    var stmts = param[1];
    var string_tokens = param[0];
    var t = Curry._2(Parser_env_048[/* token */0], undefined, env);
    var exit = 0;
    if (typeof t === "number" && t === 105) {
      return /* tuple */[
              env,
              string_tokens,
              stmts
            ];
    } else {
      exit = 1;
    }
    if (exit === 1) {
      if (Curry._1(term_fn, t)) {
        return /* tuple */[
                env,
                string_tokens,
                stmts
              ];
      } else {
        var string_token_000 = Curry._2(Parser_env_048[/* loc */2], undefined, env);
        var string_token_001 = Curry._2(Parser_env_048[/* token */0], undefined, env);
        var string_token = /* tuple */[
          string_token_000,
          string_token_001
        ];
        var possible_directive = Curry._1(item_fn, env);
        var stmts$1 = /* :: */[
          possible_directive,
          stmts
        ];
        var match = possible_directive[1];
        if (typeof match === "number" || match.tag !== 1) {
          return /* tuple */[
                  env,
                  string_tokens,
                  stmts$1
                ];
        } else {
          var match$1 = match[0][/* expression */0];
          var match$2 = match$1[1];
          if (typeof match$2 === "number" || match$2.tag !== 19) {
            return /* tuple */[
                    env,
                    string_tokens,
                    stmts$1
                  ];
          } else {
            var match$3 = match$2[0][/* value */0];
            if (typeof match$3 === "number" || match$3.tag) {
              return /* tuple */[
                      env,
                      string_tokens,
                      stmts$1
                    ];
            } else {
              var loc = match$1[0];
              var len = loc[/* _end */2][/* column */1] - loc[/* start */1][/* column */1] | 0;
              var strict = env[/* in_strict_mode */5] || match$3[0] === "use strict" && len === 12;
              var string_tokens$1 = /* :: */[
                string_token,
                string_tokens
              ];
              _param = /* tuple */[
                string_tokens$1,
                stmts$1
              ];
              _env = with_strict(strict, env);
              continue ;
            }
          }
        }
      }
    }
    
  };
}

function directives(env, term_fn, item_fn) {
  var match = statement_list$1(env, term_fn, item_fn, /* tuple */[
        /* [] */0,
        /* [] */0
      ]);
  var env$1 = match[0];
  List.iter((function (param) {
          var env$2 = env$1;
          var param$1 = param;
          var token = param$1[1];
          var exit = 0;
          if (typeof token === "number" || token.tag !== 1) {
            exit = 1;
          } else if (token[0][3]) {
            return strict_error_at(env$2, /* tuple */[
                        param$1[0],
                        /* StrictOctalLiteral */31
                      ]);
          } else {
            return 0;
          }
          if (exit === 1) {
            var s = "Nooo: " + (token_to_string(token) + "\n");
            throw [
                  Caml_builtin_exceptions.failure,
                  s
                ];
          }
          
        }), List.rev(match[1]));
  return /* tuple */[
          env$1,
          match[2]
        ];
}

function module_body(term_fn, env) {
  var env$1 = env;
  var term_fn$1 = term_fn;
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var t = Curry._2(Parser_env_048[/* token */0], undefined, env$1);
    var exit = 0;
    if (typeof t === "number" && t === 105) {
      return List.rev(acc);
    } else {
      exit = 1;
    }
    if (exit === 1) {
      if (Curry._1(term_fn$1, t)) {
        return List.rev(acc);
      } else {
        _acc = /* :: */[
          module_item(env$1),
          acc
        ];
        continue ;
      }
    }
    
  };
}

var class_declaration$1 = class_declaration;

function module_body_with_directives(env, term_fn) {
  var match = Curry._3(directives, env, term_fn, module_item);
  var stmts = Curry._2(module_body, term_fn, match[0]);
  return List.fold_left((function (acc, stmt) {
                return /* :: */[
                        stmt,
                        acc
                      ];
              }), stmts, match[1]);
}

function statement_list_with_directives(term_fn, env) {
  var match = Curry._3(directives, env, term_fn, (function (eta) {
          return statement_list_item(undefined, eta);
        }));
  var env$1 = match[0];
  var stmts = Curry._2(statement_list, term_fn, env$1);
  var stmts$1 = List.fold_left((function (acc, stmt) {
          return /* :: */[
                  stmt,
                  acc
                ];
        }), stmts, match[1]);
  return /* tuple */[
          stmts$1,
          env$1[/* in_strict_mode */5]
        ];
}

function identifier$2(restricted_error, env) {
  var loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  var name = Curry._2(Parser_env_048[/* value */1], undefined, env);
  var t = Curry._2(Parser_env_048[/* token */0], undefined, env);
  var exit = 0;
  if (typeof t === "number" && t === 26) {
    if (env[/* in_strict_mode */5]) {
      strict_error(env, /* StrictReservedWord */39);
    } else if (env[/* no_let */12]) {
      error$1(env, /* UnexpectedToken */Block.__(1, [name]));
    }
    token$3(env);
  } else {
    exit = 1;
  }
  if (exit === 1) {
    if (is_strict_reserved(name)) {
      strict_error(env, /* StrictReservedWord */39);
      token$3(env);
    } else if (typeof t === "number" && !(t > 62 || t < 58)) {
      token$4(env, t);
    } else {
      token$4(env, /* T_IDENTIFIER */0);
    }
  }
  if (restricted_error !== undefined) {
    if (is_restricted(name)) {
      strict_error_at(env, /* tuple */[
            loc,
            restricted_error
          ]);
    }
    
  }
  return /* tuple */[
          loc,
          /* record */[
            /* name */name,
            /* typeAnnotation */undefined,
            /* optional */false
          ]
        ];
}

function program(env) {
  var stmts = module_body_with_directives(env, (function (param) {
          return false;
        }));
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_EOF */105);
  var loc = stmts ? btwn(List.hd(stmts)[0], List.hd(List.rev(stmts))[0]) : end_loc;
  var comments = List.rev(env[/* comments */1][0]);
  return /* tuple */[
          loc,
          stmts,
          comments
        ];
}

function expression$1(env) {
  var expr = Curry._1(assignment, env);
  var match = Curry._2(Parser_env_048[/* token */0], undefined, env);
  if (typeof match === "number" && match === 8) {
    return sequence(env, /* :: */[
                expr,
                /* [] */0
              ]);
  } else {
    return expr;
  }
}

function identifier_with_type(env, restricted_error) {
  var match = identifier$2(restricted_error, env);
  var id = match[1];
  var loc = match[0];
  var match$1;
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_PLING */76) {
    if (!env[/* parse_options */20][/* types */4]) {
      error$1(env, /* UnexpectedTypeAnnotation */6);
    }
    var loc$1 = btwn(loc, Curry._2(Parser_env_048[/* loc */2], undefined, env));
    token$4(env, /* T_PLING */76);
    match$1 = /* tuple */[
      loc$1,
      /* record */[
        /* name */id[/* name */0],
        /* typeAnnotation */id[/* typeAnnotation */1],
        /* optional */true
      ]
    ];
  } else {
    match$1 = /* tuple */[
      loc,
      id
    ];
  }
  var id$1 = match$1[1];
  var loc$2 = match$1[0];
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_COLON */77) {
    var typeAnnotation = wrap(annotation, env);
    var loc$3 = btwn(loc$2, typeAnnotation[0]);
    var typeAnnotation$1 = typeAnnotation;
    return /* tuple */[
            loc$3,
            /* record */[
              /* name */id$1[/* name */0],
              /* typeAnnotation */typeAnnotation$1,
              /* optional */id$1[/* optional */2]
            ]
          ];
  } else {
    return /* tuple */[
            loc$2,
            id$1
          ];
  }
}

function block_body(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_LCURLY */1);
  var term_fn = function (t) {
    return t === /* T_RCURLY */2;
  };
  var body = Curry._2(statement_list, term_fn, env);
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_RCURLY */2);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* body */body]
        ];
}

function function_block_body(env) {
  var start_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_LCURLY */1);
  var term_fn = function (t) {
    return t === /* T_RCURLY */2;
  };
  var match = statement_list_with_directives(term_fn, env);
  var end_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  token$4(env, /* T_RCURLY */2);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* body */match[0]],
          match[1]
        ];
}

function predicate(env) {
  var checks_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
  if (Curry._2(Parser_env_048[/* token */0], undefined, env) === /* T_IDENTIFIER */0 && Curry._2(Parser_env_048[/* value */1], undefined, env) === "checks") {
    token$4(env, /* T_IDENTIFIER */0);
    if (maybe(env, /* T_LPAREN */3)) {
      var exp = Curry._1(Parse[/* expression */6], env);
      var rparen_loc = Curry._2(Parser_env_048[/* loc */2], undefined, env);
      token$4(env, /* T_RPAREN */4);
      var loc = btwn(checks_loc, rparen_loc);
      return /* tuple */[
              loc,
              /* Declared */[exp]
            ];
    } else {
      return /* tuple */[
              checks_loc,
              /* Inferred */0
            ];
    }
  }
  
}

Caml_module.update_mod([[
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0
      ]], Parse, /* module */[
      /* program */program,
      /* statement */statement,
      /* statement_list_item */statement_list_item,
      /* statement_list */statement_list,
      /* statement_list_with_directives */statement_list_with_directives,
      /* module_body */module_body,
      /* expression */expression$1,
      /* assignment */assignment,
      /* object_initializer */_initializer,
      /* array_initializer */array_initializer,
      /* identifier */identifier$2,
      /* identifier_or_reserved_keyword */identifier_or_reserved_keyword,
      /* identifier_with_type */identifier_with_type,
      /* block_body */block_body,
      /* function_block_body */function_block_body,
      /* jsx_element */element,
      /* pattern */pattern$1,
      /* pattern_from_expr */from_expr,
      /* object_key */key,
      /* class_declaration */class_declaration$1,
      /* class_expression */class_expression,
      /* is_assignable_lhs */is_assignable_lhs,
      /* predicate */predicate
    ]);

function program$1($staropt$star, $staropt$star$1, $staropt$star$2, content) {
  var fail = $staropt$star !== undefined ? $staropt$star : true;
  var token_sink = $staropt$star$1 !== undefined ? Caml_option.valFromOption($staropt$star$1) : undefined;
  var parse_options = $staropt$star$2 !== undefined ? Caml_option.valFromOption($staropt$star$2) : undefined;
  var fail$1 = fail;
  var $staropt$star$3 = Caml_option.some(token_sink);
  var $staropt$star$4 = Caml_option.some(parse_options);
  var filename = undefined;
  var content$1 = content;
  var token_sink$1 = $staropt$star$3 !== undefined ? Caml_option.valFromOption($staropt$star$3) : undefined;
  var parse_options$1 = $staropt$star$4 !== undefined ? Caml_option.valFromOption($staropt$star$4) : undefined;
  var env = init_env(Caml_option.some(token_sink$1), Caml_option.some(parse_options$1), filename, content$1);
  var env$1 = env;
  var parser = Parse[/* program */0];
  var fail$2 = fail$1;
  var ast = Curry._1(parser, env$1);
  var error_list = filter_duplicate_errors(env$1[/* errors */0][0]);
  if (fail$2 && error_list !== /* [] */0) {
    throw [
          $$Error,
          error_list
        ];
  }
  return /* tuple */[
          ast,
          error_list
        ];
}

var translation_errors = /* record */[/* contents : [] */0];

var string = (function (x) {return x;});

var bool = (function (x) {x ? 1 : 0;});

var obj = (function(arr) {var ret = {}; arr.forEach(function(a) {ret[a[0]]=a[1];}); return ret});

var array = (function (x) {return x;});

var number$1 = (function (x) {return x;});

var $$null = null;

function regexp$1(loc, pattern, flags) {
  try {
    return new RegExp(pattern, flags);
  }
  catch (exn){
    translation_errors[0] = /* :: */[
      /* tuple */[
        loc,
        /* InvalidRegExp */12
      ],
      translation_errors[0]
    ];
    return new RegExp("", flags);
  }
}

function parse(content, options) {
  try {
    var match = program$1(false, undefined, Caml_option.some(undefined), content);
    translation_errors[0] = /* [] */0;
    var array_of_list = function (fn, list) {
      return Curry._1(array, $$Array.of_list(List.map(fn, list)));
    };
    var option = function (f, param) {
      if (param !== undefined) {
        return Curry._1(f, Caml_option.valFromOption(param));
      } else {
        return $$null;
      }
    };
    var position = function (p) {
      return Curry._1(obj, /* array */[
                  /* tuple */[
                    "line",
                    Curry._1(number$1, p[/* line */0])
                  ],
                  /* tuple */[
                    "column",
                    Curry._1(number$1, p[/* column */1])
                  ]
                ]);
    };
    var loc = function ($$location) {
      var match = $$location[/* source */0];
      var source;
      if (match !== undefined) {
        var match$1 = match;
        source = typeof match$1 === "number" ? Curry._1(string, "(global)") : Curry._1(string, match$1[0]);
      } else {
        source = $$null;
      }
      return Curry._1(obj, /* array */[
                  /* tuple */[
                    "source",
                    source
                  ],
                  /* tuple */[
                    "start",
                    position($$location[/* start */1])
                  ],
                  /* tuple */[
                    "end",
                    position($$location[/* _end */2])
                  ]
                ]);
    };
    var range = function ($$location) {
      return Curry._1(array, /* array */[
                  Curry._1(number$1, $$location[/* start */1][/* offset */2]),
                  Curry._1(number$1, $$location[/* _end */2][/* offset */2])
                ]);
    };
    var node = function (_type, $$location, props) {
      return Curry._1(obj, $$Array.append(/* array */[
                      /* tuple */[
                        "type",
                        Curry._1(string, _type)
                      ],
                      /* tuple */[
                        "loc",
                        loc($$location)
                      ],
                      /* tuple */[
                        "range",
                        range($$location)
                      ]
                    ], props));
    };
    var errors = function (l) {
      var error$2 = function (param) {
        return Curry._1(obj, /* array */[
                    /* tuple */[
                      "loc",
                      loc(param[0])
                    ],
                    /* tuple */[
                      "message",
                      Curry._1(string, error(param[1]))
                    ]
                  ]);
      };
      return array_of_list(error$2, l);
    };
    var function_type_param = function (param) {
      var param$1 = param[1];
      return node("FunctionTypeParam", param[0], /* array */[
                  /* tuple */[
                    "name",
                    identifier(param$1[/* name */0])
                  ],
                  /* tuple */[
                    "typeAnnotation",
                    _type(param$1[/* typeAnnotation */1])
                  ],
                  /* tuple */[
                    "optional",
                    Curry._1(bool, param$1[/* optional */2])
                  ]
                ]);
    };
    var type_annotation = function (param) {
      return node("TypeAnnotation", param[0], /* array */[/* tuple */[
                    "typeAnnotation",
                    _type(param[1])
                  ]]);
    };
    var identifier = function (param) {
      var id = param[1];
      return node("Identifier", param[0], /* array */[
                  /* tuple */[
                    "name",
                    Curry._1(string, id[/* name */0])
                  ],
                  /* tuple */[
                    "typeAnnotation",
                    option(type_annotation, id[/* typeAnnotation */1])
                  ],
                  /* tuple */[
                    "optional",
                    Curry._1(bool, id[/* optional */2])
                  ]
                ]);
    };
    var pattern = function (param) {
      var match = param[1];
      var loc = param[0];
      switch (match.tag | 0) {
        case 0 : 
            var obj = match[0];
            return node("ObjectPattern", loc, /* array */[
                        /* tuple */[
                          "properties",
                          array_of_list(object_pattern_property, obj[/* properties */0])
                        ],
                        /* tuple */[
                          "typeAnnotation",
                          option(type_annotation, obj[/* typeAnnotation */1])
                        ]
                      ]);
        case 1 : 
            var arr = match[0];
            return node("ArrayPattern", loc, /* array */[
                        /* tuple */[
                          "elements",
                          array_of_list((function (param) {
                                  return option(array_pattern_element, param);
                                }), arr[/* elements */0])
                        ],
                        /* tuple */[
                          "typeAnnotation",
                          option(type_annotation, arr[/* typeAnnotation */1])
                        ]
                      ]);
        case 2 : 
            var match$1 = match[0];
            return node("AssignmentPattern", loc, /* array */[
                        /* tuple */[
                          "left",
                          pattern(match$1[/* left */0])
                        ],
                        /* tuple */[
                          "right",
                          expression(match$1[/* right */1])
                        ]
                      ]);
        case 3 : 
            return identifier(match[0]);
        case 4 : 
            return expression(match[0]);
        
      }
    };
    var block = function (param) {
      return node("BlockStatement", param[0], /* array */[/* tuple */[
                    "body",
                    array_of_list(statement, param[1][/* body */0])
                  ]]);
    };
    var expression = function (param) {
      var match = param[1];
      var loc = param[0];
      if (typeof match === "number") {
        return node("ThisExpression", loc, /* array */[]);
      } else {
        switch (match.tag | 0) {
          case 0 : 
              return node("ArrayExpression", loc, /* array */[/* tuple */[
                            "elements",
                            array_of_list((function (param) {
                                    return option(expression_or_spread, param);
                                  }), match[0][/* elements */0])
                          ]]);
          case 1 : 
              return node("ObjectExpression", loc, /* array */[/* tuple */[
                            "properties",
                            array_of_list(object_property, match[0][/* properties */0])
                          ]]);
          case 2 : 
              return function_expression(/* tuple */[
                          loc,
                          match[0]
                        ]);
          case 3 : 
              var arrow = match[0];
              var match$1 = arrow[/* body */4];
              var body;
              body = match$1.tag ? expression(match$1[0]) : block(match$1[0]);
              return node("ArrowFunctionExpression", loc, /* array */[
                          /* tuple */[
                            "id",
                            option(identifier, arrow[/* id */0])
                          ],
                          /* tuple */[
                            "params",
                            array_of_list(pattern, arrow[/* params */1])
                          ],
                          /* tuple */[
                            "defaults",
                            array_of_list((function (param) {
                                    return option(expression, param);
                                  }), arrow[/* defaults */2])
                          ],
                          /* tuple */[
                            "rest",
                            option(identifier, arrow[/* rest */3])
                          ],
                          /* tuple */[
                            "body",
                            body
                          ],
                          /* tuple */[
                            "async",
                            Curry._1(bool, arrow[/* async */5])
                          ],
                          /* tuple */[
                            "generator",
                            Curry._1(bool, arrow[/* generator */6])
                          ],
                          /* tuple */[
                            "expression",
                            Curry._1(bool, arrow[/* expression */8])
                          ],
                          /* tuple */[
                            "returnType",
                            option(type_annotation, arrow[/* returnType */9])
                          ],
                          /* tuple */[
                            "typeParameters",
                            option(type_parameter_declaration, arrow[/* typeParameters */10])
                          ]
                        ]);
          case 4 : 
              return node("SequenceExpression", loc, /* array */[/* tuple */[
                            "expressions",
                            array_of_list(expression, match[0][/* expressions */0])
                          ]]);
          case 5 : 
              var unary = match[0];
              var match$2 = unary[/* operator */0];
              if (match$2 >= 7) {
                return node("AwaitExpression", loc, /* array */[/* tuple */[
                              "argument",
                              expression(unary[/* argument */2])
                            ]]);
              } else {
                var match$3 = unary[/* operator */0];
                var operator;
                switch (match$3) {
                  case 0 : 
                      operator = "-";
                      break;
                  case 1 : 
                      operator = "+";
                      break;
                  case 2 : 
                      operator = "!";
                      break;
                  case 3 : 
                      operator = "~";
                      break;
                  case 4 : 
                      operator = "typeof";
                      break;
                  case 5 : 
                      operator = "void";
                      break;
                  case 6 : 
                      operator = "delete";
                      break;
                  case 7 : 
                      throw [
                            Caml_builtin_exceptions.failure,
                            "matched above"
                          ];
                  
                }
                return node("UnaryExpression", loc, /* array */[
                            /* tuple */[
                              "operator",
                              Curry._1(string, operator)
                            ],
                            /* tuple */[
                              "prefix",
                              Curry._1(bool, unary[/* prefix */1])
                            ],
                            /* tuple */[
                              "argument",
                              expression(unary[/* argument */2])
                            ]
                          ]);
              }
          case 6 : 
              var binary = match[0];
              var match$4 = binary[/* operator */0];
              var operator$1;
              switch (match$4) {
                case 0 : 
                    operator$1 = "==";
                    break;
                case 1 : 
                    operator$1 = "!=";
                    break;
                case 2 : 
                    operator$1 = "===";
                    break;
                case 3 : 
                    operator$1 = "!==";
                    break;
                case 4 : 
                    operator$1 = "<";
                    break;
                case 5 : 
                    operator$1 = "<=";
                    break;
                case 6 : 
                    operator$1 = ">";
                    break;
                case 7 : 
                    operator$1 = ">=";
                    break;
                case 8 : 
                    operator$1 = "<<";
                    break;
                case 9 : 
                    operator$1 = ">>";
                    break;
                case 10 : 
                    operator$1 = ">>>";
                    break;
                case 11 : 
                    operator$1 = "+";
                    break;
                case 12 : 
                    operator$1 = "-";
                    break;
                case 13 : 
                    operator$1 = "*";
                    break;
                case 14 : 
                    operator$1 = "**";
                    break;
                case 15 : 
                    operator$1 = "/";
                    break;
                case 16 : 
                    operator$1 = "%";
                    break;
                case 17 : 
                    operator$1 = "|";
                    break;
                case 18 : 
                    operator$1 = "^";
                    break;
                case 19 : 
                    operator$1 = "&";
                    break;
                case 20 : 
                    operator$1 = "in";
                    break;
                case 21 : 
                    operator$1 = "instanceof";
                    break;
                
              }
              return node("BinaryExpression", loc, /* array */[
                          /* tuple */[
                            "operator",
                            Curry._1(string, operator$1)
                          ],
                          /* tuple */[
                            "left",
                            expression(binary[/* left */1])
                          ],
                          /* tuple */[
                            "right",
                            expression(binary[/* right */2])
                          ]
                        ]);
          case 7 : 
              var assignment = match[0];
              var match$5 = assignment[/* operator */0];
              var operator$2;
              switch (match$5) {
                case 0 : 
                    operator$2 = "=";
                    break;
                case 1 : 
                    operator$2 = "+=";
                    break;
                case 2 : 
                    operator$2 = "-=";
                    break;
                case 3 : 
                    operator$2 = "*=";
                    break;
                case 4 : 
                    operator$2 = "**=";
                    break;
                case 5 : 
                    operator$2 = "/=";
                    break;
                case 6 : 
                    operator$2 = "%=";
                    break;
                case 7 : 
                    operator$2 = "<<=";
                    break;
                case 8 : 
                    operator$2 = ">>=";
                    break;
                case 9 : 
                    operator$2 = ">>>=";
                    break;
                case 10 : 
                    operator$2 = "|=";
                    break;
                case 11 : 
                    operator$2 = "^=";
                    break;
                case 12 : 
                    operator$2 = "&=";
                    break;
                
              }
              return node("AssignmentExpression", loc, /* array */[
                          /* tuple */[
                            "operator",
                            Curry._1(string, operator$2)
                          ],
                          /* tuple */[
                            "left",
                            pattern(assignment[/* left */1])
                          ],
                          /* tuple */[
                            "right",
                            expression(assignment[/* right */2])
                          ]
                        ]);
          case 8 : 
              var update = match[0];
              var match$6 = update[/* operator */0];
              var operator$3 = match$6 ? "--" : "++";
              return node("UpdateExpression", loc, /* array */[
                          /* tuple */[
                            "operator",
                            Curry._1(string, operator$3)
                          ],
                          /* tuple */[
                            "argument",
                            expression(update[/* argument */1])
                          ],
                          /* tuple */[
                            "prefix",
                            Curry._1(bool, update[/* prefix */2])
                          ]
                        ]);
          case 9 : 
              var logical = match[0];
              var match$7 = logical[/* operator */0];
              var operator$4 = match$7 ? "&&" : "||";
              return node("LogicalExpression", loc, /* array */[
                          /* tuple */[
                            "operator",
                            Curry._1(string, operator$4)
                          ],
                          /* tuple */[
                            "left",
                            expression(logical[/* left */1])
                          ],
                          /* tuple */[
                            "right",
                            expression(logical[/* right */2])
                          ]
                        ]);
          case 10 : 
              var conditional = match[0];
              return node("ConditionalExpression", loc, /* array */[
                          /* tuple */[
                            "test",
                            expression(conditional[/* test */0])
                          ],
                          /* tuple */[
                            "consequent",
                            expression(conditional[/* consequent */1])
                          ],
                          /* tuple */[
                            "alternate",
                            expression(conditional[/* alternate */2])
                          ]
                        ]);
          case 11 : 
              var _new = match[0];
              return node("NewExpression", loc, /* array */[
                          /* tuple */[
                            "callee",
                            expression(_new[/* callee */0])
                          ],
                          /* tuple */[
                            "arguments",
                            array_of_list(expression_or_spread, _new[/* arguments */1])
                          ]
                        ]);
          case 12 : 
              var call = match[0];
              return node("CallExpression", loc, /* array */[
                          /* tuple */[
                            "callee",
                            expression(call[/* callee */0])
                          ],
                          /* tuple */[
                            "arguments",
                            array_of_list(expression_or_spread, call[/* arguments */1])
                          ]
                        ]);
          case 13 : 
              var member = match[0];
              var match$8 = member[/* property */1];
              var property;
              property = match$8.tag ? expression(match$8[0]) : identifier(match$8[0]);
              return node("MemberExpression", loc, /* array */[
                          /* tuple */[
                            "object",
                            expression(member[/* _object */0])
                          ],
                          /* tuple */[
                            "property",
                            property
                          ],
                          /* tuple */[
                            "computed",
                            Curry._1(bool, member[/* computed */2])
                          ]
                        ]);
          case 14 : 
              var $$yield = match[0];
              return node("YieldExpression", loc, /* array */[
                          /* tuple */[
                            "argument",
                            option(expression, $$yield[/* argument */0])
                          ],
                          /* tuple */[
                            "delegate",
                            Curry._1(bool, $$yield[/* delegate */1])
                          ]
                        ]);
          case 15 : 
              var comp = match[0];
              return node("ComprehensionExpression", loc, /* array */[
                          /* tuple */[
                            "blocks",
                            array_of_list(comprehension_block, comp[/* blocks */0])
                          ],
                          /* tuple */[
                            "filter",
                            option(expression, comp[/* filter */1])
                          ]
                        ]);
          case 16 : 
              var gen = match[0];
              return node("GeneratorExpression", loc, /* array */[
                          /* tuple */[
                            "blocks",
                            array_of_list(comprehension_block, gen[/* blocks */0])
                          ],
                          /* tuple */[
                            "filter",
                            option(expression, gen[/* filter */1])
                          ]
                        ]);
          case 17 : 
              var _let = match[0];
              return node("LetExpression", loc, /* array */[
                          /* tuple */[
                            "head",
                            array_of_list(let_assignment, _let[/* head */0])
                          ],
                          /* tuple */[
                            "body",
                            expression(_let[/* body */1])
                          ]
                        ]);
          case 18 : 
              return identifier(match[0]);
          case 19 : 
              return literal(/* tuple */[
                          loc,
                          match[0]
                        ]);
          case 20 : 
              return template_literal(/* tuple */[
                          loc,
                          match[0]
                        ]);
          case 21 : 
              var param$1 = /* tuple */[
                loc,
                match[0]
              ];
              var tagged = param$1[1];
              return node("TaggedTemplateExpression", param$1[0], /* array */[
                          /* tuple */[
                            "tag",
                            expression(tagged[/* tag */0])
                          ],
                          /* tuple */[
                            "quasi",
                            template_literal(tagged[/* quasi */1])
                          ]
                        ]);
          case 22 : 
              return jsx_element(/* tuple */[
                          loc,
                          match[0]
                        ]);
          case 23 : 
              var param$2 = /* tuple */[
                loc,
                match[0]
              ];
              var c = param$2[1];
              return node("ClassExpression", param$2[0], /* array */[
                          /* tuple */[
                            "id",
                            option(identifier, c[/* id */0])
                          ],
                          /* tuple */[
                            "body",
                            class_body(c[/* body */1])
                          ],
                          /* tuple */[
                            "superClass",
                            option(expression, c[/* superClass */2])
                          ],
                          /* tuple */[
                            "typeParameters",
                            option(type_parameter_declaration, c[/* typeParameters */3])
                          ],
                          /* tuple */[
                            "superTypeParameters",
                            option(type_parameter_instantiation, c[/* superTypeParameters */4])
                          ],
                          /* tuple */[
                            "implements",
                            array_of_list(class_implements, c[/* implements */5])
                          ],
                          /* tuple */[
                            "decorators",
                            array_of_list(expression, c[/* classDecorators */6])
                          ]
                        ]);
          case 24 : 
              var typecast = match[0];
              return node("TypeCastExpression", loc, /* array */[
                          /* tuple */[
                            "expression",
                            expression(typecast[/* expression */0])
                          ],
                          /* tuple */[
                            "typeAnnotation",
                            type_annotation(typecast[/* typeAnnotation */1])
                          ]
                        ]);
          
        }
      }
    };
    var type_parameter_declaration = function (param) {
      return node("TypeParameterDeclaration", param[0], /* array */[/* tuple */[
                    "params",
                    array_of_list(type_param, param[1][/* params */0])
                  ]]);
    };
    var template_element = function (param) {
      var element = param[1];
      var value = Curry._1(obj, /* array */[
            /* tuple */[
              "raw",
              Curry._1(string, element[/* value */0][/* raw */0])
            ],
            /* tuple */[
              "cooked",
              Curry._1(string, element[/* value */0][/* cooked */1])
            ]
          ]);
      return node("TemplateElement", param[0], /* array */[
                  /* tuple */[
                    "value",
                    value
                  ],
                  /* tuple */[
                    "tail",
                    Curry._1(bool, element[/* tail */1])
                  ]
                ]);
    };
    var jsx_name = function (param) {
      switch (param.tag | 0) {
        case 0 : 
            return jsx_identifier(param[0]);
        case 1 : 
            return jsx_namespaced_name(param[0]);
        case 2 : 
            return jsx_member_expression(param[0]);
        
      }
    };
    var _type = function (param) {
      var t = param[1];
      var loc = param[0];
      if (typeof t === "number") {
        switch (t) {
          case 0 : 
              return node("AnyTypeAnnotation", loc, /* array */[]);
          case 1 : 
              return node("VoidTypeAnnotation", loc, /* array */[]);
          case 2 : 
              return node("NullTypeAnnotation", loc, /* array */[]);
          case 3 : 
              return node("NumberTypeAnnotation", loc, /* array */[]);
          case 4 : 
              return node("StringTypeAnnotation", loc, /* array */[]);
          case 5 : 
              return node("BooleanTypeAnnotation", loc, /* array */[]);
          case 6 : 
              return node("ExistsTypeAnnotation", loc, /* array */[]);
          
        }
      } else {
        switch (t.tag | 0) {
          case 0 : 
              var loc$1 = loc;
              var t$1 = t[0];
              return node("NullableTypeAnnotation", loc$1, /* array */[/* tuple */[
                            "typeAnnotation",
                            _type(t$1)
                          ]]);
          case 1 : 
              return function_type(/* tuple */[
                          loc,
                          t[0]
                        ]);
          case 2 : 
              return object_type(/* tuple */[
                          loc,
                          t[0]
                        ]);
          case 3 : 
              var loc$2 = loc;
              var t$2 = t[0];
              return node("ArrayTypeAnnotation", loc$2, /* array */[/* tuple */[
                            "elementType",
                            _type(t$2)
                          ]]);
          case 4 : 
              var param$1 = /* tuple */[
                loc,
                t[0]
              ];
              var g = param$1[1];
              var match = g[/* id */0];
              var id;
              id = match.tag ? generic_type_qualified_identifier(match[0]) : identifier(match[0]);
              return node("GenericTypeAnnotation", param$1[0], /* array */[
                          /* tuple */[
                            "id",
                            id
                          ],
                          /* tuple */[
                            "typeParameters",
                            option(type_parameter_instantiation, g[/* typeParameters */1])
                          ]
                        ]);
          case 5 : 
              var param$2 = /* tuple */[
                loc,
                t[0]
              ];
              return node("UnionTypeAnnotation", param$2[0], /* array */[/* tuple */[
                            "types",
                            array_of_list(_type, param$2[1])
                          ]]);
          case 6 : 
              var param$3 = /* tuple */[
                loc,
                t[0]
              ];
              return node("IntersectionTypeAnnotation", param$3[0], /* array */[/* tuple */[
                            "types",
                            array_of_list(_type, param$3[1])
                          ]]);
          case 7 : 
              var param$4 = /* tuple */[
                loc,
                t[0]
              ];
              return node("TypeofTypeAnnotation", param$4[0], /* array */[/* tuple */[
                            "argument",
                            _type(param$4[1])
                          ]]);
          case 8 : 
              var param$5 = /* tuple */[
                loc,
                t[0]
              ];
              return node("TupleTypeAnnotation", param$5[0], /* array */[/* tuple */[
                            "types",
                            array_of_list(_type, param$5[1])
                          ]]);
          case 9 : 
              var param$6 = /* tuple */[
                loc,
                t[0]
              ];
              var s = param$6[1];
              return node("StringLiteralTypeAnnotation", param$6[0], /* array */[
                          /* tuple */[
                            "value",
                            Curry._1(string, s[/* value */0])
                          ],
                          /* tuple */[
                            "raw",
                            Curry._1(string, s[/* raw */1])
                          ]
                        ]);
          case 10 : 
              var param$7 = /* tuple */[
                loc,
                t[0]
              ];
              var s$1 = param$7[1];
              return node("NumberLiteralTypeAnnotation", param$7[0], /* array */[
                          /* tuple */[
                            "value",
                            Curry._1(number$1, s$1[/* value */0])
                          ],
                          /* tuple */[
                            "raw",
                            Curry._1(string, s$1[/* raw */1])
                          ]
                        ]);
          case 11 : 
              var param$8 = /* tuple */[
                loc,
                t[0]
              ];
              var s$2 = param$8[1];
              return node("BooleanLiteralTypeAnnotation", param$8[0], /* array */[
                          /* tuple */[
                            "value",
                            Curry._1(bool, s$2[/* value */0])
                          ],
                          /* tuple */[
                            "raw",
                            Curry._1(string, s$2[/* raw */1])
                          ]
                        ]);
          
        }
      }
    };
    var literal = function (param) {
      var lit = param[1];
      var raw = lit[/* raw */1];
      var value = lit[/* value */0];
      var loc = param[0];
      var value_;
      if (typeof value === "number") {
        value_ = $$null;
      } else {
        switch (value.tag | 0) {
          case 0 : 
              value_ = Curry._1(string, value[0]);
              break;
          case 1 : 
              value_ = Curry._1(bool, value[0]);
              break;
          case 2 : 
              value_ = Curry._1(number$1, value[0]);
              break;
          case 3 : 
              var match = value[0];
              value_ = regexp$1(loc, match[/* pattern */0], match[/* flags */1]);
              break;
          
        }
      }
      var props;
      var exit = 0;
      if (typeof value === "number" || value.tag !== 3) {
        exit = 1;
      } else {
        var match$1 = value[0];
        var regex = Curry._1(obj, /* array */[
              /* tuple */[
                "pattern",
                Curry._1(string, match$1[/* pattern */0])
              ],
              /* tuple */[
                "flags",
                Curry._1(string, match$1[/* flags */1])
              ]
            ]);
        props = /* array */[
          /* tuple */[
            "value",
            value_
          ],
          /* tuple */[
            "raw",
            Curry._1(string, raw)
          ],
          /* tuple */[
            "regex",
            regex
          ]
        ];
      }
      if (exit === 1) {
        props = /* array */[
          /* tuple */[
            "value",
            value_
          ],
          /* tuple */[
            "raw",
            Curry._1(string, raw)
          ]
        ];
      }
      return node("Literal", loc, props);
    };
    var variable_declarator = function (param) {
      var declarator = param[1];
      return node("VariableDeclarator", param[0], /* array */[
                  /* tuple */[
                    "id",
                    pattern(declarator[/* id */0])
                  ],
                  /* tuple */[
                    "init",
                    option(expression, declarator[/* init */1])
                  ]
                ]);
    };
    var type_param = function (param) {
      var tp = param[1];
      var variance = function (param) {
        if (param) {
          return Curry._1(string, "minus");
        } else {
          return Curry._1(string, "plus");
        }
      };
      return node("TypeParameter", param[0], /* array */[
                  /* tuple */[
                    "name",
                    Curry._1(string, tp[/* name */0])
                  ],
                  /* tuple */[
                    "bound",
                    option(type_annotation, tp[/* bound */1])
                  ],
                  /* tuple */[
                    "variance",
                    option(variance, tp[/* variance */2])
                  ],
                  /* tuple */[
                    "default",
                    option(_type, tp[/* default */3])
                  ]
                ]);
    };
    var jsx_identifier = function (param) {
      return node("JSXIdentifier", param[0], /* array */[/* tuple */[
                    "name",
                    Curry._1(string, param[1][/* name */0])
                  ]]);
    };
    var jsx_member_expression = function (param) {
      var member_expression = param[1];
      var match = member_expression[/* _object */0];
      var _object;
      _object = match.tag ? jsx_member_expression(match[0]) : jsx_identifier(match[0]);
      return node("JSXMemberExpression", param[0], /* array */[
                  /* tuple */[
                    "object",
                    _object
                  ],
                  /* tuple */[
                    "property",
                    jsx_identifier(member_expression[/* property */1])
                  ]
                ]);
    };
    var statement = function (param) {
      var match = param[1];
      var loc = param[0];
      if (typeof match === "number") {
        if (match === 0) {
          return node("EmptyStatement", loc, /* array */[]);
        } else {
          return node("DebuggerStatement", loc, /* array */[]);
        }
      } else {
        switch (match.tag | 0) {
          case 0 : 
              return block(/* tuple */[
                          loc,
                          match[0]
                        ]);
          case 1 : 
              return node("ExpressionStatement", loc, /* array */[/* tuple */[
                            "expression",
                            expression(match[0][/* expression */0])
                          ]]);
          case 2 : 
              var _if = match[0];
              return node("IfStatement", loc, /* array */[
                          /* tuple */[
                            "test",
                            expression(_if[/* test */0])
                          ],
                          /* tuple */[
                            "consequent",
                            statement(_if[/* consequent */1])
                          ],
                          /* tuple */[
                            "alternate",
                            option(statement, _if[/* alternate */2])
                          ]
                        ]);
          case 3 : 
              var labeled = match[0];
              return node("LabeledStatement", loc, /* array */[
                          /* tuple */[
                            "label",
                            identifier(labeled[/* label */0])
                          ],
                          /* tuple */[
                            "body",
                            statement(labeled[/* body */1])
                          ]
                        ]);
          case 4 : 
              return node("BreakStatement", loc, /* array */[/* tuple */[
                            "label",
                            option(identifier, match[0][/* label */0])
                          ]]);
          case 5 : 
              return node("ContinueStatement", loc, /* array */[/* tuple */[
                            "label",
                            option(identifier, match[0][/* label */0])
                          ]]);
          case 6 : 
              var _with = match[0];
              return node("WithStatement", loc, /* array */[
                          /* tuple */[
                            "object",
                            expression(_with[/* _object */0])
                          ],
                          /* tuple */[
                            "body",
                            statement(_with[/* body */1])
                          ]
                        ]);
          case 7 : 
              return type_alias(/* tuple */[
                          loc,
                          match[0]
                        ]);
          case 8 : 
              var $$switch = match[0];
              return node("SwitchStatement", loc, /* array */[
                          /* tuple */[
                            "discriminant",
                            expression($$switch[/* discriminant */0])
                          ],
                          /* tuple */[
                            "cases",
                            array_of_list($$case, $$switch[/* cases */1])
                          ],
                          /* tuple */[
                            "lexical",
                            Curry._1(bool, $$switch[/* lexical */2])
                          ]
                        ]);
          case 9 : 
              return node("ReturnStatement", loc, /* array */[/* tuple */[
                            "argument",
                            option(expression, match[0][/* argument */0])
                          ]]);
          case 10 : 
              return node("ThrowStatement", loc, /* array */[/* tuple */[
                            "argument",
                            expression(match[0][/* argument */0])
                          ]]);
          case 11 : 
              var _try = match[0];
              return node("TryStatement", loc, /* array */[
                          /* tuple */[
                            "block",
                            block(_try[/* block */0])
                          ],
                          /* tuple */[
                            "handler",
                            option($$catch, _try[/* handler */1])
                          ],
                          /* tuple */[
                            "guardedHandlers",
                            array_of_list($$catch, _try[/* guardedHandlers */2])
                          ],
                          /* tuple */[
                            "finalizer",
                            option(block, _try[/* finalizer */3])
                          ]
                        ]);
          case 12 : 
              var _while = match[0];
              return node("WhileStatement", loc, /* array */[
                          /* tuple */[
                            "test",
                            expression(_while[/* test */0])
                          ],
                          /* tuple */[
                            "body",
                            statement(_while[/* body */1])
                          ]
                        ]);
          case 13 : 
              var dowhile = match[0];
              return node("DoWhileStatement", loc, /* array */[
                          /* tuple */[
                            "body",
                            statement(dowhile[/* body */0])
                          ],
                          /* tuple */[
                            "test",
                            expression(dowhile[/* test */1])
                          ]
                        ]);
          case 14 : 
              var _for = match[0];
              var init = function (param) {
                if (param.tag) {
                  return expression(param[0]);
                } else {
                  return variable_declaration(param[0]);
                }
              };
              return node("ForStatement", loc, /* array */[
                          /* tuple */[
                            "init",
                            option(init, _for[/* init */0])
                          ],
                          /* tuple */[
                            "test",
                            option(expression, _for[/* test */1])
                          ],
                          /* tuple */[
                            "update",
                            option(expression, _for[/* update */2])
                          ],
                          /* tuple */[
                            "body",
                            statement(_for[/* body */3])
                          ]
                        ]);
          case 15 : 
              var forin = match[0];
              var match$1 = forin[/* left */0];
              var left;
              left = match$1.tag ? expression(match$1[0]) : variable_declaration(match$1[0]);
              return node("ForInStatement", loc, /* array */[
                          /* tuple */[
                            "left",
                            left
                          ],
                          /* tuple */[
                            "right",
                            expression(forin[/* right */1])
                          ],
                          /* tuple */[
                            "body",
                            statement(forin[/* body */2])
                          ],
                          /* tuple */[
                            "each",
                            Curry._1(bool, forin[/* each */3])
                          ]
                        ]);
          case 16 : 
              var forof = match[0];
              var match$2 = forof[/* left */0];
              var left$1;
              left$1 = match$2.tag ? expression(match$2[0]) : variable_declaration(match$2[0]);
              return node("ForOfStatement", loc, /* array */[
                          /* tuple */[
                            "left",
                            left$1
                          ],
                          /* tuple */[
                            "right",
                            expression(forof[/* right */1])
                          ],
                          /* tuple */[
                            "body",
                            statement(forof[/* body */2])
                          ]
                        ]);
          case 17 : 
              var _let = match[0];
              return node("LetStatement", loc, /* array */[
                          /* tuple */[
                            "head",
                            array_of_list(let_assignment, _let[/* head */0])
                          ],
                          /* tuple */[
                            "body",
                            statement(_let[/* body */1])
                          ]
                        ]);
          case 18 : 
              var fn = match[0];
              var match$3 = fn[/* id */0];
              var match$4 = match$3 !== undefined ? /* tuple */[
                  "FunctionDeclaration",
                  identifier(match$3)
                ] : /* tuple */[
                  "FunctionExpression",
                  $$null
                ];
              var match$5 = fn[/* body */4];
              var body;
              body = match$5.tag ? expression(match$5[0]) : block(match$5[0]);
              return node(match$4[0], loc, /* array */[
                          /* tuple */[
                            "id",
                            match$4[1]
                          ],
                          /* tuple */[
                            "params",
                            array_of_list(pattern, fn[/* params */1])
                          ],
                          /* tuple */[
                            "defaults",
                            array_of_list((function (param) {
                                    return option(expression, param);
                                  }), fn[/* defaults */2])
                          ],
                          /* tuple */[
                            "rest",
                            option(identifier, fn[/* rest */3])
                          ],
                          /* tuple */[
                            "body",
                            body
                          ],
                          /* tuple */[
                            "async",
                            Curry._1(bool, fn[/* async */5])
                          ],
                          /* tuple */[
                            "generator",
                            Curry._1(bool, fn[/* generator */6])
                          ],
                          /* tuple */[
                            "expression",
                            Curry._1(bool, fn[/* expression */8])
                          ],
                          /* tuple */[
                            "returnType",
                            option(type_annotation, fn[/* returnType */9])
                          ],
                          /* tuple */[
                            "typeParameters",
                            option(type_parameter_declaration, fn[/* typeParameters */10])
                          ]
                        ]);
          case 19 : 
              return variable_declaration(/* tuple */[
                          loc,
                          match[0]
                        ]);
          case 20 : 
              var param$1 = /* tuple */[
                loc,
                match[0]
              ];
              var c = param$1[1];
              var match$6 = c[/* id */0];
              var match$7 = match$6 !== undefined ? /* tuple */[
                  "ClassDeclaration",
                  identifier(match$6)
                ] : /* tuple */[
                  "ClassExpression",
                  $$null
                ];
              return node(match$7[0], param$1[0], /* array */[
                          /* tuple */[
                            "id",
                            match$7[1]
                          ],
                          /* tuple */[
                            "body",
                            class_body(c[/* body */1])
                          ],
                          /* tuple */[
                            "superClass",
                            option(expression, c[/* superClass */2])
                          ],
                          /* tuple */[
                            "typeParameters",
                            option(type_parameter_declaration, c[/* typeParameters */3])
                          ],
                          /* tuple */[
                            "superTypeParameters",
                            option(type_parameter_instantiation, c[/* superTypeParameters */4])
                          ],
                          /* tuple */[
                            "implements",
                            array_of_list(class_implements, c[/* implements */5])
                          ],
                          /* tuple */[
                            "decorators",
                            array_of_list(expression, c[/* classDecorators */6])
                          ]
                        ]);
          case 21 : 
              return interface_declaration(/* tuple */[
                          loc,
                          match[0]
                        ]);
          case 22 : 
              return declare_variable(/* tuple */[
                          loc,
                          match[0]
                        ]);
          case 23 : 
              return declare_function(/* tuple */[
                          loc,
                          match[0]
                        ]);
          case 24 : 
              return declare_class(/* tuple */[
                          loc,
                          match[0]
                        ]);
          case 25 : 
              var m = match[0];
              var match$8 = m[/* id */0];
              var id;
              id = match$8.tag ? literal(match$8[0]) : identifier(match$8[0]);
              var match$9 = m[/* kind */2];
              var tmp;
              tmp = match$9.tag ? Curry._1(string, "ES") : Curry._1(string, "CommonJS");
              return node("DeclareModule", loc, /* array */[
                          /* tuple */[
                            "id",
                            id
                          ],
                          /* tuple */[
                            "body",
                            block(m[/* body */1])
                          ],
                          /* tuple */[
                            "kind",
                            tmp
                          ]
                        ]);
          case 26 : 
              return node("DeclareModuleExports", loc, /* array */[/* tuple */[
                            "typeAnnotation",
                            type_annotation(match[0])
                          ]]);
          case 27 : 
              var $$export = match[0];
              var match$10 = $$export[/* declaration */1];
              var declaration;
              if (match$10 !== undefined) {
                var match$11 = match$10;
                switch (match$11.tag | 0) {
                  case 0 : 
                      declaration = declare_variable(match$11[0]);
                      break;
                  case 1 : 
                      declaration = declare_function(match$11[0]);
                      break;
                  case 2 : 
                      declaration = declare_class(match$11[0]);
                      break;
                  case 3 : 
                      declaration = _type(match$11[0]);
                      break;
                  case 4 : 
                      declaration = type_alias(match$11[0]);
                      break;
                  case 5 : 
                      declaration = interface_declaration(match$11[0]);
                      break;
                  
                }
              } else {
                declaration = $$null;
              }
              return node("DeclareExportDeclaration", loc, /* array */[
                          /* tuple */[
                            "default",
                            Curry._1(bool, $$export[/* default */0])
                          ],
                          /* tuple */[
                            "declaration",
                            declaration
                          ],
                          /* tuple */[
                            "specifiers",
                            export_specifiers($$export[/* specifiers */2])
                          ],
                          /* tuple */[
                            "source",
                            option(literal, $$export[/* source */3])
                          ]
                        ]);
          case 28 : 
              var $$export$1 = match[0];
              var match$12 = $$export$1[/* declaration */1];
              var declaration$1;
              if (match$12 !== undefined) {
                var match$13 = match$12;
                declaration$1 = match$13.tag ? expression(match$13[0]) : statement(match$13[0]);
              } else {
                declaration$1 = $$null;
              }
              return node("ExportDeclaration", loc, /* array */[
                          /* tuple */[
                            "default",
                            Curry._1(bool, $$export$1[/* default */0])
                          ],
                          /* tuple */[
                            "declaration",
                            declaration$1
                          ],
                          /* tuple */[
                            "specifiers",
                            export_specifiers($$export$1[/* specifiers */2])
                          ],
                          /* tuple */[
                            "source",
                            option(literal, $$export$1[/* source */3])
                          ],
                          /* tuple */[
                            "exportKind",
                            Curry._1(string, $$export$1[/* exportKind */4] ? "value" : "type")
                          ]
                        ]);
          case 29 : 
              var $$import = match[0];
              var specifiers = List.map((function (param) {
                      switch (param.tag | 0) {
                        case 0 : 
                            var match = param[0];
                            var local_id = match[/* local */0];
                            var remote_id = match[/* remote */1];
                            var span_loc = local_id !== undefined ? btwn(remote_id[0], local_id[0]) : remote_id[0];
                            return node("ImportSpecifier", span_loc, /* array */[
                                        /* tuple */[
                                          "id",
                                          identifier(remote_id)
                                        ],
                                        /* tuple */[
                                          "name",
                                          option(identifier, local_id)
                                        ]
                                      ]);
                        case 1 : 
                            var id = param[0];
                            return node("ImportDefaultSpecifier", id[0], /* array */[/* tuple */[
                                          "id",
                                          identifier(id)
                                        ]]);
                        case 2 : 
                            var param$1 = param[0];
                            return node("ImportNamespaceSpecifier", param$1[0], /* array */[/* tuple */[
                                          "id",
                                          identifier(param$1[1])
                                        ]]);
                        
                      }
                    }), $$import[/* specifiers */2]);
              var match$14 = $$import[/* importKind */0];
              var import_kind;
              switch (match$14) {
                case 0 : 
                    import_kind = "type";
                    break;
                case 1 : 
                    import_kind = "typeof";
                    break;
                case 2 : 
                    import_kind = "value";
                    break;
                
              }
              return node("ImportDeclaration", loc, /* array */[
                          /* tuple */[
                            "specifiers",
                            Curry._1(array, $$Array.of_list(specifiers))
                          ],
                          /* tuple */[
                            "source",
                            literal($$import[/* source */1])
                          ],
                          /* tuple */[
                            "importKind",
                            Curry._1(string, import_kind)
                          ]
                        ]);
          
        }
      }
    };
    var jsx_opening_attribute = function (param) {
      if (param.tag) {
        var param$1 = param[0];
        return node("JSXSpreadAttribute", param$1[0], /* array */[/* tuple */[
                      "argument",
                      expression(param$1[1][/* argument */0])
                    ]]);
      } else {
        var param$2 = param[0];
        var attribute = param$2[1];
        var match = attribute[/* name */0];
        var name;
        name = match.tag ? jsx_namespaced_name(match[0]) : jsx_identifier(match[0]);
        return node("JSXAttribute", param$2[0], /* array */[
                    /* tuple */[
                      "name",
                      name
                    ],
                    /* tuple */[
                      "value",
                      option(jsx_attribute_value, attribute[/* value */1])
                    ]
                  ]);
      }
    };
    var jsx_namespaced_name = function (param) {
      var namespaced_name = param[1];
      return node("JSXNamespacedName", param[0], /* array */[
                  /* tuple */[
                    "namespace",
                    jsx_identifier(namespaced_name[/* namespace */0])
                  ],
                  /* tuple */[
                    "name",
                    jsx_identifier(namespaced_name[/* name */1])
                  ]
                ]);
    };
    var function_expression = function (param) {
      var _function = param[1];
      var match = _function[/* body */4];
      var body;
      body = match.tag ? expression(match[0]) : block(match[0]);
      return node("FunctionExpression", param[0], /* array */[
                  /* tuple */[
                    "id",
                    option(identifier, _function[/* id */0])
                  ],
                  /* tuple */[
                    "params",
                    array_of_list(pattern, _function[/* params */1])
                  ],
                  /* tuple */[
                    "defaults",
                    array_of_list((function (param) {
                            return option(expression, param);
                          }), _function[/* defaults */2])
                  ],
                  /* tuple */[
                    "rest",
                    option(identifier, _function[/* rest */3])
                  ],
                  /* tuple */[
                    "body",
                    body
                  ],
                  /* tuple */[
                    "async",
                    Curry._1(bool, _function[/* async */5])
                  ],
                  /* tuple */[
                    "generator",
                    Curry._1(bool, _function[/* generator */6])
                  ],
                  /* tuple */[
                    "expression",
                    Curry._1(bool, _function[/* expression */8])
                  ],
                  /* tuple */[
                    "returnType",
                    option(type_annotation, _function[/* returnType */9])
                  ],
                  /* tuple */[
                    "typeParameters",
                    option(type_parameter_declaration, _function[/* typeParameters */10])
                  ]
                ]);
    };
    var generic_type_qualified_identifier = function (param) {
      var q = param[1];
      var match = q[/* qualification */0];
      var qualification;
      qualification = match.tag ? generic_type_qualified_identifier(match[0]) : identifier(match[0]);
      return node("QualifiedTypeIdentifier", param[0], /* array */[
                  /* tuple */[
                    "qualification",
                    qualification
                  ],
                  /* tuple */[
                    "id",
                    identifier(q[/* id */1])
                  ]
                ]);
    };
    var type_parameter_instantiation = function (param) {
      return node("TypeParameterInstantiation", param[0], /* array */[/* tuple */[
                    "params",
                    array_of_list(_type, param[1][/* params */0])
                  ]]);
    };
    var interface_extends = function (param) {
      var g = param[1];
      var match = g[/* id */0];
      var id;
      id = match.tag ? generic_type_qualified_identifier(match[0]) : identifier(match[0]);
      return node("InterfaceExtends", param[0], /* array */[
                  /* tuple */[
                    "id",
                    id
                  ],
                  /* tuple */[
                    "typeParameters",
                    option(type_parameter_instantiation, g[/* typeParameters */1])
                  ]
                ]);
    };
    var object_type = function (param) {
      var o = param[1];
      return node("ObjectTypeAnnotation", param[0], /* array */[
                  /* tuple */[
                    "properties",
                    array_of_list(object_type_property, o[/* properties */0])
                  ],
                  /* tuple */[
                    "indexers",
                    array_of_list(object_type_indexer, o[/* indexers */1])
                  ],
                  /* tuple */[
                    "callProperties",
                    array_of_list(object_type_call_property, o[/* callProperties */2])
                  ]
                ]);
    };
    var export_specifier = function (param) {
      var specifier = param[1];
      return node("ExportSpecifier", param[0], /* array */[
                  /* tuple */[
                    "id",
                    identifier(specifier[/* id */0])
                  ],
                  /* tuple */[
                    "name",
                    option(identifier, specifier[/* name */1])
                  ]
                ]);
    };
    var class_body = function (param) {
      return node("ClassBody", param[0], /* array */[/* tuple */[
                    "body",
                    array_of_list(class_element, param[1][/* body */0])
                  ]]);
    };
    var class_implements = function (param) {
      var $$implements = param[1];
      return node("ClassImplements", param[0], /* array */[
                  /* tuple */[
                    "id",
                    identifier($$implements[/* id */0])
                  ],
                  /* tuple */[
                    "typeParameters",
                    option(type_parameter_instantiation, $$implements[/* typeParameters */1])
                  ]
                ]);
    };
    var function_type = function (param) {
      var fn = param[1];
      return node("FunctionTypeAnnotation", param[0], /* array */[
                  /* tuple */[
                    "params",
                    array_of_list(function_type_param, fn[/* params */0])
                  ],
                  /* tuple */[
                    "returnType",
                    _type(fn[/* returnType */1])
                  ],
                  /* tuple */[
                    "rest",
                    option(function_type_param, fn[/* rest */2])
                  ],
                  /* tuple */[
                    "typeParameters",
                    option(type_parameter_declaration, fn[/* typeParameters */3])
                  ]
                ]);
    };
    var class_element = function (param) {
      if (param.tag) {
        var param$1 = param[0];
        var prop = param$1[1];
        var match = prop[/* key */0];
        var match$1;
        switch (match.tag | 0) {
          case 0 : 
              match$1 = /* tuple */[
                literal(match[0]),
                false
              ];
              break;
          case 1 : 
              match$1 = /* tuple */[
                identifier(match[0]),
                false
              ];
              break;
          case 2 : 
              match$1 = /* tuple */[
                expression(match[0]),
                true
              ];
              break;
          
        }
        return node("ClassProperty", param$1[0], /* array */[
                    /* tuple */[
                      "key",
                      match$1[0]
                    ],
                    /* tuple */[
                      "value",
                      option(expression, prop[/* value */1])
                    ],
                    /* tuple */[
                      "typeAnnotation",
                      option(type_annotation, prop[/* typeAnnotation */2])
                    ],
                    /* tuple */[
                      "computed",
                      Curry._1(bool, match$1[1])
                    ],
                    /* tuple */[
                      "static",
                      Curry._1(bool, prop[/* static */3])
                    ]
                  ]);
      } else {
        var param$2 = param[0];
        var method_ = param$2[1];
        var key = method_[/* key */1];
        var match$2;
        switch (key.tag | 0) {
          case 0 : 
              match$2 = /* tuple */[
                literal(key[0]),
                false
              ];
              break;
          case 1 : 
              match$2 = /* tuple */[
                identifier(key[0]),
                false
              ];
              break;
          case 2 : 
              match$2 = /* tuple */[
                expression(key[0]),
                true
              ];
              break;
          
        }
        var kind;
        switch (method_[/* kind */0]) {
          case 0 : 
              kind = "constructor";
              break;
          case 1 : 
              kind = "method";
              break;
          case 2 : 
              kind = "get";
              break;
          case 3 : 
              kind = "set";
              break;
          
        }
        return node("MethodDefinition", param$2[0], /* array */[
                    /* tuple */[
                      "key",
                      match$2[0]
                    ],
                    /* tuple */[
                      "value",
                      function_expression(method_[/* value */2])
                    ],
                    /* tuple */[
                      "kind",
                      Curry._1(string, kind)
                    ],
                    /* tuple */[
                      "static",
                      Curry._1(bool, method_[/* static */3])
                    ],
                    /* tuple */[
                      "computed",
                      Curry._1(bool, match$2[1])
                    ],
                    /* tuple */[
                      "decorators",
                      array_of_list(expression, method_[/* decorators */4])
                    ]
                  ]);
      }
    };
    var template_literal = function (param) {
      var value = param[1];
      return node("TemplateLiteral", param[0], /* array */[
                  /* tuple */[
                    "quasis",
                    array_of_list(template_element, value[/* quasis */0])
                  ],
                  /* tuple */[
                    "expressions",
                    array_of_list(expression, value[/* expressions */1])
                  ]
                ]);
    };
    var comprehension_block = function (param) {
      var b = param[1];
      return node("ComprehensionBlock", param[0], /* array */[
                  /* tuple */[
                    "left",
                    pattern(b[/* left */0])
                  ],
                  /* tuple */[
                    "right",
                    expression(b[/* right */1])
                  ],
                  /* tuple */[
                    "each",
                    Curry._1(bool, b[/* each */2])
                  ]
                ]);
    };
    var expression_or_spread = function (param) {
      if (param.tag) {
        var match = param[0];
        return node("SpreadElement", match[0], /* array */[/* tuple */[
                      "argument",
                      expression(match[1][/* argument */0])
                    ]]);
      } else {
        return expression(param[0]);
      }
    };
    var jsx_element = function (param) {
      var element = param[1];
      return node("JSXElement", param[0], /* array */[
                  /* tuple */[
                    "openingElement",
                    jsx_opening(element[/* openingElement */0])
                  ],
                  /* tuple */[
                    "closingElement",
                    option(jsx_closing, element[/* closingElement */1])
                  ],
                  /* tuple */[
                    "children",
                    array_of_list(jsx_child, element[/* children */2])
                  ]
                ]);
    };
    var let_assignment = function (assignment) {
      return Curry._1(obj, /* array */[
                  /* tuple */[
                    "id",
                    pattern(assignment[/* id */0])
                  ],
                  /* tuple */[
                    "init",
                    option(expression, assignment[/* init */1])
                  ]
                ]);
    };
    var object_property = function (param) {
      if (param.tag) {
        var match = param[0];
        return node("SpreadProperty", match[0], /* array */[/* tuple */[
                      "argument",
                      expression(match[1][/* argument */0])
                    ]]);
      } else {
        var match$1 = param[0];
        var prop = match$1[1];
        var match$2 = prop[/* key */0];
        var match$3;
        switch (match$2.tag | 0) {
          case 0 : 
              match$3 = /* tuple */[
                literal(match$2[0]),
                false
              ];
              break;
          case 1 : 
              match$3 = /* tuple */[
                identifier(match$2[0]),
                false
              ];
              break;
          case 2 : 
              match$3 = /* tuple */[
                expression(match$2[0]),
                true
              ];
              break;
          
        }
        var match$4 = prop[/* kind */2];
        var kind;
        switch (match$4) {
          case 0 : 
              kind = "init";
              break;
          case 1 : 
              kind = "get";
              break;
          case 2 : 
              kind = "set";
              break;
          
        }
        return node("Property", match$1[0], /* array */[
                    /* tuple */[
                      "key",
                      match$3[0]
                    ],
                    /* tuple */[
                      "value",
                      expression(prop[/* value */1])
                    ],
                    /* tuple */[
                      "kind",
                      Curry._1(string, kind)
                    ],
                    /* tuple */[
                      "method",
                      Curry._1(bool, prop[/* _method */3])
                    ],
                    /* tuple */[
                      "shorthand",
                      Curry._1(bool, prop[/* shorthand */4])
                    ],
                    /* tuple */[
                      "computed",
                      Curry._1(bool, match$3[1])
                    ]
                  ]);
      }
    };
    var jsx_expression_container = function (param) {
      var match = param[1][/* expression */0];
      var expression$1;
      expression$1 = match.tag ? node("JSXEmptyExpression", match[0], /* array */[]) : expression(match[0]);
      return node("JSXExpressionContainer", param[0], /* array */[/* tuple */[
                    "expression",
                    expression$1
                  ]]);
    };
    var jsx_attribute_value = function (param) {
      if (param.tag) {
        return jsx_expression_container(/* tuple */[
                    param[0],
                    param[1]
                  ]);
      } else {
        return literal(/* tuple */[
                    param[0],
                    param[1]
                  ]);
      }
    };
    var object_pattern_property = function (param) {
      if (param.tag) {
        var match = param[0];
        return node("SpreadPropertyPattern", match[0], /* array */[/* tuple */[
                      "argument",
                      pattern(match[1][/* argument */0])
                    ]]);
      } else {
        var match$1 = param[0];
        var prop = match$1[1];
        var match$2 = prop[/* key */0];
        var match$3;
        switch (match$2.tag | 0) {
          case 0 : 
              match$3 = /* tuple */[
                literal(match$2[0]),
                false
              ];
              break;
          case 1 : 
              match$3 = /* tuple */[
                identifier(match$2[0]),
                false
              ];
              break;
          case 2 : 
              match$3 = /* tuple */[
                expression(match$2[0]),
                true
              ];
              break;
          
        }
        return node("PropertyPattern", match$1[0], /* array */[
                    /* tuple */[
                      "key",
                      match$3[0]
                    ],
                    /* tuple */[
                      "pattern",
                      pattern(prop[/* pattern */1])
                    ],
                    /* tuple */[
                      "computed",
                      Curry._1(bool, match$3[1])
                    ],
                    /* tuple */[
                      "shorthand",
                      Curry._1(bool, prop[/* shorthand */2])
                    ]
                  ]);
      }
    };
    var array_pattern_element = function (param) {
      if (param.tag) {
        var match = param[0];
        return node("SpreadElementPattern", match[0], /* array */[/* tuple */[
                      "argument",
                      pattern(match[1][/* argument */0])
                    ]]);
      } else {
        return pattern(param[0]);
      }
    };
    var object_type_indexer = function (param) {
      var indexer = param[1];
      return node("ObjectTypeIndexer", param[0], /* array */[
                  /* tuple */[
                    "id",
                    identifier(indexer[/* id */0])
                  ],
                  /* tuple */[
                    "key",
                    _type(indexer[/* key */1])
                  ],
                  /* tuple */[
                    "value",
                    _type(indexer[/* value */2])
                  ],
                  /* tuple */[
                    "static",
                    Curry._1(bool, indexer[/* static */3])
                  ]
                ]);
    };
    var object_type_call_property = function (param) {
      var callProperty = param[1];
      return node("ObjectTypeCallProperty", param[0], /* array */[
                  /* tuple */[
                    "value",
                    function_type(callProperty[/* value */0])
                  ],
                  /* tuple */[
                    "static",
                    Curry._1(bool, callProperty[/* static */1])
                  ]
                ]);
    };
    var object_type_property = function (param) {
      var prop = param[1];
      var match = prop[/* key */0];
      var key;
      switch (match.tag | 0) {
        case 0 : 
            key = literal(match[0]);
            break;
        case 1 : 
            key = identifier(match[0]);
            break;
        case 2 : 
            throw [
                  Caml_builtin_exceptions.failure,
                  "There should not be computed object type property keys"
                ];
        
      }
      return node("ObjectTypeProperty", param[0], /* array */[
                  /* tuple */[
                    "key",
                    key
                  ],
                  /* tuple */[
                    "value",
                    _type(prop[/* value */1])
                  ],
                  /* tuple */[
                    "optional",
                    Curry._1(bool, prop[/* optional */2])
                  ],
                  /* tuple */[
                    "static",
                    Curry._1(bool, prop[/* static */3])
                  ]
                ]);
    };
    var $$case = function (param) {
      var c = param[1];
      return node("SwitchCase", param[0], /* array */[
                  /* tuple */[
                    "test",
                    option(expression, c[/* test */0])
                  ],
                  /* tuple */[
                    "consequent",
                    array_of_list(statement, c[/* consequent */1])
                  ]
                ]);
    };
    var type_alias = function (param) {
      var alias = param[1];
      return node("TypeAlias", param[0], /* array */[
                  /* tuple */[
                    "id",
                    identifier(alias[/* id */0])
                  ],
                  /* tuple */[
                    "typeParameters",
                    option(type_parameter_declaration, alias[/* typeParameters */1])
                  ],
                  /* tuple */[
                    "right",
                    _type(alias[/* right */2])
                  ]
                ]);
    };
    var declare_variable = function (param) {
      return node("DeclareVariable", param[0], /* array */[/* tuple */[
                    "id",
                    identifier(param[1][/* id */0])
                  ]]);
    };
    var interface_declaration = function (param) {
      var i = param[1];
      return node("InterfaceDeclaration", param[0], /* array */[
                  /* tuple */[
                    "id",
                    identifier(i[/* id */0])
                  ],
                  /* tuple */[
                    "typeParameters",
                    option(type_parameter_declaration, i[/* typeParameters */1])
                  ],
                  /* tuple */[
                    "body",
                    object_type(i[/* body */2])
                  ],
                  /* tuple */[
                    "extends",
                    array_of_list(interface_extends, i[/* extends */3])
                  ]
                ]);
    };
    var variable_declaration = function (param) {
      var $$var = param[1];
      var match = $$var[/* kind */1];
      var kind;
      switch (match) {
        case 0 : 
            kind = "var";
            break;
        case 1 : 
            kind = "let";
            break;
        case 2 : 
            kind = "const";
            break;
        
      }
      return node("VariableDeclaration", param[0], /* array */[
                  /* tuple */[
                    "declarations",
                    array_of_list(variable_declarator, $$var[/* declarations */0])
                  ],
                  /* tuple */[
                    "kind",
                    Curry._1(string, kind)
                  ]
                ]);
    };
    var $$catch = function (param) {
      var c = param[1];
      return node("CatchClause", param[0], /* array */[
                  /* tuple */[
                    "param",
                    pattern(c[/* param */0])
                  ],
                  /* tuple */[
                    "guard",
                    option(expression, c[/* guard */1])
                  ],
                  /* tuple */[
                    "body",
                    block(c[/* body */2])
                  ]
                ]);
    };
    var declare_class = function (param) {
      var d = param[1];
      return node("DeclareClass", param[0], /* array */[
                  /* tuple */[
                    "id",
                    identifier(d[/* id */0])
                  ],
                  /* tuple */[
                    "typeParameters",
                    option(type_parameter_declaration, d[/* typeParameters */1])
                  ],
                  /* tuple */[
                    "body",
                    object_type(d[/* body */2])
                  ],
                  /* tuple */[
                    "extends",
                    array_of_list(interface_extends, d[/* extends */3])
                  ]
                ]);
    };
    var export_specifiers = function (param) {
      if (param !== undefined) {
        var match = param;
        if (match.tag) {
          return Curry._1(array, /* array */[node("ExportBatchSpecifier", match[0], /* array */[/* tuple */[
                              "name",
                              option(identifier, match[1])
                            ]])]);
        } else {
          return array_of_list(export_specifier, match[0]);
        }
      } else {
        return Curry._1(array, /* array */[]);
      }
    };
    var declare_function = function (param) {
      return node("DeclareFunction", param[0], /* array */[/* tuple */[
                    "id",
                    identifier(param[1][/* id */0])
                  ]]);
    };
    var comment = function (param) {
      var c = param[1];
      var match;
      match = c.tag ? /* tuple */[
          "Line",
          c[0]
        ] : /* tuple */[
          "Block",
          c[0]
        ];
      return node(match[0], param[0], /* array */[/* tuple */[
                    "value",
                    Curry._1(string, match[1])
                  ]]);
    };
    var jsx_opening = function (param) {
      var opening = param[1];
      return node("JSXOpeningElement", param[0], /* array */[
                  /* tuple */[
                    "name",
                    jsx_name(opening[/* name */0])
                  ],
                  /* tuple */[
                    "attributes",
                    array_of_list(jsx_opening_attribute, opening[/* attributes */2])
                  ],
                  /* tuple */[
                    "selfClosing",
                    Curry._1(bool, opening[/* selfClosing */1])
                  ]
                ]);
    };
    var jsx_closing = function (param) {
      return node("JSXClosingElement", param[0], /* array */[/* tuple */[
                    "name",
                    jsx_name(param[1][/* name */0])
                  ]]);
    };
    var jsx_child = function (param) {
      var match = param[1];
      var loc = param[0];
      switch (match.tag | 0) {
        case 0 : 
            return jsx_element(/* tuple */[
                        loc,
                        match[0]
                      ]);
        case 1 : 
            return jsx_expression_container(/* tuple */[
                        loc,
                        match[0]
                      ]);
        case 2 : 
            var param$1 = /* tuple */[
              loc,
              match[0]
            ];
            var text = param$1[1];
            return node("JSXText", param$1[0], /* array */[
                        /* tuple */[
                          "value",
                          Curry._1(string, text[/* value */0])
                        ],
                        /* tuple */[
                          "raw",
                          Curry._1(string, text[/* raw */1])
                        ]
                      ]);
        
      }
    };
    var program$2 = function (param) {
      return node("Program", param[0], /* array */[
                  /* tuple */[
                    "body",
                    array_of_list(statement, param[1])
                  ],
                  /* tuple */[
                    "comments",
                    array_of_list(comment, param[2])
                  ]
                ]);
    };
    var ret = program$2(match[0]);
    var translation_errors$1 = translation_errors[0];
    ret["errors"] = errors(Pervasives.$at(match[1], translation_errors$1));
    return ret;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn[0] === $$Error) {
      var e = new Error(String(List.length(exn[1])) + " errors");
      e["name"] = "Parse Error";
      throw(e);
      return ({});
    } else {
      throw exn;
    }
  }
}

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

var match = typeof __dirname === "undefined" ? undefined : __dirname;

if (match !== undefined) {
  var f = Path.join(match, "flow_parser_sample.js");
  var v = parse(Fs.readFileSync(f, "utf8"), undefined);
  eq("File \"runParser.ml\", line 14, characters 7-14", /* tuple */[
        0,
        2842
      ], v.range);
} else {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "runParser.ml",
          15,
          12
        ]
      ];
}

Mt.from_pair_suites("Flow_parser_reg_test", suites[0]);

/* Literal Not a pure module */
