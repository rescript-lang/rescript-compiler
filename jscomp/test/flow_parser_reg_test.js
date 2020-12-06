'use strict';

var Mt = require("./mt.js");
var Fs = require("fs");
var Sys = require("../../lib/js/sys.js");
var Char = require("../../lib/js/char.js");
var List = require("../../lib/js/list.js");
var Path = require("path");
var $$Array = require("../../lib/js/array.js");
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

var none = {
  source: undefined,
  start: {
    line: 0,
    column: 0,
    offset: 0
  },
  _end: {
    line: 0,
    column: 0,
    offset: 0
  }
};

function from_lb_p(source, start, _end) {
  return {
          source: source,
          start: {
            line: start.pos_lnum,
            column: start.pos_cnum - start.pos_bol | 0,
            offset: start.pos_cnum
          },
          _end: {
            line: _end.pos_lnum,
            column: Caml_primitive.caml_int_max(0, _end.pos_cnum - _end.pos_bol | 0),
            offset: _end.pos_cnum
          }
        };
}

function from_lb(source, lb) {
  var start = lb.lex_start_p;
  var _end = lb.lex_curr_p;
  return from_lb_p(source, start, _end);
}

function from_curr_lb(source, lb) {
  var curr = lb.lex_curr_p;
  return from_lb_p(source, curr, curr);
}

function btwn(loc1, loc2) {
  return {
          source: loc1.source,
          start: loc1.start,
          _end: loc2._end
        };
}

function btwn_exclusive(loc1, loc2) {
  return {
          source: loc1.source,
          start: loc1._end,
          _end: loc2.start
        };
}

function string_of_filename(param) {
  if (typeof param === "number") {
    return "(global)";
  } else {
    return param._0;
  }
}

function order_of_filename(param) {
  if (typeof param === "number") {
    return 1;
  }
  switch (param.TAG | 0) {
    case /* LibFile */0 :
        return 2;
    case /* SourceFile */1 :
    case /* JsonFile */2 :
        return 3;
    
  }
}

function source_cmp(a, b) {
  if (a === undefined) {
    if (b !== undefined) {
      return 1;
    } else {
      return 0;
    }
  }
  if (b === undefined) {
    return -1;
  }
  var k = order_of_filename(a) - order_of_filename(b) | 0;
  if (k !== 0) {
    return k;
  } else {
    return Caml_primitive.caml_string_compare(string_of_filename(a), string_of_filename(b));
  }
}

function pos_cmp(a, b) {
  return Caml_obj.caml_compare([
              a.line,
              a.column
            ], [
              b.line,
              b.column
            ]);
}

function compare(loc1, loc2) {
  var k = source_cmp(loc1.source, loc2.source);
  if (k !== 0) {
    return k;
  }
  var k$1 = pos_cmp(loc1.start, loc2.start);
  if (k$1 === 0) {
    return pos_cmp(loc1._end, loc2._end);
  } else {
    return k$1;
  }
}

var $$Error = Caml_exceptions.create("Flow_parser_reg_test.Parse_error.Error");

function error(str) {
  if (typeof str === "number") {
    switch (str) {
      case /* UnexpectedNumber */0 :
          return "Unexpected number";
      case /* UnexpectedString */1 :
          return "Unexpected string";
      case /* UnexpectedIdentifier */2 :
          return "Unexpected identifier";
      case /* UnexpectedReserved */3 :
          return "Unexpected reserved word";
      case /* UnexpectedEOS */4 :
          return "Unexpected end of input";
      case /* UnexpectedTypeAlias */5 :
          return "Type aliases are not allowed in untyped mode";
      case /* UnexpectedTypeAnnotation */6 :
          return "Type annotations are not allowed in untyped mode";
      case /* UnexpectedTypeDeclaration */7 :
          return "Type declarations are not allowed in untyped mode";
      case /* UnexpectedTypeImport */8 :
          return "Type imports are not allowed in untyped mode";
      case /* UnexpectedTypeExport */9 :
          return "Type exports are not allowed in untyped mode";
      case /* UnexpectedTypeInterface */10 :
          return "Interfaces are not allowed in untyped mode";
      case /* NewlineAfterThrow */11 :
          return "Illegal newline after throw";
      case /* InvalidRegExp */12 :
          return "Invalid regular expression";
      case /* UnterminatedRegExp */13 :
          return "Invalid regular expression: missing /";
      case /* InvalidLHSInAssignment */14 :
          return "Invalid left-hand side in assignment";
      case /* InvalidLHSInExponentiation */15 :
          return "Invalid left-hand side in exponentiation expression";
      case /* InvalidLHSInForIn */16 :
          return "Invalid left-hand side in for-in";
      case /* InvalidLHSInForOf */17 :
          return "Invalid left-hand side in for-of";
      case /* ExpectedPatternFoundExpression */18 :
          return "Expected an object pattern, array pattern, or an identifier but found an expression instead";
      case /* MultipleDefaultsInSwitch */19 :
          return "More than one default clause in switch statement";
      case /* NoCatchOrFinally */20 :
          return "Missing catch or finally after try";
      case /* IllegalContinue */21 :
          return "Illegal continue statement";
      case /* IllegalBreak */22 :
          return "Illegal break statement";
      case /* IllegalReturn */23 :
          return "Illegal return statement";
      case /* IllegalYield */24 :
          return "Illegal yield expression";
      case /* StrictModeWith */25 :
          return "Strict mode code may not include a with statement";
      case /* StrictCatchVariable */26 :
          return "Catch variable may not be eval or arguments in strict mode";
      case /* StrictVarName */27 :
          return "Variable name may not be eval or arguments in strict mode";
      case /* StrictParamName */28 :
          return "Parameter name eval or arguments is not allowed in strict mode";
      case /* StrictParamDupe */29 :
          return "Strict mode function may not have duplicate parameter names";
      case /* StrictFunctionName */30 :
          return "Function name may not be eval or arguments in strict mode";
      case /* StrictOctalLiteral */31 :
          return "Octal literals are not allowed in strict mode.";
      case /* StrictDelete */32 :
          return "Delete of an unqualified identifier in strict mode.";
      case /* StrictDuplicateProperty */33 :
          return "Duplicate data property in object literal not allowed in strict mode";
      case /* AccessorDataProperty */34 :
          return "Object literal may not have data and accessor property with the same name";
      case /* AccessorGetSet */35 :
          return "Object literal may not have multiple get/set accessors with the same name";
      case /* StrictLHSAssignment */36 :
          return "Assignment to eval or arguments is not allowed in strict mode";
      case /* StrictLHSPostfix */37 :
          return "Postfix increment/decrement may not have eval or arguments operand in strict mode";
      case /* StrictLHSPrefix */38 :
          return "Prefix increment/decrement may not have eval or arguments operand in strict mode";
      case /* StrictReservedWord */39 :
          return "Use of future reserved word in strict mode";
      case /* JSXAttributeValueEmptyExpression */40 :
          return "JSX attributes must only be assigned a non-empty expression";
      case /* InvalidJSXAttributeValue */41 :
          return "JSX value should be either an expression or a quoted JSX text";
      case /* NoUninitializedConst */42 :
          return "Const must be initialized";
      case /* NoUninitializedDestructuring */43 :
          return "Destructuring assignment must be initialized";
      case /* NewlineBeforeArrow */44 :
          return "Illegal newline before arrow";
      case /* StrictFunctionStatement */45 :
          return "In strict mode code, functions can only be declared at top level or immediately within another function.";
      case /* AdjacentJSXElements */46 :
          return "Unexpected token <. Remember, adjacent JSX elements must be wrapped in an enclosing parent tag";
      case /* ParameterAfterRestParameter */47 :
          return "Rest parameter must be final parameter of an argument list";
      case /* AsyncGenerator */48 :
          return "A function may not be both async and a generator";
      case /* DeclareAsync */49 :
          return "async is an implementation detail and isn't necessary for your declare function statement. It is sufficient for your declare function to just have a Promise return type.";
      case /* DeclareExportLet */50 :
          return "`declare export let` is not supported. Use `declare export var` instead.";
      case /* DeclareExportConst */51 :
          return "`declare export const` is not supported. Use `declare export var` instead.";
      case /* DeclareExportType */52 :
          return "`declare export type` is not supported. Use `export type` instead.";
      case /* DeclareExportInterface */53 :
          return "`declare export interface` is not supported. Use `export interface` instead.";
      case /* UnexpectedExportStarAs */54 :
          return "`export * as` is an early-stage proposal and is not enabled by default. To enable support in the parser, use the `esproposal_export_star_as` option";
      case /* ExportNamelessClass */55 :
          return "When exporting a class as a named export, you must specify a class name. Did you mean `export default class ...`?";
      case /* ExportNamelessFunction */56 :
          return "When exporting a function as a named export, you must specify a function name. Did you mean `export default function ...`?";
      case /* UnsupportedDecorator */57 :
          return "Found a decorator in an unsupported position.";
      case /* MissingTypeParamDefault */58 :
          return "Type parameter declaration needs a default, since a preceding type parameter declaration has a default.";
      case /* WindowsFloatOfString */59 :
          return "The Windows version of OCaml has a bug in how it parses hexidecimal numbers. It is fixed in OCaml 4.03.0. Until we can switch to 4.03.0, please avoid either hexidecimal notation or Windows.";
      case /* DuplicateDeclareModuleExports */60 :
          return "Duplicate `declare module.exports` statement!";
      case /* AmbiguousDeclareModuleKind */61 :
          return "Found both `declare module.exports` and `declare export` in the same module. Modules can only have 1 since they are either an ES module xor they are a CommonJS module.";
      
    }
  } else {
    switch (str.TAG | 0) {
      case /* Assertion */0 :
          return "Unexpected parser state: " + str._0;
      case /* UnexpectedToken */1 :
          return "Unexpected token " + str._0;
      case /* UnexpectedTokenWithSuggestion */2 :
          return Curry._2(Printf.sprintf(/* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "Unexpected token `",
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* String_literal */11,
                                _0: "`. Did you mean `",
                                _1: {
                                  TAG: /* String */2,
                                  _0: /* No_padding */0,
                                  _1: {
                                    TAG: /* String_literal */11,
                                    _0: "`?",
                                    _1: /* End_of_format */0
                                  }
                                }
                              }
                            }
                          },
                          _1: "Unexpected token `%s`. Did you mean `%s`?"
                        }), str._0, str._1);
      case /* InvalidRegExpFlags */3 :
          return "Invalid flags supplied to RegExp constructor '" + (str._0 + "'");
      case /* UnknownLabel */4 :
          return "Undefined label '" + (str._0 + "'");
      case /* Redeclaration */5 :
          return str._0 + (" '" + (str._1 + "' has already been declared"));
      case /* ExpectedJSXClosingTag */6 :
          return "Expected corresponding JSX closing tag for " + str._0;
      case /* DuplicateExport */7 :
          return Curry._1(Printf.sprintf(/* Format */{
                          _0: {
                            TAG: /* String_literal */11,
                            _0: "Duplicate export for `",
                            _1: {
                              TAG: /* String */2,
                              _0: /* No_padding */0,
                              _1: {
                                TAG: /* Char_literal */12,
                                _0: /* "`" */96,
                                _1: /* End_of_format */0
                              }
                            }
                          },
                          _1: "Duplicate export for `%s`"
                        }), str._0);
      
    }
  }
}

var Literal = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      44,
      6
    ], {
      TAG: /* Module */0,
      _0: [[
          {
            TAG: /* Module */0,
            _0: []
          },
          "RegExp"
        ]]
    });

var Type = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      191,
      6
    ], {
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Param"
              ]]
          },
          "Function"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Property"
              ],
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Indexer"
              ],
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "CallProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Identifier"
              ]]
          },
          "Generic"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "StringLiteral"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "NumberLiteral"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "BooleanLiteral"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: [[
                      {
                        TAG: /* Module */0,
                        _0: []
                      },
                      "Variance"
                    ]]
                },
                "TypeParam"
              ]]
          },
          "ParameterDeclaration"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "ParameterInstantiation"
        ]
      ]
    });

var Statement = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      493,
      6
    ], {
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Block"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "If"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Labeled"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Break"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Continue"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "With"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "TypeAlias"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Case"
              ]]
          },
          "Switch"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Return"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Throw"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "CatchClause"
              ]]
          },
          "Try"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Declarator"
              ]]
          },
          "VariableDeclaration"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "While"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "DoWhile"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "For"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "ForIn"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "ForOf"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Let"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Interface"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "DeclareVariable"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "DeclareFunction"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "DeclareModule"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Specifier"
              ]]
          },
          "ExportDeclaration"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "DeclareExportDeclaration"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "NamedSpecifier"
              ]]
          },
          "ImportDeclaration"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Expression"
        ]
      ]
    });

var Expression = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      758,
      6
    ], {
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "SpreadElement"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Array"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Element"
              ]]
          },
          "TemplateLiteral"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "TaggedTemplate"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Property"
              ],
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "SpreadProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Sequence"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Unary"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Binary"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Assignment"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Update"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Logical"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Conditional"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "New"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Call"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Member"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Yield"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Block"
              ]]
          },
          "Comprehension"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Generator"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Let"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "TypeCast"
        ]
      ]
    });

var JSX = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      861,
      6
    ], {
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Identifier"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "NamespacedName"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "ExpressionContainer"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Text"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Attribute"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "SpreadAttribute"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "MemberExpression"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Opening"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Closing"
        ]
      ]
    });

var Pattern = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      919,
      6
    ], {
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: [
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Property"
              ],
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "SpreadProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "SpreadElement"
              ]]
          },
          "Array"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Assignment"
        ]
      ]
    });

var Class = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      978,
      6
    ], {
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Method"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Property"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Implements"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Body"
        ]
      ]
    });

Caml_module.update_mod({
      TAG: /* Module */0,
      _0: [[
          {
            TAG: /* Module */0,
            _0: []
          },
          "RegExp"
        ]]
    }, Literal, Literal);

Caml_module.update_mod({
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Param"
              ]]
          },
          "Function"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Property"
              ],
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Indexer"
              ],
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "CallProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Identifier"
              ]]
          },
          "Generic"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "StringLiteral"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "NumberLiteral"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "BooleanLiteral"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: [[
                      {
                        TAG: /* Module */0,
                        _0: []
                      },
                      "Variance"
                    ]]
                },
                "TypeParam"
              ]]
          },
          "ParameterDeclaration"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "ParameterInstantiation"
        ]
      ]
    }, Type, Type);

Caml_module.update_mod({
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Block"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "If"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Labeled"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Break"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Continue"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "With"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "TypeAlias"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Case"
              ]]
          },
          "Switch"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Return"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Throw"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "CatchClause"
              ]]
          },
          "Try"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Declarator"
              ]]
          },
          "VariableDeclaration"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "While"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "DoWhile"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "For"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "ForIn"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "ForOf"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Let"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Interface"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "DeclareVariable"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "DeclareFunction"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "DeclareModule"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Specifier"
              ]]
          },
          "ExportDeclaration"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "DeclareExportDeclaration"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "NamedSpecifier"
              ]]
          },
          "ImportDeclaration"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Expression"
        ]
      ]
    }, Statement, Statement);

Caml_module.update_mod({
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "SpreadElement"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Array"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Element"
              ]]
          },
          "TemplateLiteral"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "TaggedTemplate"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Property"
              ],
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "SpreadProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Sequence"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Unary"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Binary"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Assignment"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Update"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Logical"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Conditional"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "New"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Call"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Member"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Yield"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Block"
              ]]
          },
          "Comprehension"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Generator"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Let"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "TypeCast"
        ]
      ]
    }, Expression, Expression);

Caml_module.update_mod({
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Identifier"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "NamespacedName"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "ExpressionContainer"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Text"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Attribute"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "SpreadAttribute"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "MemberExpression"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Opening"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Closing"
        ]
      ]
    }, JSX, JSX);

Caml_module.update_mod({
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: [
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "Property"
              ],
              [
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "SpreadProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          {
            TAG: /* Module */0,
            _0: [[
                {
                  TAG: /* Module */0,
                  _0: []
                },
                "SpreadElement"
              ]]
          },
          "Array"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Assignment"
        ]
      ]
    }, Pattern, Pattern);

Caml_module.update_mod({
      TAG: /* Module */0,
      _0: [
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Method"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Property"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Implements"
        ],
        [
          {
            TAG: /* Module */0,
            _0: []
          },
          "Body"
        ]
      ]
    }, Class, Class);

function token_to_string(param) {
  if (typeof param === "number") {
    switch (param) {
      case /* T_IDENTIFIER */0 :
          return "T_IDENTIFIER";
      case /* T_LCURLY */1 :
          return "T_LCURLY";
      case /* T_RCURLY */2 :
          return "T_RCURLY";
      case /* T_LPAREN */3 :
          return "T_LPAREN";
      case /* T_RPAREN */4 :
          return "T_RPAREN";
      case /* T_LBRACKET */5 :
          return "T_LBRACKET";
      case /* T_RBRACKET */6 :
          return "T_RBRACKET";
      case /* T_SEMICOLON */7 :
          return "T_SEMICOLON";
      case /* T_COMMA */8 :
          return "T_COMMA";
      case /* T_PERIOD */9 :
          return "T_PERIOD";
      case /* T_ARROW */10 :
          return "T_ARROW";
      case /* T_ELLIPSIS */11 :
          return "T_ELLIPSIS";
      case /* T_AT */12 :
          return "T_AT";
      case /* T_FUNCTION */13 :
          return "T_FUNCTION";
      case /* T_IF */14 :
          return "T_IF";
      case /* T_IN */15 :
          return "T_IN";
      case /* T_INSTANCEOF */16 :
          return "T_INSTANCEOF";
      case /* T_RETURN */17 :
          return "T_RETURN";
      case /* T_SWITCH */18 :
          return "T_SWITCH";
      case /* T_THIS */19 :
          return "T_THIS";
      case /* T_THROW */20 :
          return "T_THROW";
      case /* T_TRY */21 :
          return "T_TRY";
      case /* T_VAR */22 :
          return "T_VAR";
      case /* T_WHILE */23 :
          return "T_WHILE";
      case /* T_WITH */24 :
          return "T_WITH";
      case /* T_CONST */25 :
          return "T_CONST";
      case /* T_LET */26 :
          return "T_LET";
      case /* T_NULL */27 :
          return "T_NULL";
      case /* T_FALSE */28 :
          return "T_FALSE";
      case /* T_TRUE */29 :
          return "T_TRUE";
      case /* T_BREAK */30 :
          return "T_BREAK";
      case /* T_CASE */31 :
          return "T_CASE";
      case /* T_CATCH */32 :
          return "T_CATCH";
      case /* T_CONTINUE */33 :
          return "T_CONTINUE";
      case /* T_DEFAULT */34 :
          return "T_DEFAULT";
      case /* T_DO */35 :
          return "T_DO";
      case /* T_FINALLY */36 :
          return "T_FINALLY";
      case /* T_FOR */37 :
          return "T_FOR";
      case /* T_CLASS */38 :
          return "T_CLASS";
      case /* T_EXTENDS */39 :
          return "T_EXTENDS";
      case /* T_STATIC */40 :
          return "T_STATIC";
      case /* T_ELSE */41 :
          return "T_ELSE";
      case /* T_NEW */42 :
          return "T_NEW";
      case /* T_DELETE */43 :
          return "T_DELETE";
      case /* T_TYPEOF */44 :
          return "T_TYPEOF";
      case /* T_VOID */45 :
          return "T_VOID";
      case /* T_ENUM */46 :
          return "T_ENUM";
      case /* T_EXPORT */47 :
          return "T_EXPORT";
      case /* T_IMPORT */48 :
          return "T_IMPORT";
      case /* T_SUPER */49 :
          return "T_SUPER";
      case /* T_IMPLEMENTS */50 :
          return "T_IMPLEMENTS";
      case /* T_INTERFACE */51 :
          return "T_INTERFACE";
      case /* T_PACKAGE */52 :
          return "T_PACKAGE";
      case /* T_PRIVATE */53 :
          return "T_PRIVATE";
      case /* T_PROTECTED */54 :
          return "T_PROTECTED";
      case /* T_PUBLIC */55 :
          return "T_PUBLIC";
      case /* T_YIELD */56 :
          return "T_YIELD";
      case /* T_DEBUGGER */57 :
          return "T_DEBUGGER";
      case /* T_DECLARE */58 :
          return "T_DECLARE";
      case /* T_TYPE */59 :
          return "T_TYPE";
      case /* T_OF */60 :
          return "T_OF";
      case /* T_ASYNC */61 :
          return "T_ASYNC";
      case /* T_AWAIT */62 :
          return "T_AWAIT";
      case /* T_RSHIFT3_ASSIGN */63 :
          return "T_RSHIFT3_ASSIGN";
      case /* T_RSHIFT_ASSIGN */64 :
          return "T_RSHIFT_ASSIGN";
      case /* T_LSHIFT_ASSIGN */65 :
          return "T_LSHIFT_ASSIGN";
      case /* T_BIT_XOR_ASSIGN */66 :
          return "T_BIT_XOR_ASSIGN";
      case /* T_BIT_OR_ASSIGN */67 :
          return "T_BIT_OR_ASSIGN";
      case /* T_BIT_AND_ASSIGN */68 :
          return "T_BIT_AND_ASSIGN";
      case /* T_MOD_ASSIGN */69 :
          return "T_MOD_ASSIGN";
      case /* T_DIV_ASSIGN */70 :
          return "T_DIV_ASSIGN";
      case /* T_MULT_ASSIGN */71 :
          return "T_MULT_ASSIGN";
      case /* T_EXP_ASSIGN */72 :
          return "T_EXP_ASSIGN";
      case /* T_MINUS_ASSIGN */73 :
          return "T_MINUS_ASSIGN";
      case /* T_PLUS_ASSIGN */74 :
          return "T_PLUS_ASSIGN";
      case /* T_ASSIGN */75 :
          return "T_ASSIGN";
      case /* T_PLING */76 :
          return "T_PLING";
      case /* T_COLON */77 :
          return "T_COLON";
      case /* T_OR */78 :
          return "T_OR";
      case /* T_AND */79 :
          return "T_AND";
      case /* T_BIT_OR */80 :
          return "T_BIT_OR";
      case /* T_BIT_XOR */81 :
          return "T_BIT_XOR";
      case /* T_BIT_AND */82 :
          return "T_BIT_AND";
      case /* T_EQUAL */83 :
          return "T_EQUAL";
      case /* T_NOT_EQUAL */84 :
          return "T_NOT_EQUAL";
      case /* T_STRICT_EQUAL */85 :
          return "T_STRICT_EQUAL";
      case /* T_STRICT_NOT_EQUAL */86 :
          return "T_STRICT_NOT_EQUAL";
      case /* T_LESS_THAN_EQUAL */87 :
          return "T_LESS_THAN_EQUAL";
      case /* T_GREATER_THAN_EQUAL */88 :
          return "T_GREATER_THAN_EQUAL";
      case /* T_LESS_THAN */89 :
          return "T_LESS_THAN";
      case /* T_GREATER_THAN */90 :
          return "T_GREATER_THAN";
      case /* T_LSHIFT */91 :
          return "T_LSHIFT";
      case /* T_RSHIFT */92 :
          return "T_RSHIFT";
      case /* T_RSHIFT3 */93 :
          return "T_RSHIFT3";
      case /* T_PLUS */94 :
          return "T_PLUS";
      case /* T_MINUS */95 :
          return "T_MINUS";
      case /* T_DIV */96 :
          return "T_DIV";
      case /* T_MULT */97 :
          return "T_MULT";
      case /* T_EXP */98 :
          return "T_EXP";
      case /* T_MOD */99 :
          return "T_MOD";
      case /* T_NOT */100 :
          return "T_NOT";
      case /* T_BIT_NOT */101 :
          return "T_BIT_NOT";
      case /* T_INCR */102 :
          return "T_INCR";
      case /* T_DECR */103 :
          return "T_DECR";
      case /* T_ERROR */104 :
          return "T_ERROR";
      case /* T_EOF */105 :
          return "T_EOF";
      case /* T_JSX_IDENTIFIER */106 :
          return "T_JSX_IDENTIFIER";
      case /* T_ANY_TYPE */107 :
          return "T_ANY_TYPE";
      case /* T_BOOLEAN_TYPE */108 :
          return "T_BOOLEAN_TYPE";
      case /* T_NUMBER_TYPE */109 :
          return "T_NUMBER_TYPE";
      case /* T_STRING_TYPE */110 :
          return "T_STRING_TYPE";
      case /* T_VOID_TYPE */111 :
          return "T_VOID_TYPE";
      
    }
  } else {
    switch (param.TAG | 0) {
      case /* T_NUMBER */0 :
          return "T_NUMBER";
      case /* T_STRING */1 :
          return "T_STRING";
      case /* T_TEMPLATE_PART */2 :
          return "T_TEMPLATE_PART";
      case /* T_REGEXP */3 :
          return "T_REGEXP";
      case /* T_JSX_TEXT */4 :
          return "T_JSX_TEXT";
      case /* T_NUMBER_SINGLETON_TYPE */5 :
          return "T_NUMBER_SINGLETON_TYPE";
      
    }
  }
}

function yyback(n, lexbuf) {
  lexbuf.lex_curr_pos = lexbuf.lex_curr_pos - n | 0;
  var currp = lexbuf.lex_curr_p;
  lexbuf.lex_curr_p = {
    pos_fname: currp.pos_fname,
    pos_lnum: currp.pos_lnum,
    pos_bol: currp.pos_bol,
    pos_cnum: currp.pos_cnum - n | 0
  };
  
}

function back(lb) {
  var n = lb.lex_curr_p.pos_cnum - lb.lex_start_p.pos_cnum | 0;
  return yyback(n, lb);
}

var empty_lex_state = {
  lex_errors_acc: /* [] */0,
  lex_comments_acc: /* [] */0
};

function new_lex_env(lex_source, lex_lb, enable_types_in_comments) {
  return {
          lex_source: lex_source,
          lex_lb: lex_lb,
          lex_in_comment_syntax: false,
          lex_enable_comment_syntax: enable_types_in_comments,
          lex_state: empty_lex_state
        };
}

function get_and_clear_state(env) {
  var state = env.lex_state;
  var env$1 = state !== empty_lex_state ? ({
        lex_source: env.lex_source,
        lex_lb: env.lex_lb,
        lex_in_comment_syntax: env.lex_in_comment_syntax,
        lex_enable_comment_syntax: env.lex_enable_comment_syntax,
        lex_state: empty_lex_state
      }) : env;
  return [
          env$1,
          state
        ];
}

function with_lexbuf(lexbuf, env) {
  return {
          lex_source: env.lex_source,
          lex_lb: lexbuf,
          lex_in_comment_syntax: env.lex_in_comment_syntax,
          lex_enable_comment_syntax: env.lex_enable_comment_syntax,
          lex_state: env.lex_state
        };
}

function in_comment_syntax(is_in, env) {
  if (is_in !== env.lex_in_comment_syntax) {
    return {
            lex_source: env.lex_source,
            lex_lb: env.lex_lb,
            lex_in_comment_syntax: is_in,
            lex_enable_comment_syntax: env.lex_enable_comment_syntax,
            lex_state: env.lex_state
          };
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
    switch (lex_token.TAG | 0) {
      case /* T_TEMPLATE_PART */2 :
          var match$2 = lex_token._0;
          match$1 = [
            match$2[0],
            match$2[1].literal
          ];
          break;
      case /* T_REGEXP */3 :
          var match$3 = lex_token._0;
          match$1 = [
            match$3[0],
            "/" + (match$3[1] + ("/" + match$3[2]))
          ];
          break;
      case /* T_STRING */1 :
      case /* T_JSX_TEXT */4 :
          exit = 1;
          break;
      default:
        exit = 2;
    }
  }
  switch (exit) {
    case 1 :
        var match$4 = lex_token._0;
        match$1 = [
          match$4[0],
          match$4[2]
        ];
        break;
    case 2 :
        match$1 = [
          from_lb(env.lex_source, env.lex_lb),
          Lexing.lexeme(env.lex_lb)
        ];
        break;
    
  }
  return [
          env,
          {
            lex_token: lex_token,
            lex_loc: match$1[0],
            lex_value: match$1[1],
            lex_errors: List.rev(state.lex_errors_acc),
            lex_comments: List.rev(state.lex_comments_acc)
          }
        ];
}

function lex_error(env, loc, err) {
  var lex_errors_acc_0 = [
    loc,
    err
  ];
  var lex_errors_acc_1 = env.lex_state.lex_errors_acc;
  var lex_errors_acc = {
    hd: lex_errors_acc_0,
    tl: lex_errors_acc_1
  };
  var init = env.lex_state;
  return {
          lex_source: env.lex_source,
          lex_lb: env.lex_lb,
          lex_in_comment_syntax: env.lex_in_comment_syntax,
          lex_enable_comment_syntax: env.lex_enable_comment_syntax,
          lex_state: {
            lex_errors_acc: lex_errors_acc,
            lex_comments_acc: init.lex_comments_acc
          }
        };
}

function unexpected_error(env, loc, value) {
  return lex_error(env, loc, {
              TAG: /* UnexpectedToken */1,
              _0: value
            });
}

function unexpected_error_w_suggest(env, loc, value, suggest) {
  return lex_error(env, loc, {
              TAG: /* UnexpectedTokenWithSuggestion */2,
              _0: value,
              _1: suggest
            });
}

function illegal_number(env, lexbuf, word, token) {
  var loc = from_lb(env.lex_source, lexbuf);
  yyback(word.length, lexbuf);
  var env$1 = lex_error(env, loc, {
        TAG: /* UnexpectedToken */1,
        _0: "ILLEGAL"
      });
  return [
          env$1,
          token
        ];
}

var No_good = Caml_exceptions.create("Flow_parser_reg_test.Lexer_flow.FloatOfString.No_good");

function eat(f) {
  var match = f.todo;
  if (match) {
    return {
            negative: f.negative,
            mantissa: f.mantissa,
            exponent: f.exponent,
            decimal_exponent: f.decimal_exponent,
            todo: match.tl
          };
  }
  throw {
        RE_EXN_ID: No_good,
        Error: new Error()
      };
}

function start(str) {
  var todo = {
    contents: /* [] */0
  };
  $$String.iter((function (c) {
          todo.contents = {
            hd: c,
            tl: todo.contents
          };
          
        }), str);
  return {
          negative: false,
          mantissa: 0,
          exponent: 0,
          decimal_exponent: undefined,
          todo: List.rev(todo.contents)
        };
}

function parse_sign(f) {
  var match = f.todo;
  if (!match) {
    return f;
  }
  switch (match.hd) {
    case 43 :
        return eat(f);
    case 44 :
        return f;
    case 45 :
        var init = eat(f);
        return {
                negative: true,
                mantissa: init.mantissa,
                exponent: init.exponent,
                decimal_exponent: init.decimal_exponent,
                todo: init.todo
              };
    default:
      return f;
  }
}

function parse_hex_symbol(f) {
  var match = f.todo;
  if (match) {
    if (match.hd !== 48) {
      throw {
            RE_EXN_ID: No_good,
            Error: new Error()
          };
    }
    var match$1 = match.tl;
    if (match$1) {
      var match$2 = match$1.hd;
      if (match$2 === 88) {
        return eat(eat(f));
      }
      if (match$2 !== 120) {
        throw {
              RE_EXN_ID: No_good,
              Error: new Error()
            };
      }
      return eat(eat(f));
    }
    throw {
          RE_EXN_ID: No_good,
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: No_good,
        Error: new Error()
      };
}

function parse_exponent(f) {
  var todo_str = $$String.concat("", List.map(Char.escaped, f.todo));
  var exponent;
  try {
    exponent = Caml_format.caml_int_of_string(todo_str);
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === "Failure") {
      throw {
            RE_EXN_ID: No_good,
            Error: new Error()
          };
    }
    throw exn;
  }
  return {
          negative: f.negative,
          mantissa: f.mantissa,
          exponent: exponent,
          decimal_exponent: f.decimal_exponent,
          todo: /* [] */0
        };
}

function parse_body(_f) {
  while(true) {
    var f = _f;
    var match = f.todo;
    if (!match) {
      return f;
    }
    var c = match.hd;
    if (c >= 81) {
      if (c !== 95) {
        if (c === 112) {
          return parse_exponent(eat(f));
        }
        
      } else {
        _f = eat(f);
        continue ;
      }
    } else if (c !== 46) {
      if (c >= 80) {
        return parse_exponent(eat(f));
      }
      
    } else {
      if (f.decimal_exponent === undefined) {
        var init = eat(f);
        _f = {
          negative: init.negative,
          mantissa: init.mantissa,
          exponent: init.exponent,
          decimal_exponent: 0,
          todo: init.todo
        };
        continue ;
      }
      throw {
            RE_EXN_ID: No_good,
            Error: new Error()
          };
    }
    var ref_char_code;
    if (c >= /* "0" */48 && c <= /* "9" */57) {
      ref_char_code = /* "0" */48;
    } else if (c >= /* "A" */65 && c <= /* "F" */70) {
      ref_char_code = 55;
    } else if (c >= /* "a" */97 && c <= /* "f" */102) {
      ref_char_code = 87;
    } else {
      throw {
            RE_EXN_ID: No_good,
            Error: new Error()
          };
    }
    var value = c - ref_char_code | 0;
    var e = f.decimal_exponent;
    var decimal_exponent = e !== undefined ? e - 4 | 0 : undefined;
    var mantissa = (f.mantissa << 4) + value | 0;
    var init$1 = eat(f);
    _f = {
      negative: init$1.negative,
      mantissa: mantissa,
      exponent: init$1.exponent,
      decimal_exponent: decimal_exponent,
      todo: init$1.todo
    };
    continue ;
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
        if (f.todo !== /* [] */0) {
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "lexer_flow.mll",
                  546,
                  4
                ],
                Error: new Error()
              };
        }
        var ret = f.mantissa;
        var decimal_exponent = f.decimal_exponent;
        var exponent = decimal_exponent !== undefined ? f.exponent + decimal_exponent | 0 : f.exponent;
        var ret$1 = exponent === 0 ? ret : Math.pow(ret, exponent);
        if (f.negative) {
          return -ret$1;
        } else {
          return ret$1;
        }
      }
      catch (raw_exn){
        var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
        if (exn.RE_EXN_ID === No_good) {
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
  var c = multiline ? ({
        TAG: /* Block */0,
        _0: s
      }) : ({
        TAG: /* Line */1,
        _0: s
      });
  var lex_comments_acc_0 = [
    loc,
    c
  ];
  var lex_comments_acc_1 = env.lex_state.lex_comments_acc;
  var lex_comments_acc = {
    hd: lex_comments_acc_0,
    tl: lex_comments_acc_1
  };
  var init = env.lex_state;
  return {
          lex_source: env.lex_source,
          lex_lb: env.lex_lb,
          lex_in_comment_syntax: env.lex_in_comment_syntax,
          lex_enable_comment_syntax: env.lex_enable_comment_syntax,
          lex_state: {
            lex_errors_acc: init.lex_errors_acc,
            lex_comments_acc: lex_comments_acc
          }
        };
}

function unicode_fix_cols(lb) {
  var count = function (_start, stop, _acc) {
    while(true) {
      var acc = _acc;
      var start = _start;
      if (start === stop) {
        return acc;
      }
      var c = Caml_bytes.get(lb.lex_buffer, start);
      var acc$1 = (c & 192) === 128 ? acc + 1 | 0 : acc;
      _acc = acc$1;
      _start = start + 1 | 0;
      continue ;
    };
  };
  var bytes = count(lb.lex_start_pos, lb.lex_curr_pos, 0);
  var new_bol = lb.lex_curr_p.pos_bol + bytes | 0;
  var init = lb.lex_curr_p;
  lb.lex_curr_p = {
    pos_fname: init.pos_fname,
    pos_lnum: init.pos_lnum,
    pos_bol: new_bol,
    pos_cnum: init.pos_cnum
  };
  
}

function oct_to_int(x) {
  if (x > 55 || x < 48) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "lexer_flow.mll",
            604,
            11
          ],
          Error: new Error()
        };
  }
  return x - /* "0" */48 | 0;
}

function hexa_to_int(x) {
  if (x >= 65) {
    if (x >= 97) {
      if (x < 103) {
        return (x - /* "a" */97 | 0) + 10 | 0;
      }
      
    } else if (x < 71) {
      return (x - /* "A" */65 | 0) + 10 | 0;
    }
    
  } else if (!(x > 57 || x < 48)) {
    return x - /* "0" */48 | 0;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "lexer_flow.mll",
          610,
          11
        ],
        Error: new Error()
      };
}

function utf16to8(code) {
  if (code >= 65536) {
    return {
            hd: Char.chr(240 | (code >>> 18)),
            tl: {
              hd: Char.chr(128 | (code >>> 12) & 63),
              tl: {
                hd: Char.chr(128 | (code >>> 6) & 63),
                tl: {
                  hd: Char.chr(128 | code & 63),
                  tl: /* [] */0
                }
              }
            }
          };
  } else if (code >= 2048) {
    return {
            hd: Char.chr(224 | (code >>> 12)),
            tl: {
              hd: Char.chr(128 | (code >>> 6) & 63),
              tl: {
                hd: Char.chr(128 | code & 63),
                tl: /* [] */0
              }
            }
          };
  } else if (code >= 128) {
    return {
            hd: Char.chr(192 | (code >>> 6)),
            tl: {
              hd: Char.chr(128 | code & 63),
              tl: /* [] */0
            }
          };
  } else {
    return {
            hd: Char.chr(code),
            tl: /* [] */0
          };
  }
}

function mk_num_singleton(number_type, num, neg) {
  var value;
  if (number_type !== 0) {
    switch (number_type - 1 | 0) {
      case /* BINARY */0 :
          value = Caml_format.caml_int_of_string("0o" + num);
          break;
      case /* LEGACY_OCTAL */1 :
          value = Caml_format.caml_int_of_string(num);
          break;
      case /* OCTAL */2 :
          value = float_of_string(num);
          break;
      
    }
  } else {
    value = Caml_format.caml_int_of_string(num);
  }
  var value$1 = neg === "" ? value : -value;
  return {
          TAG: /* T_NUMBER_SINGLETON_TYPE */5,
          _0: number_type,
          _1: value$1
        };
}

var keywords = Hashtbl.create(undefined, 53);

var type_keywords = Hashtbl.create(undefined, 53);

List.iter((function (param) {
        return Hashtbl.add(keywords, param[0], param[1]);
      }), {
      hd: [
        "function",
        /* T_FUNCTION */13
      ],
      tl: {
        hd: [
          "if",
          /* T_IF */14
        ],
        tl: {
          hd: [
            "in",
            /* T_IN */15
          ],
          tl: {
            hd: [
              "instanceof",
              /* T_INSTANCEOF */16
            ],
            tl: {
              hd: [
                "return",
                /* T_RETURN */17
              ],
              tl: {
                hd: [
                  "switch",
                  /* T_SWITCH */18
                ],
                tl: {
                  hd: [
                    "this",
                    /* T_THIS */19
                  ],
                  tl: {
                    hd: [
                      "throw",
                      /* T_THROW */20
                    ],
                    tl: {
                      hd: [
                        "try",
                        /* T_TRY */21
                      ],
                      tl: {
                        hd: [
                          "var",
                          /* T_VAR */22
                        ],
                        tl: {
                          hd: [
                            "while",
                            /* T_WHILE */23
                          ],
                          tl: {
                            hd: [
                              "with",
                              /* T_WITH */24
                            ],
                            tl: {
                              hd: [
                                "const",
                                /* T_CONST */25
                              ],
                              tl: {
                                hd: [
                                  "let",
                                  /* T_LET */26
                                ],
                                tl: {
                                  hd: [
                                    "null",
                                    /* T_NULL */27
                                  ],
                                  tl: {
                                    hd: [
                                      "false",
                                      /* T_FALSE */28
                                    ],
                                    tl: {
                                      hd: [
                                        "true",
                                        /* T_TRUE */29
                                      ],
                                      tl: {
                                        hd: [
                                          "break",
                                          /* T_BREAK */30
                                        ],
                                        tl: {
                                          hd: [
                                            "case",
                                            /* T_CASE */31
                                          ],
                                          tl: {
                                            hd: [
                                              "catch",
                                              /* T_CATCH */32
                                            ],
                                            tl: {
                                              hd: [
                                                "continue",
                                                /* T_CONTINUE */33
                                              ],
                                              tl: {
                                                hd: [
                                                  "default",
                                                  /* T_DEFAULT */34
                                                ],
                                                tl: {
                                                  hd: [
                                                    "do",
                                                    /* T_DO */35
                                                  ],
                                                  tl: {
                                                    hd: [
                                                      "finally",
                                                      /* T_FINALLY */36
                                                    ],
                                                    tl: {
                                                      hd: [
                                                        "for",
                                                        /* T_FOR */37
                                                      ],
                                                      tl: {
                                                        hd: [
                                                          "class",
                                                          /* T_CLASS */38
                                                        ],
                                                        tl: {
                                                          hd: [
                                                            "extends",
                                                            /* T_EXTENDS */39
                                                          ],
                                                          tl: {
                                                            hd: [
                                                              "static",
                                                              /* T_STATIC */40
                                                            ],
                                                            tl: {
                                                              hd: [
                                                                "else",
                                                                /* T_ELSE */41
                                                              ],
                                                              tl: {
                                                                hd: [
                                                                  "new",
                                                                  /* T_NEW */42
                                                                ],
                                                                tl: {
                                                                  hd: [
                                                                    "delete",
                                                                    /* T_DELETE */43
                                                                  ],
                                                                  tl: {
                                                                    hd: [
                                                                      "typeof",
                                                                      /* T_TYPEOF */44
                                                                    ],
                                                                    tl: {
                                                                      hd: [
                                                                        "void",
                                                                        /* T_VOID */45
                                                                      ],
                                                                      tl: {
                                                                        hd: [
                                                                          "enum",
                                                                          /* T_ENUM */46
                                                                        ],
                                                                        tl: {
                                                                          hd: [
                                                                            "export",
                                                                            /* T_EXPORT */47
                                                                          ],
                                                                          tl: {
                                                                            hd: [
                                                                              "import",
                                                                              /* T_IMPORT */48
                                                                            ],
                                                                            tl: {
                                                                              hd: [
                                                                                "super",
                                                                                /* T_SUPER */49
                                                                              ],
                                                                              tl: {
                                                                                hd: [
                                                                                  "implements",
                                                                                  /* T_IMPLEMENTS */50
                                                                                ],
                                                                                tl: {
                                                                                  hd: [
                                                                                    "interface",
                                                                                    /* T_INTERFACE */51
                                                                                  ],
                                                                                  tl: {
                                                                                    hd: [
                                                                                      "package",
                                                                                      /* T_PACKAGE */52
                                                                                    ],
                                                                                    tl: {
                                                                                      hd: [
                                                                                        "private",
                                                                                        /* T_PRIVATE */53
                                                                                      ],
                                                                                      tl: {
                                                                                        hd: [
                                                                                          "protected",
                                                                                          /* T_PROTECTED */54
                                                                                        ],
                                                                                        tl: {
                                                                                          hd: [
                                                                                            "public",
                                                                                            /* T_PUBLIC */55
                                                                                          ],
                                                                                          tl: {
                                                                                            hd: [
                                                                                              "yield",
                                                                                              /* T_YIELD */56
                                                                                            ],
                                                                                            tl: {
                                                                                              hd: [
                                                                                                "debugger",
                                                                                                /* T_DEBUGGER */57
                                                                                              ],
                                                                                              tl: {
                                                                                                hd: [
                                                                                                  "declare",
                                                                                                  /* T_DECLARE */58
                                                                                                ],
                                                                                                tl: {
                                                                                                  hd: [
                                                                                                    "type",
                                                                                                    /* T_TYPE */59
                                                                                                  ],
                                                                                                  tl: {
                                                                                                    hd: [
                                                                                                      "of",
                                                                                                      /* T_OF */60
                                                                                                    ],
                                                                                                    tl: {
                                                                                                      hd: [
                                                                                                        "async",
                                                                                                        /* T_ASYNC */61
                                                                                                      ],
                                                                                                      tl: {
                                                                                                        hd: [
                                                                                                          "await",
                                                                                                          /* T_AWAIT */62
                                                                                                        ],
                                                                                                        tl: /* [] */0
                                                                                                      }
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    });

List.iter((function (param) {
        return Hashtbl.add(type_keywords, param[0], param[1]);
      }), {
      hd: [
        "static",
        /* T_STATIC */40
      ],
      tl: {
        hd: [
          "typeof",
          /* T_TYPEOF */44
        ],
        tl: {
          hd: [
            "any",
            /* T_ANY_TYPE */107
          ],
          tl: {
            hd: [
              "bool",
              /* T_BOOLEAN_TYPE */108
            ],
            tl: {
              hd: [
                "boolean",
                /* T_BOOLEAN_TYPE */108
              ],
              tl: {
                hd: [
                  "true",
                  /* T_TRUE */29
                ],
                tl: {
                  hd: [
                    "false",
                    /* T_FALSE */28
                  ],
                  tl: {
                    hd: [
                      "number",
                      /* T_NUMBER_TYPE */109
                    ],
                    tl: {
                      hd: [
                        "string",
                        /* T_STRING_TYPE */110
                      ],
                      tl: {
                        hd: [
                          "void",
                          /* T_VOID_TYPE */111
                        ],
                        tl: {
                          hd: [
                            "null",
                            /* T_NULL */27
                          ],
                          tl: /* [] */0
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    });

var __ocaml_lex_tables = {
  lex_base: "\0\0\xb2\xff\xb3\xff\xb9\xffB\0C\0T\0W\0F\0I\0J\0K\0M\0e\0\xdd\xff\xde\xff\xdf\xff\xe0\xff\xe3\xff\xe4\xff\xe5\xff\xe6\xff\xe7\xff\xe8\xff\xc0\0L\0e\0\x17\x01n\x01\xf6\xff\xf7\xffl\0u\0v\0\0\0\x0e\0\x0f\0\x07\x003\x01\xfe\xff\xff\xff\x01\0\x12\0(\0\f\0\x15\0*\0\f\0=\0-\0\t\0\xb6\xff\xf9\xff\xe0\x01B\0u\0\x0f\x000\x004\0\x17\0\xe5\x01(\x008\0\x1a\0K\0:\0\x17\0\xfb\xffh\0a\0\xac\0q\0m\0y\0q\0i\0{\0{\0\xa8\0\xca\xff\xfa\xff\xc9\xff\xf8\xff\x0b\x02\xa5\x02\xfc\x02S\x03\xaa\x03\x01\x04X\x04\xaf\x04\x06\x05]\x05\xb4\x05\x0b\x06b\x06\xb9\x06\xc3\x01\x10\x07g\x07\xbe\x07\x15\bl\b\xc3\b\x1a\tq\t\xc8\t\xb8\0\xe2\xffE\x02\xc7\xff\xdc\xff\xc6\xff\xdb\xff\xb7\xff\xaa\0\xda\xff\xab\0\xd9\xff\xac\0\xd8\xff\xd2\xff\xad\0\xd7\xff\xb0\0\xd0\xff\xcf\xff\xcc\xff\xd4\xff\xcb\xff\xd3\xff\xc8\xff\xc5\xff:\n\xcf\xff\xd0\xff\xd2\xff\xd6\xff\xd7\xff\xb0\0\xdc\xff\xdd\xff\xe0\xff\xe1\xff\xe2\xff\xe3\xff\xe6\xff\xe7\xff\xe8\xff\xe9\xff\xea\xff\xeb\xff\x94\n\xfa\n\xd6\x01Q\x0b\xa8\x0b\x1a\f\xf9\xff\xcc\0\xf1\0A\0}\0~\0\xa3\0\xc4\x0b\xff\xffa\0\x9d\0\xc1\0\xa4\0\x90\0\xc6\0\xb2\0\xcb\t\xd2\0\x95\0\xfa\xff\x1f\f\xe9\0\x1c\x01\x9c\0\xf2\0\xf3\0\xf9\0$\f\xe7\0\xf7\0\xf5\0\xdf\x0b\x15\x01\xd7\0\xfc\xff(\x01!\x01m\x012\x01/\x01E\x01=\x015\x01G\x01G\x01\xfb\xff\xf3\x01\xf2\0.\x01I\x01P\x01K\f=\x01L\x01/\x01\xec\x0bk\x010\x01x\f\xff\fV\r\xad\r\0\x02\x04\x0e[\x0e\xb2\x0e\t\x0f`\x0f\xb7\x0f\x0e\x10e\x10\xbc\x10\x13\x11j\x11\xc1\x11\x18\x12o\x12\xc6\x12\x1d\x13t\x13\xcb\x13\"\x14\xcf\x01\xe5\xffy\x14\xd0\x14'\x15~\x15\xd4\xff\x1b\f\xfc\xff\xfd\xff\xfe\xff\xff\xff\xcf\x15\xee\xff\x01\0\xef\xff\x18\x16\xf4\xff\xf5\xff\xf6\xff\xf7\xff\xf8\xff\xf9\xff\xf1\x02H\x03>\x16\xfe\xff\xff\xffU\x16\xfd\xff\x9f\x03\xfc\xff{\x16\x92\x16\xb8\x16\xcf\x16\xf2\xff\xf5\x16\xf1\xff\xd7\x02\xfb\xff\xd2\x01\xfe\xff\xff\xff\xcf\x01\xfd\xff\xfc\xff;\x02\xfd\xff\xfe\xff\xff\xff\0\x17\xf9\xff\xe8\x01G\x01\x83\x01\x90\x01y\x01)\fC\x15\xfe\xff\xff\xff]\x01\x9b\x01\x9c\x01*\x02\x90\x01\xa0\x01\x82\x01\x87\x15\xad\x01o\x01\xfb\xff\xfc\xff\x0b\x16\xf8\xff\x04\0\xf9\xff\xfa\xff8\x17,\x03\xff\xff\xfd\xff\x05\0\xfe\xff\xc0\x17\x96\t\xfb\xff\xfc\xff\xeb\x01\xff\xff\xfd\xff\xfe\xff2\x18\xf1\xff\xf2\xff\x8a\x18\xf4\xff\xf5\xff\xf6\xff\xf7\xff\xf8\xff\xfa\xff<\x02\x7f\x01\xc9\x01\xe7\x01+\x02\x88\x167\x18\xfe\xff\xff\xff\x8f\x01 \x02!\x023\x02\x15\x02%\x02!\x02\xbd\x16L\x02\x0f\x02\xfb\xff\xfc\xff|\f\xfb\xff\xfc\xff\xfd\xff\xfe\xff\x06\0\xff\xff\xfc\x18\xf9\xff\xf8\x18\x07\0\xfd\xff\xfe\xff\xff\xffO\x19\xdf\n_\f\x84\x17\x9c\x19\xfc\xff\xfb\xff\xd3\x19\xfa\xff*\x1a\x81\x1a\xd8\x1a/\x1b\x86\x1b\x96\x02\xf8\x1b\xfa\xff\xfb\xff\xb5\x02%\x02b\x02\x82\x02\xf3\x02\x04\x19K\x1b\xff\xff(\x02e\x02\xa9\x02J\x03r\x02\x85\x02\x8c\x02\xc9\x16\xb7\x02y\x02\xfc\xff\xfd\xff\xc3\x16\xf9\xff\xfa\xff\b\0\xfc\xff\xbf\x02\xfe\xff\xff\xff\xfd\xff\xfb\xff",
  lex_backtrk: "\xff\xff\xff\xff\xff\xff\xff\xffD\0A\0>\0=\0<\0;\0E\0G\0B\0C\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x16\0K\0\x1e\0\x15\0\x15\0\xff\xff\xff\xffM\0?\0J\0M\0M\0M\0M\0\x02\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x03\0\xff\xff\x04\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff@\0\xff\xff\xff\xff\xff\xff\xff\xff\x14\0\x14\0\x15\0\x14\0\x0f\0\x14\0\x14\0\x0b\0\n\0\r\0\f\0\x0e\0\x0e\0\x0e\0\xff\xff\x0e\0\x0e\0\x13\0\x12\0\x11\0\x10\0\x15\0\x13\0\x12\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff)\0\xff\xff*\0\xff\xff.\0\xff\xff\xff\xff2\0\xff\xff1\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff$\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x13\0\x13\0\x1b\0\x12\0\x12\0.\0\xff\xff&\x000\x000\x000\x000\x000\0\x01\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x02\0\xff\xff\x03\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x12\0\x11\0\x11\0\x10\0\xff\xff\x10\0\x0f\0\x0f\0\x12\0\x11\0\f\0\x11\0\x11\0\b\0\x07\0\n\0\t\0\x0b\0\x0b\0\x0b\0\x0b\0\x0b\0\x0e\0\r\0\xff\xff\xff\xff\x13\0\x13\0\x13\0\x13\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x10\0\xff\xff\x0f\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\f\0\x05\0\x0f\0\xff\xff\xff\xff\xff\xff\xff\xff\x04\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x04\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x05\0\x06\0\x06\0\x06\0\x06\0\x02\0\x01\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x06\0\xff\xff\xff\xff\x04\0\x07\0\xff\xff\xff\xff\x01\0\xff\xff\x03\0\xff\xff\xff\xff\xff\xff\x04\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\f\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x06\0\x0e\0\x0e\0\x0e\0\x0e\0\x02\0\x01\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\x06\0\x02\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x05\0\x05\0\x05\0\x05\0\x05\0\x01\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x05\0\xff\xff\x06\0\xff\xff\xff\xff\xff\xff\xff\xff",
  lex_default: "\x01\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\xff\xff\0\0\xff\xff\0\0\xff\xff\0\0\0\0\xff\xff\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x86\0\0\0\0\0\0\0\0\0\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xf8\0\0\0\0\0\0\0\0\0\xfd\0\0\0\xff\xff\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\0\0\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\0\0\x18\x01\0\0\xff\xff\0\0\0\0\xff\xff\0\0\0\0 \x01\0\0\0\0\0\0$\x01\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0;\x01\0\0\xff\xff\0\0\0\0\xff\xffB\x01\0\0\0\0\xff\xff\0\0\xff\xffG\x01\0\0\0\0\xff\xff\0\0\0\0\0\0N\x01\0\0\0\0\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0m\x01\0\0\0\0\0\0\0\0\xff\xff\0\0t\x01\0\0\xff\xff\xff\xff\0\0\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x8a\x01\0\0\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xa1\x01\0\0\0\0\xff\xff\0\0\xff\xff\0\0\0\0\0\0\0\0",
  lex_trans: "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0&\0(\0\xff\0&\0&\0=\x01D\x01r\x01w\x01\xa9\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0&\0\n\0\x1e\0\x1f\0\x18\0\x05\0\r\0\x1e\0\x15\0\x14\0 \0\x07\0\x10\0\x06\0\x1a\0!\0\x1c\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x0f\0\x11\0\t\0\x0b\0\b\0\x0e\0\x19\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x13\0'\0\x12\0\x04\0\x18\0\x1d\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x17\0\f\0\x16\0\x03\0\x84\0\x83\0\x82\0\x80\0{\0z\0w\0x\0u\0s\0r\0p\0o\0m\0R\x001\x000\0/\0\x81\x001\0k\0\x7f\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0N\x005\0.\0n\0&\0P\x004\0.\0-\x000\0/\0&\0&\0-\0&\0D\0C\0A\0>\0O\x003\0@\0?\0<\0=\0<\0<\0<\x002\x002\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0&\0q\0B\0<\0<\0<\0<\0<\0<\0<\0<\0<\0<\0<\0<\0E\0F\0G\0H\0I\0J\0K\0L\0M\0C\0%\0$\0#\0\x18\0Q\0l\0t\0v\0y\0}\0|\0&\0~\0\xf6\0\"\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0<\0\xcb\0\xb0\0\xaf\0\xae\0\xad\0\x02\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\xb2\0\xb0\0\xaf\0\xa5\0\x18\0\xb1\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0S\0&\0\xac\0\xac\0&\0&\0\xae\0\xad\0\xab\0\xab\0U\0\xa5\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\xa5\0\xa5\0&\0\xa5\0\xc1\0\xc0\0\xbf\0S\0S\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\xbe\0\xbd\0\xbc\0\xb9\0S\0\xb9\0S\0S\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\xbb\0\xb9\0\xb9\0\xb9\0\xc2\0\xc3\0\xba\0\xc4\0\xc5\0U\0\xc6\0W\0W\0W\0W\0W\0W\0W\0W\0\x1b\0\x1b\0\xc7\0\xc8\0\xc9\0\xca\0\xc0\0\xd7\0\xd6\0S\0Y\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0X\0S\0S\0S\0S\0S\0S\0S\0S\0V\0S\0S\0\xd5\0\xd4\0\xd1\0\xd1\0S\0\xd1\0S\0Y\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0X\0S\0S\0S\0S\0S\0S\0S\0S\0V\0S\0S\0<\0\xd3\0\xd1\0<\0<\0<\0\xd1\0\xd2\0<\0<\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0\xf1\0\x1e\x01\x1c\x01<\0\x1d\x017\x016\x01\xf0\0<\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\x005\x014\x018\x013\x01,\0+\0*\x009\x017\x012\x017\x006\x015\x014\x01*\x017\0*\x01*\x01)\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0*\x01*\x01S\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0i\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0!\x016\0L\x01K\x01h\x01i\x016\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0j\x01g\x01f\x01\x18\0S\0k\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0h\x01g\x01f\x01\\\x01\x18\0\\\x01\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\\\x01;\0:\x009\x003\x01e\x01;\0:\x009\0S\x002\x01d\x01\\\x01e\x01\\\x018\0a\0\x82\x01a\0d\x018\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0\x9e\x01\x9d\x01\x1a\x01\x9c\x01\x9d\x01\x9f\x01\x9c\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\x91\x01\x19\x01\x9b\x01\x9a\x01S\0\x91\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x9b\x01\x9a\x01\x91\x01h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0D\x01\x91\x01\x91\x01C\x01\xa8\x01\"\x01\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0S\0\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\0\0\0\0\0\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0\x99\x01\0\0\0\0\0\0\0\0\0\0\x98\x01f\0f\0f\0f\0f\0f\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0S\0\0\0f\0f\0f\0f\0f\0f\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0_\0\x0f\x01\x0f\x01\x0f\x01\x0f\x01\x0f\x01\x0f\x01\x0f\x01\x0f\x01\x1b\x01U\0\0\0W\0W\0W\0W\0W\0W\0W\0W\0^\0^\0\x99\x01\0\0\0\0\0\0\0\0\0\0\x98\x01_\0_\0_\0_\0`\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0_\0\0\0_\0_\0_\0_\0`\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0S\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff\0\0\0\0\0\0\0\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0S\0S\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0S\0\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0Z\0Z\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0S\0\0\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0[\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0Z\0Z\0[\0[\0[\0[\0[\0[\0[\0[\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\0\0\0\0\0\0\0\0[\0\0\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\0\0\0\0\0\0\0\0[\0\0\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0]\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0]\0]\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\0\0\0\0\0\0\0\0]\0\0\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\0\0\0\0\0\0\0\0]\0\0\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0_\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0U\0\0\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0_\0_\0_\0_\0`\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0_\0\0\0_\0_\0_\0_\0`\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0_\0\0\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0\0\0\0\0a\0\0\0a\0\0\0\0\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\0\0\0\0\0\0\0\0_\0\0\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0c\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\0\0\0\0\0\0\0\0c\0\0\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\0\0\0\0\0\0\0\0c\0\0\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0e\0\0\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0e\0\0\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0g\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0f\0f\0f\0f\0f\0f\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\0\0\0\0\0\0\0\0g\0\0\0f\0f\0f\0f\0f\0f\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\0\0\0\0\0\0\0\0g\0\0\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0S\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0S\0S\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\0\0\0\0\0\0\0\0S\0\0\0S\0S\0S\0S\0T\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0j\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\0\0\0\0\0\0\0\0j\0\0\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\0\0\0\0\0\0\0\0\0\0I\x01H\x01\0\0\0\0\0\0\0\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\0\0\0\0\0\0\0\0j\0\0\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\xa5\0\xa6\0\0\0\xa5\0\xa5\0\0\0\0\0\0\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\xa5\0\0\0\0\0\0\0\0\0\xa5\0\0\0\x9e\0\0\0\x98\0\0\0\x89\0\x9e\0\x93\0\x92\0\x9f\0\x88\0\x90\0\x9d\0\x9a\0\xa0\0\x9c\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x8f\0\x91\0\x8d\0\x8b\0\x8c\0\x8e\0\xa5\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x97\0J\x01\x96\0\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x99\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x95\0\x8a\0\x94\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01\0\0\0\0\xa4\0\xa3\0\xa2\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xa1\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\x87\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0}\x01\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xf2\0\x98\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe0\0\0\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xa5\0\0\0\0\0\xa5\0\xa5\0\0\0\0\0\0\0\0\0\xe0\0\0\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\x9b\0\x9b\0\0\0\0\0\xa5\0\0\0\0\0\0\0\0\0\xd9\0\xe4\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xe3\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xe1\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xe4\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xe3\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xe1\0\xd9\0\xd9\0\xd1\0\0\0\xf9\0\xd1\0\xd1\0\xb9\0\0\0\0\0\xb9\0\xb9\0\xb9\0\0\0\0\0\xb9\0\xb9\0*\x01\0\0\0\0*\x01*\x01\0\0\0\0\0\0\xd1\0\0\0\0\0\xfb\0\0\0\xb9\0\0\0\0\0\xfb\0\0\0\xb9\0\0\0\0\0\0\0\xcc\0*\x01\x9c\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\xd1\0\0\0\0\0\xd1\0\xd1\0\xb4\0\0\0\0\0\0\0\0\0\xb4\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\xb9\0\0\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xfa\0\0\0\xcc\0\0\0\x9c\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\xb3\0r\x01\0\0\0\0q\x01\xb3\0\0\0\0\0\0\0\xb9\0|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01\0\0\x80\x01\xd1\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xaa\0\xa9\0\xa8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\0\0\xa7\0\0\0\0\0\0\0\0\0o\x01\xd9\0\xd9\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xda\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0n\x01\0\0\0\0\0\0\xd0\0\xcf\0\xce\0\0\0\0\0\xb8\0\xb7\0\xb6\0\0\0\0\0\xb8\0\xb7\0\xb6\0\0\0\xcd\x001\x010\x01/\x01\0\0\xb5\0\0\0\0\0\0\0\0\0\xb5\0\0\0\0\0\0\0\0\0.\x01\0\0\0\0\xf9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd0\0\xcf\0\xce\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\xcd\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0p\x01\0\0\0\0\0\0\0\0\xdc\0\0\0\xdc\0\0\0\0\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xdf\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\0\0\0\0\0\0\0\0\xdf\0\0\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xde\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\0\0\0\0\0\0\0\0\xde\0\0\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\0\0\0\0\0\0\0\0\xde\0\0\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xdf\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\0\0\0\0\0\0\0\0\xdf\0\0\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xea\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe0\0\0\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe9\0\xe9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xea\0\xea\0\xea\0\xea\0\xeb\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\xea\0\0\0\xea\0\xea\0\xea\0\xea\0\xeb\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe5\0\xe5\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\0\0\0\0\0\0\0\0\xd9\0\0\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xe6\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe5\0\xe5\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\0\0\0\0\0\0\0\0\xe6\0\0\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\0\0\0\0\0\0\0\0\xe6\0\0\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe8\0\xe8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\0\0\0\0\0\0\0\0\xe8\0\0\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\0\0\0\0\0\0\0\0\xe8\0\0\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xea\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xe0\0\0\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xea\0\xea\0\xea\0\xea\0\xeb\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\xea\0\0\0\xea\0\xea\0\xea\0\xea\0\xeb\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\xea\0\0\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\0\0\0\0\xdc\0\0\0\xdc\0\0\0\0\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\0\0\0\0\0\0\0\0\xea\0\0\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xed\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\0\0\0\0\0\0\0\0\xed\0\0\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\0\0\0\0\0\0\0\0\xed\0\0\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xef\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\0\0\0\0\0\0\0\0\xef\0\0\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\0\0\0\0\0\0\0\0\xef\0\0\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xf3\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\xf4\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0*\x01,\x01\0\0*\x01*\x01\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0*\x01\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xf5\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xff\0\0\0\0\0\xfe\0\x98\0\0\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\0\0\0\0\0\0\0\0\0\0\0\0\b\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01*\x01*\x01*\x01*\x01*\x01*\x01*\x01*\x01*\x01*\x01*\x01\0\0\0\0\0\0=\x01\0\0\0\0<\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x001\x010\x01/\x01\0\0\0\0\0\0\0\0\n\x01\0\0\0\0\0\0\0\0\0\0\x06\x01.\x01\0\0\0\0\x05\x01*\x01\0\0\0\0\0\0?\x01\0\0\0\0\x04\x01\0\0\0\0\0\0\x03\x01\0\0\x02\x01\0\x01\x01\x01\0\0\t\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0>\x01@\x01\0\0\0\0\0\0\0\0\0\0\0\0\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\0\0\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\0\0\0\0\\\x01\0\0\x10\x01\\\x01\\\x01\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\0\0\0\0\0\0\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\0\0\0\0\0\0\\\x01\0\0\0\0\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\0\0\r\x01\r\x01\r\x01\r\x01\r\x01\r\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\0\0\xa2\x01\0\0\x0b\x01\xa3\x01\0\0\0\0\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\0\0\0\0\0\0\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\0\0\0\0\0\0\0\0\0\0\xa5\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\0\0\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01*\x01,\x01A\x01*\x01+\x01\0\0\0\0\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\0\0\0\0\0\0\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\xa4\x01*\x01\0\0\0\0\xa6\x01\0\0\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01%\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x14\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\0\0\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\\\x01\0\0\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\x91\x01\0\0\0\0\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01E\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0c\x01b\x01a\x01\\\x01\0\0\0\0\0\0\0\0\0\0\x16\x01\0\0\0\0\0\0\0\0`\x01\x91\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01\0\0\0\0\0\0\0\0E\x01\0\0E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01\0\0~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01\0\0\0\0\0\0\0\0\0\0\xa7\x01\0\0~\x01~\x01~\x01~\x01~\x01~\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0)\x01(\x01'\x01E\x01~\x01~\x01~\x01~\x01~\x01~\x01\0\0\0\0\0\0\0\0&\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0-\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01\0\0\0\0\0\0\0\0E\x01\0\0E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01\\\x01^\x01\0\0\\\x01]\x01\\\x01^\x01\0\0\\\x01\\\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\\\x01\0\0O\x01\0\0P\x01\\\x01\0\0O\x01\0\0\0\0\0\0\0\0\0\0\0\0R\x01W\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0S\x01\0\0V\x01Q\x01U\x01\0\0\0\0P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\0\0\0\0\0\0\0\0P\x01\0\0P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01T\x01P\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0P\x01\0\0\0\0P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\0\0\0\0\0\0\0\0P\x01\0\0P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\0\0w\x01\0\0\0\0v\x01\0\0\0\0\0\0\x91\x01\0\0\0\0\x91\x01\x91\x01\0\0[\x01Z\x01Y\x01\0\0\0\0c\x01b\x01a\x01{\x01z\x01\0\0y\x01\0\0\0\0X\x01u\x01y\x01\x91\x01\0\0`\x01\0\0z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01_\x01\0\0\0\0\0\0\0\0\0\0y\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01\0\0\0\0\0\0\0\0z\x01\0\0z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01\x81\x01\0\0\0\0\0\0y\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\0\0\0\0\0\0\0\0\x81\x01\0\0\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\0\0\0\0~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01\0\0\x7f\x01\0\0\0\0\0\0\0\0\0\0~\x01~\x01~\x01~\x01~\x01~\x01\0\0\0\0\x97\x01\x96\x01\x95\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x94\x01\0\0\0\0\0\0\x83\x01\0\0\0\0\0\0\0\0x\x01~\x01~\x01~\x01~\x01~\x01~\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\0\0\x82\x01\0\0\0\0\0\0\0\0\0\0\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\0\0\0\0\0\0\0\0\x83\x01\0\0\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x84\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\0\0\x82\x01\0\0\0\0\0\0\0\0\0\0\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\0\0\0\0\0\0\0\0\x84\x01\0\0\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x85\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\0\0\x82\x01\0\0\0\0\0\0\0\0\0\0\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\0\0\0\0\0\0\0\0\x85\x01\0\0\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x86\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\0\0\x82\x01\0\0\0\0\0\0\0\0\0\0\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\0\0\0\0\0\0\0\0\x86\x01\0\0\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x87\x01\x91\x01\x93\x01\0\0\x91\x01\x91\x01\0\0\0\0\0\0\0\0\0\0\0\0\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\0\0\x82\x01\x91\x01\0\0\0\0\0\0\0\0\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\0\0\0\0\0\0\0\0\x87\x01\0\0\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x88\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\0\0\x82\x01\0\0\0\0\0\0\0\0\0\0\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\0\0\0\0\0\0\0\0\x88\x01\0\0\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x88\x01\x91\x01\x93\x01\0\0\x91\x01\x92\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x91\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x8c\x01\0\0\0\0\0\0\0\0\x97\x01\x96\x01\x95\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x94\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x8b\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x90\x01\x8f\x01\x8e\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x8d\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\xff\xff",
  lex_check: "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\xfe\0\0\0\0\0<\x01C\x01q\x01v\x01\xa3\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x04\0\x05\0\x06\0\x07\0\b\0\b\0\t\0\t\0\n\0\x0b\0\x0b\0\f\0\r\0\x19\0\x1f\0#\0$\0$\0\x06\0*\0\x1a\0\x07\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0 \0!\0%\0\r\0-\0 \0!\0,\0%\0+\0+\0.\0/\0,\x001\x006\x007\x009\0;\0 \0!\0:\0:\0=\0;\0>\0?\0A\0\"\0)\x000\x000\x000\x000\x000\x000\x000\x000\x000\x000\x000\x002\0\f\x008\0@\0@\0@\0@\0@\0@\0@\0@\0@\0@\0@\0B\0D\0E\0F\0G\0H\0I\0J\0K\0L\0M\0\0\0\0\0\0\0\x18\0N\0k\0s\0u\0w\0z\0z\x000\0|\0\x8b\0\0\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0@\0\x9f\0\xa1\0\xa2\0\xa3\0\xa3\0\0\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\xa0\0\xa7\0\xa8\0\xab\0\x18\0\xa0\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x1b\0&\0\xa4\0\xaa\0&\0&\0\xa9\0\xa9\0\xa4\0\xaa\0\x1b\0\xac\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\xad\0\xaf\0&\0\xb0\0\xb3\0\xb4\0\xb5\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\xb6\0\xb7\0\xb7\0\xba\0\x1b\0\xbb\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1c\0\xb8\0\xbc\0\xbe\0\xbf\0\xc1\0\xc2\0\xb8\0\xc3\0\xc4\0\x1c\0\xc5\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\xc6\0\xc7\0\xc8\0\xc9\0\xca\0\xcd\0\xce\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\xcf\0\xcf\0\xd2\0\xd3\0\x1c\0\xd4\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\x005\0\xd0\0\xd6\x005\x005\0<\0\xd7\0\xd0\0<\0<\0a\0a\0a\0a\0a\0a\0a\0a\0a\0a\0\xf0\0\x1c\x01\x19\x015\0\x19\x01&\x01'\x01\x9a\0<\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0(\x01(\x01%\x01)\x01&\0&\0&\0%\x01.\x01)\x015\0/\x010\x010\x012\x01<\x003\x014\x01&\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\x006\x017\x01S\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0X\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0\x1f\x015\0I\x01I\x01Y\x01`\x01<\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0W\x01Z\x01Z\x01m\0S\0W\x01S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0S\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0a\x01b\x01b\x01d\x01m\0e\x01m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0m\0f\x015\x005\x005\x001\x01[\x01<\0<\0<\0T\x001\x01[\x01h\x01c\x01i\x015\0T\0\x88\x01T\0c\x01<\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0\x8c\x01\x8d\x01\x17\x01\x8e\x01\x94\x01\x8c\x01\x95\x01T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0\x98\x01\x17\x01\x8f\x01\x8f\x01T\0\x99\x01T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0U\0\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x07\x01\x96\x01\x96\x01\x9a\x01U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0@\x01\x9c\x01\x9d\x01@\x01\xa5\x01\x1f\x01\xff\xffU\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0\xff\xff\xff\xff\xff\xff\xff\xffU\0\xff\xffU\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0V\0\b\x01\b\x01\b\x01\b\x01\b\x01\b\x01\b\x01\b\x01\xff\xff\xff\xff\xff\xffV\0V\0V\0V\0V\0V\0V\0V\0V\0V\0\x90\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x90\x01V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0\xff\xff\xff\xff\xff\xff\xff\xffV\0\xff\xffV\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0W\0\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x0e\x01\x17\x01W\0\xff\xffW\0W\0W\0W\0W\0W\0W\0W\0W\0W\0\x97\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x97\x01W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0\xff\xff\xff\xff\xff\xff\xff\xffW\0\xff\xffW\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0W\0X\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff@\x01\xff\xff\xff\xff\xff\xff\xff\xffX\0X\0X\0X\0X\0X\0X\0X\0X\0X\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffX\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0\xff\xff\xff\xff\xff\xff\xff\xffX\0\xff\xffX\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0X\0Y\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffY\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffY\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0\xff\xff\xff\xff\xff\xff\xff\xffY\0\xff\xffY\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Y\0Z\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffZ\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffZ\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0\xff\xff\xff\xff\xff\xff\xff\xffZ\0\xff\xffZ\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0Z\0[\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\xff\xff\xff\xff\xff\xff\xff\xff[\0\xff\xff[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0[\0\\\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\xff\xff\xff\xff\xff\xff\xff\xff\\\0\xff\xff\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0]\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0\xff\xff\xff\xff\xff\xff\xff\xff]\0\xff\xff]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0]\0^\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff^\0\xff\xff^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0\xff\xff\xff\xff\xff\xff\xff\xff^\0\xff\xff^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0_\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0\xff\xff\xff\xff\xff\xff\xff\xff_\0\xff\xff_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0_\0`\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff`\0\xff\xff`\0\xff\xff\xff\xff`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0\xff\xff\xff\xff\xff\xff\xff\xff`\0\xff\xff`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0b\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffb\0b\0b\0b\0b\0b\0b\0b\0b\0b\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffb\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0\xff\xff\xff\xff\xff\xff\xff\xffb\0\xff\xffb\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0c\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffc\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffc\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0\xff\xff\xff\xff\xff\xff\xff\xffc\0\xff\xffc\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0c\0d\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffd\0d\0d\0d\0d\0d\0d\0d\0d\0d\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffd\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0\xff\xff\xff\xff\xff\xff\xff\xffd\0\xff\xffd\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0e\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffe\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffe\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\xff\xff\xff\xff\xff\xff\xff\xffe\0\xff\xffe\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0f\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfff\0f\0f\0f\0f\0f\0f\0f\0f\0f\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfff\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0\xff\xff\xff\xff\xff\xff\xff\xfff\0\xff\xfff\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0g\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffg\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffg\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0\xff\xff\xff\xff\xff\xff\xff\xffg\0\xff\xffg\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0g\0h\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffh\0h\0h\0h\0h\0h\0h\0h\0h\0h\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffh\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0\xff\xff\xff\xff\xff\xff\xff\xffh\0\xff\xffh\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0i\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffi\0i\0i\0i\0i\0i\0i\0i\0i\0i\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffi\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0\xff\xff\xff\xff\xff\xff\xff\xffi\0\xff\xffi\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0j\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffF\x01F\x01\xff\xff\xff\xff\xff\xff\xff\xffj\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffj\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\xff\xff\xff\xff\xff\xff\xff\xffj\0\xff\xffj\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0j\0\x85\0\x85\0\xff\xff\x85\0\x85\0\xff\xff\xff\xff\xff\xff\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xae\0\xff\xff\xff\xff\xff\xff\xff\xff\x85\0\xff\xff\x85\0\xff\xff\x85\0\xff\xff\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\xae\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0F\x01\x85\0\xff\xff\x85\0\xff\xff\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x98\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\xff\xff\xff\xff\xff\xff\xff\xff\x98\0\xff\xff\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0\x98\0{\x01{\x01{\x01{\x01{\x01{\x01{\x01{\x01{\x01{\x01\xff\xff\xff\xff\x85\0\x85\0\x85\0\x99\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x85\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x85\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\xff\xff\xff\xff{\x01\xff\xff\x99\0\xff\xff\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x99\0\x9b\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x9b\0\xff\xff\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\xff\xff\xff\xff\xff\xff\xff\xff\x9b\0\xff\xff\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9c\0\xa5\0\xff\xff\xff\xff\xa5\0\xa5\0\xff\xff\xff\xff\xff\xff\xff\xff\x9c\0\xff\xff\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\xff\xff\xff\xff\xa5\0\xff\xff\xff\xff\xff\xff\xff\xff\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\xff\xff\xff\xff\xff\xff\xff\xff\x9c\0\xff\xff\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9d\0\xff\xff\xf7\0\x9d\0\x9d\0\xb2\0\xff\xff\xff\xff\xb2\0\xb2\0\xb9\0\xff\xff\xff\xff\xb9\0\xb9\0*\x01\xff\xff\xff\xff*\x01*\x01\xff\xff\xff\xff\xff\xff\x9d\0\xff\xff\xff\xff\xf7\0\xff\xff\xb2\0\xff\xff\xff\xff\xf7\0\xff\xff\xb9\0\xff\xff\xff\xff\xff\xff\x9d\0*\x01\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\xd1\0\xff\xff\xff\xff\xd1\0\xd1\0\xb2\0\xff\xff\xff\xff\xff\xff\xff\xff\xb9\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xff\xff\xd1\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xf7\0\xff\xff\xd1\0\xff\xff\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xb2\0l\x01\xff\xff\xff\xffl\x01\xb9\0\xff\xff\xff\xff\xff\xff\xbd\0|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01|\x01\xff\xff|\x01\xd5\0\xd8\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xa5\0\xa5\0\xa5\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xff\xff\xa5\0\xff\xff\xff\xff\xff\xff\xff\xffl\x01\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xff\xff\xff\xff\xff\xff\xff\xff\xd8\0\xff\xff\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xff\xff\xff\xff\xff\xff\xff\xffl\x01\xff\xff\xff\xff\xff\xff\x9d\0\x9d\0\x9d\0\xff\xff\xff\xff\xb2\0\xb2\0\xb2\0\xff\xff\xff\xff\xb9\0\xb9\0\xb9\0\xff\xff\x9d\0*\x01*\x01*\x01\xff\xff\xb2\0\xff\xff\xff\xff\xff\xff\xff\xff\xb9\0\xff\xff\xff\xff\xff\xff\xff\xff*\x01\xff\xff\xff\xff\xf7\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xd9\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xd1\0\xd1\0\xd1\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xff\xff\xd1\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xff\xff\xff\xff\xff\xff\xff\xff\xd9\0\xff\xff\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xd9\0\xda\0\xff\xffl\x01\xff\xff\xff\xff\xff\xff\xff\xff\xda\0\xff\xff\xda\0\xff\xff\xff\xff\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xff\xff\xff\xff\xff\xff\xff\xff\xda\0\xff\xff\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xdb\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xff\xff\xff\xff\xff\xff\xff\xff\xdb\0\xff\xff\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdd\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xff\xff\xff\xff\xff\xff\xff\xff\xdd\0\xff\xff\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xde\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xff\xff\xff\xff\xff\xff\xff\xff\xde\0\xff\xff\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xde\0\xdf\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xff\xff\xff\xff\xff\xff\xff\xff\xdf\0\xff\xff\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xdf\0\xe0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xff\xff\xff\xff\xff\xff\xff\xff\xe0\0\xff\xff\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe1\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xff\xff\xff\xff\xff\xff\xff\xff\xe1\0\xff\xff\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe2\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe2\0\xff\xff\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xff\xff\xff\xff\xff\xff\xff\xff\xe2\0\xff\xff\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe3\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xff\xff\xff\xff\xff\xff\xff\xff\xe3\0\xff\xff\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe4\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xff\xff\xff\xff\xff\xff\xff\xff\xe4\0\xff\xff\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe4\0\xe5\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xff\xff\xff\xff\xff\xff\xff\xff\xe5\0\xff\xff\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe5\0\xe6\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xff\xff\xff\xff\xff\xff\xff\xff\xe6\0\xff\xff\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe6\0\xe7\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xff\xff\xff\xff\xff\xff\xff\xff\xe7\0\xff\xff\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe8\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xff\xff\xff\xff\xff\xff\xff\xff\xe8\0\xff\xff\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe8\0\xe9\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe9\0\xff\xff\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xff\xff\xff\xff\xff\xff\xff\xff\xe9\0\xff\xff\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xea\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xff\xff\xff\xff\xff\xff\xff\xff\xea\0\xff\xff\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xea\0\xeb\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xeb\0\xff\xff\xeb\0\xff\xff\xff\xff\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xff\xff\xff\xff\xff\xff\xff\xff\xeb\0\xff\xff\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xec\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xff\xff\xff\xff\xff\xff\xff\xff\xec\0\xff\xff\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xed\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xff\xff\xff\xff\xff\xff\xff\xff\xed\0\xff\xff\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xed\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xee\0\xff\xff\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xef\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xff\xff\xff\xff\xff\xff\xff\xff\xef\0\xff\xff\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xef\0\xf2\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xff\xff\xff\xff\xff\xff\xff\xff\xf2\0\xff\xff\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf2\0\xf3\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xff\xff\xff\xff\xff\xff\xff\xff\xf3\0\xff\xff\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf3\0\xf4\0+\x01+\x01\xff\xff+\x01+\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xff\xff\xff\xff+\x01\xff\xff\xff\xff\xff\xff\xff\xff\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xff\xff\xff\xff\xff\xff\xff\xff\xf4\0\xff\xff\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf4\0\xf5\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xfc\0\xff\xff\xff\xff\xfc\0\xf5\0\xff\xff\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xf5\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfc\0\xfc\0\xfc\0\xfc\0\xfc\0\xfc\0\xfc\0\xfc\x005\x015\x015\x015\x015\x015\x015\x015\x015\x015\x015\x01\xff\xff\xff\xff\xff\xff:\x01\xff\xff\xff\xff:\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff+\x01+\x01+\x01\xff\xff\xff\xff\xff\xff\xff\xff\xfc\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfc\0+\x01\xff\xff\xff\xff\xfc\x005\x01\xff\xff\xff\xff\xff\xff:\x01\xff\xff\xff\xff\xfc\0\xff\xff\xff\xff\xff\xff\xfc\0\xff\xff\xfc\0\xfc\0\xfc\0\xff\xff\xfc\0\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff:\x01:\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\xff\xff\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\xff\xff\xff\xff\\\x01\xff\xff\0\x01\\\x01\\\x01\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\xff\xff\xff\xff\xff\xff\t\x01\t\x01\t\x01\t\x01\t\x01\t\x01\xff\xff\xff\xff\xff\xff\\\x01\xff\xff\xff\xff\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\xff\xff\f\x01\f\x01\f\x01\f\x01\f\x01\f\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\xff\xff\xa0\x01\xff\xff\xfc\0\xa0\x01\xff\xff\xff\xff\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\xff\xff\xff\xff\xff\xff\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\x10\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xa0\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\xff\xff\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x11\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01#\x01#\x01:\x01#\x01#\x01\xff\xff\xff\xff\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\xff\xff\xff\xff\xff\xff\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\x12\x01\xa0\x01#\x01\xff\xff\xff\xff\xa0\x01\xff\xff\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01#\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x13\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\xff\xffg\x01g\x01g\x01g\x01g\x01g\x01g\x01g\x01g\x01g\x01g\x01\xff\xff\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\x9b\x01\xff\xff\xff\xff\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01\x15\x01?\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\\\x01\\\x01\\\x01g\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x15\x01\xff\xff\xff\xff\xff\xff\xff\xff\\\x01\x9b\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01\xff\xff\xff\xff\xff\xff\xff\xff?\x01\xff\xff?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01?\x01\xff\xff}\x01}\x01}\x01}\x01}\x01}\x01}\x01}\x01}\x01}\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xa0\x01\xff\xff}\x01}\x01}\x01}\x01}\x01}\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff#\x01#\x01#\x01E\x01}\x01}\x01}\x01}\x01}\x01}\x01\xff\xff\xff\xff\xff\xff\xff\xff#\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff#\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01\xff\xff\xff\xff\xff\xff\xff\xffE\x01\xff\xffE\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01E\x01M\x01M\x01\xff\xffM\x01M\x01]\x01]\x01\xff\xff]\x01]\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffM\x01\xff\xffM\x01\xff\xffM\x01]\x01\xff\xffM\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffM\x01M\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffM\x01\xff\xffM\x01M\x01M\x01\xff\xff\xff\xffM\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01\xff\xff\xff\xff\xff\xff\xff\xffM\x01\xff\xffM\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01M\x01P\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffP\x01\xff\xff\xff\xffP\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffP\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\xff\xff\xff\xff\xff\xff\xff\xffP\x01\xff\xffP\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01P\x01\xff\xffs\x01\xff\xff\xff\xffs\x01\xff\xff\xff\xff\xff\xff\x91\x01\xff\xff\xff\xff\x91\x01\x91\x01\xff\xffM\x01M\x01M\x01\xff\xff\xff\xff]\x01]\x01]\x01u\x01u\x01\xff\xffs\x01\xff\xff\xff\xffM\x01s\x01s\x01\x91\x01\xff\xff]\x01\xff\xffu\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01M\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffs\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01\xff\xff\xff\xff\xff\xff\xff\xffu\x01\xff\xffu\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01u\x01z\x01\xff\xff\xff\xff\xff\xffs\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffz\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffz\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01\xff\xff\xff\xff\xff\xff\xff\xffz\x01\xff\xffz\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01z\x01\xff\xff\xff\xff~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01~\x01\xff\xff~\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff~\x01~\x01~\x01~\x01~\x01~\x01\xff\xff\xff\xff\x91\x01\x91\x01\x91\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x91\x01\xff\xff\xff\xff\xff\xff\x81\x01\xff\xff\xff\xff\xff\xff\xff\xffs\x01~\x01~\x01~\x01~\x01~\x01~\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\xff\xff\x81\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\xff\xff\xff\xff\xff\xff\xff\xff\x81\x01\xff\xff\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x81\x01\x83\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\xff\xff\x83\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\xff\xff\xff\xff\xff\xff\xff\xff\x83\x01\xff\xff\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x83\x01\x84\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\xff\xff\x84\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\xff\xff\xff\xff\xff\xff\xff\xff\x84\x01\xff\xff\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x84\x01\x85\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\xff\xff\x85\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\xff\xff\xff\xff\xff\xff\xff\xff\x85\x01\xff\xff\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x86\x01\x92\x01\x92\x01\xff\xff\x92\x01\x92\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\xff\xff\x86\x01\x92\x01\xff\xff\xff\xff\xff\xff\xff\xff\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\xff\xff\xff\xff\xff\xff\xff\xff\x86\x01\xff\xff\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x86\x01\x87\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\xff\xff\x87\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\xff\xff\xff\xff\xff\xff\xff\xff\x87\x01\xff\xff\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x87\x01\x89\x01\x89\x01\xff\xff\x89\x01\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\x92\x01\x92\x01\x92\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x92\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01\x89\x01\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x89\x01",
  lex_base_code: "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\n\0\x16\0\"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x02\0\0\0\0\0\0\0\x01\0\f\0\0\0\f\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0,\x006\0_\0B\0v\0L\0N\0\0\0\x81\0\0\0\x98\0\0\0\xa2\0\xac\0\xb6\0\0\0\xc0\0\0\0\xca\0\0\0\xe1\0\xeb\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x04\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x0e\x01\x1a\x01&\x01W\x01\0\0\0\0\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x07\0\0\0\0\0\0\0\0\0\0\0\0\0\t\0\x0b\0\r\0\x0f\0\xe5\0\x1a\0\b\0h\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0H\x01\0\0\0\0\0\0\0\0y\x01\r\0\x1c\0\x10\0\x1a\x01\x1d\0E\0\x83\x01\0\0\x8d\x01\x9a\x01\xa4\x01\xae\x01\0\0\0\0\xb8\x01\xc2\x01\xdb\x01\xe5\x01\x89\0\x8b\0\0\0\xf9\x01\0\0\x03\x02\0\0\r\x02\x17\x02\0\0!\x02\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  lex_backtrk_code: "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\f\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x0f\0\x0f\0\0\0\x0f\0\0\0\x0f\0\x0f\0\0\0#\0\0\0&\0)\0)\0)\0\0\0)\0)\0\0\0,\0\0\0/\0\0\0\0\0,\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0W\0W\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0h\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0W\0k\0k\0s\0\0\0s\0v\0v\0W\0k\0~\0k\0k\0&\0\x8f\0/\0\x94\0\x99\0\x99\0\x99\0\x99\0\x99\0\x9e\0\xa1\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  lex_default_code: "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  lex_trans_code: "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\t\0\0\0\t\0\t\0\t\0\t\0\t\0e\0\0\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\t\0\0\0\t\0\0\0\0\0\0\0\0\0e\0\0\0e\0\t\0e\0\0\0\0\0\0\0\0\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\0\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\0\0\x04\0\x04\0\x04\0\x04\0\x04\0\x04\0\x04\0\x04\0\x01\0\x01\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\0\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x18\0\x01\0\x01\0 \0 \0 \0 \0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0\t\0e\0\t\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0e\0e\x002\x002\x002\0\0\0\t\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0e\x002\0\t\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x1d\0\x8c\0\x8c\0\x8c\0\x8c\0\0\0\0\0\t\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x01\0e\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\x002\0\0\0\0\0\0\0\0\0\0\0\0\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x01\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\x12\0\0\0\0\0\0\0\0\0\0\0\0\0\x15\0\x15\0\x15\0\x15\0\x15\0\x15\x002\0\0\0\0\0M\0M\0M\0M\0M\0M\0M\0M\0M\0M\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0\0\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0M\0\0\0`\0`\0`\0`\0`\0`\0`\0`\0R\0R\x002\0\0\0\0\x002\x002\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0e\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x002\0M\0M\0M\0M\0M\0M\0M\0M\0M\0M\x002\0\0\0\0\x002\x002\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0\0\0\0\0\0\0e\0\0\0\0\0\0\0\0\x002\x002\x002\x002\x002\x002\x002\x002\x002\x002\x002\x002\0\0\0\0\0\0\0\0\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0\0\0\0\x002\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0R\0R\0R\0R\0R\0R\0R\0R\0R\0R\0{\0{\0{\0{\0{\0{\0{\0{\0{\0{\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0{\0{\0{\0{\0{\0{\0R\0\0\0\x81\0\x81\0\x81\0\x81\0\x81\0\x81\0\x81\0\x81\0\x86\0\x86\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0\0\0\0\0\0\0\0\0\0\0\0\0{\0{\0{\0{\0{\0{\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0\x89\0R\0\0\0\x86\0\x86\0\x86\0\x86\0\x86\0\x86\0\x86\0\x86\0\x86\0\x86\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0p\0{\0{\0{\0{\0{\0{\0{\0{\0{\0{\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0{\0{\0{\0{\0{\0{\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0{\0{\0{\0{\0{\0{\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0",
  lex_check_code: "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff5\0\xff\xff<\x005\x005\0<\0<\0\xb2\0\xff\xff\xb9\0\xb2\0\xb2\0\xb9\0\xb9\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff5\0\xff\xff<\0\xff\xff\xff\xff\xff\xff\xff\xff\xb2\0\xff\xff\xb9\0!\0\xa0\0\xff\xff\xff\xff\xff\xff\xff\xff\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1a\0\x1b\0\xff\xff\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1b\0\x1c\0\xff\xff\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0\x1c\0T\0T\0T\0T\0T\0T\0T\0T\0T\0T\0U\0U\0U\0U\0U\0U\0U\0U\0U\0U\0W\0\xff\xffW\0W\0W\0W\0W\0W\0W\0W\0W\0W\0Y\0Y\0Z\0Z\0>\0@\0@\0@\0@\0@\0@\0@\0@\0@\0@\0@\0A\0\xbb\0=\0V\0V\0V\0V\0V\0V\0V\0V\0V\0V\0\xba\0\xbe\0\xd2\0\xd3\0\xd6\0\xff\xff?\0V\0V\0V\0V\0V\0V\0X\0X\0X\0X\0X\0X\0X\0X\0\xbc\0\xd4\0@\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\\\0\xe4\0\xe4\0\xe5\0\xe5\0\xff\xff\xff\xffB\0V\0V\0V\0V\0V\0V\0^\0\xbf\0^\0^\0^\0^\0^\0^\0^\0^\0^\0^\0`\0`\0`\0`\0`\0`\0`\0`\0`\0`\0a\0a\0a\0a\0a\0a\0a\0a\0a\0a\0b\0b\0b\0b\0b\0b\0b\0b\0b\0b\0d\0d\0d\0d\0d\0d\0d\0d\0d\0d\0f\0f\0f\0f\0f\0f\0f\0f\0f\0f\0\xd7\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfff\0f\0f\0f\0f\0f\0h\0h\0h\0h\0h\0h\0h\0h\0h\0h\0i\0i\0i\0i\0i\0i\0i\0i\0i\0i\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfff\0f\0f\0f\0f\0f\0\x85\0\xff\xff\xff\xff\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x85\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9a\0\x9b\0\xff\xff\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9b\0\x9c\0\xff\xff\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9c\0\x9d\0\xff\xff\xff\xff\x9d\0\x9d\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xbd\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x9d\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xcc\0\xd1\0\xff\xff\xff\xff\xd1\0\xd1\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\x9d\0\xff\xff\xff\xff\xff\xff\xbd\0\xff\xff\xff\xff\xff\xff\xff\xff\xd1\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xd5\0\xff\xff\xff\xff\xff\xff\xff\xff\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd1\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xd8\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xda\0\xff\xff\xff\xff\xd5\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdb\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdc\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xdd\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe0\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe2\0\xff\xff\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe2\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xe3\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe1\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe7\0\xe9\0\xff\xff\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xe9\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xeb\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xec\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xee\0\xee\0\xee\0\xee\0\xee\0\xee\0\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff",
  lex_code: "\xff\x01\xff\xff\x03\xff\x01\xff\xff\x02\xff\xff\0\x02\xff\0\x01\xff\x06\xff\xff\x07\xff\xff\x01\xff\x03\xff\xff\x05\xff\xff\x04\xff\xff\0\x04\xff\0\x05\xff\0\x03\xff\0\x06\xff\0\x07\xff\x11\xff\x10\xff\x0e\xff\r\xff\f\xff\x0b\xff\n\xff\t\xff\b\xff\x07\xff\x06\xff\x05\xff\x04\xff\xff\x13\xff\x12\xff\xff\x12\xff\x13\xff\xff\x03\x11\x02\x12\x01\x0f\0\x10\xff\x16\xff\x13\xff\xff\x14\xff\xff\0\x14\xff\x01\x13\0\x0e\xff\x15\xff\xff\0\r\xff\x01\x15\0\f\xff\x19\xff\xff\0\t\xff\x13\xff\x16\xff\xff\x13\xff\xff\x18\xff\xff\x17\xff\xff\x01\x17\0\x04\xff\x01\x18\0\x06\xff\x01\x16\0\b\xff\0\x0b\xff\x01\x19\0\n\xff"
};

function token(env, lexbuf) {
  lexbuf.lex_mem = Caml_array.caml_make_vect(8, -1);
  var ___ocaml_lex_state = 0;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.new_engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          Lexing.new_line(lexbuf);
          return token(env, lexbuf);
      case 1 :
          var env$1 = lex_error(env, from_lb(env.lex_source, lexbuf), {
                TAG: /* UnexpectedToken */1,
                _0: "ILLEGAL"
              });
          return token(env$1, lexbuf);
      case 2 :
          unicode_fix_cols(lexbuf);
          return token(env, lexbuf);
      case 3 :
          var start = from_lb(env.lex_source, lexbuf);
          var buf = $$Buffer.create(127);
          var match = comment(env, buf, lexbuf);
          var env$2 = save_comment(match[0], start, match[1], buf, true);
          return token(env$2, lexbuf);
      case 4 :
          var sp = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos + 2 | 0, Caml_array.get(lexbuf.lex_mem, 0));
          var escape_type = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), lexbuf.lex_curr_pos);
          var pattern = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_curr_pos);
          if (env.lex_enable_comment_syntax) {
            var env$3;
            if (env.lex_in_comment_syntax) {
              var loc = from_lb(env.lex_source, lexbuf);
              env$3 = unexpected_error(env, loc, pattern);
            } else {
              env$3 = env;
            }
            var env$4 = in_comment_syntax(true, env$3);
            if (escape_type === ":") {
              return [
                      env$4,
                      /* T_COLON */77
                    ];
            } else {
              return token(env$4, lexbuf);
            }
          }
          var start$1 = from_lb(env.lex_source, lexbuf);
          var buf$1 = $$Buffer.create(127);
          $$Buffer.add_string(buf$1, sp);
          $$Buffer.add_string(buf$1, escape_type);
          var match$1 = comment(env, buf$1, lexbuf);
          var env$5 = save_comment(match$1[0], start$1, match$1[1], buf$1, true);
          return token(env$5, lexbuf);
      case 5 :
          if (env.lex_in_comment_syntax) {
            var env$6 = in_comment_syntax(false, env);
            return token(env$6, lexbuf);
          }
          yyback(1, lexbuf);
          return [
                  env,
                  /* T_MULT */97
                ];
      case 6 :
          var start$2 = from_lb(env.lex_source, lexbuf);
          var buf$2 = $$Buffer.create(127);
          var match$2 = line_comment(env, buf$2, lexbuf);
          var env$7 = save_comment(match$2[0], start$2, match$2[1], buf$2, false);
          return token(env$7, lexbuf);
      case 7 :
          if (lexbuf.lex_start_pos !== 0) {
            return [
                    env,
                    /* T_ERROR */104
                  ];
          }
          var match$3 = line_comment(env, $$Buffer.create(127), lexbuf);
          return token(match$3[0], lexbuf);
      case 8 :
          var quote = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          var start$3 = from_lb(env.lex_source, lexbuf);
          var buf$3 = $$Buffer.create(127);
          var raw = $$Buffer.create(127);
          $$Buffer.add_char(raw, quote);
          var match$4 = string_quote(env, quote, buf$3, raw, false, lexbuf);
          return [
                  match$4[0],
                  {
                    TAG: /* T_STRING */1,
                    _0: [
                      btwn(start$3, match$4[1]),
                      $$Buffer.contents(buf$3),
                      $$Buffer.contents(raw),
                      match$4[2]
                    ]
                  }
                ];
      case 9 :
          var cooked = $$Buffer.create(127);
          var raw$1 = $$Buffer.create(127);
          var literal = $$Buffer.create(127);
          $$Buffer.add_string(literal, Lexing.lexeme(lexbuf));
          var start$4 = from_lb(env.lex_source, lexbuf);
          var match$5 = template_part(env, start$4, cooked, raw$1, literal, lexbuf);
          return [
                  match$5[0],
                  {
                    TAG: /* T_TEMPLATE_PART */2,
                    _0: [
                      match$5[1],
                      {
                        cooked: $$Buffer.contents(cooked),
                        raw: $$Buffer.contents(raw$1),
                        literal: $$Buffer.contents(literal)
                      },
                      match$5[2]
                    ]
                  }
                ];
      case 10 :
          var w = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), lexbuf.lex_curr_pos);
          return illegal_number(env, lexbuf, w, {
                      TAG: /* T_NUMBER */0,
                      _0: /* BINARY */0
                    });
      case 11 :
          return [
                  env,
                  {
                    TAG: /* T_NUMBER */0,
                    _0: /* BINARY */0
                  }
                ];
      case 12 :
          var w$1 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), lexbuf.lex_curr_pos);
          return illegal_number(env, lexbuf, w$1, {
                      TAG: /* T_NUMBER */0,
                      _0: /* OCTAL */2
                    });
      case 13 :
          return [
                  env,
                  {
                    TAG: /* T_NUMBER */0,
                    _0: /* OCTAL */2
                  }
                ];
      case 14 :
          var w$2 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), lexbuf.lex_curr_pos);
          return illegal_number(env, lexbuf, w$2, {
                      TAG: /* T_NUMBER */0,
                      _0: /* LEGACY_OCTAL */1
                    });
      case 15 :
          return [
                  env,
                  {
                    TAG: /* T_NUMBER */0,
                    _0: /* LEGACY_OCTAL */1
                  }
                ];
      case 16 :
      case 18 :
      case 20 :
          break;
      case 17 :
      case 19 :
      case 21 :
          return [
                  env,
                  {
                    TAG: /* T_NUMBER */0,
                    _0: /* NORMAL */3
                  }
                ];
      case 22 :
          var word = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_curr_pos);
          unicode_fix_cols(lexbuf);
          try {
            return [
                    env,
                    Hashtbl.find(keywords, word)
                  ];
          }
          catch (raw_exn){
            var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
            if (exn.RE_EXN_ID === "Not_found") {
              return [
                      env,
                      /* T_IDENTIFIER */0
                    ];
            }
            throw exn;
          }
      case 23 :
          return [
                  env,
                  /* T_LCURLY */1
                ];
      case 24 :
          return [
                  env,
                  /* T_RCURLY */2
                ];
      case 25 :
          return [
                  env,
                  /* T_LPAREN */3
                ];
      case 26 :
          return [
                  env,
                  /* T_RPAREN */4
                ];
      case 27 :
          return [
                  env,
                  /* T_LBRACKET */5
                ];
      case 28 :
          return [
                  env,
                  /* T_RBRACKET */6
                ];
      case 29 :
          return [
                  env,
                  /* T_ELLIPSIS */11
                ];
      case 30 :
          return [
                  env,
                  /* T_PERIOD */9
                ];
      case 31 :
          return [
                  env,
                  /* T_SEMICOLON */7
                ];
      case 32 :
          return [
                  env,
                  /* T_COMMA */8
                ];
      case 33 :
          return [
                  env,
                  /* T_COLON */77
                ];
      case 34 :
          return [
                  env,
                  /* T_PLING */76
                ];
      case 35 :
          return [
                  env,
                  /* T_AND */79
                ];
      case 36 :
          return [
                  env,
                  /* T_OR */78
                ];
      case 37 :
          return [
                  env,
                  /* T_STRICT_EQUAL */85
                ];
      case 38 :
          return [
                  env,
                  /* T_STRICT_NOT_EQUAL */86
                ];
      case 39 :
          return [
                  env,
                  /* T_LESS_THAN_EQUAL */87
                ];
      case 40 :
          return [
                  env,
                  /* T_GREATER_THAN_EQUAL */88
                ];
      case 41 :
          return [
                  env,
                  /* T_EQUAL */83
                ];
      case 42 :
          return [
                  env,
                  /* T_NOT_EQUAL */84
                ];
      case 43 :
          return [
                  env,
                  /* T_INCR */102
                ];
      case 44 :
          return [
                  env,
                  /* T_DECR */103
                ];
      case 45 :
          return [
                  env,
                  /* T_LSHIFT_ASSIGN */65
                ];
      case 46 :
          return [
                  env,
                  /* T_LSHIFT */91
                ];
      case 47 :
          return [
                  env,
                  /* T_RSHIFT_ASSIGN */64
                ];
      case 48 :
          return [
                  env,
                  /* T_RSHIFT3_ASSIGN */63
                ];
      case 49 :
          return [
                  env,
                  /* T_RSHIFT3 */93
                ];
      case 50 :
          return [
                  env,
                  /* T_RSHIFT */92
                ];
      case 51 :
          return [
                  env,
                  /* T_PLUS_ASSIGN */74
                ];
      case 52 :
          return [
                  env,
                  /* T_MINUS_ASSIGN */73
                ];
      case 53 :
          return [
                  env,
                  /* T_MULT_ASSIGN */71
                ];
      case 54 :
          return [
                  env,
                  /* T_EXP_ASSIGN */72
                ];
      case 55 :
          return [
                  env,
                  /* T_MOD_ASSIGN */69
                ];
      case 56 :
          return [
                  env,
                  /* T_BIT_AND_ASSIGN */68
                ];
      case 57 :
          return [
                  env,
                  /* T_BIT_OR_ASSIGN */67
                ];
      case 58 :
          return [
                  env,
                  /* T_BIT_XOR_ASSIGN */66
                ];
      case 59 :
          return [
                  env,
                  /* T_LESS_THAN */89
                ];
      case 60 :
          return [
                  env,
                  /* T_GREATER_THAN */90
                ];
      case 61 :
          return [
                  env,
                  /* T_PLUS */94
                ];
      case 62 :
          return [
                  env,
                  /* T_MINUS */95
                ];
      case 63 :
          return [
                  env,
                  /* T_MULT */97
                ];
      case 64 :
          return [
                  env,
                  /* T_EXP */98
                ];
      case 65 :
          return [
                  env,
                  /* T_MOD */99
                ];
      case 66 :
          return [
                  env,
                  /* T_BIT_OR */80
                ];
      case 67 :
          return [
                  env,
                  /* T_BIT_AND */82
                ];
      case 68 :
          return [
                  env,
                  /* T_BIT_XOR */81
                ];
      case 69 :
          return [
                  env,
                  /* T_NOT */100
                ];
      case 70 :
          return [
                  env,
                  /* T_BIT_NOT */101
                ];
      case 71 :
          return [
                  env,
                  /* T_ASSIGN */75
                ];
      case 72 :
          return [
                  env,
                  /* T_ARROW */10
                ];
      case 73 :
          return [
                  env,
                  /* T_DIV_ASSIGN */70
                ];
      case 74 :
          return [
                  env,
                  /* T_DIV */96
                ];
      case 75 :
          return [
                  env,
                  /* T_AT */12
                ];
      case 76 :
          var env$8;
          if (env.lex_in_comment_syntax) {
            var loc$1 = from_lb(env.lex_source, lexbuf);
            env$8 = lex_error(env, loc$1, /* UnexpectedEOS */4);
          } else {
            env$8 = env;
          }
          return [
                  env$8,
                  /* T_EOF */105
                ];
      case 77 :
          var env$9 = lex_error(env, from_lb(env.lex_source, lexbuf), {
                TAG: /* UnexpectedToken */1,
                _0: "ILLEGAL"
              });
          return [
                  env$9,
                  /* T_ERROR */104
                ];
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
    var w$3 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), lexbuf.lex_curr_pos);
    return illegal_number(env, lexbuf, w$3, {
                TAG: /* T_NUMBER */0,
                _0: /* NORMAL */3
              });
  };
}

function string_quote(env, q, buf, raw, octal, lexbuf) {
  var ___ocaml_lex_state = 247;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          var q$prime = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(raw, q$prime);
          if (q === q$prime) {
            return [
                    env,
                    from_lb(env.lex_source, lexbuf),
                    octal
                  ];
          } else {
            $$Buffer.add_char(buf, q$prime);
            return string_quote(env, q, buf, raw, octal, lexbuf);
          }
      case 1 :
          var e = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(raw, e);
          var match = string_escape(env, buf, lexbuf);
          var octal$1 = match[1] || octal;
          $$Buffer.add_string(raw, Lexing.lexeme(lexbuf));
          return string_quote(match[0], q, buf, raw, octal$1, lexbuf);
      case 2 :
          var x = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_curr_pos);
          $$Buffer.add_string(raw, x);
          var env$1 = lex_error(env, from_lb(env.lex_source, lexbuf), {
                TAG: /* UnexpectedToken */1,
                _0: "ILLEGAL"
              });
          $$Buffer.add_string(buf, x);
          return [
                  env$1,
                  from_lb(env$1.lex_source, lexbuf),
                  octal
                ];
      case 3 :
          var x$1 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(raw, x$1);
          $$Buffer.add_char(buf, x$1);
          return string_quote(env, q, buf, raw, octal, lexbuf);
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function type_token(env, lexbuf) {
  lexbuf.lex_mem = Caml_array.caml_make_vect(26, -1);
  Caml_array.set(lexbuf.lex_mem, 17, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 16, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 15, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 14, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 13, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 12, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 11, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 10, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 9, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 8, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 7, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 6, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 5, lexbuf.lex_curr_pos);
  Caml_array.set(lexbuf.lex_mem, 4, lexbuf.lex_curr_pos);
  var ___ocaml_lex_state = 133;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.new_engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          Lexing.new_line(lexbuf);
          return type_token(env, lexbuf);
      case 1 :
          unicode_fix_cols(lexbuf);
          return type_token(env, lexbuf);
      case 2 :
          var start = from_lb(env.lex_source, lexbuf);
          var buf = $$Buffer.create(127);
          var match = comment(env, buf, lexbuf);
          var env$1 = save_comment(match[0], start, match[1], buf, true);
          return type_token(env$1, lexbuf);
      case 3 :
          var sp = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos + 2 | 0, Caml_array.get(lexbuf.lex_mem, 0));
          var escape_type = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), lexbuf.lex_curr_pos);
          var pattern = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_curr_pos);
          if (env.lex_enable_comment_syntax) {
            var env$2;
            if (env.lex_in_comment_syntax) {
              var loc = from_lb(env.lex_source, lexbuf);
              env$2 = unexpected_error(env, loc, pattern);
            } else {
              env$2 = env;
            }
            var env$3 = in_comment_syntax(true, env$2);
            if (escape_type === ":") {
              return [
                      env$3,
                      /* T_COLON */77
                    ];
            } else {
              return type_token(env$3, lexbuf);
            }
          }
          var start$1 = from_lb(env.lex_source, lexbuf);
          var buf$1 = $$Buffer.create(127);
          $$Buffer.add_string(buf$1, sp);
          $$Buffer.add_string(buf$1, escape_type);
          var match$1 = comment(env, buf$1, lexbuf);
          var env$4 = save_comment(match$1[0], start$1, match$1[1], buf$1, true);
          return type_token(env$4, lexbuf);
      case 4 :
          if (env.lex_in_comment_syntax) {
            var env$5 = in_comment_syntax(false, env);
            return type_token(env$5, lexbuf);
          }
          yyback(1, lexbuf);
          return [
                  env,
                  /* T_MULT */97
                ];
      case 5 :
          var start$2 = from_lb(env.lex_source, lexbuf);
          var buf$2 = $$Buffer.create(127);
          var match$2 = line_comment(env, buf$2, lexbuf);
          var env$6 = save_comment(match$2[0], start$2, match$2[1], buf$2, true);
          return type_token(env$6, lexbuf);
      case 6 :
          var quote = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          var start$3 = from_lb(env.lex_source, lexbuf);
          var buf$3 = $$Buffer.create(127);
          var raw = $$Buffer.create(127);
          $$Buffer.add_char(raw, quote);
          var match$3 = string_quote(env, quote, buf$3, raw, false, lexbuf);
          return [
                  match$3[0],
                  {
                    TAG: /* T_STRING */1,
                    _0: [
                      btwn(start$3, match$3[1]),
                      $$Buffer.contents(buf$3),
                      $$Buffer.contents(raw),
                      match$3[2]
                    ]
                  }
                ];
      case 7 :
          var neg = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, Caml_array.get(lexbuf.lex_mem, 0));
          var num = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), Caml_array.get(lexbuf.lex_mem, 1));
          var w = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 1), lexbuf.lex_curr_pos);
          return illegal_number(env, lexbuf, w, mk_num_singleton(/* BINARY */0, num, neg));
      case 8 :
          var neg$1 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, Caml_array.get(lexbuf.lex_mem, 0));
          var num$1 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), lexbuf.lex_curr_pos);
          return [
                  env,
                  mk_num_singleton(/* BINARY */0, num$1, neg$1)
                ];
      case 9 :
          var neg$2 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, Caml_array.get(lexbuf.lex_mem, 0));
          var num$2 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), Caml_array.get(lexbuf.lex_mem, 1));
          var w$1 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 1), lexbuf.lex_curr_pos);
          return illegal_number(env, lexbuf, w$1, mk_num_singleton(/* OCTAL */2, num$2, neg$2));
      case 10 :
          var neg$3 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, Caml_array.get(lexbuf.lex_mem, 0));
          var num$3 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), lexbuf.lex_curr_pos);
          return [
                  env,
                  mk_num_singleton(/* OCTAL */2, num$3, neg$3)
                ];
      case 11 :
          var neg$4 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, Caml_array.get(lexbuf.lex_mem, 0));
          var num$4 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), Caml_array.get(lexbuf.lex_mem, 1));
          var w$2 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 1), lexbuf.lex_curr_pos);
          return illegal_number(env, lexbuf, w$2, mk_num_singleton(/* LEGACY_OCTAL */1, num$4, neg$4));
      case 12 :
          var neg$5 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, Caml_array.get(lexbuf.lex_mem, 0));
          var num$5 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), lexbuf.lex_curr_pos);
          return [
                  env,
                  mk_num_singleton(/* LEGACY_OCTAL */1, num$5, neg$5)
                ];
      case 13 :
          var neg$6 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, Caml_array.get(lexbuf.lex_mem, 0));
          var num$6 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), Caml_array.get(lexbuf.lex_mem, 1));
          var w$3 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 1), lexbuf.lex_curr_pos);
          var match$4;
          try {
            match$4 = [
              env,
              mk_num_singleton(/* NORMAL */3, num$6, neg$6)
            ];
          }
          catch (exn){
            if (Sys.win32) {
              var loc$1 = from_lb(env.lex_source, lexbuf);
              var env$7 = lex_error(env, loc$1, /* WindowsFloatOfString */59);
              match$4 = [
                env$7,
                {
                  TAG: /* T_NUMBER_SINGLETON_TYPE */5,
                  _0: /* NORMAL */3,
                  _1: 789.0
                }
              ];
            } else {
              throw exn;
            }
          }
          return illegal_number(match$4[0], lexbuf, w$3, match$4[1]);
      case 14 :
          var neg$7 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, Caml_array.get(lexbuf.lex_mem, 0));
          var num$7 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), lexbuf.lex_curr_pos);
          try {
            return [
                    env,
                    mk_num_singleton(/* NORMAL */3, num$7, neg$7)
                  ];
          }
          catch (exn$1){
            if (Sys.win32) {
              var loc$2 = from_lb(env.lex_source, lexbuf);
              var env$8 = lex_error(env, loc$2, /* WindowsFloatOfString */59);
              return [
                      env$8,
                      {
                        TAG: /* T_NUMBER_SINGLETON_TYPE */5,
                        _0: /* NORMAL */3,
                        _1: 789.0
                      }
                    ];
            }
            throw exn$1;
          }
      case 15 :
          var neg$8 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, Caml_array.get(lexbuf.lex_mem, 0));
          var num$8 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), Caml_array.get(lexbuf.lex_mem, 1));
          var w$4 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 1), lexbuf.lex_curr_pos);
          return illegal_number(env, lexbuf, w$4, mk_num_singleton(/* NORMAL */3, num$8, neg$8));
      case 16 :
          var neg$9 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, Caml_array.get(lexbuf.lex_mem, 0));
          var num$9 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), lexbuf.lex_curr_pos);
          return [
                  env,
                  mk_num_singleton(/* NORMAL */3, num$9, neg$9)
                ];
      case 17 :
          var neg$10 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, Caml_array.get(lexbuf.lex_mem, 0));
          var num$10 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 0), Caml_array.get(lexbuf.lex_mem, 1));
          var w$5 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 1), lexbuf.lex_curr_pos);
          return illegal_number(env, lexbuf, w$5, mk_num_singleton(/* NORMAL */3, num$10, neg$10));
      case 18 :
          var neg$11 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 1), Caml_array.get(lexbuf.lex_mem, 0));
          var num$11 = Lexing.sub_lexeme(lexbuf, Caml_array.get(lexbuf.lex_mem, 3), Caml_array.get(lexbuf.lex_mem, 2));
          return [
                  env,
                  mk_num_singleton(/* NORMAL */3, num$11, neg$11)
                ];
      case 19 :
          var word = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_curr_pos);
          unicode_fix_cols(lexbuf);
          try {
            return [
                    env,
                    Hashtbl.find(type_keywords, word)
                  ];
          }
          catch (raw_exn){
            var exn$2 = Caml_js_exceptions.internalToOCamlException(raw_exn);
            if (exn$2.RE_EXN_ID === "Not_found") {
              return [
                      env,
                      /* T_IDENTIFIER */0
                    ];
            }
            throw exn$2;
          }
      case 22 :
          return [
                  env,
                  /* T_LCURLY */1
                ];
      case 23 :
          return [
                  env,
                  /* T_RCURLY */2
                ];
      case 24 :
          return [
                  env,
                  /* T_LPAREN */3
                ];
      case 25 :
          return [
                  env,
                  /* T_RPAREN */4
                ];
      case 26 :
          return [
                  env,
                  /* T_ELLIPSIS */11
                ];
      case 27 :
          return [
                  env,
                  /* T_PERIOD */9
                ];
      case 28 :
          return [
                  env,
                  /* T_SEMICOLON */7
                ];
      case 29 :
          return [
                  env,
                  /* T_COMMA */8
                ];
      case 20 :
      case 32 :
          return [
                  env,
                  /* T_LBRACKET */5
                ];
      case 21 :
      case 33 :
          return [
                  env,
                  /* T_RBRACKET */6
                ];
      case 34 :
          return [
                  env,
                  /* T_LESS_THAN */89
                ];
      case 35 :
          return [
                  env,
                  /* T_GREATER_THAN */90
                ];
      case 31 :
      case 37 :
          return [
                  env,
                  /* T_PLING */76
                ];
      case 38 :
          return [
                  env,
                  /* T_MULT */97
                ];
      case 30 :
      case 39 :
          return [
                  env,
                  /* T_COLON */77
                ];
      case 40 :
          return [
                  env,
                  /* T_BIT_OR */80
                ];
      case 41 :
          return [
                  env,
                  /* T_BIT_AND */82
                ];
      case 42 :
          return [
                  env,
                  /* T_TYPEOF */44
                ];
      case 43 :
          return [
                  env,
                  /* T_ARROW */10
                ];
      case 36 :
      case 44 :
          return [
                  env,
                  /* T_ASSIGN */75
                ];
      case 45 :
          return [
                  env,
                  /* T_PLUS */94
                ];
      case 46 :
          return [
                  env,
                  /* T_MINUS */95
                ];
      case 47 :
          var env$9;
          if (env.lex_in_comment_syntax) {
            var loc$3 = from_lb(env.lex_source, lexbuf);
            env$9 = lex_error(env, loc$3, /* UnexpectedEOS */4);
          } else {
            env$9 = env;
          }
          return [
                  env$9,
                  /* T_EOF */105
                ];
      case 48 :
          return [
                  env,
                  /* T_ERROR */104
                ];
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function line_comment(env, buf, lexbuf) {
  var ___ocaml_lex_state = 287;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          return [
                  env,
                  from_lb(env.lex_source, lexbuf)
                ];
      case 1 :
          var match = from_lb(env.lex_source, lexbuf);
          var match$1 = match._end;
          Lexing.new_line(lexbuf);
          var _end_line = match$1.line;
          var _end_column = match$1.column - 1 | 0;
          var _end_offset = match$1.offset - 1 | 0;
          var _end = {
            line: _end_line,
            column: _end_column,
            offset: _end_offset
          };
          return [
                  env,
                  {
                    source: match.source,
                    start: match.start,
                    _end: _end
                  }
                ];
      case 2 :
          var c = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(buf, c);
          return line_comment(env, buf, lexbuf);
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function comment(env, buf, lexbuf) {
  var ___ocaml_lex_state = 279;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          var env$1 = lex_error(env, from_lb(env.lex_source, lexbuf), {
                TAG: /* UnexpectedToken */1,
                _0: "ILLEGAL"
              });
          return [
                  env$1,
                  from_lb(env$1.lex_source, lexbuf)
                ];
      case 1 :
          Lexing.new_line(lexbuf);
          $$Buffer.add_char(buf, /* "\n" */10);
          return comment(env, buf, lexbuf);
      case 2 :
          var loc = from_lb(env.lex_source, lexbuf);
          var env$2 = env.lex_in_comment_syntax ? unexpected_error_w_suggest(env, loc, "*/", "*-/") : env;
          return [
                  env$2,
                  loc
                ];
      case 3 :
          if (env.lex_in_comment_syntax) {
            return [
                    env,
                    from_lb(env.lex_source, lexbuf)
                  ];
          } else {
            $$Buffer.add_string(buf, "*-/");
            return comment(env, buf, lexbuf);
          }
      case 4 :
          var c = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(buf, c);
          return comment(env, buf, lexbuf);
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function jsx_text(env, mode, buf, raw, lexbuf) {
  var ___ocaml_lex_state = 371;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          var c = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          switch (mode) {
            case /* JSX_SINGLE_QUOTED_TEXT */0 :
                if (c === 39) {
                  return [
                          env,
                          from_lb(env.lex_source, lexbuf)
                        ];
                }
                break;
            case /* JSX_DOUBLE_QUOTED_TEXT */1 :
                if (c === 34) {
                  return [
                          env,
                          from_lb(env.lex_source, lexbuf)
                        ];
                }
                break;
            case /* JSX_CHILD_TEXT */2 :
                var exit = 0;
                if (!(c !== 60 && c !== 123)) {
                  exit = 2;
                }
                if (exit === 2) {
                  back(lexbuf);
                  return [
                          env,
                          from_lb(env.lex_source, lexbuf)
                        ];
                }
                break;
            
          }
          $$Buffer.add_char(raw, c);
          $$Buffer.add_char(buf, c);
          return jsx_text(env, mode, buf, raw, lexbuf);
      case 1 :
          var env$1 = lex_error(env, from_lb(env.lex_source, lexbuf), {
                TAG: /* UnexpectedToken */1,
                _0: "ILLEGAL"
              });
          return [
                  env$1,
                  from_lb(env$1.lex_source, lexbuf)
                ];
      case 2 :
          var lt = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_curr_pos);
          $$Buffer.add_string(raw, lt);
          $$Buffer.add_string(buf, lt);
          Lexing.new_line(lexbuf);
          return jsx_text(env, mode, buf, raw, lexbuf);
      case 3 :
          var n = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos + 3 | 0, lexbuf.lex_curr_pos - 1 | 0);
          var s = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_curr_pos);
          $$Buffer.add_string(raw, s);
          var code = Caml_format.caml_int_of_string("0x" + n);
          List.iter((function (param) {
                  return $$Buffer.add_char(buf, param);
                }), utf16to8(code));
          return jsx_text(env, mode, buf, raw, lexbuf);
      case 4 :
          var n$1 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos + 2 | 0, lexbuf.lex_curr_pos - 1 | 0);
          var s$1 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_curr_pos);
          $$Buffer.add_string(raw, s$1);
          var code$1 = Caml_format.caml_int_of_string(n$1);
          List.iter((function (param) {
                  return $$Buffer.add_char(buf, param);
                }), utf16to8(code$1));
          return jsx_text(env, mode, buf, raw, lexbuf);
      case 5 :
          var entity = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos + 1 | 0, lexbuf.lex_curr_pos - 1 | 0);
          var s$2 = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_curr_pos);
          $$Buffer.add_string(raw, s$2);
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
                    return $$Buffer.add_char(buf, param);
                  }), utf16to8(code$2));
          } else {
            $$Buffer.add_string(buf, "&" + (entity + ";"));
          }
          return jsx_text(env, mode, buf, raw, lexbuf);
      case 6 :
          var c$1 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(raw, c$1);
          $$Buffer.add_char(buf, c$1);
          return jsx_text(env, mode, buf, raw, lexbuf);
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function regexp_class(env, buf, lexbuf) {
  var ___ocaml_lex_state = 326;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          return env;
      case 1 :
      case 2 :
          break;
      case 3 :
          var c = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(buf, c);
          return env;
      case 4 :
          var c$1 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(buf, c$1);
          return regexp_class(env, buf, lexbuf);
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
    var s = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_start_pos + 2 | 0);
    $$Buffer.add_string(buf, s);
    return regexp_class(env, buf, lexbuf);
  };
}

function template_part(env, start, cooked, raw, literal, lexbuf) {
  var ___ocaml_lex_state = 416;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          var env$1 = lex_error(env, from_lb(env.lex_source, lexbuf), {
                TAG: /* UnexpectedToken */1,
                _0: "ILLEGAL"
              });
          return [
                  env$1,
                  btwn(start, from_lb(env$1.lex_source, lexbuf)),
                  true
                ];
      case 1 :
          $$Buffer.add_char(literal, /* "`" */96);
          return [
                  env,
                  btwn(start, from_lb(env.lex_source, lexbuf)),
                  true
                ];
      case 2 :
          $$Buffer.add_string(literal, "${");
          return [
                  env,
                  btwn(start, from_lb(env.lex_source, lexbuf)),
                  false
                ];
      case 3 :
          $$Buffer.add_char(raw, /* "\\" */92);
          $$Buffer.add_char(literal, /* "\\" */92);
          var match = string_escape(env, cooked, lexbuf);
          var str = Lexing.lexeme(lexbuf);
          $$Buffer.add_string(raw, str);
          $$Buffer.add_string(literal, str);
          return template_part(match[0], start, cooked, raw, literal, lexbuf);
      case 4 :
          var lf = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_start_pos + 2 | 0);
          $$Buffer.add_string(raw, lf);
          $$Buffer.add_string(literal, lf);
          $$Buffer.add_string(cooked, "\n");
          Lexing.new_line(lexbuf);
          return template_part(env, start, cooked, raw, literal, lexbuf);
      case 5 :
          var lf$1 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(raw, lf$1);
          $$Buffer.add_char(literal, lf$1);
          $$Buffer.add_char(cooked, /* "\n" */10);
          Lexing.new_line(lexbuf);
          return template_part(env, start, cooked, raw, literal, lexbuf);
      case 6 :
          var c = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(raw, c);
          $$Buffer.add_char(literal, c);
          $$Buffer.add_char(cooked, c);
          return template_part(env, start, cooked, raw, literal, lexbuf);
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
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
          var start = from_lb(env.lex_source, lexbuf);
          var buf = $$Buffer.create(127);
          var match = line_comment(env, buf, lexbuf);
          var env$1 = save_comment(match[0], start, match[1], buf, true);
          ___ocaml_lex_state = 393;
          _env = env$1;
          continue ;
      case 3 :
          var start$1 = from_lb(env.lex_source, lexbuf);
          var buf$1 = $$Buffer.create(127);
          var match$1 = comment(env, buf$1, lexbuf);
          var env$2 = save_comment(match$1[0], start$1, match$1[1], buf$1, true);
          ___ocaml_lex_state = 393;
          _env = env$2;
          continue ;
      case 4 :
          var start$2 = from_lb(env.lex_source, lexbuf);
          var cooked = $$Buffer.create(127);
          var raw = $$Buffer.create(127);
          var literal = $$Buffer.create(127);
          $$Buffer.add_string(literal, "}");
          var match$2 = template_part(env, start$2, cooked, raw, literal, lexbuf);
          return [
                  match$2[0],
                  {
                    TAG: /* T_TEMPLATE_PART */2,
                    _0: [
                      match$2[1],
                      {
                        cooked: $$Buffer.contents(cooked),
                        raw: $$Buffer.contents(raw),
                        literal: $$Buffer.contents(literal)
                      },
                      match$2[2]
                    ]
                  }
                ];
      case 5 :
          var env$3 = lex_error(env, from_lb(env.lex_source, lexbuf), {
                TAG: /* UnexpectedToken */1,
                _0: "ILLEGAL"
              });
          return [
                  env$3,
                  {
                    TAG: /* T_TEMPLATE_PART */2,
                    _0: [
                      from_lb(env$3.lex_source, lexbuf),
                      {
                        cooked: "",
                        raw: "",
                        literal: ""
                      },
                      true
                    ]
                  }
                ];
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function string_escape(env, buf, lexbuf) {
  var ___ocaml_lex_state = 252;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          return [
                  env,
                  false
                ];
      case 1 :
          $$Buffer.add_string(buf, "\\");
          return [
                  env,
                  false
                ];
      case 2 :
          var a = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos + 1 | 0);
          var b = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos + 2 | 0);
          var code = (hexa_to_int(a) << 4) + hexa_to_int(b) | 0;
          List.iter((function (param) {
                  return $$Buffer.add_char(buf, param);
                }), utf16to8(code));
          return [
                  env,
                  false
                ];
      case 3 :
          var a$1 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          var b$1 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos + 1 | 0);
          var c = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos + 2 | 0);
          var code$1 = ((oct_to_int(a$1) << 6) + (oct_to_int(b$1) << 3) | 0) + oct_to_int(c) | 0;
          if (code$1 < 256) {
            List.iter((function (param) {
                    return $$Buffer.add_char(buf, param);
                  }), utf16to8(code$1));
          } else {
            var code$2 = (oct_to_int(a$1) << 3) + oct_to_int(b$1) | 0;
            List.iter((function (param) {
                    return $$Buffer.add_char(buf, param);
                  }), utf16to8(code$2));
            $$Buffer.add_char(buf, c);
          }
          return [
                  env,
                  true
                ];
      case 4 :
          var a$2 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          var b$2 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos + 1 | 0);
          var code$3 = (oct_to_int(a$2) << 3) + oct_to_int(b$2) | 0;
          List.iter((function (param) {
                  return $$Buffer.add_char(buf, param);
                }), utf16to8(code$3));
          return [
                  env,
                  true
                ];
      case 5 :
          $$Buffer.add_char(buf, Char.chr(0));
          return [
                  env,
                  false
                ];
      case 6 :
          $$Buffer.add_char(buf, Char.chr(8));
          return [
                  env,
                  false
                ];
      case 7 :
          $$Buffer.add_char(buf, Char.chr(12));
          return [
                  env,
                  false
                ];
      case 8 :
          $$Buffer.add_char(buf, Char.chr(10));
          return [
                  env,
                  false
                ];
      case 9 :
          $$Buffer.add_char(buf, Char.chr(13));
          return [
                  env,
                  false
                ];
      case 10 :
          $$Buffer.add_char(buf, Char.chr(9));
          return [
                  env,
                  false
                ];
      case 11 :
          $$Buffer.add_char(buf, Char.chr(11));
          return [
                  env,
                  false
                ];
      case 12 :
          var a$3 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          var code$4 = oct_to_int(a$3);
          List.iter((function (param) {
                  return $$Buffer.add_char(buf, param);
                }), utf16to8(code$4));
          return [
                  env,
                  true
                ];
      case 13 :
          var a$4 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos + 1 | 0);
          var b$3 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos + 2 | 0);
          var c$1 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos + 3 | 0);
          var d = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos + 4 | 0);
          var code$5 = (((hexa_to_int(a$4) << 12) + (hexa_to_int(b$3) << 8) | 0) + (hexa_to_int(c$1) << 4) | 0) + hexa_to_int(d) | 0;
          List.iter((function (param) {
                  return $$Buffer.add_char(buf, param);
                }), utf16to8(code$5));
          return [
                  env,
                  false
                ];
      case 14 :
          var hex_code = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos + 2 | 0, lexbuf.lex_curr_pos - 1 | 0);
          var code$6 = Caml_format.caml_int_of_string("0x" + hex_code);
          var env$1 = code$6 > 1114111 ? lex_error(env, from_lb(env.lex_source, lexbuf), {
                  TAG: /* UnexpectedToken */1,
                  _0: "ILLEGAL"
                }) : env;
          List.iter((function (param) {
                  return $$Buffer.add_char(buf, param);
                }), utf16to8(code$6));
          return [
                  env$1,
                  false
                ];
      case 15 :
          var c$2 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          var env$2 = lex_error(env, from_lb(env.lex_source, lexbuf), {
                TAG: /* UnexpectedToken */1,
                _0: "ILLEGAL"
              });
          $$Buffer.add_char(buf, c$2);
          return [
                  env$2,
                  false
                ];
      case 16 :
          Lexing.new_line(lexbuf);
          return [
                  env,
                  false
                ];
      case 17 :
          var c$3 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(buf, c$3);
          return [
                  env,
                  false
                ];
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function regexp_body(env, buf, lexbuf) {
  var ___ocaml_lex_state = 314;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          var loc = from_lb(env.lex_source, lexbuf);
          var env$1 = lex_error(env, loc, /* UnterminatedRegExp */13);
          return [
                  env$1,
                  ""
                ];
      case 1 :
          var loc$1 = from_lb(env.lex_source, lexbuf);
          var env$2 = lex_error(env, loc$1, /* UnterminatedRegExp */13);
          return [
                  env$2,
                  ""
                ];
      case 2 :
          var s = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_start_pos + 2 | 0);
          $$Buffer.add_string(buf, s);
          return regexp_body(env, buf, lexbuf);
      case 3 :
          var flags = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos + 1 | 0, lexbuf.lex_curr_pos);
          return [
                  env,
                  flags
                ];
      case 4 :
          return [
                  env,
                  ""
                ];
      case 5 :
          var c = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(buf, c);
          var env$3 = regexp_class(env, buf, lexbuf);
          return regexp_body(env$3, buf, lexbuf);
      case 6 :
          var loc$2 = from_lb(env.lex_source, lexbuf);
          var env$4 = lex_error(env, loc$2, /* UnterminatedRegExp */13);
          return [
                  env$4,
                  ""
                ];
      case 7 :
          var c$1 = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(buf, c$1);
          return regexp_body(env, buf, lexbuf);
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
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
          return [
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
          var start = from_lb(env.lex_source, lexbuf);
          var buf = $$Buffer.create(127);
          var match = line_comment(env, buf, lexbuf);
          var env$1 = save_comment(match[0], start, match[1], buf, true);
          ___ocaml_lex_state = 291;
          _env = env$1;
          continue ;
      case 4 :
          var start$1 = from_lb(env.lex_source, lexbuf);
          var buf$1 = $$Buffer.create(127);
          var match$1 = comment(env, buf$1, lexbuf);
          var env$2 = save_comment(match$1[0], start$1, match$1[1], buf$1, true);
          ___ocaml_lex_state = 291;
          _env = env$2;
          continue ;
      case 5 :
          var start$2 = from_lb(env.lex_source, lexbuf);
          var buf$2 = $$Buffer.create(127);
          var match$2 = regexp_body(env, buf$2, lexbuf);
          var env$3 = match$2[0];
          var end_ = from_lb(env$3.lex_source, lexbuf);
          var loc = btwn(start$2, end_);
          return [
                  env$3,
                  {
                    TAG: /* T_REGEXP */3,
                    _0: [
                      loc,
                      $$Buffer.contents(buf$2),
                      match$2[1]
                    ]
                  }
                ];
      case 6 :
          var env$4 = lex_error(env, from_lb(env.lex_source, lexbuf), {
                TAG: /* UnexpectedToken */1,
                _0: "ILLEGAL"
              });
          return [
                  env$4,
                  /* T_ERROR */104
                ];
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
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
          return [
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
          var start = from_lb(env.lex_source, lexbuf);
          var buf = $$Buffer.create(127);
          var match = line_comment(env, buf, lexbuf);
          var env$1 = save_comment(match[0], start, match[1], buf, true);
          ___ocaml_lex_state = 333;
          _env = env$1;
          continue ;
      case 4 :
          var start$1 = from_lb(env.lex_source, lexbuf);
          var buf$1 = $$Buffer.create(127);
          var match$1 = comment(env, buf$1, lexbuf);
          var env$2 = save_comment(match$1[0], start$1, match$1[1], buf$1, true);
          ___ocaml_lex_state = 333;
          _env = env$2;
          continue ;
      case 5 :
          return [
                  env,
                  /* T_LESS_THAN */89
                ];
      case 6 :
          return [
                  env,
                  /* T_DIV */96
                ];
      case 7 :
          return [
                  env,
                  /* T_GREATER_THAN */90
                ];
      case 8 :
          return [
                  env,
                  /* T_LCURLY */1
                ];
      case 9 :
          return [
                  env,
                  /* T_COLON */77
                ];
      case 10 :
          return [
                  env,
                  /* T_PERIOD */9
                ];
      case 11 :
          return [
                  env,
                  /* T_ASSIGN */75
                ];
      case 12 :
          unicode_fix_cols(lexbuf);
          return [
                  env,
                  /* T_JSX_IDENTIFIER */106
                ];
      case 13 :
          var quote = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          var start$2 = from_lb(env.lex_source, lexbuf);
          var buf$2 = $$Buffer.create(127);
          var raw = $$Buffer.create(127);
          $$Buffer.add_char(raw, quote);
          var mode = quote === /* "'" */39 ? /* JSX_SINGLE_QUOTED_TEXT */0 : /* JSX_DOUBLE_QUOTED_TEXT */1;
          var match$2 = jsx_text(env, mode, buf$2, raw, lexbuf);
          $$Buffer.add_char(raw, quote);
          var value = $$Buffer.contents(buf$2);
          var raw$1 = $$Buffer.contents(raw);
          return [
                  match$2[0],
                  {
                    TAG: /* T_JSX_TEXT */4,
                    _0: [
                      btwn(start$2, match$2[1]),
                      value,
                      raw$1
                    ]
                  }
                ];
      case 14 :
          return [
                  env,
                  /* T_ERROR */104
                ];
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function jsx_child(env, start, buf, raw, lexbuf) {
  var ___ocaml_lex_state = 364;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf);
    switch (__ocaml_lex_state$1) {
      case 0 :
          var lt = Lexing.sub_lexeme(lexbuf, lexbuf.lex_start_pos, lexbuf.lex_curr_pos);
          $$Buffer.add_string(raw, lt);
          $$Buffer.add_string(buf, lt);
          Lexing.new_line(lexbuf);
          var match = jsx_text(env, /* JSX_CHILD_TEXT */2, buf, raw, lexbuf);
          var value = $$Buffer.contents(buf);
          var raw$1 = $$Buffer.contents(raw);
          return [
                  match[0],
                  {
                    TAG: /* T_JSX_TEXT */4,
                    _0: [
                      btwn(start, match[1]),
                      value,
                      raw$1
                    ]
                  }
                ];
      case 1 :
          return [
                  env,
                  /* T_EOF */105
                ];
      case 2 :
          return [
                  env,
                  /* T_LESS_THAN */89
                ];
      case 3 :
          return [
                  env,
                  /* T_LCURLY */1
                ];
      case 4 :
          var c = Caml_bytes.get(lexbuf.lex_buffer, lexbuf.lex_start_pos);
          $$Buffer.add_char(raw, c);
          $$Buffer.add_char(buf, c);
          var match$1 = jsx_text(env, /* JSX_CHILD_TEXT */2, buf, raw, lexbuf);
          var value$1 = $$Buffer.contents(buf);
          var raw$2 = $$Buffer.contents(raw);
          return [
                  match$1[0],
                  {
                    TAG: /* T_JSX_TEXT */4,
                    _0: [
                      btwn(start, match$1[1]),
                      value$1,
                      raw$2
                    ]
                  }
                ];
      default:
        Curry._1(lexbuf.refill_buff, lexbuf);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
  };
}

function regexp(env) {
  return get_result_and_clear_state(__ocaml_lex_regexp_rec(env, env.lex_lb, 291));
}

function jsx_child$1(env) {
  var start = from_curr_lb(env.lex_source, env.lex_lb);
  var buf = $$Buffer.create(127);
  var raw = $$Buffer.create(127);
  var match = jsx_child(env, start, buf, raw, env.lex_lb);
  return get_result_and_clear_state([
              match[0],
              match[1]
            ]);
}

function jsx_tag(env) {
  return get_result_and_clear_state(__ocaml_lex_jsx_tag_rec(env, env.lex_lb, 333));
}

function template_tail(env) {
  return get_result_and_clear_state(__ocaml_lex_template_tail_rec(env, env.lex_lb, 393));
}

function type_token$1(env) {
  return get_result_and_clear_state(type_token(env, env.lex_lb));
}

function token$1(env) {
  return get_result_and_clear_state(token(env, env.lex_lb));
}

function height(param) {
  if (param) {
    return param.h;
  } else {
    return 0;
  }
}

function create(l, v, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  return /* Node */{
          l: l,
          v: v,
          r: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal(l, v, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l.r;
      var lv = l.v;
      var ll = l.l;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      }
      if (lr) {
        return create(create(ll, lv, lr.l), lr.v, create(lr.r, v, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Set.bal",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */{
            l: l,
            v: v,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r.r;
    var rv = r.v;
    var rl = r.l;
    if (height(rr) >= height(rl)) {
      return create(create(l, v, rl), rv, rr);
    }
    if (rl) {
      return create(create(l, v, rl.l), rl.v, create(rl.r, rv, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Set.bal",
        Error: new Error()
      };
}

function add(x, t) {
  if (!t) {
    return /* Node */{
            l: /* Empty */0,
            v: x,
            r: /* Empty */0,
            h: 1
          };
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    return t;
  }
  if (c < 0) {
    var ll = add(x, l);
    if (l === ll) {
      return t;
    } else {
      return bal(ll, v, r);
    }
  }
  var rr = add(x, r);
  if (r === rr) {
    return t;
  } else {
    return bal(l, v, rr);
  }
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    var c = Caml_primitive.caml_string_compare(x, param.v);
    if (c === 0) {
      return true;
    }
    _param = c < 0 ? param.l : param.r;
    continue ;
  };
}

function create$1(lex_env, mode) {
  var lexbuf = lex_env.lex_lb;
  var lexbuf$1 = {
    refill_buff: lexbuf.refill_buff,
    lex_buffer: lexbuf.lex_buffer,
    lex_buffer_len: lexbuf.lex_buffer_len,
    lex_abs_pos: lexbuf.lex_abs_pos,
    lex_start_pos: lexbuf.lex_start_pos,
    lex_curr_pos: lexbuf.lex_curr_pos,
    lex_last_pos: lexbuf.lex_last_pos,
    lex_last_action: lexbuf.lex_last_action,
    lex_eof_reached: lexbuf.lex_eof_reached,
    lex_mem: lexbuf.lex_mem,
    lex_start_p: lexbuf.lex_start_p,
    lex_curr_p: lexbuf.lex_curr_p
  };
  var lex_env$1 = with_lexbuf(lexbuf$1, lex_env);
  return {
          la_results: [],
          la_num_lexed: 0,
          la_lex_mode: mode,
          la_lex_env: lex_env$1
        };
}

function next_power_of_two(n) {
  var _i = 1;
  while(true) {
    var i = _i;
    if (i >= n) {
      return i;
    }
    _i = (i << 1);
    continue ;
  };
}

function grow(t, n) {
  if (t.la_results.length >= n) {
    return ;
  }
  var new_size = next_power_of_two(n);
  var filler = function (i) {
    if (i < t.la_results.length) {
      return Caml_array.get(t.la_results, i);
    }
    
  };
  var new_arr = $$Array.init(new_size, filler);
  t.la_results = new_arr;
  
}

function lex(t) {
  var lex_env = t.la_lex_env;
  var match = t.la_lex_mode;
  var match$1;
  switch (match) {
    case /* TYPE */1 :
        match$1 = type_token$1(lex_env);
        break;
    case /* JSX_TAG */2 :
        match$1 = jsx_tag(lex_env);
        break;
    case /* JSX_CHILD */3 :
        match$1 = jsx_child$1(lex_env);
        break;
    case /* TEMPLATE */4 :
        match$1 = template_tail(lex_env);
        break;
    case /* REGEXP */5 :
        match$1 = regexp(lex_env);
        break;
    case /* NORMAL */0 :
    case /* PREDICATE */6 :
        match$1 = token$1(lex_env);
        break;
    
  }
  var lex_env$1 = match$1[0];
  var lexbuf = lex_env$1.lex_lb;
  var lexbuf$1 = {
    refill_buff: lexbuf.refill_buff,
    lex_buffer: lexbuf.lex_buffer,
    lex_buffer_len: lexbuf.lex_buffer_len,
    lex_abs_pos: lexbuf.lex_abs_pos,
    lex_start_pos: lexbuf.lex_start_pos,
    lex_curr_pos: lexbuf.lex_curr_pos,
    lex_last_pos: lexbuf.lex_last_pos,
    lex_last_action: lexbuf.lex_last_action,
    lex_eof_reached: lexbuf.lex_eof_reached,
    lex_mem: lexbuf.lex_mem,
    lex_start_p: lexbuf.lex_start_p,
    lex_curr_p: lexbuf.lex_curr_p
  };
  var cloned_env = with_lexbuf(lexbuf$1, lex_env$1);
  t.la_lex_env = lex_env$1;
  Caml_array.set(t.la_results, t.la_num_lexed, [
        cloned_env,
        match$1[1]
      ]);
  t.la_num_lexed = t.la_num_lexed + 1 | 0;
  
}

function lex_until(t, i) {
  grow(t, i + 1 | 0);
  while(t.la_num_lexed <= i) {
    lex(t);
  };
  
}

var default_parse_options = {
  esproposal_class_instance_fields: false,
  esproposal_class_static_fields: false,
  esproposal_decorators: false,
  esproposal_export_star_as: false,
  types: true,
  use_strict: false
};

function init_env(token_sinkOpt, parse_optionsOpt, source, content) {
  var token_sink = token_sinkOpt !== undefined ? Caml_option.valFromOption(token_sinkOpt) : undefined;
  var parse_options = parse_optionsOpt !== undefined ? Caml_option.valFromOption(parse_optionsOpt) : undefined;
  var lb = Lexing.from_string(content);
  if (source !== undefined && typeof source !== "number") {
    var init = lb.lex_curr_p;
    lb.lex_curr_p = {
      pos_fname: source._0,
      pos_lnum: init.pos_lnum,
      pos_bol: init.pos_bol,
      pos_cnum: init.pos_cnum
    };
  }
  var parse_options$1 = parse_options !== undefined ? parse_options : default_parse_options;
  var enable_types_in_comments = parse_options$1.types;
  var lex_env = new_lex_env(source, lb, enable_types_in_comments);
  return {
          errors: {
            contents: /* [] */0
          },
          comments: {
            contents: /* [] */0
          },
          labels: /* Empty */0,
          exports: {
            contents: /* Empty */0
          },
          last_loc: {
            contents: undefined
          },
          in_strict_mode: parse_options$1.use_strict,
          in_export: false,
          in_loop: false,
          in_switch: false,
          in_function: false,
          no_in: false,
          no_call: false,
          no_let: false,
          allow_yield: true,
          allow_await: false,
          error_callback: undefined,
          lex_mode_stack: {
            contents: {
              hd: /* NORMAL */0,
              tl: /* [] */0
            }
          },
          lex_env: {
            contents: lex_env
          },
          lookahead: {
            contents: create$1(lex_env, /* NORMAL */0)
          },
          token_sink: {
            contents: token_sink
          },
          parse_options: parse_options$1,
          source: source
        };
}

function error_at(env, param) {
  var e = param[1];
  env.errors.contents = {
    hd: [
      param[0],
      e
    ],
    tl: env.errors.contents
  };
  var callback = env.error_callback;
  if (callback !== undefined) {
    return Curry._2(callback, env, e);
  }
  
}

function comment_list(env) {
  return function (param) {
    return List.iter((function (c) {
                  env.comments.contents = {
                    hd: c,
                    tl: env.comments.contents
                  };
                  
                }), param);
  };
}

function record_export(env, param) {
  var export_name = param[1];
  var $$exports = env.exports.contents;
  if (mem(export_name, $$exports)) {
    return error_at(env, [
                param[0],
                {
                  TAG: /* DuplicateExport */7,
                  _0: export_name
                }
              ]);
  } else {
    env.exports.contents = add(export_name, env.exports.contents);
    return ;
  }
}

function lookahead(iOpt, env) {
  var i = iOpt !== undefined ? iOpt : 0;
  if (i >= 2) {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "parser_env.ml",
            288,
            2
          ],
          Error: new Error()
        };
  }
  var t = env.lookahead.contents;
  lex_until(t, i);
  var match = Caml_array.get(t.la_results, i);
  if (match !== undefined) {
    return match[1];
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "Lookahead.peek failed",
        Error: new Error()
      };
}

function with_strict(in_strict_mode, env) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.in_strict_mode = in_strict_mode;
  return newrecord;
}

function with_in_function(in_function, env) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.in_function = in_function;
  return newrecord;
}

function with_allow_yield(allow_yield, env) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.allow_yield = allow_yield;
  return newrecord;
}

function with_no_let(no_let, env) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.no_let = no_let;
  return newrecord;
}

function with_in_loop(in_loop, env) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.in_loop = in_loop;
  return newrecord;
}

function with_no_in(no_in, env) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.no_in = no_in;
  return newrecord;
}

function with_in_switch(in_switch, env) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.in_switch = in_switch;
  return newrecord;
}

function with_in_export(in_export, env) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.in_export = in_export;
  return newrecord;
}

function with_no_call(no_call, env) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.no_call = no_call;
  return newrecord;
}

function with_error_callback(error_callback, env) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.error_callback = error_callback;
  return newrecord;
}

function error_list(env) {
  return function (param) {
    return List.iter((function (param) {
                  return error_at(env, param);
                }), param);
  };
}

function without_error_callback(env) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.error_callback = undefined;
  return newrecord;
}

function add_label(env, label) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.labels = add(label, env.labels);
  return newrecord;
}

function enter_function(env, async, generator) {
  var newrecord = Caml_obj.caml_obj_dup(env);
  newrecord.allow_await = async;
  newrecord.allow_yield = generator;
  newrecord.in_function = true;
  newrecord.in_switch = false;
  newrecord.in_loop = false;
  newrecord.labels = /* Empty */0;
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

function token$2(iOpt, env) {
  var i = iOpt !== undefined ? iOpt : 0;
  return lookahead(i, env).lex_token;
}

function value(iOpt, env) {
  var i = iOpt !== undefined ? iOpt : 0;
  return lookahead(i, env).lex_value;
}

function loc(iOpt, env) {
  var i = iOpt !== undefined ? iOpt : 0;
  return lookahead(i, env).lex_loc;
}

function errors(iOpt, env) {
  var i = iOpt !== undefined ? iOpt : 0;
  return lookahead(i, env).lex_errors;
}

function comments(iOpt, env) {
  var i = iOpt !== undefined ? iOpt : 0;
  return lookahead(i, env).lex_comments;
}

function lex_env(iOpt, env) {
  var i = iOpt !== undefined ? iOpt : 0;
  var t = env.lookahead.contents;
  lex_until(t, i);
  var match = Caml_array.get(t.la_results, i);
  if (match !== undefined) {
    return match[0];
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "Lookahead.peek failed",
        Error: new Error()
      };
}

function is_line_terminator(env) {
  var loc$prime = env.last_loc.contents;
  if (loc$prime !== undefined) {
    return loc(undefined, env).start.line > loc$prime.start.line;
  } else {
    return false;
  }
}

function is_implicit_semicolon(env) {
  var match = token$2(undefined, env);
  if (typeof match !== "number") {
    return is_line_terminator(env);
  }
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
}

function semicolon_loc(iOpt, env) {
  var i = iOpt !== undefined ? iOpt : 0;
  if (token$2(i, env) === /* T_SEMICOLON */7) {
    return loc(i, env);
  }
  
}

function is_identifier(iOpt, env) {
  var i = iOpt !== undefined ? iOpt : 0;
  var name = value(i, env);
  var match = token$2(i, env);
  if (is_strict_reserved(name) || is_restricted(name) || is_future_reserved(name)) {
    return true;
  }
  if (typeof match !== "number") {
    return false;
  }
  var switcher = match - 1 | 0;
  if (switcher > 56 || switcher < 0) {
    return switcher < 62;
  } else {
    return switcher === 25;
  }
}

function is_function(iOpt, env) {
  var i = iOpt !== undefined ? iOpt : 0;
  if (token$2(i, env) === /* T_FUNCTION */13) {
    return true;
  } else if (token$2(i, env) === /* T_ASYNC */61) {
    return token$2(i + 1 | 0, env) === /* T_FUNCTION */13;
  } else {
    return false;
  }
}

function is_class(iOpt, env) {
  var i = iOpt !== undefined ? iOpt : 0;
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
  return error_at(env, [
              loc$1,
              e
            ]);
}

function get_unexpected_error(param) {
  var tmp = param[0];
  if (typeof tmp === "number") {
    switch (tmp) {
      case /* T_IDENTIFIER */0 :
          return /* UnexpectedIdentifier */2;
      case /* T_EOF */105 :
          return /* UnexpectedEOS */4;
      default:
        
    }
  } else {
    switch (tmp.TAG | 0) {
      case /* T_NUMBER */0 :
          return /* UnexpectedNumber */0;
      case /* T_STRING */1 :
      case /* T_JSX_TEXT */4 :
          return /* UnexpectedString */1;
      default:
        
    }
  }
  var word = param[1];
  if (is_future_reserved(word)) {
    return /* UnexpectedReserved */3;
  } else if (is_strict_reserved(word)) {
    return /* StrictReservedWord */39;
  } else {
    return {
            TAG: /* UnexpectedToken */1,
            _0: word
          };
  }
}

function error_unexpected(env) {
  error_list(env)(errors(undefined, env));
  return error$1(env, get_unexpected_error([
                  token$2(undefined, env),
                  value(undefined, env)
                ]));
}

function error_on_decorators(env) {
  return function (param) {
    return List.iter((function (decorator) {
                  return error_at(env, [
                              decorator[0],
                              /* UnsupportedDecorator */57
                            ]);
                }), param);
  };
}

function strict_error(env, e) {
  if (env.in_strict_mode) {
    return error$1(env, e);
  }
  
}

function strict_error_at(env, param) {
  if (env.in_strict_mode) {
    return error_at(env, [
                param[0],
                param[1]
              ]);
  }
  
}

function token$3(env) {
  var token_sink = env.token_sink.contents;
  if (token_sink !== undefined) {
    var token_loc = loc(undefined, env);
    var token$4 = token$2(undefined, env);
    var token_value = value(undefined, env);
    Curry._1(token_sink, {
          token_loc: token_loc,
          token: token$4,
          token_context: List.hd(env.lex_mode_stack.contents),
          token_value: token_value
        });
  }
  env.lex_env.contents = lex_env(undefined, env);
  error_list(env)(errors(undefined, env));
  comment_list(env)(comments(undefined, env));
  env.last_loc.contents = loc(undefined, env);
  var t = env.lookahead.contents;
  lex_until(t, 0);
  if (t.la_num_lexed > 1) {
    $$Array.blit(t.la_results, 1, t.la_results, 0, t.la_num_lexed - 1 | 0);
  }
  Caml_array.set(t.la_results, t.la_num_lexed - 1 | 0, undefined);
  t.la_num_lexed = t.la_num_lexed - 1 | 0;
  
}

function push_lex_mode(env, mode) {
  env.lex_mode_stack.contents = {
    hd: mode,
    tl: env.lex_mode_stack.contents
  };
  env.lookahead.contents = create$1(env.lex_env.contents, List.hd(env.lex_mode_stack.contents));
  
}

function pop_lex_mode(env) {
  var match = env.lex_mode_stack.contents;
  var new_stack;
  if (match) {
    new_stack = match.tl;
  } else {
    throw {
          RE_EXN_ID: "Failure",
          _1: "Popping lex mode from empty stack",
          Error: new Error()
        };
  }
  env.lex_mode_stack.contents = new_stack;
  env.lookahead.contents = create$1(env.lex_env.contents, List.hd(env.lex_mode_stack.contents));
  
}

function double_pop_lex_mode(env) {
  var match = env.lex_mode_stack.contents;
  var new_stack;
  if (match) {
    var match$1 = match.tl;
    if (match$1) {
      new_stack = match$1.tl;
    } else {
      throw {
            RE_EXN_ID: "Failure",
            _1: "Popping lex mode from empty stack",
            Error: new Error()
          };
    }
  } else {
    throw {
          RE_EXN_ID: "Failure",
          _1: "Popping lex mode from empty stack",
          Error: new Error()
        };
  }
  env.lex_mode_stack.contents = new_stack;
  env.lookahead.contents = create$1(env.lex_env.contents, List.hd(env.lex_mode_stack.contents));
  
}

function semicolon(env) {
  if (!is_implicit_semicolon(env)) {
    if (token$2(undefined, env) === /* T_SEMICOLON */7) {
      return token$3(env);
    } else {
      return error_unexpected(env);
    }
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
  var orig_token_sink = env.token_sink.contents;
  var token_buffer;
  if (orig_token_sink !== undefined) {
    var buffer = {
      length: 0,
      first: /* Nil */0,
      last: /* Nil */0
    };
    env.token_sink.contents = (function (token_data) {
        return Queue.add(token_data, buffer);
      });
    token_buffer = [
      orig_token_sink,
      buffer
    ];
  } else {
    token_buffer = undefined;
  }
  return {
          saved_errors: env.errors.contents,
          saved_comments: env.comments.contents,
          saved_last_loc: env.last_loc.contents,
          saved_lex_mode_stack: env.lex_mode_stack.contents,
          saved_lex_env: env.lex_env.contents,
          token_buffer: token_buffer
        };
}

function reset_token_sink(flush, env, token_buffer_info) {
  if (token_buffer_info === undefined) {
    return ;
  }
  var orig_token_sink = token_buffer_info[0];
  env.token_sink.contents = orig_token_sink;
  if (flush) {
    return Queue.iter(orig_token_sink, token_buffer_info[1]);
  }
  
}

function to_parse(env, parse) {
  var saved_state = save_state(env);
  try {
    var result = Curry._1(parse, env);
    reset_token_sink(true, env, saved_state.token_buffer);
    return /* ParsedSuccessfully */{
            _0: result
          };
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === Rollback) {
      reset_token_sink(false, env, saved_state.token_buffer);
      env.errors.contents = saved_state.saved_errors;
      env.comments.contents = saved_state.saved_comments;
      env.last_loc.contents = saved_state.saved_last_loc;
      env.lex_mode_stack.contents = saved_state.saved_lex_mode_stack;
      env.lex_env.contents = saved_state.saved_lex_env;
      env.lookahead.contents = create$1(env.lex_env.contents, List.hd(env.lex_mode_stack.contents));
      return /* FailedToParse */0;
    }
    throw exn;
  }
}

var Parser_env_Peek = {
  token: token$2,
  value: value,
  loc: loc,
  errors: errors,
  comments: comments,
  is_line_terminator: is_line_terminator,
  is_implicit_semicolon: is_implicit_semicolon,
  semicolon_loc: semicolon_loc,
  is_identifier: is_identifier,
  is_function: is_function,
  is_class: is_class
};

var Parser_env_Try = {
  Rollback: Rollback,
  to_parse: to_parse
};

function height$1(param) {
  if (param) {
    return param.h;
  } else {
    return 0;
  }
}

function create$2(l, v, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  return /* Node */{
          l: l,
          v: v,
          r: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal$1(l, v, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l.r;
      var lv = l.v;
      var ll = l.l;
      if (height$1(ll) >= height$1(lr)) {
        return create$2(ll, lv, create$2(lr, v, r));
      }
      if (lr) {
        return create$2(create$2(ll, lv, lr.l), lr.v, create$2(lr.r, v, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Set.bal",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */{
            l: l,
            v: v,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r.r;
    var rv = r.v;
    var rl = r.l;
    if (height$1(rr) >= height$1(rl)) {
      return create$2(create$2(l, v, rl), rv, rr);
    }
    if (rl) {
      return create$2(create$2(l, v, rl.l), rl.v, create$2(rl.r, rv, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Set.bal",
        Error: new Error()
      };
}

function add$1(x, t) {
  if (!t) {
    return /* Node */{
            l: /* Empty */0,
            v: x,
            r: /* Empty */0,
            h: 1
          };
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    return t;
  }
  if (c < 0) {
    var ll = add$1(x, l);
    if (l === ll) {
      return t;
    } else {
      return bal$1(ll, v, r);
    }
  }
  var rr = add$1(x, r);
  if (r === rr) {
    return t;
  } else {
    return bal$1(l, v, rr);
  }
}

function mem$1(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    var c = Caml_primitive.caml_string_compare(x, param.v);
    if (c === 0) {
      return true;
    }
    _param = c < 0 ? param.l : param.r;
    continue ;
  };
}

function height$2(param) {
  if (param) {
    return param.h;
  } else {
    return 0;
  }
}

function create$3(l, x, d, r) {
  var hl = height$2(l);
  var hr = height$2(r);
  return /* Node */{
          l: l,
          v: x,
          d: d,
          r: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal$2(l, x, d, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l.r;
      var ld = l.d;
      var lv = l.v;
      var ll = l.l;
      if (height$2(ll) >= height$2(lr)) {
        return create$3(ll, lv, ld, create$3(lr, x, d, r));
      }
      if (lr) {
        return create$3(create$3(ll, lv, ld, lr.l), lr.v, lr.d, create$3(lr.r, x, d, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Map.bal",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */{
            l: l,
            v: x,
            d: d,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r.r;
    var rd = r.d;
    var rv = r.v;
    var rl = r.l;
    if (height$2(rr) >= height$2(rl)) {
      return create$3(create$3(l, x, d, rl), rv, rd, rr);
    }
    if (rl) {
      return create$3(create$3(l, x, d, rl.l), rl.v, rl.d, create$3(rl.r, rv, rd, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Map.bal",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Map.bal",
        Error: new Error()
      };
}

function add$2(x, data, m) {
  if (!m) {
    return /* Node */{
            l: /* Empty */0,
            v: x,
            d: data,
            r: /* Empty */0,
            h: 1
          };
  }
  var r = m.r;
  var d = m.d;
  var v = m.v;
  var l = m.l;
  var c = Caml_primitive.caml_string_compare(x, v);
  if (c === 0) {
    if (d === data) {
      return m;
    } else {
      return /* Node */{
              l: l,
              v: x,
              d: data,
              r: r,
              h: m.h
            };
    }
  }
  if (c < 0) {
    var ll = add$2(x, data, l);
    if (l === ll) {
      return m;
    } else {
      return bal$2(ll, v, d, r);
    }
  }
  var rr = add$2(x, data, r);
  if (r === rr) {
    return m;
  } else {
    return bal$2(l, v, d, rr);
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param) {
      var c = Caml_primitive.caml_string_compare(x, param.v);
      if (c === 0) {
        return param.d;
      }
      _param = c < 0 ? param.l : param.r;
      continue ;
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
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
    return param.h;
  } else {
    return 0;
  }
}

function create$4(l, v, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  return /* Node */{
          l: l,
          v: v,
          r: r,
          h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal$3(l, v, r) {
  var hl = l ? l.h : 0;
  var hr = r ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    if (l) {
      var lr = l.r;
      var lv = l.v;
      var ll = l.l;
      if (height$3(ll) >= height$3(lr)) {
        return create$4(ll, lv, create$4(lr, v, r));
      }
      if (lr) {
        return create$4(create$4(ll, lv, lr.l), lr.v, create$4(lr.r, v, r));
      }
      throw {
            RE_EXN_ID: "Invalid_argument",
            _1: "Set.bal",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  if (hr <= (hl + 2 | 0)) {
    return /* Node */{
            l: l,
            v: v,
            r: r,
            h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
  if (r) {
    var rr = r.r;
    var rv = r.v;
    var rl = r.l;
    if (height$3(rr) >= height$3(rl)) {
      return create$4(create$4(l, v, rl), rv, rr);
    }
    if (rl) {
      return create$4(create$4(l, v, rl.l), rl.v, create$4(rl.r, rv, rr));
    }
    throw {
          RE_EXN_ID: "Invalid_argument",
          _1: "Set.bal",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Set.bal",
        Error: new Error()
      };
}

function add$3(x, t) {
  if (!t) {
    return /* Node */{
            l: /* Empty */0,
            v: x,
            r: /* Empty */0,
            h: 1
          };
  }
  var r = t.r;
  var v = t.v;
  var l = t.l;
  var c = compare$1(x, v);
  if (c === 0) {
    return t;
  }
  if (c < 0) {
    var ll = add$3(x, l);
    if (l === ll) {
      return t;
    } else {
      return bal$3(ll, v, r);
    }
  }
  var rr = add$3(x, r);
  if (r === rr) {
    return t;
  } else {
    return bal$3(l, v, rr);
  }
}

function mem$2(x, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    var c = compare$1(x, param.v);
    if (c === 0) {
      return true;
    }
    _param = c < 0 ? param.l : param.r;
    continue ;
  };
}

function filter_duplicate_errors(errs) {
  var errs$1 = List.rev(errs);
  var match = List.fold_left((function (param, err) {
          var deduped = param[1];
          var set = param[0];
          if (mem$2(err, set)) {
            return [
                    set,
                    deduped
                  ];
          } else {
            return [
                    add$3(err, set),
                    {
                      hd: err,
                      tl: deduped
                    }
                  ];
          }
        }), [
        /* Empty */0,
        /* [] */0
      ], errs$1);
  return List.rev(match[1]);
}

function with_loc(fn, env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var result = Curry._1(fn, env);
  var loc = env.last_loc.contents;
  var end_loc = loc !== undefined ? loc : (error$1(env, {
            TAG: /* Assertion */0,
            _0: "did not consume any tokens"
          }), Curry._2(Parser_env_Peek.loc, undefined, env));
  return [
          btwn(start_loc, end_loc),
          result
        ];
}

var Parse = Caml_module.init_mod([
      "parser_flow.ml",
      95,
      6
    ], {
      TAG: /* Module */0,
      _0: [
        [
          /* Function */0,
          "program"
        ],
        [
          /* Function */0,
          "statement"
        ],
        [
          /* Function */0,
          "statement_list_item"
        ],
        [
          /* Function */0,
          "statement_list"
        ],
        [
          /* Function */0,
          "statement_list_with_directives"
        ],
        [
          /* Function */0,
          "module_body"
        ],
        [
          /* Function */0,
          "expression"
        ],
        [
          /* Function */0,
          "assignment"
        ],
        [
          /* Function */0,
          "object_initializer"
        ],
        [
          /* Function */0,
          "array_initializer"
        ],
        [
          /* Function */0,
          "identifier"
        ],
        [
          /* Function */0,
          "identifier_or_reserved_keyword"
        ],
        [
          /* Function */0,
          "identifier_with_type"
        ],
        [
          /* Function */0,
          "block_body"
        ],
        [
          /* Function */0,
          "function_block_body"
        ],
        [
          /* Function */0,
          "jsx_element"
        ],
        [
          /* Function */0,
          "pattern"
        ],
        [
          /* Function */0,
          "pattern_from_expr"
        ],
        [
          /* Function */0,
          "object_key"
        ],
        [
          /* Function */0,
          "class_declaration"
        ],
        [
          /* Function */0,
          "class_expression"
        ],
        [
          /* Function */0,
          "is_assignable_lhs"
        ],
        [
          /* Function */0,
          "predicate"
        ]
      ]
    });

function union(env) {
  maybe(env, /* T_BIT_OR */80);
  var left = intersection(env);
  return Curry._2(union_with, env, left);
}

function intersection(env) {
  maybe(env, /* T_BIT_AND */82);
  var left = prefix(env);
  return Curry._2(intersection_with, env, left);
}

function rev_nonempty_acc(acc) {
  var end_loc;
  if (acc) {
    end_loc = acc.hd[0];
  } else {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "parser_flow.ml",
            127,
            13
          ],
          Error: new Error()
        };
  }
  var acc$1 = List.rev(acc);
  var start_loc;
  if (acc$1) {
    start_loc = acc$1.hd[0];
  } else {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "parser_flow.ml",
            131,
            13
          ],
          Error: new Error()
        };
  }
  return [
          btwn(start_loc, end_loc),
          acc$1
        ];
}

function generic_type_with_identifier(env, id) {
  var match = Curry._2(raw_generic_with_identifier, env, id);
  return [
          match[0],
          {
            TAG: /* Generic */4,
            _0: match[1]
          }
        ];
}

function function_param_with_id(env, name) {
  if (!env.parse_options.types) {
    error$1(env, /* UnexpectedTypeAnnotation */6);
  }
  var optional = maybe(env, /* T_PLING */76);
  token$4(env, /* T_COLON */77);
  var typeAnnotation = union(env);
  return [
          btwn(name[0], typeAnnotation[0]),
          {
            name: name,
            typeAnnotation: typeAnnotation,
            optional: optional
          }
        ];
}

function postfix_with(env, _t) {
  while(true) {
    var t = _t;
    if (!(!Curry._1(Parser_env_Peek.is_line_terminator, env) && maybe(env, /* T_LBRACKET */5))) {
      return t;
    }
    var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    token$4(env, /* T_RBRACKET */6);
    var loc = btwn(t[0], end_loc);
    var t_1 = {
      TAG: /* Array */3,
      _0: t
    };
    var t$1 = [
      loc,
      t_1
    ];
    _t = t$1;
    continue ;
  };
}

function param_list_or_type(env) {
  token$4(env, /* T_LPAREN */3);
  var token$5 = Curry._2(Parser_env_Peek.token, undefined, env);
  var ret;
  var exit = 0;
  if (typeof token$5 === "number") {
    if (token$5 !== 105) {
      if (token$5 >= 12) {
        exit = 1;
      } else {
        switch (token$5) {
          case /* T_IDENTIFIER */0 :
              ret = function_param_or_generic_type(env);
              break;
          case /* T_RPAREN */4 :
              ret = {
                TAG: /* ParamList */0,
                _0: [
                  undefined,
                  /* [] */0
                ]
              };
              break;
          case /* T_LCURLY */1 :
          case /* T_RCURLY */2 :
          case /* T_LPAREN */3 :
          case /* T_LBRACKET */5 :
          case /* T_RBRACKET */6 :
          case /* T_SEMICOLON */7 :
          case /* T_COMMA */8 :
          case /* T_PERIOD */9 :
          case /* T_ARROW */10 :
              exit = 1;
              break;
          case /* T_ELLIPSIS */11 :
              ret = {
                TAG: /* ParamList */0,
                _0: Curry._2(function_param_list_without_parens, env, /* [] */0)
              };
              break;
          
        }
      }
    } else {
      ret = {
        TAG: /* ParamList */0,
        _0: Curry._2(function_param_list_without_parens, env, /* [] */0)
      };
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var match = primitive(token$5);
    if (match !== undefined) {
      var match$1 = Curry._2(Parser_env_Peek.token, 1, env);
      if (typeof match$1 === "number" && (match$1 === 77 || match$1 === 76)) {
        var match$2 = Curry._1(Parse.identifier_or_reserved_keyword, env);
        var name = match$2[0];
        if (!env.parse_options.types) {
          error$1(env, /* UnexpectedTypeAnnotation */6);
        }
        var optional = maybe(env, /* T_PLING */76);
        token$4(env, /* T_COLON */77);
        var typeAnnotation = union(env);
        if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_RPAREN */4) {
          token$4(env, /* T_COMMA */8);
        }
        var param_0 = btwn(name[0], typeAnnotation[0]);
        var param_1 = {
          name: name,
          typeAnnotation: typeAnnotation,
          optional: optional
        };
        var param = [
          param_0,
          param_1
        ];
        ret = {
          TAG: /* ParamList */0,
          _0: Curry._2(function_param_list_without_parens, env, {
                hd: param,
                tl: /* [] */0
              })
        };
      } else {
        ret = {
          TAG: /* Type */1,
          _0: union(env)
        };
      }
    } else {
      ret = {
        TAG: /* Type */1,
        _0: union(env)
      };
    }
  }
  token$4(env, /* T_RPAREN */4);
  return ret;
}

function postfix(env) {
  var t = primary(env);
  return postfix_with(env, t);
}

function prefix(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match !== "number") {
    return postfix(env);
  }
  if (match !== 76) {
    return postfix(env);
  }
  var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_PLING */76);
  var t = prefix(env);
  return [
          btwn(loc, t[0]),
          {
            TAG: /* Nullable */0,
            _0: t
          }
        ];
}

function function_param_list(env) {
  token$4(env, /* T_LPAREN */3);
  var ret = Curry._2(function_param_list_without_parens, env, /* [] */0);
  token$4(env, /* T_RPAREN */4);
  return ret;
}

function primitive(param) {
  if (typeof param !== "number") {
    return ;
  }
  if (param === 27) {
    return /* Null */2;
  }
  if (param < 107) {
    return ;
  }
  switch (param - 107 | 0) {
    case /* T_IDENTIFIER */0 :
        return /* Any */0;
    case /* T_LCURLY */1 :
        return /* Boolean */5;
    case /* T_RCURLY */2 :
        return /* Number */3;
    case /* T_LPAREN */3 :
        return /* String */4;
    case /* T_RPAREN */4 :
        return /* Void */1;
    
  }
}

function function_param_or_generic_type(env) {
  var id = Curry._2(Parse.identifier, undefined, env);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "number" && (match === 77 || match === 76)) {
    var param = function_param_with_id(env, id);
    maybe(env, /* T_COMMA */8);
    return {
            TAG: /* ParamList */0,
            _0: Curry._2(function_param_list_without_parens, env, {
                  hd: param,
                  tl: /* [] */0
                })
          };
  }
  return {
          TAG: /* Type */1,
          _0: Curry._2(union_with, env, Curry._2(intersection_with, env, postfix_with(env, generic_type_with_identifier(env, id))))
        };
}

function generic(env) {
  return Curry._2(raw_generic_with_identifier, env, Curry._2(Parse.identifier, undefined, env));
}

function primary(env) {
  var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var token$5 = Curry._2(Parser_env_Peek.token, undefined, env);
  var exit = 0;
  if (typeof token$5 === "number") {
    switch (token$5) {
      case /* T_IDENTIFIER */0 :
          var match = generic(env);
          return [
                  match[0],
                  {
                    TAG: /* Generic */4,
                    _0: match[1]
                  }
                ];
      case /* T_LCURLY */1 :
          var match$1 = Curry._2(_object, undefined, env);
          return [
                  match$1[0],
                  {
                    TAG: /* Object */2,
                    _0: match$1[1]
                  }
                ];
      case /* T_LPAREN */3 :
          var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
          var _type = param_list_or_type(env);
          if (_type.TAG !== /* ParamList */0) {
            return _type._0;
          }
          var match$2 = _type._0;
          token$4(env, /* T_ARROW */10);
          var returnType = union(env);
          var end_loc = returnType[0];
          return [
                  btwn(start_loc, end_loc),
                  {
                    TAG: /* Function */1,
                    _0: {
                      params: match$2[1],
                      returnType: returnType,
                      rest: match$2[0],
                      typeParameters: undefined
                    }
                  }
                ];
      case /* T_LBRACKET */5 :
          var start_loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env);
          token$4(env, /* T_LBRACKET */5);
          var tl = types(env, /* [] */0);
          var end_loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env);
          token$4(env, /* T_RBRACKET */6);
          return [
                  btwn(start_loc$1, end_loc$1),
                  {
                    TAG: /* Tuple */8,
                    _0: tl
                  }
                ];
      case /* T_FALSE */28 :
      case /* T_TRUE */29 :
          exit = 2;
          break;
      case /* T_TYPEOF */44 :
          var start_loc$2 = Curry._2(Parser_env_Peek.loc, undefined, env);
          token$4(env, /* T_TYPEOF */44);
          var t = primary(env);
          return [
                  btwn(start_loc$2, t[0]),
                  {
                    TAG: /* Typeof */7,
                    _0: t
                  }
                ];
      case /* T_LESS_THAN */89 :
          var start_loc$3 = Curry._2(Parser_env_Peek.loc, undefined, env);
          var typeParameters = Curry._2(type_parameter_declaration, false, env);
          var match$3 = function_param_list(env);
          token$4(env, /* T_ARROW */10);
          var returnType$1 = union(env);
          var end_loc$2 = returnType$1[0];
          return [
                  btwn(start_loc$3, end_loc$2),
                  {
                    TAG: /* Function */1,
                    _0: {
                      params: match$3[1],
                      returnType: returnType$1,
                      rest: match$3[0],
                      typeParameters: typeParameters
                    }
                  }
                ];
      case /* T_MULT */97 :
          token$4(env, /* T_MULT */97);
          return [
                  loc,
                  /* Exists */6
                ];
      default:
        exit = 1;
    }
  } else {
    switch (token$5.TAG | 0) {
      case /* T_STRING */1 :
          var match$4 = token$5._0;
          var octal = match$4[3];
          var raw = match$4[2];
          var value = match$4[1];
          var loc$1 = match$4[0];
          if (octal) {
            strict_error(env, /* StrictOctalLiteral */31);
          }
          token$4(env, {
                TAG: /* T_STRING */1,
                _0: [
                  loc$1,
                  value,
                  raw,
                  octal
                ]
              });
          return [
                  loc$1,
                  {
                    TAG: /* StringLiteral */9,
                    _0: {
                      value: value,
                      raw: raw
                    }
                  }
                ];
      case /* T_NUMBER_SINGLETON_TYPE */5 :
          var value$1 = token$5._1;
          var number_type = token$5._0;
          var raw$1 = Curry._2(Parser_env_Peek.value, undefined, env);
          token$4(env, {
                TAG: /* T_NUMBER_SINGLETON_TYPE */5,
                _0: number_type,
                _1: value$1
              });
          if (number_type === /* LEGACY_OCTAL */1) {
            strict_error(env, /* StrictOctalLiteral */31);
          }
          return [
                  loc,
                  {
                    TAG: /* NumberLiteral */10,
                    _0: {
                      value: value$1,
                      raw: raw$1
                    }
                  }
                ];
      default:
        exit = 1;
    }
  }
  switch (exit) {
    case 1 :
        var t$1 = primitive(token$5);
        if (t$1 !== undefined) {
          token$4(env, token$5);
          return [
                  loc,
                  t$1
                ];
        } else {
          error_unexpected(env);
          return [
                  loc,
                  /* Any */0
                ];
        }
    case 2 :
        var raw$2 = Curry._2(Parser_env_Peek.value, undefined, env);
        token$4(env, token$5);
        var value$2 = token$5 === /* T_TRUE */29;
        return [
                loc,
                {
                  TAG: /* BooleanLiteral */11,
                  _0: {
                    value: value$2,
                    raw: raw$2
                  }
                }
              ];
    
  }
}

function union_with(env, left) {
  if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_BIT_OR */80) {
    var _acc = {
      hd: left,
      tl: /* [] */0
    };
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_Peek.token, undefined, env);
      if (typeof match === "number" && match === 80) {
        token$4(env, /* T_BIT_OR */80);
        _acc = {
          hd: intersection(env),
          tl: acc
        };
        continue ;
      }
      var match$1 = rev_nonempty_acc(acc);
      return [
              match$1[0],
              {
                TAG: /* Union */5,
                _0: match$1[1]
              }
            ];
    };
  } else {
    return left;
  }
}

function param(env) {
  var match = Curry._1(Parse.identifier_or_reserved_keyword, env);
  return function_param_with_id(env, match[0]);
}

function function_param_list_without_parens(env) {
  return function (param$1) {
    var _acc = param$1;
    while(true) {
      var acc = _acc;
      var t = Curry._2(Parser_env_Peek.token, undefined, env);
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
            var acc_0 = param(env);
            var acc$1 = {
              hd: acc_0,
              tl: acc
            };
            if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_RPAREN */4) {
              token$4(env, /* T_COMMA */8);
            }
            _acc = acc$1;
            continue ;
        case 2 :
            var rest = t === /* T_ELLIPSIS */11 ? (token$4(env, /* T_ELLIPSIS */11), param(env)) : undefined;
            return [
                    rest,
                    List.rev(acc)
                  ];
        
      }
    };
  };
}

function intersection_with(env, left) {
  if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_BIT_AND */82) {
    var _acc = {
      hd: left,
      tl: /* [] */0
    };
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_Peek.token, undefined, env);
      if (typeof match === "number" && match === 82) {
        token$4(env, /* T_BIT_AND */82);
        _acc = {
          hd: prefix(env),
          tl: acc
        };
        continue ;
      }
      var match$1 = rev_nonempty_acc(acc);
      return [
              match$1[0],
              {
                TAG: /* Intersection */6,
                _0: match$1[1]
              }
            ];
    };
  } else {
    return left;
  }
}

function identifier(env, _param) {
  while(true) {
    var param = _param;
    var qualification = param[1];
    var q_loc = param[0];
    if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_PERIOD */9) {
      return [
              q_loc,
              qualification
            ];
    }
    token$4(env, /* T_PERIOD */9);
    var id = Curry._2(Parse.identifier, undefined, env);
    var loc = btwn(q_loc, id[0]);
    var qualification$1 = {
      TAG: /* Qualified */1,
      _0: [
        loc,
        {
          qualification: qualification,
          id: id
        }
      ]
    };
    _param = [
      loc,
      qualification$1
    ];
    continue ;
  };
}

function raw_generic_with_identifier(env, id) {
  var id_0 = id[0];
  var id_1 = {
    TAG: /* Unqualified */0,
    _0: id
  };
  var id$1 = [
    id_0,
    id_1
  ];
  var match = identifier(env, id$1);
  var id_loc = match[0];
  var typeParameters = Curry._1(type_parameter_instantiation, env);
  var loc = typeParameters !== undefined ? btwn(id_loc, typeParameters[0]) : id_loc;
  return [
          loc,
          {
            id: match[1],
            typeParameters: typeParameters
          }
        ];
}

function params(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      if (match === 90) {
        return List.rev(acc);
      }
      if (match === 105) {
        return List.rev(acc);
      }
      
    }
    var acc_0 = union(env);
    var acc$1 = {
      hd: acc_0,
      tl: acc
    };
    if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_GREATER_THAN */90) {
      token$4(env, /* T_COMMA */8);
    }
    _acc = acc$1;
    continue ;
  };
}

function type_parameter_instantiation(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_LESS_THAN */89) {
    return ;
  }
  token$4(env, /* T_LESS_THAN */89);
  var params$1 = params(env, /* [] */0);
  var loc = btwn(start_loc, Curry._2(Parser_env_Peek.loc, undefined, env));
  token$4(env, /* T_GREATER_THAN */90);
  return [
          loc,
          {
            params: params$1
          }
        ];
}

function params$1(env, allow_default, _require_default, _acc) {
  while(true) {
    var acc = _acc;
    var require_default = _require_default;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    var variance = typeof match === "number" ? (
        match !== 94 ? (
            match !== 95 ? undefined : (token$3(env), /* Minus */1)
          ) : (token$3(env), /* Plus */0)
      ) : undefined;
    var match$1 = Curry._2(Parse.identifier_with_type, env, /* StrictParamName */28);
    var id = match$1[1];
    var loc = match$1[0];
    var match$2 = Curry._2(Parser_env_Peek.token, undefined, env);
    var match$3;
    if (allow_default) {
      var exit = 0;
      if (typeof match$2 === "number" && match$2 === 75) {
        token$3(env);
        match$3 = [
          union(env),
          true
        ];
      } else {
        exit = 1;
      }
      if (exit === 1) {
        if (require_default) {
          error_at(env, [
                loc,
                /* MissingTypeParamDefault */58
              ]);
        }
        match$3 = [
          undefined,
          require_default
        ];
      }
      
    } else {
      match$3 = [
        undefined,
        false
      ];
    }
    var param_1 = {
      name: id.name,
      bound: id.typeAnnotation,
      variance: variance,
      default: match$3[0]
    };
    var param = [
      loc,
      param_1
    ];
    var acc$1 = {
      hd: param,
      tl: acc
    };
    var match$4 = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match$4 === "number") {
      if (match$4 === 90) {
        return List.rev(acc$1);
      }
      if (match$4 === 105) {
        return List.rev(acc$1);
      }
      
    }
    token$4(env, /* T_COMMA */8);
    if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_GREATER_THAN */90) {
      return List.rev(acc$1);
    }
    _acc = acc$1;
    _require_default = match$3[1];
    continue ;
  };
}

function type_parameter_declaration(allow_default, env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_LESS_THAN */89) {
    return ;
  }
  if (!env.parse_options.types) {
    error$1(env, /* UnexpectedTypeAnnotation */6);
  }
  token$4(env, /* T_LESS_THAN */89);
  var params$2 = params$1(env, allow_default, false, /* [] */0);
  var loc = btwn(start_loc, Curry._2(Parser_env_Peek.loc, undefined, env));
  token$4(env, /* T_GREATER_THAN */90);
  return [
          loc,
          {
            params: params$2
          }
        ];
}

function methodish(env, start_loc) {
  var typeParameters = Curry._2(type_parameter_declaration, false, env);
  var match = function_param_list(env);
  token$4(env, /* T_COLON */77);
  var returnType = union(env);
  var loc = btwn(start_loc, returnType[0]);
  return [
          loc,
          {
            params: match[1],
            returnType: returnType,
            rest: match[0],
            typeParameters: typeParameters
          }
        ];
}

function method_property(env, start_loc, $$static, key) {
  var value = methodish(env, start_loc);
  var value_0 = value[0];
  var value_1 = {
    TAG: /* Function */1,
    _0: value[1]
  };
  var value$1 = [
    value_0,
    value_1
  ];
  return [
          value_0,
          {
            key: key,
            value: value$1,
            optional: false,
            static: $$static,
            _method: true
          }
        ];
}

function call_property(env, start_loc, $$static) {
  var value = methodish(env, Curry._2(Parser_env_Peek.loc, undefined, env));
  return [
          btwn(start_loc, value[0]),
          {
            value: value,
            static: $$static
          }
        ];
}

function property(env, start_loc, $$static, key) {
  if (!env.parse_options.types) {
    error$1(env, /* UnexpectedTypeAnnotation */6);
  }
  var optional = maybe(env, /* T_PLING */76);
  token$4(env, /* T_COLON */77);
  var value = union(env);
  return [
          btwn(start_loc, value[0]),
          {
            key: key,
            value: value,
            optional: optional,
            static: $$static,
            _method: false
          }
        ];
}

function indexer_property(env, start_loc, $$static) {
  token$4(env, /* T_LBRACKET */5);
  var match = Curry._1(Parse.identifier_or_reserved_keyword, env);
  token$4(env, /* T_COLON */77);
  var key = union(env);
  token$4(env, /* T_RBRACKET */6);
  token$4(env, /* T_COLON */77);
  var value = union(env);
  return [
          btwn(start_loc, value[0]),
          {
            id: match[0],
            key: key,
            value: value,
            static: $$static
          }
        ];
}

function semicolon$1(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
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
      return ;
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
    var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    var $$static = allow_static && maybe(env, /* T_STATIC */40);
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    var exit = 0;
    if (typeof match === "number") {
      if (match !== 89) {
        if (match !== 105) {
          if (match >= 6) {
            exit = 1;
          } else {
            switch (match) {
              case /* T_RCURLY */2 :
                  exit = 2;
                  break;
              case /* T_LPAREN */3 :
                  exit = 3;
                  break;
              case /* T_IDENTIFIER */0 :
              case /* T_LCURLY */1 :
              case /* T_RPAREN */4 :
                  exit = 1;
                  break;
              case /* T_LBRACKET */5 :
                  var indexer = indexer_property(env, start_loc, $$static);
                  semicolon$1(env);
                  _param = [
                    acc,
                    {
                      hd: indexer,
                      tl: indexers
                    },
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
          var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
          var match$2;
          var exit$1 = 0;
          if ($$static && typeof match$1 === "number" && match$1 === 77) {
            strict_error_at(env, [
                  start_loc,
                  /* StrictReservedWord */39
                ]);
            var static_key_1 = {
              TAG: /* Identifier */1,
              _0: [
                start_loc,
                {
                  name: "static",
                  typeAnnotation: undefined,
                  optional: false
                }
              ]
            };
            var static_key = [
              start_loc,
              static_key_1
            ];
            match$2 = [
              false,
              static_key
            ];
          } else {
            exit$1 = 4;
          }
          if (exit$1 === 4) {
            push_lex_mode(env, /* NORMAL */0);
            var key = Curry._1(Parse.object_key, env);
            pop_lex_mode(env);
            match$2 = [
              $$static,
              key
            ];
          }
          var key$1 = match$2[1][1];
          var $$static$1 = match$2[0];
          var match$3 = Curry._2(Parser_env_Peek.token, undefined, env);
          var property$1 = typeof match$3 === "number" && !(match$3 !== 3 && match$3 !== 89) ? method_property(env, start_loc, $$static$1, key$1) : property(env, start_loc, $$static$1, key$1);
          semicolon$1(env);
          _param = [
            {
              hd: property$1,
              tl: acc
            },
            indexers,
            callProperties
          ];
          continue ;
      case 2 :
          return [
                  List.rev(acc),
                  List.rev(indexers),
                  List.rev(callProperties)
                ];
      case 3 :
          var call_prop = call_property(env, start_loc, $$static);
          semicolon$1(env);
          _param = [
            acc,
            indexers,
            {
              hd: call_prop,
              tl: callProperties
            }
          ];
          continue ;
      
    }
  };
}

function _object(allow_staticOpt, env) {
  var allow_static = allow_staticOpt !== undefined ? allow_staticOpt : false;
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_LCURLY */1);
  var match = properties(allow_static, env, [
        /* [] */0,
        /* [] */0,
        /* [] */0
      ]);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_RCURLY */2);
  return [
          btwn(start_loc, end_loc),
          {
            properties: match[0],
            indexers: match[1],
            callProperties: match[2]
          }
        ];
}

function types(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      if (match === 6) {
        return List.rev(acc);
      }
      if (match === 105) {
        return List.rev(acc);
      }
      
    }
    var acc_0 = union(env);
    var acc$1 = {
      hd: acc_0,
      tl: acc
    };
    if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_RBRACKET */6) {
      token$4(env, /* T_COMMA */8);
    }
    _acc = acc$1;
    continue ;
  };
}

var _type = union;

function annotation(env) {
  if (!env.parse_options.types) {
    error$1(env, /* UnexpectedTypeAnnotation */6);
  }
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_COLON */77);
  var typeAnnotation = union(env);
  var loc = env.last_loc.contents;
  var end_loc;
  if (loc !== undefined) {
    end_loc = loc;
  } else {
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "parser_flow.ml",
            121,
            16
          ],
          Error: new Error()
        };
  }
  return [
          btwn(start_loc, end_loc),
          typeAnnotation
        ];
}

function annotation_opt(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
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

function _object$1(allow_staticOpt, env) {
  var allow_static = allow_staticOpt !== undefined ? allow_staticOpt : false;
  return wrap(Curry._1(_object, allow_static), env);
}

function pattern(check_env, _param) {
  while(true) {
    var param = _param;
    var p = param[1];
    switch (p.TAG | 0) {
      case /* Object */0 :
          var o = p._0;
          return List.fold_left(object_property, check_env, o.properties);
      case /* Array */1 :
          var arr = p._0;
          return List.fold_left(array_element, check_env, arr.elements);
      case /* Assignment */2 :
          _param = p._0.left;
          continue ;
      case /* Identifier */3 :
          var id = p._0;
          var name = id[1].name;
          var param_names = check_env[1];
          var env = check_env[0];
          if (mem$1(name, param_names)) {
            error_at(env, [
                  id[0],
                  /* StrictParamDupe */29
                ]);
          }
          var match = identifier_no_dupe_check([
                env,
                param_names
              ], id);
          return [
                  match[0],
                  add$1(name, match[1])
                ];
      case /* Expression */4 :
          error_at(check_env[0], [
                param[0],
                /* ExpectedPatternFoundExpression */18
              ]);
          return check_env;
      
    }
  };
}

function object_property(check_env, param) {
  if (param.TAG !== /* Property */0) {
    return pattern(check_env, param._0[1].argument);
  }
  var property = param._0[1];
  var id = property.key;
  var check_env$1;
  switch (id.TAG | 0) {
    case /* Identifier */1 :
        check_env$1 = identifier_no_dupe_check(check_env, id._0);
        break;
    case /* Literal */0 :
    case /* Computed */2 :
        check_env$1 = check_env;
        break;
    
  }
  return pattern(check_env$1, property.pattern);
}

function array_element(check_env, param) {
  if (param !== undefined) {
    if (param.TAG === /* Element */0) {
      return pattern(check_env, param._0);
    } else {
      return pattern(check_env, param._0[1].argument);
    }
  } else {
    return check_env;
  }
}

function identifier_no_dupe_check(param, param$1) {
  var name = param$1[1].name;
  var loc = param$1[0];
  var env = param[0];
  if (is_restricted(name)) {
    strict_error_at(env, [
          loc,
          /* StrictParamName */28
        ]);
  }
  if (is_future_reserved(name) || is_strict_reserved(name)) {
    strict_error_at(env, [
          loc,
          /* StrictReservedWord */39
        ]);
  }
  return [
          env,
          param[1]
        ];
}

function strict_post_check(env, strict, simple, id, params) {
  if (!(strict || !simple)) {
    return ;
  }
  var env$1 = strict ? with_strict(!env.in_strict_mode, env) : env;
  if (id !== undefined) {
    var name = id[1].name;
    var loc = id[0];
    if (is_restricted(name)) {
      strict_error_at(env$1, [
            loc,
            /* StrictFunctionName */30
          ]);
    }
    if (is_future_reserved(name) || is_strict_reserved(name)) {
      strict_error_at(env$1, [
            loc,
            /* StrictReservedWord */39
          ]);
    }
    
  }
  List.fold_left(pattern, [
        env$1,
        /* Empty */0
      ], params);
  
}

function param$1(env) {
  var id = Curry._2(Parse.pattern, env, /* StrictParamName */28);
  if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_ASSIGN */75) {
    return [
            id,
            undefined
          ];
  }
  token$4(env, /* T_ASSIGN */75);
  var $$default = Curry._1(Parse.assignment, env);
  return [
          id,
          $$default
        ];
}

function param_list(env, _param) {
  while(true) {
    var param$2 = _param;
    var has_default = param$2[2];
    var defaults = param$2[1];
    var params = param$2[0];
    var t = Curry._2(Parser_env_Peek.token, undefined, env);
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
          if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_RPAREN */4) {
            token$4(env, /* T_COMMA */8);
          }
          _param = [
            {
              hd: match[0],
              tl: params
            },
            {
              hd: $$default,
              tl: defaults
            },
            has_default$1
          ];
          continue ;
      case 2 :
          var rest = t === /* T_ELLIPSIS */11 ? (token$4(env, /* T_ELLIPSIS */11), Curry._2(Parse.identifier_with_type, env, /* StrictParamName */28)) : undefined;
          if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_RPAREN */4) {
            error$1(env, /* ParameterAfterRestParameter */47);
          }
          return [
                  List.rev(params),
                  has_default ? List.rev(defaults) : /* [] */0,
                  rest
                ];
      
    }
  };
}

function function_params(env) {
  token$4(env, /* T_LPAREN */3);
  var match = param_list(env, [
        /* [] */0,
        /* [] */0,
        false
      ]);
  token$4(env, /* T_RPAREN */4);
  return [
          match[0],
          match[1],
          match[2]
        ];
}

function function_body(env, async, generator) {
  var env$1 = enter_function(env, async, generator);
  var match = Curry._1(Parse.function_block_body, env$1);
  var loc = match[0];
  return [
          loc,
          {
            TAG: /* BodyBlock */0,
            _0: [
              loc,
              match[1]
            ]
          },
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
  if (param[1].TAG === /* Identifier */3) {
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
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var async = maybe(env, /* T_ASYNC */61);
  token$4(env, /* T_FUNCTION */13);
  var generator$1 = generator(env, async);
  var match = env.in_export;
  var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
  var match$2;
  var exit = 0;
  if (match && typeof match$1 === "number") {
    if (match$1 !== 3) {
      if (match$1 !== 89) {
        exit = 1;
      } else {
        var typeParams = Curry._1(type_parameter_declaration$1, env);
        var id = Curry._2(Parser_env_Peek.token, undefined, env) === /* T_LPAREN */3 ? undefined : Curry._2(Parse.identifier, /* StrictFunctionName */30, env);
        match$2 = [
          typeParams,
          id
        ];
      }
    } else {
      match$2 = [
        undefined,
        undefined
      ];
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var id$1 = Curry._2(Parse.identifier, /* StrictFunctionName */30, env);
    match$2 = [
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
  var predicate = Curry._1(Parse.predicate, env);
  var match$4 = function_body(env, async, generator$1);
  var body = match$4[1];
  var simple = is_simple_function_params(params, defaults, rest);
  strict_post_check(env, match$4[2], simple, id$2, params);
  var match$5;
  match$5 = body.TAG === /* BodyBlock */0 ? [
      body._0[0],
      false
    ] : [
      body._0[0],
      true
    ];
  return [
          btwn(start_loc, match$5[0]),
          {
            TAG: /* FunctionDeclaration */18,
            _0: {
              id: id$2,
              params: params,
              defaults: defaults,
              rest: rest,
              body: body,
              async: async,
              generator: generator$1,
              predicate: predicate,
              expression: match$5[1],
              returnType: returnType,
              typeParameters: match$2[0]
            }
          }
        ];
}

function variable_declaration(env) {
  var id = Curry._2(Parse.pattern, env, /* StrictVarName */27);
  var match;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_ASSIGN */75) {
    token$4(env, /* T_ASSIGN */75);
    match = [
      Curry._1(Parse.assignment, env),
      /* [] */0
    ];
  } else {
    match = id[1].TAG === /* Identifier */3 ? [
        undefined,
        /* [] */0
      ] : [
        undefined,
        {
          hd: [
            id[0],
            /* NoUninitializedDestructuring */43
          ],
          tl: /* [] */0
        }
      ];
  }
  var init = match[0];
  var end_loc = init !== undefined ? init[0] : id[0];
  return [
          [
            btwn(id[0], end_loc),
            {
              id: id,
              init: init
            }
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
    var decls$1 = {
      hd: decl,
      tl: decls
    };
    var errs$1 = Pervasives.$at(match[1], errs);
    if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_COMMA */8) {
      token$4(env, /* T_COMMA */8);
      _errs = errs$1;
      _decls = decls$1;
      continue ;
    }
    var end_loc = decl[0];
    var declarations = List.rev(decls$1);
    var start_loc = decl[0];
    return [
            btwn(start_loc, end_loc),
            declarations,
            List.rev(errs$1)
          ];
  };
}

function declarations(token$5, kind, env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, token$5);
  var match = helper(env, /* [] */0, /* [] */0);
  return [
          [
            btwn(start_loc, match[0]),
            {
              declarations: match[1],
              kind: kind
            }
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
          if (decl[1].init !== undefined) {
            return errs;
          } else {
            return {
                    hd: [
                      decl[0],
                      /* NoUninitializedConst */42
                    ],
                    tl: errs
                  };
          }
        }), match[1], variable.declarations);
  return [
          [
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
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var match$1;
  if (typeof match === "number") {
    switch (match) {
      case /* T_VAR */22 :
          match$1 = declarations(/* T_VAR */22, /* Var */0, env);
          break;
      case /* T_WHILE */23 :
      case /* T_WITH */24 :
          error_unexpected(env);
          match$1 = declarations(/* T_VAR */22, /* Var */0, env);
          break;
      case /* T_CONST */25 :
          match$1 = $$const(env);
          break;
      case /* T_LET */26 :
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
  return [
          [
            btwn(start_loc, match$2[0]),
            {
              TAG: /* VariableDeclaration */19,
              _0: match$2[1]
            }
          ],
          match$1[1]
        ];
}

function is_tighter(a, b) {
  var a_prec;
  a_prec = a.TAG === /* Left_assoc */0 ? a._0 : a._0 - 1 | 0;
  return a_prec >= b._0;
}

function is_lhs(param) {
  var tmp = param[1];
  if (typeof tmp === "number") {
    return false;
  }
  switch (tmp.TAG | 0) {
    case /* Member */13 :
    case /* Identifier */18 :
        return true;
    default:
      return false;
  }
}

function is_assignable_lhs(param) {
  var tmp = param[1];
  if (typeof tmp === "number") {
    return false;
  }
  switch (tmp.TAG | 0) {
    case /* Array */0 :
    case /* Object */1 :
    case /* Member */13 :
    case /* Identifier */18 :
        return true;
    default:
      return false;
  }
}

function assignment_op(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var op;
  if (typeof match === "number") {
    switch (match) {
      case /* T_RSHIFT3_ASSIGN */63 :
          op = /* RShift3Assign */9;
          break;
      case /* T_RSHIFT_ASSIGN */64 :
          op = /* RShiftAssign */8;
          break;
      case /* T_LSHIFT_ASSIGN */65 :
          op = /* LShiftAssign */7;
          break;
      case /* T_BIT_XOR_ASSIGN */66 :
          op = /* BitXorAssign */11;
          break;
      case /* T_BIT_OR_ASSIGN */67 :
          op = /* BitOrAssign */10;
          break;
      case /* T_BIT_AND_ASSIGN */68 :
          op = /* BitAndAssign */12;
          break;
      case /* T_MOD_ASSIGN */69 :
          op = /* ModAssign */6;
          break;
      case /* T_DIV_ASSIGN */70 :
          op = /* DivAssign */5;
          break;
      case /* T_MULT_ASSIGN */71 :
          op = /* MultAssign */3;
          break;
      case /* T_EXP_ASSIGN */72 :
          op = /* ExpAssign */4;
          break;
      case /* T_MINUS_ASSIGN */73 :
          op = /* MinusAssign */2;
          break;
      case /* T_PLUS_ASSIGN */74 :
          op = /* PlusAssign */1;
          break;
      case /* T_ASSIGN */75 :
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
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var expr = Curry._1(logical, env);
  if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_PLING */76) {
    return expr;
  }
  token$4(env, /* T_PLING */76);
  var env$prime = with_no_in(false, env);
  var consequent = Curry._1(assignment, env$prime);
  token$4(env, /* T_COLON */77);
  var match = with_loc(assignment, env);
  var loc = btwn(start_loc, match[0]);
  return [
          loc,
          {
            TAG: /* Conditional */10,
            _0: {
              test: expr,
              consequent: consequent,
              alternate: match[1]
            }
          }
        ];
}

function peek_unary_op(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match !== "number") {
    return ;
  }
  if (match >= 46) {
    if (match < 94) {
      if (match !== 62 || !env.allow_await) {
        return ;
      } else {
        return /* Await */7;
      }
    }
    if (match >= 102) {
      return ;
    }
    switch (match - 94 | 0) {
      case /* T_IDENTIFIER */0 :
          return /* Plus */1;
      case /* T_LCURLY */1 :
          return /* Minus */0;
      case /* T_RCURLY */2 :
      case /* T_LPAREN */3 :
      case /* T_RPAREN */4 :
      case /* T_LBRACKET */5 :
          return ;
      case /* T_RBRACKET */6 :
          return /* Not */2;
      case /* T_SEMICOLON */7 :
          return /* BitNot */3;
      
    }
  } else {
    if (match < 43) {
      return ;
    }
    switch (match - 43 | 0) {
      case /* T_IDENTIFIER */0 :
          return /* Delete */6;
      case /* T_LCURLY */1 :
          return /* Typeof */4;
      case /* T_RCURLY */2 :
          return /* Void */5;
      
    }
  }
}

function unary(env) {
  var begin_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var op = peek_unary_op(env);
  if (op !== undefined) {
    token$3(env);
    var argument = unary(env);
    var loc = btwn(begin_loc, argument[0]);
    if (op === 6) {
      var tmp = argument[1];
      if (typeof tmp !== "number" && tmp.TAG === /* Identifier */18) {
        strict_error_at(env, [
              loc,
              /* StrictDelete */32
            ]);
      }
      
    }
    return [
            loc,
            {
              TAG: /* Unary */5,
              _0: {
                operator: op,
                prefix: true,
                argument: argument
              }
            }
          ];
  }
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var op$1 = typeof match === "number" ? (
      match !== 102 ? (
          match !== 103 ? undefined : /* Decrement */1
        ) : /* Increment */0
    ) : undefined;
  if (op$1 === undefined) {
    var argument$1 = left_hand_side(env);
    if (Curry._1(Parser_env_Peek.is_line_terminator, env)) {
      return argument$1;
    }
    var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
    var op$2 = typeof match$1 === "number" ? (
        match$1 !== 102 ? (
            match$1 !== 103 ? undefined : /* Decrement */1
          ) : /* Increment */0
      ) : undefined;
    if (op$2 === undefined) {
      return argument$1;
    }
    if (!is_lhs(argument$1)) {
      error_at(env, [
            argument$1[0],
            /* InvalidLHSInAssignment */14
          ]);
    }
    var match$2 = argument$1[1];
    if (typeof match$2 !== "number" && match$2.TAG === /* Identifier */18) {
      if (is_restricted(match$2._0[1].name)) {
        strict_error(env, /* StrictLHSPostfix */37);
      }
      
    }
    var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    token$3(env);
    return [
            btwn(argument$1[0], end_loc),
            {
              TAG: /* Update */8,
              _0: {
                operator: op$2,
                argument: argument$1,
                prefix: false
              }
            }
          ];
  }
  token$3(env);
  var argument$2 = unary(env);
  if (!is_lhs(argument$2)) {
    error_at(env, [
          argument$2[0],
          /* InvalidLHSInAssignment */14
        ]);
  }
  var match$3 = argument$2[1];
  if (typeof match$3 !== "number" && match$3.TAG === /* Identifier */18) {
    if (is_restricted(match$3._0[1].name)) {
      strict_error(env, /* StrictLHSPrefix */38);
    }
    
  }
  return [
          btwn(begin_loc, argument$2[0]),
          {
            TAG: /* Update */8,
            _0: {
              operator: op$1,
              argument: argument$2,
              prefix: true
            }
          }
        ];
}

function left_hand_side(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
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
    expr = Curry._2(Parser_env_Peek.is_function, undefined, env) ? _function$1(env) : primary$1(env);
  }
  var expr$1 = member(env, expr);
  var part = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof part === "number") {
    if (part === /* T_LPAREN */3) {
      return call(env, expr$1);
    } else {
      return expr$1;
    }
  } else if (part.TAG === /* T_TEMPLATE_PART */2) {
    return member(env, tagged_template(env, expr$1, part._0));
  } else {
    return expr$1;
  }
}

function call(env, _left) {
  while(true) {
    var left = _left;
    var part = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof part !== "number") {
      if (part.TAG === /* T_TEMPLATE_PART */2) {
        return tagged_template(env, left, part._0);
      } else {
        return left;
      }
    }
    switch (part) {
      case /* T_LPAREN */3 :
          if (env.no_call) {
            return left;
          }
          var match = Curry._1($$arguments, env);
          _left = [
            btwn(left[0], match[0]),
            {
              TAG: /* Call */12,
              _0: {
                callee: left,
                arguments: match[1]
              }
            }
          ];
          continue ;
      case /* T_LBRACKET */5 :
          token$4(env, /* T_LBRACKET */5);
          var expr = Curry._1(Parse.expression, env);
          var last_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
          var loc = btwn(left[0], last_loc);
          token$4(env, /* T_RBRACKET */6);
          _left = [
            loc,
            {
              TAG: /* Member */13,
              _0: {
                _object: left,
                property: {
                  TAG: /* PropertyExpression */1,
                  _0: expr
                },
                computed: true
              }
            }
          ];
          continue ;
      case /* T_PERIOD */9 :
          token$4(env, /* T_PERIOD */9);
          var match$1 = identifier_or_reserved_keyword(env);
          var id = match$1[0];
          _left = [
            btwn(left[0], id[0]),
            {
              TAG: /* Member */13,
              _0: {
                _object: left,
                property: {
                  TAG: /* PropertyIdentifier */0,
                  _0: id
                },
                computed: false
              }
            }
          ];
          continue ;
      default:
        return left;
    }
  };
}

function _new(env, _finish_fn) {
  while(true) {
    var finish_fn = _finish_fn;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number" && match === 42) {
      var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, /* T_NEW */42);
      var finish_fn$prime = (function(finish_fn,start_loc){
      return function finish_fn$prime(callee, args) {
        var match = args !== undefined ? [
            args[0],
            args[1]
          ] : [
            callee[0],
            /* [] */0
          ];
        var callee$prime_0 = btwn(start_loc, match[0]);
        var callee$prime_1 = {
          TAG: /* New */11,
          _0: {
            callee: callee,
            arguments: match[1]
          }
        };
        var callee$prime = [
          callee$prime_0,
          callee$prime_1
        ];
        return Curry._2(finish_fn, callee$prime, undefined);
      }
      }(finish_fn,start_loc));
      _finish_fn = finish_fn$prime;
      continue ;
    }
    Curry._2(Parser_env_Peek.token, undefined, env);
    var expr = Curry._2(Parser_env_Peek.is_function, undefined, env) ? _function$1(env) : primary$1(env);
    var callee = member(with_no_call(true, env), expr);
    var part = Curry._2(Parser_env_Peek.token, undefined, env);
    var callee$1;
    callee$1 = typeof part === "number" || part.TAG !== /* T_TEMPLATE_PART */2 ? callee : tagged_template(env, callee, part._0);
    var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
    var args = typeof match$1 === "number" && match$1 === 3 ? Curry._1($$arguments, env) : undefined;
    return Curry._2(finish_fn, callee$1, args);
  };
}

function member(env, left) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match !== "number") {
    return left;
  }
  if (match !== 5) {
    if (match !== 9) {
      return left;
    }
    token$4(env, /* T_PERIOD */9);
    var match$1 = identifier_or_reserved_keyword(env);
    var id = match$1[0];
    return call(env, [
                btwn(left[0], id[0]),
                {
                  TAG: /* Member */13,
                  _0: {
                    _object: left,
                    property: {
                      TAG: /* PropertyIdentifier */0,
                      _0: id
                    },
                    computed: false
                  }
                }
              ]);
  }
  token$4(env, /* T_LBRACKET */5);
  var expr = Curry._1(Parse.expression, with_no_call(false, env));
  var last_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_RBRACKET */6);
  return call(env, [
              btwn(left[0], last_loc),
              {
                TAG: /* Member */13,
                _0: {
                  _object: left,
                  property: {
                    TAG: /* PropertyExpression */1,
                    _0: expr
                  },
                  computed: true
                }
              }
            ]);
}

function _function$1(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var async = maybe(env, /* T_ASYNC */61);
  token$4(env, /* T_FUNCTION */13);
  var generator$1 = generator(env, async);
  var match;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_LPAREN */3) {
    match = [
      undefined,
      undefined
    ];
  } else {
    var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
    var id = typeof match$1 === "number" && match$1 === 89 ? undefined : Curry._2(Parse.identifier, /* StrictFunctionName */30, env);
    match = [
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
  var predicate = Curry._1(Parse.predicate, env);
  var match$3 = function_body(env, async, generator$1);
  var body = match$3[1];
  var simple = is_simple_function_params(params, defaults, rest);
  strict_post_check(env, match$3[2], simple, id$1, params);
  var expression;
  expression = body.TAG === /* BodyBlock */0 ? false : true;
  return [
          btwn(start_loc, match$3[0]),
          {
            TAG: /* Function */2,
            _0: {
              id: id$1,
              params: params,
              defaults: defaults,
              rest: rest,
              body: body,
              async: async,
              generator: generator$1,
              predicate: predicate,
              expression: expression,
              returnType: returnType,
              typeParameters: match[1]
            }
          }
        ];
}

function number(env, number_type) {
  var value = Curry._2(Parser_env_Peek.value, undefined, env);
  var value$1;
  if (number_type !== 0) {
    switch (number_type - 1 | 0) {
      case /* BINARY */0 :
          strict_error(env, /* StrictOctalLiteral */31);
          value$1 = Caml_format.caml_int_of_string("0o" + value);
          break;
      case /* LEGACY_OCTAL */1 :
          value$1 = Caml_format.caml_int_of_string(value);
          break;
      case /* OCTAL */2 :
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
  token$4(env, {
        TAG: /* T_NUMBER */0,
        _0: number_type
      });
  return value$1;
}

function primary$1(env) {
  var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var number_type = Curry._2(Parser_env_Peek.token, undefined, env);
  var exit = 0;
  if (typeof number_type === "number") {
    switch (number_type) {
      case /* T_LCURLY */1 :
          var match = Curry._1(Parse.object_initializer, env);
          return [
                  match[0],
                  {
                    TAG: /* Object */1,
                    _0: match[1]
                  }
                ];
      case /* T_LPAREN */3 :
          token$4(env, /* T_LPAREN */3);
          var expression = Curry._1(assignment, env);
          var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
          var ret;
          if (typeof match$1 === "number") {
            if (match$1 !== 8) {
              if (match$1 !== 77) {
                ret = expression;
              } else {
                var typeAnnotation = wrap(annotation, env);
                ret = [
                  btwn(expression[0], typeAnnotation[0]),
                  {
                    TAG: /* TypeCast */24,
                    _0: {
                      expression: expression,
                      typeAnnotation: typeAnnotation
                    }
                  }
                ];
              }
            } else {
              ret = sequence(env, {
                    hd: expression,
                    tl: /* [] */0
                  });
            }
          } else {
            ret = expression;
          }
          token$4(env, /* T_RPAREN */4);
          return ret;
      case /* T_LBRACKET */5 :
          var match$2 = Curry._1(array_initializer, env);
          return [
                  match$2[0],
                  {
                    TAG: /* Array */0,
                    _0: match$2[1]
                  }
                ];
      case /* T_THIS */19 :
          token$4(env, /* T_THIS */19);
          return [
                  loc,
                  /* This */0
                ];
      case /* T_NULL */27 :
          var raw = Curry._2(Parser_env_Peek.value, undefined, env);
          token$4(env, /* T_NULL */27);
          return [
                  loc,
                  {
                    TAG: /* Literal */19,
                    _0: {
                      value: /* Null */0,
                      raw: raw
                    }
                  }
                ];
      case /* T_FALSE */28 :
      case /* T_TRUE */29 :
          exit = 2;
          break;
      case /* T_CLASS */38 :
          return Curry._1(Parse.class_expression, env);
      case /* T_SUPER */49 :
          var loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env);
          token$4(env, /* T_SUPER */49);
          var id_1 = {
            name: "super",
            typeAnnotation: undefined,
            optional: false
          };
          var id = [
            loc$1,
            id_1
          ];
          return [
                  loc$1,
                  {
                    TAG: /* Identifier */18,
                    _0: id
                  }
                ];
      case /* T_LESS_THAN */89 :
          var match$3 = Curry._1(Parse.jsx_element, env);
          return [
                  match$3[0],
                  {
                    TAG: /* JSXElement */22,
                    _0: match$3[1]
                  }
                ];
      case /* T_DIV_ASSIGN */70 :
      case /* T_DIV */96 :
          push_lex_mode(env, /* REGEXP */5);
          var loc$2 = Curry._2(Parser_env_Peek.loc, undefined, env);
          var match$4 = Curry._2(Parser_env_Peek.token, undefined, env);
          var match$5;
          if (typeof match$4 === "number") {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "parser_flow.ml",
                    1699,
                    15
                  ],
                  Error: new Error()
                };
          }
          if (match$4.TAG === /* T_REGEXP */3) {
            var match$6 = match$4._0;
            var raw$1 = Curry._2(Parser_env_Peek.value, undefined, env);
            token$3(env);
            match$5 = [
              raw$1,
              match$6[1],
              match$6[2]
            ];
          } else {
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "parser_flow.ml",
                    1699,
                    15
                  ],
                  Error: new Error()
                };
          }
          var raw_flags = match$5[2];
          pop_lex_mode(env);
          var filtered_flags = $$Buffer.create(raw_flags.length);
          $$String.iter((function (c) {
                  if (c >= 110) {
                    if (c !== 121) {
                      return ;
                    } else {
                      return $$Buffer.add_char(filtered_flags, c);
                    }
                  }
                  if (c < 103) {
                    return ;
                  }
                  switch (c - 103 | 0) {
                    case 1 :
                    case 3 :
                    case 4 :
                    case 5 :
                        return ;
                    case 0 :
                    case 2 :
                    case 6 :
                        return $$Buffer.add_char(filtered_flags, c);
                    
                  }
                }), raw_flags);
          var flags = $$Buffer.contents(filtered_flags);
          if (flags !== raw_flags) {
            error$1(env, {
                  TAG: /* InvalidRegExpFlags */3,
                  _0: raw_flags
                });
          }
          var value = {
            TAG: /* RegExp */3,
            _0: {
              pattern: match$5[1],
              flags: flags
            }
          };
          return [
                  loc$2,
                  {
                    TAG: /* Literal */19,
                    _0: {
                      value: value,
                      raw: match$5[0]
                    }
                  }
                ];
      default:
        exit = 1;
    }
  } else {
    switch (number_type.TAG | 0) {
      case /* T_NUMBER */0 :
          var raw$2 = Curry._2(Parser_env_Peek.value, undefined, env);
          var value$1 = {
            TAG: /* Number */2,
            _0: number(env, number_type._0)
          };
          return [
                  loc,
                  {
                    TAG: /* Literal */19,
                    _0: {
                      value: value$1,
                      raw: raw$2
                    }
                  }
                ];
      case /* T_STRING */1 :
          var match$7 = number_type._0;
          var octal = match$7[3];
          var raw$3 = match$7[2];
          var value$2 = match$7[1];
          var loc$3 = match$7[0];
          if (octal) {
            strict_error(env, /* StrictOctalLiteral */31);
          }
          token$4(env, {
                TAG: /* T_STRING */1,
                _0: [
                  loc$3,
                  value$2,
                  raw$3,
                  octal
                ]
              });
          var value$3 = {
            TAG: /* String */0,
            _0: value$2
          };
          return [
                  loc$3,
                  {
                    TAG: /* Literal */19,
                    _0: {
                      value: value$3,
                      raw: raw$3
                    }
                  }
                ];
      case /* T_TEMPLATE_PART */2 :
          var match$8 = Curry._2(template_literal, env, number_type._0);
          return [
                  match$8[0],
                  {
                    TAG: /* TemplateLiteral */20,
                    _0: match$8[1]
                  }
                ];
      default:
        exit = 1;
    }
  }
  switch (exit) {
    case 1 :
        if (Curry._2(Parser_env_Peek.is_identifier, undefined, env)) {
          var id$1 = Curry._2(Parse.identifier, undefined, env);
          return [
                  id$1[0],
                  {
                    TAG: /* Identifier */18,
                    _0: id$1
                  }
                ];
        }
        error_unexpected(env);
        if (number_type === /* T_ERROR */104) {
          token$3(env);
        }
        return [
                loc,
                {
                  TAG: /* Literal */19,
                  _0: {
                    value: /* Null */0,
                    raw: "null"
                  }
                }
              ];
    case 2 :
        var raw$4 = Curry._2(Parser_env_Peek.value, undefined, env);
        token$4(env, number_type);
        var value$4 = {
          TAG: /* Boolean */1,
          _0: number_type === /* T_TRUE */29
        };
        return [
                loc,
                {
                  TAG: /* Literal */19,
                  _0: {
                    value: value$4,
                    raw: raw$4
                  }
                }
              ];
    
  }
}

function tagged_template(env, tag, part) {
  var quasi = Curry._2(template_literal, env, part);
  return [
          btwn(tag[0], quasi[0]),
          {
            TAG: /* TaggedTemplate */21,
            _0: {
              tag: tag,
              quasi: quasi
            }
          }
        ];
}

function sequence(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number" && match === 8) {
      token$4(env, /* T_COMMA */8);
      var expr = Curry._1(assignment, env);
      _acc = {
        hd: expr,
        tl: acc
      };
      continue ;
    }
    var last_loc = acc ? acc.hd[0] : none;
    var expressions = List.rev(acc);
    var first_loc = expressions ? expressions.hd[0] : none;
    return [
            btwn(first_loc, last_loc),
            {
              TAG: /* Sequence */4,
              _0: {
                expressions: expressions
              }
            }
          ];
  };
}

function identifier_or_reserved_keyword(env) {
  var lex_token = Curry._2(Parser_env_Peek.token, undefined, env);
  var lex_value = Curry._2(Parser_env_Peek.value, undefined, env);
  var lex_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var exit = 0;
  if (typeof lex_token === "number") {
    if (lex_token >= 58) {
      if (lex_token < 62) {
        return [
                Curry._2(Parse.identifier, undefined, env),
                undefined
              ];
      }
      exit = 1;
    } else {
      if (lex_token === 0) {
        return [
                Curry._2(Parse.identifier, undefined, env),
                undefined
              ];
      }
      exit = 1;
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
      err = [
        lex_loc,
        get_unexpected_error([
              lex_token,
              lex_value
            ])
      ];
    }
    token$3(env);
    return [
            [
              lex_loc,
              {
                name: lex_value,
                typeAnnotation: undefined,
                optional: false
              }
            ],
            err
          ];
  }
  
}

function assignment_but_not_arrow_function(env) {
  var expr = conditional(env);
  var operator = assignment_op(env);
  if (operator === undefined) {
    return expr;
  }
  if (!is_assignable_lhs(expr)) {
    error_at(env, [
          expr[0],
          /* InvalidLHSInAssignment */14
        ]);
  }
  var match = expr[1];
  if (typeof match !== "number" && match.TAG === /* Identifier */18) {
    if (is_restricted(match._0[1].name)) {
      strict_error_at(env, [
            expr[0],
            /* StrictLHSAssignment */36
          ]);
    }
    
  }
  var left = Curry._2(Parse.pattern_from_expr, env, expr);
  var right = Curry._1(assignment, env);
  var loc = btwn(left[0], right[0]);
  return [
          loc,
          {
            TAG: /* Assignment */7,
            _0: {
              operator: operator,
              left: left,
              right: right
            }
          }
        ];
}

function error_callback(param, param$1) {
  throw {
        RE_EXN_ID: Parser_env_Try.Rollback,
        Error: new Error()
      };
}

function try_assignment_but_not_arrow_function(env) {
  var env$1 = with_error_callback(error_callback, env);
  var ret = assignment_but_not_arrow_function(env$1);
  var match = Curry._2(Parser_env_Peek.token, undefined, env$1);
  if (typeof match === "number") {
    if (match !== 10) {
      if (match === 77) {
        throw {
              RE_EXN_ID: Parser_env_Try.Rollback,
              Error: new Error()
            };
      }
      
    } else {
      throw {
            RE_EXN_ID: Parser_env_Try.Rollback,
            Error: new Error()
          };
    }
  }
  if (!Curry._2(Parser_env_Peek.is_identifier, undefined, env$1)) {
    return ret;
  }
  if (Curry._2(Parser_env_Peek.value, undefined, env$1) === "checks") {
    throw {
          RE_EXN_ID: Parser_env_Try.Rollback,
          Error: new Error()
        };
  }
  var match$1 = ret[1];
  if (typeof match$1 === "number") {
    return ret;
  }
  if (match$1.TAG !== /* Identifier */18) {
    return ret;
  }
  if (match$1._0[1].name !== "async") {
    return ret;
  }
  if (!Curry._1(Parser_env_Peek.is_line_terminator, env$1)) {
    throw {
          RE_EXN_ID: Parser_env_Try.Rollback,
          Error: new Error()
        };
  }
  return ret;
}

function assignment(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var match$1 = Curry._2(Parser_env_Peek.is_identifier, undefined, env);
  var exit = 0;
  if (typeof match === "number") {
    var switcher = match - 4 | 0;
    if (switcher > 84 || switcher < 0) {
      if ((switcher + 1 >>> 0) > 86) {
        exit = 2;
      }
      
    } else if (switcher !== 52) {
      exit = 2;
    } else {
      if (env.allow_yield) {
        var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
        token$4(env, /* T_YIELD */56);
        if (!env.allow_yield) {
          error$1(env, /* IllegalYield */24);
        }
        var delegate = maybe(env, /* T_MULT */97);
        var has_argument = !(Curry._2(Parser_env_Peek.token, undefined, env) === /* T_SEMICOLON */7 || Curry._1(Parser_env_Peek.is_implicit_semicolon, env));
        var argument = delegate || has_argument ? Curry._1(assignment, env) : undefined;
        var end_loc;
        if (argument !== undefined) {
          end_loc = argument[0];
        } else {
          var loc = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
          var end_loc$1 = loc !== undefined ? loc : start_loc;
          semicolon(env);
          end_loc = end_loc$1;
        }
        return [
                btwn(start_loc, end_loc),
                {
                  TAG: /* Yield */14,
                  _0: {
                    argument: argument,
                    delegate: delegate
                  }
                }
              ];
      }
      exit = 2;
    }
  } else {
    exit = 2;
  }
  if (exit === 2 && !match$1) {
    return assignment_but_not_arrow_function(env);
  }
  var expr = Curry._2(Parser_env_Try.to_parse, env, try_assignment_but_not_arrow_function);
  if (expr) {
    return expr._0;
  }
  var expr$1 = Curry._2(Parser_env_Try.to_parse, env, try_arrow_function);
  if (expr$1) {
    return expr$1._0;
  } else {
    return assignment_but_not_arrow_function(env);
  }
}

function make_logical(left, right, operator, loc) {
  return [
          loc,
          {
            TAG: /* Logical */9,
            _0: {
              operator: operator,
              left: left,
              right: right
            }
          }
        ];
}

function logical_and(env, _left, _lloc) {
  while(true) {
    var lloc = _lloc;
    var left = _left;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match !== "number") {
      return [
              lloc,
              left
            ];
    }
    if (match !== 79) {
      return [
              lloc,
              left
            ];
    }
    token$4(env, /* T_AND */79);
    var match$1 = with_loc(binary, env);
    var loc = btwn(lloc, match$1[0]);
    _lloc = loc;
    _left = make_logical(left, match$1[1], /* And */1, loc);
    continue ;
  };
}

function logical_or(env, _left, _lloc) {
  while(true) {
    var lloc = _lloc;
    var left = _left;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match !== "number") {
      return [
              lloc,
              left
            ];
    }
    if (match !== 78) {
      return [
              lloc,
              left
            ];
    }
    token$4(env, /* T_OR */78);
    var match$1 = with_loc(binary, env);
    var match$2 = logical_and(env, match$1[1], match$1[0]);
    var loc = btwn(lloc, match$2[0]);
    _lloc = loc;
    _left = make_logical(left, match$2[1], /* Or */0, loc);
    continue ;
  };
}

function logical(env) {
  var match = with_loc(binary, env);
  var match$1 = logical_and(env, match[1], match[0]);
  return logical_or(env, match$1[1], match$1[0])[1];
}

function binary_op(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var ret;
  if (typeof match === "number") {
    var switcher = match - 15 | 0;
    if (switcher === 0 || switcher === 1) {
      ret = switcher !== 0 ? [
          /* Instanceof */21,
          {
            TAG: /* Left_assoc */0,
            _0: 6
          }
        ] : (
          env.no_in ? undefined : [
              /* In */20,
              {
                TAG: /* Left_assoc */0,
                _0: 6
              }
            ]
        );
    } else if (switcher >= 65) {
      switch (switcher - 65 | 0) {
        case /* T_IDENTIFIER */0 :
            ret = [
              /* BitOr */17,
              {
                TAG: /* Left_assoc */0,
                _0: 2
              }
            ];
            break;
        case /* T_LCURLY */1 :
            ret = [
              /* Xor */18,
              {
                TAG: /* Left_assoc */0,
                _0: 3
              }
            ];
            break;
        case /* T_RCURLY */2 :
            ret = [
              /* BitAnd */19,
              {
                TAG: /* Left_assoc */0,
                _0: 4
              }
            ];
            break;
        case /* T_LPAREN */3 :
            ret = [
              /* Equal */0,
              {
                TAG: /* Left_assoc */0,
                _0: 5
              }
            ];
            break;
        case /* T_RPAREN */4 :
            ret = [
              /* NotEqual */1,
              {
                TAG: /* Left_assoc */0,
                _0: 5
              }
            ];
            break;
        case /* T_LBRACKET */5 :
            ret = [
              /* StrictEqual */2,
              {
                TAG: /* Left_assoc */0,
                _0: 5
              }
            ];
            break;
        case /* T_RBRACKET */6 :
            ret = [
              /* StrictNotEqual */3,
              {
                TAG: /* Left_assoc */0,
                _0: 5
              }
            ];
            break;
        case /* T_SEMICOLON */7 :
            ret = [
              /* LessThanEqual */5,
              {
                TAG: /* Left_assoc */0,
                _0: 6
              }
            ];
            break;
        case /* T_COMMA */8 :
            ret = [
              /* GreaterThanEqual */7,
              {
                TAG: /* Left_assoc */0,
                _0: 6
              }
            ];
            break;
        case /* T_PERIOD */9 :
            ret = [
              /* LessThan */4,
              {
                TAG: /* Left_assoc */0,
                _0: 6
              }
            ];
            break;
        case /* T_ARROW */10 :
            ret = [
              /* GreaterThan */6,
              {
                TAG: /* Left_assoc */0,
                _0: 6
              }
            ];
            break;
        case /* T_ELLIPSIS */11 :
            ret = [
              /* LShift */8,
              {
                TAG: /* Left_assoc */0,
                _0: 7
              }
            ];
            break;
        case /* T_AT */12 :
            ret = [
              /* RShift */9,
              {
                TAG: /* Left_assoc */0,
                _0: 7
              }
            ];
            break;
        case /* T_FUNCTION */13 :
            ret = [
              /* RShift3 */10,
              {
                TAG: /* Left_assoc */0,
                _0: 7
              }
            ];
            break;
        case /* T_IF */14 :
            ret = [
              /* Plus */11,
              {
                TAG: /* Left_assoc */0,
                _0: 8
              }
            ];
            break;
        case /* T_IN */15 :
            ret = [
              /* Minus */12,
              {
                TAG: /* Left_assoc */0,
                _0: 8
              }
            ];
            break;
        case /* T_INSTANCEOF */16 :
            ret = [
              /* Div */15,
              {
                TAG: /* Left_assoc */0,
                _0: 9
              }
            ];
            break;
        case /* T_RETURN */17 :
            ret = [
              /* Mult */13,
              {
                TAG: /* Left_assoc */0,
                _0: 9
              }
            ];
            break;
        case /* T_SWITCH */18 :
            ret = [
              /* Exp */14,
              {
                TAG: /* Right_assoc */1,
                _0: 10
              }
            ];
            break;
        case /* T_THIS */19 :
            ret = [
              /* Mod */16,
              {
                TAG: /* Left_assoc */0,
                _0: 9
              }
            ];
            break;
        case /* T_THROW */20 :
        case /* T_TRY */21 :
        case /* T_VAR */22 :
        case /* T_WHILE */23 :
        case /* T_WITH */24 :
        case /* T_CONST */25 :
        case /* T_LET */26 :
        case /* T_NULL */27 :
        case /* T_FALSE */28 :
        case /* T_TRUE */29 :
        case /* T_BREAK */30 :
        case /* T_CASE */31 :
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
  return [
          loc,
          {
            TAG: /* Binary */6,
            _0: {
              operator: operator,
              left: left,
              right: right
            }
          }
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
    if (stack) {
      var match = stack.hd;
      var match$1 = match[1];
      if (is_tighter(match$1[1], rpri)) {
        var loc = btwn(match[2], rloc);
        var right$1 = make_binary(match[0], right, match$1[0], loc);
        _stack = stack.tl;
        _rloc = loc;
        _param = [
          rop,
          rpri
        ];
        _right = right$1;
        continue ;
      }
      
    }
    return {
            hd: [
              right,
              [
                rop,
                rpri
              ],
              rloc
            ],
            tl: stack
          };
  };
}

function binary(env) {
  var _stack = /* [] */0;
  while(true) {
    var stack = _stack;
    var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    var is_unary = peek_unary_op(env) !== undefined;
    var right = unary(with_no_in(false, env));
    var loc = env.last_loc.contents;
    var end_loc = loc !== undefined ? loc : right[0];
    var right_loc = btwn(start_loc, end_loc);
    if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_LESS_THAN */89) {
      var tmp = right[1];
      if (typeof tmp !== "number" && tmp.TAG === /* JSXElement */22) {
        error$1(env, /* AdjacentJSXElements */46);
      }
      
    }
    var match = binary_op(env);
    if (match === undefined) {
      var _right = right;
      var _rloc = right_loc;
      var _param = stack;
      while(true) {
        var param = _param;
        var rloc = _rloc;
        var right$1 = _right;
        if (!param) {
          return right$1;
        }
        var match$1 = param.hd;
        var loc$1 = btwn(match$1[2], rloc);
        _param = param.tl;
        _rloc = loc$1;
        _right = make_binary(match$1[0], right$1, match$1[1][0], loc$1);
        continue ;
      };
    }
    var rop = match[0];
    if (is_unary && rop === /* Exp */14) {
      error_at(env, [
            right_loc,
            /* InvalidLHSInExponentiation */15
          ]);
    }
    _stack = add_to_stack(right, [
          rop,
          match[1]
        ], right_loc, stack);
    continue ;
  };
}

function argument(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match !== "number") {
    return {
            TAG: /* Expression */0,
            _0: Curry._1(assignment, env)
          };
  }
  if (match !== 11) {
    return {
            TAG: /* Expression */0,
            _0: Curry._1(assignment, env)
          };
  }
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_ELLIPSIS */11);
  var argument$1 = Curry._1(assignment, env);
  var loc = btwn(start_loc, argument$1[0]);
  return {
          TAG: /* Spread */1,
          _0: [
            loc,
            {
              argument: argument$1
            }
          ]
        };
}

function arguments$prime(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      if (match === 4) {
        return List.rev(acc);
      }
      if (match === 105) {
        return List.rev(acc);
      }
      
    }
    var acc_0 = argument(env);
    var acc$1 = {
      hd: acc_0,
      tl: acc
    };
    if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_RPAREN */4) {
      token$4(env, /* T_COMMA */8);
    }
    _acc = acc$1;
    continue ;
  };
}

function $$arguments(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_LPAREN */3);
  var args = arguments$prime(env, /* [] */0);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_RPAREN */4);
  return [
          btwn(start_loc, end_loc),
          args
        ];
}

function template_parts(env, _quasis, _expressions) {
  while(true) {
    var expressions = _expressions;
    var quasis = _quasis;
    var expr = Curry._1(Parse.expression, env);
    var expressions$1 = {
      hd: expr,
      tl: expressions
    };
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number" && match === 2) {
      push_lex_mode(env, /* TEMPLATE */4);
      var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
      var match$2;
      if (typeof match$1 === "number") {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "parser_flow.ml",
                1602,
                19
              ],
              Error: new Error()
            };
      }
      if (match$1.TAG === /* T_TEMPLATE_PART */2) {
        var match$3 = match$1._0;
        var tail = match$3[2];
        var match$4 = match$3[1];
        token$3(env);
        match$2 = [
          match$3[0],
          {
            value: {
              raw: match$4.raw,
              cooked: match$4.cooked
            },
            tail: tail
          },
          tail
        ];
      } else {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "parser_flow.ml",
                1602,
                19
              ],
              Error: new Error()
            };
      }
      var loc = match$2[0];
      pop_lex_mode(env);
      var quasis_0 = [
        loc,
        match$2[1]
      ];
      var quasis$1 = {
        hd: quasis_0,
        tl: quasis
      };
      if (match$2[2]) {
        return [
                loc,
                List.rev(quasis$1),
                List.rev(expressions$1)
              ];
      }
      _expressions = expressions$1;
      _quasis = quasis$1;
      continue ;
    }
    error_unexpected(env);
    var imaginary_quasi_0 = expr[0];
    var imaginary_quasi_1 = {
      value: {
        raw: "",
        cooked: ""
      },
      tail: true
    };
    var imaginary_quasi = [
      imaginary_quasi_0,
      imaginary_quasi_1
    ];
    return [
            expr[0],
            List.rev({
                  hd: imaginary_quasi,
                  tl: quasis
                }),
            List.rev(expressions$1)
          ];
  };
}

function template_literal(env, part) {
  var is_tail = part[2];
  var match = part[1];
  var start_loc = part[0];
  token$4(env, {
        TAG: /* T_TEMPLATE_PART */2,
        _0: part
      });
  var head_1 = {
    value: {
      raw: match.raw,
      cooked: match.cooked
    },
    tail: is_tail
  };
  var head = [
    start_loc,
    head_1
  ];
  var match$1 = is_tail ? [
      start_loc,
      {
        hd: head,
        tl: /* [] */0
      },
      /* [] */0
    ] : template_parts(env, {
          hd: head,
          tl: /* [] */0
        }, /* [] */0);
  var loc = btwn(start_loc, match$1[0]);
  return [
          loc,
          {
            quasis: match$1[1],
            expressions: match$1[2]
          }
        ];
}

function elements(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      if (match === 105) {
        return List.rev(acc);
      }
      if (match < 12) {
        switch (match) {
          case /* T_RBRACKET */6 :
              return List.rev(acc);
          case /* T_COMMA */8 :
              token$4(env, /* T_COMMA */8);
              _acc = {
                hd: undefined,
                tl: acc
              };
              continue ;
          case /* T_IDENTIFIER */0 :
          case /* T_LCURLY */1 :
          case /* T_RCURLY */2 :
          case /* T_LPAREN */3 :
          case /* T_RPAREN */4 :
          case /* T_LBRACKET */5 :
          case /* T_SEMICOLON */7 :
          case /* T_PERIOD */9 :
          case /* T_ARROW */10 :
              break;
          case /* T_ELLIPSIS */11 :
              var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
              token$4(env, /* T_ELLIPSIS */11);
              var argument = Curry._1(assignment, env);
              var loc = btwn(start_loc, argument[0]);
              var elem = {
                TAG: /* Spread */1,
                _0: [
                  loc,
                  {
                    argument: argument
                  }
                ]
              };
              _acc = {
                hd: elem,
                tl: acc
              };
              continue ;
          
        }
      }
      
    }
    var elem$1 = {
      TAG: /* Expression */0,
      _0: Curry._1(assignment, env)
    };
    if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_RBRACKET */6) {
      token$4(env, /* T_COMMA */8);
    }
    _acc = {
      hd: elem$1,
      tl: acc
    };
    continue ;
  };
}

function array_initializer(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_LBRACKET */5);
  var elements$1 = elements(env, /* [] */0);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_RBRACKET */6);
  return [
          btwn(start_loc, end_loc),
          {
            elements: elements$1
          }
        ];
}

function error_callback$1(param, param$1) {
  if (typeof param$1 === "number") {
    var switcher = param$1 - 28 | 0;
    if (switcher > 16 || switcher < 0) {
      if (switcher === 19) {
        return ;
      }
      throw {
            RE_EXN_ID: Parser_env_Try.Rollback,
            Error: new Error()
          };
    }
    if (switcher > 15 || switcher < 1) {
      return ;
    }
    throw {
          RE_EXN_ID: Parser_env_Try.Rollback,
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: Parser_env_Try.Rollback,
        Error: new Error()
      };
}

function try_arrow_function(env) {
  var env$1 = with_error_callback(error_callback$1, env);
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
  var async = Curry._2(Parser_env_Peek.token, 1, env$1) !== /* T_ARROW */10 && maybe(env$1, /* T_ASYNC */61);
  var typeParameters = Curry._1(type_parameter_declaration$1, env$1);
  var match;
  if (Curry._2(Parser_env_Peek.is_identifier, undefined, env$1) && typeParameters === undefined) {
    var id = Curry._2(Parse.identifier, /* StrictParamName */28, env$1);
    var param_0 = id[0];
    var param_1 = {
      TAG: /* Identifier */3,
      _0: id
    };
    var param = [
      param_0,
      param_1
    ];
    match = [
      {
        hd: param,
        tl: /* [] */0
      },
      /* [] */0,
      undefined,
      undefined
    ];
  } else {
    var match$1 = function_params(env$1);
    match = [
      match$1[0],
      match$1[1],
      match$1[2],
      wrap(annotation_opt, env$1)
    ];
  }
  var rest = match[2];
  var defaults = match[1];
  var params = match[0];
  var predicate = Curry._1(Parse.predicate, env$1);
  var env$2 = params === /* [] */0 || rest !== undefined ? without_error_callback(env$1) : env$1;
  if (Curry._1(Parser_env_Peek.is_line_terminator, env$2) && Curry._2(Parser_env_Peek.token, undefined, env$2) === /* T_ARROW */10) {
    error$1(env$2, /* NewlineBeforeArrow */44);
  }
  token$4(env$2, /* T_ARROW */10);
  var env$3 = without_error_callback(env$2);
  var match$2 = with_loc((function (param) {
          var generator = false;
          var env = with_in_function(true, param);
          var match = Curry._2(Parser_env_Peek.token, undefined, env);
          if (typeof match === "number" && match === 1) {
            var match$1 = function_body(env, async, generator);
            return [
                    match$1[1],
                    match$1[2]
                  ];
          }
          var env$1 = enter_function(env, async, generator);
          var expr = Curry._1(Parse.assignment, env$1);
          return [
                  {
                    TAG: /* BodyExpression */1,
                    _0: expr
                  },
                  env$1.in_strict_mode
                ];
        }), env$3);
  var match$3 = match$2[1];
  var body = match$3[0];
  var simple = is_simple_function_params(params, defaults, rest);
  strict_post_check(env$3, match$3[1], simple, undefined, params);
  var expression;
  expression = body.TAG === /* BodyBlock */0 ? false : true;
  var loc = btwn(start_loc, match$2[0]);
  return [
          loc,
          {
            TAG: /* ArrowFunction */3,
            _0: {
              id: undefined,
              params: params,
              defaults: defaults,
              rest: rest,
              body: body,
              async: async,
              generator: false,
              predicate: predicate,
              expression: expression,
              returnType: match[3],
              typeParameters: typeParameters
            }
          }
        ];
}

function decorator_list_helper(env, _decorators) {
  while(true) {
    var decorators = _decorators;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match !== "number") {
      return decorators;
    }
    if (match !== 12) {
      return decorators;
    }
    token$3(env);
    _decorators = {
      hd: left_hand_side(env),
      tl: decorators
    };
    continue ;
  };
}

function decorator_list(env) {
  if (env.parse_options.esproposal_decorators) {
    return List.rev(decorator_list_helper(env, /* [] */0));
  } else {
    return /* [] */0;
  }
}

function key(env) {
  var number_type = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof number_type === "number") {
    if (number_type === /* T_LBRACKET */5) {
      var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, /* T_LBRACKET */5);
      var expr = Curry._1(Parse.assignment, with_no_in(false, env));
      var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, /* T_RBRACKET */6);
      return [
              btwn(start_loc, end_loc),
              {
                TAG: /* Computed */2,
                _0: expr
              }
            ];
    }
    
  } else {
    switch (number_type.TAG | 0) {
      case /* T_NUMBER */0 :
          var raw = Curry._2(Parser_env_Peek.value, undefined, env);
          var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
          var value = number(env, number_type._0);
          var value$1 = {
            TAG: /* Number */2,
            _0: value
          };
          return [
                  loc,
                  {
                    TAG: /* Literal */0,
                    _0: [
                      loc,
                      {
                        value: value$1,
                        raw: raw
                      }
                    ]
                  }
                ];
      case /* T_STRING */1 :
          var match = number_type._0;
          var octal = match[3];
          var raw$1 = match[2];
          var value$2 = match[1];
          var loc$1 = match[0];
          if (octal) {
            strict_error(env, /* StrictOctalLiteral */31);
          }
          token$4(env, {
                TAG: /* T_STRING */1,
                _0: [
                  loc$1,
                  value$2,
                  raw$1,
                  octal
                ]
              });
          var value$3 = {
            TAG: /* String */0,
            _0: value$2
          };
          return [
                  loc$1,
                  {
                    TAG: /* Literal */0,
                    _0: [
                      loc$1,
                      {
                        value: value$3,
                        raw: raw$1
                      }
                    ]
                  }
                ];
      default:
        
    }
  }
  var match$1 = identifier_or_reserved_keyword(env);
  var id = match$1[0];
  return [
          id[0],
          {
            TAG: /* Identifier */1,
            _0: id
          }
        ];
}

function _method(env, kind) {
  var generator$1 = generator(env, false);
  var match = key(env);
  var typeParameters = kind !== 0 ? undefined : Curry._1(type_parameter_declaration$1, env);
  token$4(env, /* T_LPAREN */3);
  var params;
  switch (kind) {
    case /* Init */0 :
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "parser_flow.ml",
                1954,
                16
              ],
              Error: new Error()
            };
    case /* Get */1 :
        params = /* [] */0;
        break;
    case /* Set */2 :
        var param = Curry._2(Parse.identifier_with_type, env, /* StrictParamName */28);
        params = {
          hd: [
            param[0],
            {
              TAG: /* Identifier */3,
              _0: param
            }
          ],
          tl: /* [] */0
        };
        break;
    
  }
  token$4(env, /* T_RPAREN */4);
  var returnType = wrap(annotation_opt, env);
  var match$1 = function_body(env, false, generator$1);
  var body = match$1[1];
  var simple = is_simple_function_params(params, /* [] */0, undefined);
  strict_post_check(env, match$1[2], simple, undefined, params);
  var match$2;
  match$2 = body.TAG === /* BodyBlock */0 ? [
      body._0[0],
      false
    ] : [
      body._0[0],
      true
    ];
  var value_0 = match$2[0];
  var value_1 = {
    id: undefined,
    params: params,
    defaults: /* [] */0,
    rest: undefined,
    body: body,
    async: false,
    generator: generator$1,
    predicate: undefined,
    expression: match$2[1],
    returnType: returnType,
    typeParameters: typeParameters
  };
  var value = [
    value_0,
    value_1
  ];
  return [
          match[1],
          value
        ];
}

function property$1(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_ELLIPSIS */11) {
    token$4(env, /* T_ELLIPSIS */11);
    var argument = Curry._1(Parse.assignment, env);
    return {
            TAG: /* SpreadProperty */1,
            _0: [
              btwn(start_loc, argument[0]),
              {
                argument: argument
              }
            ]
          };
  }
  var async = Curry._2(Parser_env_Peek.is_identifier, 1, env) && maybe(env, /* T_ASYNC */61);
  var match = generator(env, async);
  var match$1 = key(env);
  var tmp;
  var exit = 0;
  if (async || match) {
    exit = 1;
  } else {
    var key$1 = match$1[1];
    switch (key$1.TAG | 0) {
      case /* Identifier */1 :
          switch (key$1._0[1].name) {
            case "get" :
                var match$2 = Curry._2(Parser_env_Peek.token, undefined, env);
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
                var match$3 = Curry._2(Parser_env_Peek.token, undefined, env);
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
      case /* Literal */0 :
      case /* Computed */2 :
          exit = 1;
          break;
      
    }
  }
  if (exit === 1) {
    tmp = init(env, start_loc, match$1[1], async, match);
  }
  return {
          TAG: /* Property */0,
          _0: tmp
        };
}

function get(env, start_loc) {
  var match = _method(env, /* Get */1);
  var match$1 = match[1];
  var end_loc = match$1[0];
  var value_1 = {
    TAG: /* Function */2,
    _0: match$1[1]
  };
  var value = [
    end_loc,
    value_1
  ];
  return [
          btwn(start_loc, end_loc),
          {
            key: match[0],
            value: value,
            kind: /* Get */1,
            _method: false,
            shorthand: false
          }
        ];
}

function set(env, start_loc) {
  var match = _method(env, /* Set */2);
  var match$1 = match[1];
  var end_loc = match$1[0];
  var value_1 = {
    TAG: /* Function */2,
    _0: match$1[1]
  };
  var value = [
    end_loc,
    value_1
  ];
  return [
          btwn(start_loc, end_loc),
          {
            key: match[0],
            value: value,
            kind: /* Set */2,
            _method: false,
            shorthand: false
          }
        ];
}

function init(env, start_loc, key, async, generator) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var match$1;
  var exit = 0;
  if (typeof match === "number") {
    if (match !== 89) {
      if (match >= 9) {
        exit = 1;
      } else {
        switch (match) {
          case /* T_LPAREN */3 :
              exit = 3;
              break;
          case /* T_IDENTIFIER */0 :
          case /* T_LCURLY */1 :
          case /* T_RPAREN */4 :
          case /* T_LBRACKET */5 :
          case /* T_RBRACKET */6 :
          case /* T_SEMICOLON */7 :
              exit = 1;
              break;
          case /* T_RCURLY */2 :
          case /* T_COMMA */8 :
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
        match$1 = [
          Curry._1(Parse.assignment, env),
          false,
          false
        ];
        break;
    case 2 :
        var tmp;
        switch (key.TAG | 0) {
          case /* Literal */0 :
              var lit = key._0;
              tmp = [
                lit[0],
                {
                  TAG: /* Literal */19,
                  _0: lit[1]
                }
              ];
              break;
          case /* Identifier */1 :
              var id = key._0;
              tmp = [
                id[0],
                {
                  TAG: /* Identifier */18,
                  _0: id
                }
              ];
              break;
          case /* Computed */2 :
              tmp = key._0;
              break;
          
        }
        match$1 = [
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
        match$4 = body.TAG === /* BodyBlock */0 ? [
            body._0[0],
            false
          ] : [
            body._0[0],
            true
          ];
        var value_0 = match$4[0];
        var value_1 = {
          TAG: /* Function */2,
          _0: {
            id: undefined,
            params: params,
            defaults: defaults,
            rest: rest,
            body: body,
            async: async,
            generator: generator,
            predicate: undefined,
            expression: match$4[1],
            returnType: returnType,
            typeParameters: typeParameters
          }
        };
        var value = [
          value_0,
          value_1
        ];
        match$1 = [
          value,
          false,
          true
        ];
        break;
    
  }
  var value$1 = match$1[0];
  return [
          btwn(start_loc, value$1[0]),
          {
            key: key,
            value: value$1,
            kind: /* Init */0,
            _method: match$1[2],
            shorthand: match$1[1]
          }
        ];
}

function check_property(env, prop_map, prop) {
  if (prop.TAG !== /* Property */0) {
    return prop_map;
  }
  var match = prop._0;
  var prop$1 = match[1];
  var prop_loc = match[0];
  var exit = 0;
  switch (prop$1.key.TAG | 0) {
    case /* Literal */0 :
    case /* Identifier */1 :
        exit = 1;
        break;
    case /* Computed */2 :
        return prop_map;
    
  }
  if (exit === 1) {
    var match$1 = prop$1.key;
    var key;
    switch (match$1.TAG | 0) {
      case /* Literal */0 :
          var s = match$1._0[1].value;
          if (typeof s === "number") {
            key = "null";
          } else {
            switch (s.TAG | 0) {
              case /* String */0 :
                  key = s._0;
                  break;
              case /* Boolean */1 :
                  var b = s._0;
                  key = b ? "true" : "false";
                  break;
              case /* Number */2 :
                  key = Pervasives.string_of_float(s._0);
                  break;
              case /* RegExp */3 :
                  throw {
                        RE_EXN_ID: "Failure",
                        _1: "RegExp cannot be property key",
                        Error: new Error()
                      };
              
            }
          }
          break;
      case /* Identifier */1 :
          key = match$1._0[1].name;
          break;
      case /* Computed */2 :
          throw {
                RE_EXN_ID: "Assert_failure",
                _1: [
                  "parser_flow.ml",
                  2103,
                  30
                ],
                Error: new Error()
              };
      
    }
    var prev_kinds;
    try {
      prev_kinds = find(key, prop_map);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        prev_kinds = /* Empty */0;
      } else {
        throw exn;
      }
    }
    var match$2 = prop$1.kind;
    var kind_string;
    switch (match$2) {
      case /* Init */0 :
          kind_string = "Init";
          break;
      case /* Get */1 :
          kind_string = "Get";
          break;
      case /* Set */2 :
          kind_string = "Set";
          break;
      
    }
    var exit$1 = 0;
    switch (kind_string) {
      case "Init" :
          if (mem$1("Init", prev_kinds)) {
            strict_error_at(env, [
                  prop_loc,
                  /* StrictDuplicateProperty */33
                ]);
          } else if (mem$1("Set", prev_kinds) || mem$1("Get", prev_kinds)) {
            error_at(env, [
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
        error_at(env, [
              prop_loc,
              /* AccessorDataProperty */34
            ]);
      } else if (mem$1(kind_string, prev_kinds)) {
        error_at(env, [
              prop_loc,
              /* AccessorGetSet */35
            ]);
      }
      
    }
    var kinds = add$1(kind_string, prev_kinds);
    return add$2(key, kinds, prop_map);
  }
  
}

function properties$1(env, _param) {
  while(true) {
    var param = _param;
    var acc = param[1];
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      if (match === 2) {
        return List.rev(acc);
      }
      if (match === 105) {
        return List.rev(acc);
      }
      
    }
    var prop = property$1(env);
    var prop_map = check_property(env, param[0], prop);
    if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_RCURLY */2) {
      token$4(env, /* T_COMMA */8);
    }
    _param = [
      prop_map,
      {
        hd: prop,
        tl: acc
      }
    ];
    continue ;
  };
}

function _initializer(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_LCURLY */1);
  var props = properties$1(env, [
        /* Empty */0,
        /* [] */0
      ]);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_RCURLY */2);
  return [
          btwn(start_loc, end_loc),
          {
            properties: props
          }
        ];
}

function class_implements(env, _acc) {
  while(true) {
    var acc = _acc;
    var id = Curry._2(Parse.identifier, undefined, env);
    var typeParameters = wrap(type_parameter_instantiation, env);
    var loc = typeParameters !== undefined ? btwn(id[0], typeParameters[0]) : id[0];
    var implement_1 = {
      id: id,
      typeParameters: typeParameters
    };
    var implement = [
      loc,
      implement_1
    ];
    var acc$1 = {
      hd: implement,
      tl: acc
    };
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match !== "number") {
      return List.rev(acc$1);
    }
    if (match !== 8) {
      return List.rev(acc$1);
    }
    token$4(env, /* T_COMMA */8);
    _acc = acc$1;
    continue ;
  };
}

function init$1(env, start_loc, decorators, key, async, generator, $$static) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var exit = 0;
  if (typeof match === "number") {
    var switcher = match - 75 | 0;
    if (switcher > 2 || switcher < 0) {
      if (switcher === -68) {
        exit = 2;
      }
      
    } else if (switcher !== 1) {
      exit = 2;
    }
    
  }
  if (exit === 2 && !async && !generator) {
    var typeAnnotation = wrap(annotation_opt, env);
    var options = env.parse_options;
    var value = Curry._2(Parser_env_Peek.token, undefined, env) === /* T_ASSIGN */75 && ($$static && options.esproposal_class_static_fields || !$$static && options.esproposal_class_instance_fields) ? (token$4(env, /* T_ASSIGN */75), Curry._1(Parse.expression, env)) : undefined;
    var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    if (maybe(env, /* T_SEMICOLON */7) || !(Curry._2(Parser_env_Peek.token, undefined, env) === /* T_LBRACKET */5 || Curry._2(Parser_env_Peek.token, undefined, env) === /* T_LPAREN */3)) {
      
    } else {
      error_unexpected(env);
    }
    var loc = btwn(start_loc, end_loc);
    return {
            TAG: /* Property */1,
            _0: [
              loc,
              {
                key: key,
                value: value,
                typeAnnotation: typeAnnotation,
                static: $$static
              }
            ]
          };
  }
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
  match$3 = body.TAG === /* BodyBlock */0 ? [
      body._0[0],
      false
    ] : [
      body._0[0],
      true
    ];
  var end_loc$1 = match$3[0];
  var value_1 = {
    id: undefined,
    params: params,
    defaults: defaults,
    rest: rest,
    body: body,
    async: async,
    generator: generator,
    predicate: undefined,
    expression: match$3[1],
    returnType: returnType,
    typeParameters: typeParameters
  };
  var value$1 = [
    end_loc$1,
    value_1
  ];
  var kind;
  switch (key.TAG | 0) {
    case /* Literal */0 :
        var match$4 = key._0[1].value;
        kind = typeof match$4 === "number" || !(match$4.TAG === /* String */0 && match$4._0 === "constructor") ? /* Method */1 : /* Constructor */0;
        break;
    case /* Identifier */1 :
        kind = key._0[1].name === "constructor" ? /* Constructor */0 : /* Method */1;
        break;
    case /* Computed */2 :
        kind = /* Method */1;
        break;
    
  }
  return {
          TAG: /* Method */0,
          _0: [
            btwn(start_loc, end_loc$1),
            {
              kind: kind,
              key: key,
              value: value$1,
              static: $$static,
              decorators: decorators
            }
          ]
        };
}

function class_element(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var decorators = decorator_list(env);
  var $$static = maybe(env, /* T_STATIC */40);
  var async = Curry._2(Parser_env_Peek.token, 1, env) !== /* T_LPAREN */3 && Curry._2(Parser_env_Peek.token, 1, env) !== /* T_COLON */77 && maybe(env, /* T_ASYNC */61);
  var generator$1 = generator(env, async);
  var match = key(env);
  if (!async && !generator$1) {
    var key$1 = match[1];
    switch (key$1.TAG | 0) {
      case /* Identifier */1 :
          switch (key$1._0[1].name) {
            case "get" :
                var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
                var exit = 0;
                exit = typeof match$1 === "number" ? (
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
                switch (exit) {
                  case 2 :
                      var match$2 = _method(env, /* Get */1);
                      var value = match$2[1];
                      return {
                              TAG: /* Method */0,
                              _0: [
                                btwn(start_loc, value[0]),
                                {
                                  kind: /* Get */2,
                                  key: match$2[0],
                                  value: value,
                                  static: $$static,
                                  decorators: decorators
                                }
                              ]
                            };
                  case 3 :
                      return init$1(env, start_loc, decorators, key$1, async, generator$1, $$static);
                  
                }
                break;
            case "set" :
                var match$3 = Curry._2(Parser_env_Peek.token, undefined, env);
                var exit$1 = 0;
                exit$1 = typeof match$3 === "number" ? (
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
                switch (exit$1) {
                  case 2 :
                      var match$4 = _method(env, /* Set */2);
                      var value$1 = match$4[1];
                      return {
                              TAG: /* Method */0,
                              _0: [
                                btwn(start_loc, value$1[0]),
                                {
                                  kind: /* Set */3,
                                  key: match$4[0],
                                  value: value$1,
                                  static: $$static,
                                  decorators: decorators
                                }
                              ]
                            };
                  case 3 :
                      return init$1(env, start_loc, decorators, key$1, async, generator$1, $$static);
                  
                }
                break;
            default:
              
          }
          break;
      case /* Literal */0 :
      case /* Computed */2 :
          break;
      
    }
  }
  return init$1(env, start_loc, decorators, match[1], async, generator$1, $$static);
}

function elements$1(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      var switcher = match - 3 | 0;
      if (switcher > 101 || switcher < 0) {
        if ((switcher + 1 >>> 0) <= 103) {
          return List.rev(acc);
        }
        
      } else if (switcher === 4) {
        token$4(env, /* T_SEMICOLON */7);
        continue ;
      }
      
    }
    _acc = {
      hd: Curry._1(class_element, env),
      tl: acc
    };
    continue ;
  };
}

function class_body(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_LCURLY */1);
  var body = elements$1(env, /* [] */0);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_RCURLY */2);
  return [
          btwn(start_loc, end_loc),
          {
            body: body
          }
        ];
}

function _class(env) {
  var match;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_EXTENDS */39) {
    token$4(env, /* T_EXTENDS */39);
    var superClass = left_hand_side(with_allow_yield(false, env));
    var superTypeParameters = wrap(type_parameter_instantiation, env);
    match = [
      superClass,
      superTypeParameters
    ];
  } else {
    match = [
      undefined,
      undefined
    ];
  }
  var $$implements = Curry._2(Parser_env_Peek.token, undefined, env) === /* T_IMPLEMENTS */50 ? (!env.parse_options.types ? error$1(env, /* UnexpectedTypeInterface */10) : undefined, token$4(env, /* T_IMPLEMENTS */50), class_implements(env, /* [] */0)) : /* [] */0;
  var body = Curry._1(class_body, env);
  return [
          body,
          match[0],
          match[1],
          $$implements
        ];
}

function class_declaration(env, decorators) {
  var env$1 = with_strict(true, env);
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
  var decorators$1 = Pervasives.$at(decorators, decorator_list(env$1));
  token$4(env$1, /* T_CLASS */38);
  var tmp_env = with_no_let(true, env$1);
  var match = env$1.in_export;
  var match$1 = Curry._2(Parser_env_Peek.is_identifier, undefined, tmp_env);
  var id = match && !match$1 ? undefined : Curry._2(Parse.identifier, undefined, tmp_env);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env$1);
  var match$2 = _class(env$1);
  var body = match$2[0];
  var loc = btwn(start_loc, body[0]);
  return [
          loc,
          {
            TAG: /* ClassDeclaration */20,
            _0: {
              id: id,
              body: body,
              superClass: match$2[1],
              typeParameters: typeParameters,
              superTypeParameters: match$2[2],
              implements: match$2[3],
              classDecorators: decorators$1
            }
          }
        ];
}

function class_expression(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var decorators = decorator_list(env);
  token$4(env, /* T_CLASS */38);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var match$1;
  var exit = 0;
  if (typeof match === "number") {
    var switcher = match - 1 | 0;
    if (switcher > 38 || switcher < 0) {
      if (switcher !== 88) {
        exit = 1;
      } else {
        match$1 = [
          undefined,
          undefined
        ];
      }
    } else if (switcher > 37 || switcher < 1) {
      match$1 = [
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
    var id = Curry._2(Parse.identifier, undefined, env);
    var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env);
    match$1 = [
      id,
      typeParameters
    ];
  }
  var match$2 = _class(env);
  var body = match$2[0];
  var loc = btwn(start_loc, body[0]);
  return [
          loc,
          {
            TAG: /* Class */23,
            _0: {
              id: match$1[0],
              body: body,
              superClass: match$2[1],
              typeParameters: match$1[1],
              superTypeParameters: match$2[2],
              implements: match$2[3],
              classDecorators: decorators
            }
          }
        ];
}

function export_specifiers_and_errs(env, _specifiers, _errs) {
  while(true) {
    var errs = _errs;
    var specifiers = _specifiers;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      if (match === 2) {
        return [
                List.rev(specifiers),
                List.rev(errs)
              ];
      }
      if (match === 105) {
        return [
                List.rev(specifiers),
                List.rev(errs)
              ];
      }
      
    }
    var match$1 = Curry._1(Parse.identifier_or_reserved_keyword, env);
    var id = match$1[0];
    var match$2;
    if (Curry._2(Parser_env_Peek.value, undefined, env) === "as") {
      contextual(env, "as");
      var match$3 = Curry._1(Parse.identifier_or_reserved_keyword, env);
      var name = match$3[0];
      record_export(env, [
            name[0],
            extract_ident_name(name)
          ]);
      match$2 = [
        name,
        undefined,
        name[0]
      ];
    } else {
      var loc = id[0];
      record_export(env, [
            loc,
            extract_ident_name(id)
          ]);
      match$2 = [
        undefined,
        match$1[1],
        loc
      ];
    }
    var err = match$2[1];
    var loc$1 = btwn(id[0], match$2[2]);
    var specifier_1 = {
      id: id,
      name: match$2[0]
    };
    var specifier = [
      loc$1,
      specifier_1
    ];
    if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_COMMA */8) {
      token$4(env, /* T_COMMA */8);
    }
    var errs$1 = err !== undefined ? ({
          hd: err,
          tl: errs
        }) : errs;
    _errs = errs$1;
    _specifiers = {
      hd: specifier,
      tl: specifiers
    };
    continue ;
  };
}

function type_alias_helper(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (!env.parse_options.types) {
    error$1(env, /* UnexpectedTypeAlias */5);
  }
  token$4(env, /* T_TYPE */59);
  push_lex_mode(env, /* TYPE */1);
  var id = Curry._2(Parse.identifier, undefined, env);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env);
  token$4(env, /* T_ASSIGN */75);
  var right = wrap(_type, env);
  var end_loc = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
  var end_loc$1 = end_loc !== undefined ? end_loc : right[0];
  semicolon(env);
  pop_lex_mode(env);
  return [
          btwn(start_loc, end_loc$1),
          {
            id: id,
            typeParameters: typeParameters,
            right: right
          }
        ];
}

function export_source(env) {
  contextual(env, "from");
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match !== "number" && match.TAG === /* T_STRING */1) {
    var match$1 = match._0;
    var octal = match$1[3];
    var raw = match$1[2];
    var value = match$1[1];
    var loc = match$1[0];
    if (octal) {
      strict_error(env, /* StrictOctalLiteral */31);
    }
    token$4(env, {
          TAG: /* T_STRING */1,
          _0: [
            loc,
            value,
            raw,
            octal
          ]
        });
    var value$1 = {
      TAG: /* String */0,
      _0: value
    };
    return [
            loc,
            {
              value: value$1,
              raw: raw
            }
          ];
  }
  var raw$1 = Curry._2(Parser_env_Peek.value, undefined, env);
  var value$2 = {
    TAG: /* String */0,
    _0: raw$1
  };
  var ret_0 = Curry._2(Parser_env_Peek.loc, undefined, env);
  var ret_1 = {
    value: value$2,
    raw: raw$1
  };
  var ret = [
    ret_0,
    ret_1
  ];
  error_unexpected(env);
  return ret;
}

function declare_var(env, start_loc) {
  token$4(env, /* T_VAR */22);
  var id = Curry._2(Parse.identifier_with_type, env, /* StrictVarName */27);
  var loc = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
  var end_loc = loc !== undefined ? loc : id[0];
  var loc$1 = btwn(start_loc, end_loc);
  semicolon(env);
  return [
          loc$1,
          {
            id: id
          }
        ];
}

function declare_function(env, start_loc) {
  token$4(env, /* T_FUNCTION */13);
  var id = Curry._2(Parse.identifier, undefined, env);
  var start_sig_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var typeParameters = Curry._1(type_parameter_declaration$1, env);
  var match = wrap(function_param_list, env);
  token$4(env, /* T_COLON */77);
  var returnType = wrap(_type, env);
  var end_loc = returnType[0];
  var loc = btwn(start_sig_loc, end_loc);
  var value_1 = {
    TAG: /* Function */1,
    _0: {
      params: match[1],
      returnType: returnType,
      rest: match[0],
      typeParameters: typeParameters
    }
  };
  var value = [
    loc,
    value_1
  ];
  var typeAnnotation = [
    loc,
    value
  ];
  var init = id[1];
  var id_0 = btwn(id[0], end_loc);
  var id_1 = {
    name: init.name,
    typeAnnotation: typeAnnotation,
    optional: init.optional
  };
  var id$1 = [
    id_0,
    id_1
  ];
  var end_loc$1 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
  var end_loc$2 = end_loc$1 !== undefined ? end_loc$1 : end_loc;
  var predicate = Curry._1(Parse.predicate, env);
  semicolon(env);
  var loc$1 = btwn(start_loc, end_loc$2);
  return [
          loc$1,
          {
            id: id$1,
            predicate: predicate
          }
        ];
}

function extract_ident_name(param) {
  return param[1].name;
}

function expression(env) {
  var expression$1 = Curry._1(Parse.expression, env);
  var loc = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
  var end_loc = loc !== undefined ? loc : expression$1[0];
  semicolon(env);
  return [
          btwn(expression$1[0], end_loc),
          {
            TAG: /* Expression */1,
            _0: {
              expression: expression$1
            }
          }
        ];
}

function declare(in_moduleOpt, env) {
  var in_module = in_moduleOpt !== undefined ? in_moduleOpt : false;
  if (!env.parse_options.types) {
    error$1(env, /* UnexpectedTypeDeclaration */7);
  }
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var match = Curry._2(Parser_env_Peek.token, 1, env);
  if (typeof match === "number") {
    if (match >= 22) {
      if (match >= 38) {
        if (match < 62) {
          switch (match - 38 | 0) {
            case /* T_IDENTIFIER */0 :
                token$4(env, /* T_DECLARE */58);
                var match$1 = Curry._2(declare_class, env, start_loc);
                return [
                        match$1[0],
                        {
                          TAG: /* DeclareClass */24,
                          _0: match$1[1]
                        }
                      ];
            case /* T_PERIOD */9 :
                if (in_module) {
                  return declare_export_declaration(in_module, env);
                }
                break;
            case /* T_FUNCTION */13 :
                token$4(env, /* T_DECLARE */58);
                return $$interface(env);
            case /* T_TRY */21 :
                token$4(env, /* T_DECLARE */58);
                return type_alias(env);
            case /* T_LCURLY */1 :
            case /* T_RCURLY */2 :
            case /* T_LPAREN */3 :
            case /* T_RPAREN */4 :
            case /* T_LBRACKET */5 :
            case /* T_RBRACKET */6 :
            case /* T_SEMICOLON */7 :
            case /* T_COMMA */8 :
            case /* T_ARROW */10 :
            case /* T_ELLIPSIS */11 :
            case /* T_AT */12 :
            case /* T_IF */14 :
            case /* T_IN */15 :
            case /* T_INSTANCEOF */16 :
            case /* T_RETURN */17 :
            case /* T_SWITCH */18 :
            case /* T_THIS */19 :
            case /* T_THROW */20 :
            case /* T_VAR */22 :
                break;
            case /* T_WHILE */23 :
                token$4(env, /* T_DECLARE */58);
                error$1(env, /* DeclareAsync */49);
                token$4(env, /* T_ASYNC */61);
                return declare_function_statement(env, start_loc);
            
          }
        }
        
      } else if (match < 23) {
        token$4(env, /* T_DECLARE */58);
        return declare_var_statement(env, start_loc);
      }
      
    } else if (match !== 13) {
      if (match === 0 && Curry._2(Parser_env_Peek.value, 1, env) === "module") {
        token$4(env, /* T_DECLARE */58);
        contextual(env, "module");
        if (in_module || Curry._2(Parser_env_Peek.token, undefined, env) === /* T_PERIOD */9) {
          token$4(env, /* T_PERIOD */9);
          contextual(env, "exports");
          var type_annot = wrap(annotation, env);
          var loc = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
          var end_loc = loc !== undefined ? loc : type_annot[0];
          semicolon(env);
          var loc$1 = btwn(start_loc, end_loc);
          return [
                  loc$1,
                  {
                    TAG: /* DeclareModuleExports */26,
                    _0: type_annot
                  }
                ];
        } else {
          var match$2 = Curry._2(Parser_env_Peek.token, undefined, env);
          var id;
          if (typeof match$2 === "number" || match$2.TAG !== /* T_STRING */1) {
            id = {
              TAG: /* Identifier */0,
              _0: Curry._2(Parse.identifier, undefined, env)
            };
          } else {
            var match$3 = match$2._0;
            var octal = match$3[3];
            var raw = match$3[2];
            var value = match$3[1];
            var loc$2 = match$3[0];
            if (octal) {
              strict_error(env, /* StrictOctalLiteral */31);
            }
            token$4(env, {
                  TAG: /* T_STRING */1,
                  _0: [
                    loc$2,
                    value,
                    raw,
                    octal
                  ]
                });
            var value$1 = {
              TAG: /* String */0,
              _0: value
            };
            id = {
              TAG: /* Literal */1,
              _0: [
                loc$2,
                {
                  value: value$1,
                  raw: raw
                }
              ]
            };
          }
          var body_start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
          token$4(env, /* T_LCURLY */1);
          var match$4 = module_items(env, undefined, /* [] */0);
          var module_kind = match$4[0];
          token$4(env, /* T_RCURLY */2);
          var body_end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
          var body_loc = btwn(body_start_loc, body_end_loc);
          var body_1 = {
            body: match$4[1]
          };
          var body = [
            body_loc,
            body_1
          ];
          var loc$3 = btwn(start_loc, body_loc);
          var kind = module_kind !== undefined ? module_kind : ({
                TAG: /* CommonJS */0,
                _0: loc$3
              });
          return [
                  loc$3,
                  {
                    TAG: /* DeclareModule */25,
                    _0: {
                      id: id,
                      body: body,
                      kind: kind
                    }
                  }
                ];
        }
      }
      
    } else {
      token$4(env, /* T_DECLARE */58);
      return declare_function_statement(env, start_loc);
    }
  }
  if (in_module) {
    token$4(env, /* T_DECLARE */58);
    return declare_var_statement(env, start_loc);
  } else {
    return Curry._1(Parse.statement, env);
  }
}

function $$interface(env) {
  if (!Curry._2(Parser_env_Peek.is_identifier, 1, env)) {
    return expression(env);
  }
  var match = Curry._1(interface_helper, env);
  return [
          match[0],
          {
            TAG: /* InterfaceDeclaration */21,
            _0: match[1]
          }
        ];
}

function declare_var_statement(env, start_loc) {
  var match = declare_var(env, start_loc);
  return [
          match[0],
          {
            TAG: /* DeclareVariable */22,
            _0: match[1]
          }
        ];
}

function declare_function_statement(env, start_loc) {
  var match = declare_function(env, start_loc);
  return [
          match[0],
          {
            TAG: /* DeclareFunction */23,
            _0: match[1]
          }
        ];
}

function type_alias(env) {
  if (!Curry._2(Parser_env_Peek.is_identifier, 1, env)) {
    return Curry._1(Parse.statement, env);
  }
  var match = type_alias_helper(env);
  return [
          match[0],
          {
            TAG: /* TypeAlias */7,
            _0: match[1]
          }
        ];
}

function declare_export_declaration(allow_export_typeOpt, env) {
  var allow_export_type = allow_export_typeOpt !== undefined ? allow_export_typeOpt : false;
  if (!env.parse_options.types) {
    error$1(env, /* UnexpectedTypeDeclaration */7);
  }
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_DECLARE */58);
  var env$1 = with_in_export(true, with_strict(true, env));
  token$4(env$1, /* T_EXPORT */47);
  var match = Curry._2(Parser_env_Peek.token, undefined, env$1);
  var exit = 0;
  if (typeof match === "number") {
    if (match >= 52) {
      if (match !== 59) {
        if (match !== 97) {
          exit = 1;
        } else {
          var loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
          token$4(env$1, /* T_MULT */97);
          var parse_export_star_as = env$1.parse_options.esproposal_export_star_as;
          var local_name = Curry._2(Parser_env_Peek.value, undefined, env$1) === "as" ? (contextual(env$1, "as"), parse_export_star_as ? Curry._2(Parse.identifier, undefined, env$1) : (error$1(env$1, /* UnexpectedTypeDeclaration */7), undefined)) : undefined;
          var specifiers = {
            TAG: /* ExportBatchSpecifier */1,
            _0: loc,
            _1: local_name
          };
          var source = export_source(env$1);
          var loc$1 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
          var end_loc = loc$1 !== undefined ? loc$1 : source[0];
          var source$1 = source;
          semicolon(env$1);
          return [
                  btwn(start_loc, end_loc),
                  {
                    TAG: /* DeclareExportDeclaration */27,
                    _0: {
                      default: false,
                      declaration: undefined,
                      specifiers: specifiers,
                      source: source$1
                    }
                  }
                ];
        }
      } else {
        if (allow_export_type) {
          var match$1 = type_alias_helper(env$1);
          var alias_loc = match$1[0];
          var loc$2 = btwn(start_loc, alias_loc);
          return [
                  loc$2,
                  {
                    TAG: /* DeclareExportDeclaration */27,
                    _0: {
                      default: false,
                      declaration: {
                        TAG: /* NamedType */4,
                        _0: [
                          alias_loc,
                          match$1[1]
                        ]
                      },
                      specifiers: undefined,
                      source: undefined
                    }
                  }
                ];
        }
        exit = 1;
      }
    } else if (match >= 39) {
      if (match >= 51) {
        if (allow_export_type) {
          var match$2 = Curry._1(interface_helper, env$1);
          var iface_loc = match$2[0];
          var loc$3 = btwn(start_loc, iface_loc);
          return [
                  loc$3,
                  {
                    TAG: /* DeclareExportDeclaration */27,
                    _0: {
                      default: false,
                      declaration: {
                        TAG: /* Interface */5,
                        _0: [
                          iface_loc,
                          match$2[1]
                        ]
                      },
                      specifiers: undefined,
                      source: undefined
                    }
                  }
                ];
        }
        exit = 1;
      } else {
        exit = 1;
      }
    } else if (match >= 13) {
      switch (match - 13 | 0) {
        case /* T_TRY */21 :
            token$4(env$1, /* T_DEFAULT */34);
            var match$3 = Curry._2(Parser_env_Peek.token, undefined, env$1);
            var match$4;
            var exit$1 = 0;
            if (typeof match$3 === "number") {
              if (match$3 !== 13) {
                if (match$3 !== 38) {
                  exit$1 = 3;
                } else {
                  var _class = Curry._2(declare_class, env$1, start_loc);
                  match$4 = [
                    _class[0],
                    {
                      TAG: /* Class */2,
                      _0: _class
                    }
                  ];
                }
              } else {
                var fn = declare_function(env$1, start_loc);
                match$4 = [
                  fn[0],
                  {
                    TAG: /* Function */1,
                    _0: fn
                  }
                ];
              }
            } else {
              exit$1 = 3;
            }
            if (exit$1 === 3) {
              var _type$1 = wrap(_type, env$1);
              var loc$4 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
              var end_loc$1 = loc$4 !== undefined ? loc$4 : _type$1[0];
              semicolon(env$1);
              match$4 = [
                end_loc$1,
                {
                  TAG: /* DefaultType */3,
                  _0: _type$1
                }
              ];
            }
            return [
                    btwn(start_loc, match$4[0]),
                    {
                      TAG: /* DeclareExportDeclaration */27,
                      _0: {
                        default: true,
                        declaration: match$4[1],
                        specifiers: undefined,
                        source: undefined
                      }
                    }
                  ];
        case /* T_LCURLY */1 :
        case /* T_RCURLY */2 :
        case /* T_LPAREN */3 :
        case /* T_RPAREN */4 :
        case /* T_LBRACKET */5 :
        case /* T_RBRACKET */6 :
        case /* T_SEMICOLON */7 :
        case /* T_COMMA */8 :
        case /* T_ARROW */10 :
        case /* T_ELLIPSIS */11 :
        case /* T_IF */14 :
        case /* T_IN */15 :
        case /* T_INSTANCEOF */16 :
        case /* T_RETURN */17 :
        case /* T_SWITCH */18 :
        case /* T_THIS */19 :
        case /* T_THROW */20 :
        case /* T_VAR */22 :
        case /* T_WHILE */23 :
        case /* T_WITH */24 :
            exit = 1;
            break;
        case /* T_IDENTIFIER */0 :
        case /* T_PERIOD */9 :
        case /* T_AT */12 :
        case /* T_FUNCTION */13 :
        case /* T_CONST */25 :
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
        var match$5 = Curry._2(Parser_env_Peek.token, undefined, env$1);
        if (typeof match$5 === "number") {
          if (match$5 !== 51) {
            if (match$5 !== 59) {
              
            } else {
              error$1(env$1, /* DeclareExportType */52);
            }
          } else {
            error$1(env$1, /* DeclareExportInterface */53);
          }
        }
        token$4(env$1, /* T_LCURLY */1);
        var match$6 = export_specifiers_and_errs(env$1, /* [] */0, /* [] */0);
        var specifiers$1 = {
          TAG: /* ExportSpecifiers */0,
          _0: match$6[0]
        };
        var end_loc$2 = Curry._2(Parser_env_Peek.loc, undefined, env$1);
        token$4(env$1, /* T_RCURLY */2);
        var source$2 = Curry._2(Parser_env_Peek.value, undefined, env$1) === "from" ? export_source(env$1) : (List.iter((function (param) {
                    return error_at(env$1, param);
                  }), match$6[1]), undefined);
        var loc$5 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
        var end_loc$3 = loc$5 !== undefined ? loc$5 : (
            source$2 !== undefined ? source$2[0] : end_loc$2
          );
        semicolon(env$1);
        return [
                btwn(start_loc, end_loc$3),
                {
                  TAG: /* DeclareExportDeclaration */27,
                  _0: {
                    default: false,
                    declaration: undefined,
                    specifiers: specifiers$1,
                    source: source$2
                  }
                }
              ];
    case 2 :
        var token$5 = Curry._2(Parser_env_Peek.token, undefined, env$1);
        var match$7;
        var exit$2 = 0;
        if (typeof token$5 === "number") {
          if (token$5 >= 23) {
            if (token$5 >= 27) {
              if (token$5 !== 38) {
                exit$2 = 3;
              } else {
                var _class$1 = Curry._2(declare_class, env$1, start_loc);
                match$7 = [
                  _class$1[0],
                  {
                    TAG: /* Class */2,
                    _0: _class$1
                  }
                ];
              }
            } else {
              exit$2 = token$5 >= 25 ? 4 : 3;
            }
          } else if (token$5 !== 13) {
            exit$2 = token$5 >= 22 ? 4 : 3;
          } else {
            var fn$1 = declare_function(env$1, start_loc);
            match$7 = [
              fn$1[0],
              {
                TAG: /* Function */1,
                _0: fn$1
              }
            ];
          }
        } else {
          exit$2 = 3;
        }
        switch (exit$2) {
          case 3 :
              throw {
                    RE_EXN_ID: "Assert_failure",
                    _1: [
                      "parser_flow.ml",
                      3480,
                      17
                    ],
                    Error: new Error()
                  };
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
              match$7 = [
                $$var[0],
                {
                  TAG: /* Variable */0,
                  _0: $$var
                }
              ];
              break;
          
        }
        return [
                btwn(start_loc, match$7[0]),
                {
                  TAG: /* DeclareExportDeclaration */27,
                  _0: {
                    default: false,
                    declaration: match$7[1],
                    specifiers: undefined,
                    source: undefined
                  }
                }
              ];
    
  }
}

function supers(env, _acc) {
  while(true) {
    var acc = _acc;
    var $$super = wrap(generic, env);
    var acc$1 = {
      hd: $$super,
      tl: acc
    };
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match !== "number") {
      return List.rev(acc$1);
    }
    if (match !== 8) {
      return List.rev(acc$1);
    }
    token$4(env, /* T_COMMA */8);
    _acc = acc$1;
    continue ;
  };
}

function interface_helper(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (!env.parse_options.types) {
    error$1(env, /* UnexpectedTypeInterface */10);
  }
  token$4(env, /* T_INTERFACE */51);
  var id = Curry._2(Parse.identifier, undefined, env);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env);
  var $$extends = Curry._2(Parser_env_Peek.token, undefined, env) === /* T_EXTENDS */39 ? (token$4(env, /* T_EXTENDS */39), supers(env, /* [] */0)) : /* [] */0;
  var body = _object$1(true, env);
  var loc = btwn(start_loc, body[0]);
  return [
          loc,
          {
            id: id,
            typeParameters: typeParameters,
            body: body,
            extends: $$extends,
            mixins: /* [] */0
          }
        ];
}

function supers$1(env, _acc) {
  while(true) {
    var acc = _acc;
    var $$super = wrap(generic, env);
    var acc$1 = {
      hd: $$super,
      tl: acc
    };
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match !== "number") {
      return List.rev(acc$1);
    }
    if (match !== 8) {
      return List.rev(acc$1);
    }
    token$4(env, /* T_COMMA */8);
    _acc = acc$1;
    continue ;
  };
}

function declare_class(env, start_loc) {
  var env$1 = with_strict(true, env);
  token$4(env$1, /* T_CLASS */38);
  var id = Curry._2(Parse.identifier, undefined, env$1);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env$1);
  var $$extends = Curry._2(Parser_env_Peek.token, undefined, env$1) === /* T_EXTENDS */39 ? (token$4(env$1, /* T_EXTENDS */39), supers$1(env$1, /* [] */0)) : /* [] */0;
  var mixins = Curry._2(Parser_env_Peek.value, undefined, env$1) === "mixins" ? (contextual(env$1, "mixins"), supers$1(env$1, /* [] */0)) : /* [] */0;
  var body = _object$1(true, env$1);
  var loc = btwn(start_loc, body[0]);
  return [
          loc,
          {
            id: id,
            typeParameters: typeParameters,
            body: body,
            extends: $$extends,
            mixins: mixins
          }
        ];
}

function module_items(env, _module_kind, _acc) {
  while(true) {
    var acc = _acc;
    var module_kind = _module_kind;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      if (match === 2) {
        return [
                module_kind,
                List.rev(acc)
              ];
      }
      if (match === 105) {
        return [
                module_kind,
                List.rev(acc)
              ];
      }
      
    }
    var stmt = declare(true, env);
    var stmt$1 = stmt[1];
    var loc = stmt[0];
    var module_kind$1;
    if (module_kind !== undefined) {
      if (module_kind.TAG === /* CommonJS */0) {
        if (typeof stmt$1 === "number") {
          module_kind$1 = module_kind;
        } else {
          switch (stmt$1.TAG | 0) {
            case /* DeclareModuleExports */26 :
                error$1(env, /* DuplicateDeclareModuleExports */60);
                module_kind$1 = module_kind;
                break;
            case /* DeclareExportDeclaration */27 :
                var declaration = stmt$1._0.declaration;
                if (declaration !== undefined) {
                  switch (declaration.TAG | 0) {
                    case /* NamedType */4 :
                    case /* Interface */5 :
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
      } else if (typeof stmt$1 === "number" || stmt$1.TAG !== /* DeclareModuleExports */26) {
        module_kind$1 = module_kind;
      } else {
        error$1(env, /* AmbiguousDeclareModuleKind */61);
        module_kind$1 = module_kind;
      }
    } else if (typeof stmt$1 === "number") {
      module_kind$1 = module_kind;
    } else {
      switch (stmt$1.TAG | 0) {
        case /* DeclareModuleExports */26 :
            module_kind$1 = {
              TAG: /* CommonJS */0,
              _0: loc
            };
            break;
        case /* DeclareExportDeclaration */27 :
            var declaration$1 = stmt$1._0.declaration;
            if (declaration$1 !== undefined) {
              switch (declaration$1.TAG | 0) {
                case /* NamedType */4 :
                case /* Interface */5 :
                    module_kind$1 = module_kind;
                    break;
                default:
                  module_kind$1 = {
                    TAG: /* ES */1,
                    _0: loc
                  };
              }
            } else {
              module_kind$1 = {
                TAG: /* ES */1,
                _0: loc
              };
            }
            break;
        default:
          module_kind$1 = module_kind;
      }
    }
    _acc = {
      hd: stmt,
      tl: acc
    };
    _module_kind = module_kind$1;
    continue ;
  };
}

function fold(acc, _param) {
  while(true) {
    var param = _param;
    var match = param[1];
    switch (match.TAG | 0) {
      case /* Object */0 :
          return List.fold_left((function (acc, prop) {
                        if (prop.TAG === /* Property */0) {
                          return fold(acc, prop._0[1].pattern);
                        } else {
                          return fold(acc, prop._0[1].argument);
                        }
                      }), acc, match._0.properties);
      case /* Array */1 :
          return List.fold_left((function (acc, elem) {
                        if (elem !== undefined) {
                          if (elem.TAG === /* Element */0) {
                            return fold(acc, elem._0);
                          } else {
                            return fold(acc, elem._0[1].argument);
                          }
                        } else {
                          return acc;
                        }
                      }), acc, match._0.elements);
      case /* Assignment */2 :
          _param = match._0.left;
          continue ;
      case /* Identifier */3 :
          var match$1 = match._0;
          return {
                  hd: [
                    match$1[0],
                    match$1[1].name
                  ],
                  tl: acc
                };
      case /* Expression */4 :
          throw {
                RE_EXN_ID: "Failure",
                _1: "Parser error: No such thing as an expression pattern!",
                Error: new Error()
              };
      
    }
  };
}

function assert_can_be_forin_or_forof(env, err, param) {
  if (param === undefined) {
    return error$1(env, err);
  }
  if (param.TAG === /* InitDeclaration */0) {
    var match = param._0;
    var declarations = match[1].declarations;
    if (declarations && declarations.hd[1].init === undefined && !declarations.tl) {
      return ;
    }
    return error_at(env, [
                match[0],
                err
              ]);
  }
  var match$1 = param._0;
  var loc = match$1[0];
  if (!Curry._1(Parse.is_assignable_lhs, [
          loc,
          match$1[1]
        ])) {
    return error_at(env, [
                loc,
                err
              ]);
  }
  
}

function _if(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_IF */14);
  token$4(env, /* T_LPAREN */3);
  var test = Curry._1(Parse.expression, env);
  token$4(env, /* T_RPAREN */4);
  Curry._2(Parser_env_Peek.token, undefined, env);
  var consequent = Curry._2(Parser_env_Peek.is_function, undefined, env) ? (strict_error(env, /* StrictFunctionStatement */45), _function(env)) : Curry._1(Parse.statement, env);
  var alternate = Curry._2(Parser_env_Peek.token, undefined, env) === /* T_ELSE */41 ? (token$4(env, /* T_ELSE */41), Curry._1(Parse.statement, env)) : undefined;
  var end_loc = alternate !== undefined ? alternate[0] : consequent[0];
  return [
          btwn(start_loc, end_loc),
          {
            TAG: /* If */2,
            _0: {
              test: test,
              consequent: consequent,
              alternate: alternate
            }
          }
        ];
}

function case_list(env, _param) {
  while(true) {
    var param = _param;
    var acc = param[1];
    var seen_default = param[0];
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      if (match === 2) {
        return List.rev(acc);
      }
      if (match === 105) {
        return List.rev(acc);
      }
      
    }
    var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
    var test = typeof match$1 === "number" && match$1 === 34 ? (seen_default ? error$1(env, /* MultipleDefaultsInSwitch */19) : undefined, token$4(env, /* T_DEFAULT */34), undefined) : (token$4(env, /* T_CASE */31), Curry._1(Parse.expression, env));
    var seen_default$1 = seen_default || test === undefined;
    var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    token$4(env, /* T_COLON */77);
    var term_fn = function (param) {
      if (typeof param !== "number") {
        return false;
      }
      var switcher = param - 2 | 0;
      if (switcher > 29 || switcher < 0) {
        return switcher === 32;
      } else {
        return switcher > 28 || switcher < 1;
      }
    };
    var consequent = Curry._2(Parse.statement_list, term_fn, with_in_switch(true, env));
    var match$2 = List.rev(consequent);
    var end_loc$1 = match$2 ? match$2.hd[0] : end_loc;
    var acc_0 = [
      btwn(start_loc, end_loc$1),
      {
        test: test,
        consequent: consequent
      }
    ];
    var acc$1 = {
      hd: acc_0,
      tl: acc
    };
    _param = [
      seen_default$1,
      acc$1
    ];
    continue ;
  };
}

function var_or_const(env) {
  var match = variable(env);
  var match$1 = match[0];
  var start_loc = match$1[0];
  var end_loc = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
  var end_loc$1 = end_loc !== undefined ? end_loc : start_loc;
  semicolon(env);
  List.iter((function (param) {
          return error_at(env, param);
        }), match[1]);
  return [
          btwn(start_loc, end_loc$1),
          match$1[1]
        ];
}

function source(env) {
  contextual(env, "from");
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match !== "number" && match.TAG === /* T_STRING */1) {
    var match$1 = match._0;
    var octal = match$1[3];
    var raw = match$1[2];
    var value = match$1[1];
    var loc = match$1[0];
    if (octal) {
      strict_error(env, /* StrictOctalLiteral */31);
    }
    token$4(env, {
          TAG: /* T_STRING */1,
          _0: [
            loc,
            value,
            raw,
            octal
          ]
        });
    var value$1 = {
      TAG: /* String */0,
      _0: value
    };
    return [
            loc,
            {
              value: value$1,
              raw: raw
            }
          ];
  }
  var raw$1 = Curry._2(Parser_env_Peek.value, undefined, env);
  var value$2 = {
    TAG: /* String */0,
    _0: raw$1
  };
  var ret_0 = Curry._2(Parser_env_Peek.loc, undefined, env);
  var ret_1 = {
    value: value$2,
    raw: raw$1
  };
  var ret = [
    ret_0,
    ret_1
  ];
  error_unexpected(env);
  return ret;
}

function specifier_list(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      if (match === 2) {
        return List.rev(acc);
      }
      if (match === 105) {
        return List.rev(acc);
      }
      
    }
    var match$1 = Curry._1(Parse.identifier_or_reserved_keyword, env);
    var err = match$1[1];
    var remote = match$1[0];
    var specifier;
    if (Curry._2(Parser_env_Peek.value, undefined, env) === "as") {
      contextual(env, "as");
      var local = Curry._2(Parse.identifier, undefined, env);
      specifier = {
        TAG: /* ImportNamedSpecifier */0,
        _0: {
          local: local,
          remote: remote
        }
      };
    } else {
      if (err !== undefined) {
        error_at(env, err);
      }
      specifier = {
        TAG: /* ImportNamedSpecifier */0,
        _0: {
          local: undefined,
          remote: remote
        }
      };
    }
    if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_COMMA */8) {
      token$4(env, /* T_COMMA */8);
    }
    _acc = {
      hd: specifier,
      tl: acc
    };
    continue ;
  };
}

function named_or_namespace_specifier(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "number" && match === 97) {
    token$4(env, /* T_MULT */97);
    contextual(env, "as");
    var id = Curry._2(Parse.identifier, undefined, env);
    return {
            hd: {
              TAG: /* ImportNamespaceSpecifier */2,
              _0: [
                btwn(start_loc, id[0]),
                id
              ]
            },
            tl: /* [] */0
          };
  }
  token$4(env, /* T_LCURLY */1);
  var specifiers = specifier_list(env, /* [] */0);
  token$4(env, /* T_RCURLY */2);
  return specifiers;
}

function from_expr(env, param) {
  var expr = param[1];
  var loc = param[0];
  if (typeof expr !== "number") {
    switch (expr.TAG | 0) {
      case /* Array */0 :
          var param$1 = [
            loc,
            expr._0
          ];
          var elements = List.map((function (param) {
                  if (param === undefined) {
                    return ;
                  }
                  if (param.TAG === /* Expression */0) {
                    var match = param._0;
                    return {
                            TAG: /* Element */0,
                            _0: Curry._2(Parse.pattern_from_expr, env, [
                                  match[0],
                                  match[1]
                                ])
                          };
                  }
                  var match$1 = param._0;
                  var argument = Curry._2(Parse.pattern_from_expr, env, match$1[1].argument);
                  return {
                          TAG: /* Spread */1,
                          _0: [
                            match$1[0],
                            {
                              argument: argument
                            }
                          ]
                        };
                }), param$1[1].elements);
          return [
                  param$1[0],
                  {
                    TAG: /* Array */1,
                    _0: {
                      elements: elements,
                      typeAnnotation: undefined
                    }
                  }
                ];
      case /* Object */1 :
          var param$2 = [
            loc,
            expr._0
          ];
          var properties = List.map((function (param) {
                  if (param.TAG === /* Property */0) {
                    var match = param._0;
                    var match$1 = match[1];
                    var key = match$1.key;
                    var key$1;
                    switch (key.TAG | 0) {
                      case /* Literal */0 :
                          key$1 = {
                            TAG: /* Literal */0,
                            _0: key._0
                          };
                          break;
                      case /* Identifier */1 :
                          key$1 = {
                            TAG: /* Identifier */1,
                            _0: key._0
                          };
                          break;
                      case /* Computed */2 :
                          key$1 = {
                            TAG: /* Computed */2,
                            _0: key._0
                          };
                          break;
                      
                    }
                    var pattern = Curry._2(Parse.pattern_from_expr, env, match$1.value);
                    return {
                            TAG: /* Property */0,
                            _0: [
                              match[0],
                              {
                                key: key$1,
                                pattern: pattern,
                                shorthand: match$1.shorthand
                              }
                            ]
                          };
                  }
                  var match$2 = param._0;
                  var argument = Curry._2(Parse.pattern_from_expr, env, match$2[1].argument);
                  return {
                          TAG: /* SpreadProperty */1,
                          _0: [
                            match$2[0],
                            {
                              argument: argument
                            }
                          ]
                        };
                }), param$2[1].properties);
          return [
                  param$2[0],
                  {
                    TAG: /* Object */0,
                    _0: {
                      properties: properties,
                      typeAnnotation: undefined
                    }
                  }
                ];
      case /* Assignment */7 :
          var match = expr._0;
          if (match.operator === 0) {
            return [
                    loc,
                    {
                      TAG: /* Assignment */2,
                      _0: {
                        left: match.left,
                        right: match.right
                      }
                    }
                  ];
          }
          break;
      case /* Identifier */18 :
          return [
                  loc,
                  {
                    TAG: /* Identifier */3,
                    _0: expr._0
                  }
                ];
      default:
        
    }
  }
  return [
          loc,
          {
            TAG: /* Expression */4,
            _0: [
              loc,
              expr
            ]
          }
        ];
}

function _object$2(restricted_error) {
  var property = function (env) {
    var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    if (maybe(env, /* T_ELLIPSIS */11)) {
      var argument = pattern$1(env, restricted_error);
      var loc = btwn(start_loc, argument[0]);
      return {
              TAG: /* SpreadProperty */1,
              _0: [
                loc,
                {
                  argument: argument
                }
              ]
            };
    }
    var match = Curry._1(Parse.object_key, env);
    var lit = match[1];
    var key;
    switch (lit.TAG | 0) {
      case /* Literal */0 :
          key = {
            TAG: /* Literal */0,
            _0: lit._0
          };
          break;
      case /* Identifier */1 :
          key = {
            TAG: /* Identifier */1,
            _0: lit._0
          };
          break;
      case /* Computed */2 :
          key = {
            TAG: /* Computed */2,
            _0: lit._0
          };
          break;
      
    }
    var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
    var prop;
    var exit = 0;
    if (typeof match$1 === "number" && match$1 === 77) {
      token$4(env, /* T_COLON */77);
      prop = [
        pattern$1(env, restricted_error),
        false
      ];
    } else {
      exit = 1;
    }
    if (exit === 1) {
      switch (key.TAG | 0) {
        case /* Identifier */1 :
            var id = key._0;
            var pattern_0 = id[0];
            var pattern_1 = {
              TAG: /* Identifier */3,
              _0: id
            };
            var pattern$2 = [
              pattern_0,
              pattern_1
            ];
            prop = [
              pattern$2,
              true
            ];
            break;
        case /* Literal */0 :
        case /* Computed */2 :
            error_unexpected(env);
            prop = undefined;
            break;
        
      }
    }
    if (prop === undefined) {
      return ;
    }
    var pattern$3 = prop[0];
    var match$2 = Curry._2(Parser_env_Peek.token, undefined, env);
    var pattern$4;
    if (typeof match$2 === "number" && match$2 === 75) {
      token$4(env, /* T_ASSIGN */75);
      var $$default = Curry._1(Parse.assignment, env);
      var loc$1 = btwn(pattern$3[0], $$default[0]);
      pattern$4 = [
        loc$1,
        {
          TAG: /* Assignment */2,
          _0: {
            left: pattern$3,
            right: $$default
          }
        }
      ];
    } else {
      pattern$4 = pattern$3;
    }
    var loc$2 = btwn(start_loc, pattern$4[0]);
    return {
            TAG: /* Property */0,
            _0: [
              loc$2,
              {
                key: key,
                pattern: pattern$4,
                shorthand: prop[1]
              }
            ]
          };
  };
  var properties = function (env, _acc) {
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_Peek.token, undefined, env);
      if (typeof match === "number") {
        if (match === 2) {
          return List.rev(acc);
        }
        if (match === 105) {
          return List.rev(acc);
        }
        
      }
      var prop = property(env);
      if (prop !== undefined) {
        if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_RCURLY */2) {
          token$4(env, /* T_COMMA */8);
        }
        _acc = {
          hd: prop,
          tl: acc
        };
        continue ;
      }
      continue ;
    };
  };
  return function (env) {
    var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    token$4(env, /* T_LCURLY */1);
    var properties$1 = properties(env, /* [] */0);
    var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    token$4(env, /* T_RCURLY */2);
    var match;
    if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_COLON */77) {
      var typeAnnotation = wrap(annotation, env);
      match = [
        typeAnnotation[0],
        typeAnnotation
      ];
    } else {
      match = [
        end_loc,
        undefined
      ];
    }
    return [
            btwn(start_loc, match[0]),
            {
              TAG: /* Object */0,
              _0: {
                properties: properties$1,
                typeAnnotation: match[1]
              }
            }
          ];
  };
}

function _array(restricted_error) {
  var elements = function (env, _acc) {
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_Peek.token, undefined, env);
      if (typeof match === "number") {
        if (match === 105) {
          return List.rev(acc);
        }
        if (match < 12) {
          switch (match) {
            case /* T_RBRACKET */6 :
                return List.rev(acc);
            case /* T_COMMA */8 :
                token$4(env, /* T_COMMA */8);
                _acc = {
                  hd: undefined,
                  tl: acc
                };
                continue ;
            case /* T_IDENTIFIER */0 :
            case /* T_LCURLY */1 :
            case /* T_RCURLY */2 :
            case /* T_LPAREN */3 :
            case /* T_RPAREN */4 :
            case /* T_LBRACKET */5 :
            case /* T_SEMICOLON */7 :
            case /* T_PERIOD */9 :
            case /* T_ARROW */10 :
                break;
            case /* T_ELLIPSIS */11 :
                var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_ELLIPSIS */11);
                var argument = pattern$1(env, restricted_error);
                var loc = btwn(start_loc, argument[0]);
                var element = {
                  TAG: /* Spread */1,
                  _0: [
                    loc,
                    {
                      argument: argument
                    }
                  ]
                };
                _acc = {
                  hd: element,
                  tl: acc
                };
                continue ;
            
          }
        }
        
      }
      var pattern$2 = pattern$1(env, restricted_error);
      var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
      var pattern$3;
      if (typeof match$1 === "number" && match$1 === 75) {
        token$4(env, /* T_ASSIGN */75);
        var $$default = Curry._1(Parse.expression, env);
        var loc$1 = btwn(pattern$2[0], $$default[0]);
        pattern$3 = [
          loc$1,
          {
            TAG: /* Assignment */2,
            _0: {
              left: pattern$2,
              right: $$default
            }
          }
        ];
      } else {
        pattern$3 = pattern$2;
      }
      var element$1 = {
        TAG: /* Element */0,
        _0: pattern$3
      };
      if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_RBRACKET */6) {
        token$4(env, /* T_COMMA */8);
      }
      _acc = {
        hd: element$1,
        tl: acc
      };
      continue ;
    };
  };
  return function (env) {
    var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    token$4(env, /* T_LBRACKET */5);
    var elements$1 = elements(env, /* [] */0);
    var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    token$4(env, /* T_RBRACKET */6);
    var match;
    if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_COLON */77) {
      var typeAnnotation = wrap(annotation, env);
      match = [
        typeAnnotation[0],
        typeAnnotation
      ];
    } else {
      match = [
        end_loc,
        undefined
      ];
    }
    return [
            btwn(start_loc, match[0]),
            {
              TAG: /* Array */1,
              _0: {
                elements: elements$1,
                typeAnnotation: match[1]
              }
            }
          ];
  };
}

function pattern$1(env, restricted_error) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "number") {
    if (match === 1) {
      return _object$2(restricted_error)(env);
    }
    if (match === 5) {
      return _array(restricted_error)(env);
    }
    
  }
  var id = Curry._2(Parse.identifier_with_type, env, restricted_error);
  return [
          id[0],
          {
            TAG: /* Identifier */3,
            _0: id
          }
        ];
}

function spread_attribute(env) {
  push_lex_mode(env, /* NORMAL */0);
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_LCURLY */1);
  token$4(env, /* T_ELLIPSIS */11);
  var argument = Curry._1(assignment, env);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_RCURLY */2);
  pop_lex_mode(env);
  return [
          btwn(start_loc, end_loc),
          {
            argument: argument
          }
        ];
}

function expression_container(env) {
  push_lex_mode(env, /* NORMAL */0);
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_LCURLY */1);
  var expression;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_RCURLY */2) {
    var empty_loc = btwn_exclusive(start_loc, Curry._2(Parser_env_Peek.loc, undefined, env));
    expression = {
      TAG: /* EmptyExpression */1,
      _0: empty_loc
    };
  } else {
    expression = {
      TAG: /* Expression */0,
      _0: Curry._1(Parse.expression, env)
    };
  }
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_RCURLY */2);
  pop_lex_mode(env);
  return [
          btwn(start_loc, end_loc),
          {
            expression: expression
          }
        ];
}

function identifier$1(env) {
  var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var name = Curry._2(Parser_env_Peek.value, undefined, env);
  token$4(env, /* T_JSX_IDENTIFIER */106);
  return [
          loc,
          {
            name: name
          }
        ];
}

function member_expression(env, _member) {
  while(true) {
    var member = _member;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match !== "number") {
      return member;
    }
    if (match !== 9) {
      return member;
    }
    var _object = {
      TAG: /* MemberExpression */1,
      _0: member
    };
    token$4(env, /* T_PERIOD */9);
    var property = identifier$1(env);
    var loc = btwn(member[0], property[0]);
    var member_1 = {
      _object: _object,
      property: property
    };
    var member$1 = [
      loc,
      member_1
    ];
    _member = member$1;
    continue ;
  };
}

function name(env) {
  var name$1 = identifier$1(env);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match !== "number") {
    return {
            TAG: /* Identifier */0,
            _0: name$1
          };
  }
  if (match !== 9) {
    if (match !== 77) {
      return {
              TAG: /* Identifier */0,
              _0: name$1
            };
    }
    token$4(env, /* T_COLON */77);
    var name$2 = identifier$1(env);
    var loc = btwn(name$1[0], name$2[0]);
    return {
            TAG: /* NamespacedName */1,
            _0: [
              loc,
              {
                namespace: name$1,
                name: name$2
              }
            ]
          };
  }
  var _object = {
    TAG: /* Identifier */0,
    _0: name$1
  };
  token$4(env, /* T_PERIOD */9);
  var property = identifier$1(env);
  var loc$1 = btwn(name$1[0], property[0]);
  var member_1 = {
    _object: _object,
    property: property
  };
  var member = [
    loc$1,
    member_1
  ];
  return {
          TAG: /* MemberExpression */2,
          _0: member_expression(env, member)
        };
}

function attribute(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var name = identifier$1(env);
  var match;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_COLON */77) {
    token$4(env, /* T_COLON */77);
    var name$1 = identifier$1(env);
    var loc = btwn(name[0], name$1[0]);
    match = [
      loc,
      {
        TAG: /* NamespacedName */1,
        _0: [
          loc,
          {
            namespace: name,
            name: name$1
          }
        ]
      }
    ];
  } else {
    match = [
      name[0],
      {
        TAG: /* Identifier */0,
        _0: name
      }
    ];
  }
  var match$1;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_ASSIGN */75) {
    token$4(env, /* T_ASSIGN */75);
    var token$5 = Curry._2(Parser_env_Peek.token, undefined, env);
    var exit = 0;
    if (typeof token$5 === "number") {
      if (token$5 === /* T_LCURLY */1) {
        var match$2 = expression_container(env);
        var expression_container$1 = match$2[1];
        var loc$1 = match$2[0];
        var match$3 = expression_container$1.expression;
        if (match$3.TAG !== /* Expression */0) {
          error$1(env, /* JSXAttributeValueEmptyExpression */40);
        }
        match$1 = [
          loc$1,
          {
            TAG: /* ExpressionContainer */1,
            _0: loc$1,
            _1: expression_container$1
          }
        ];
      } else {
        exit = 1;
      }
    } else if (token$5.TAG === /* T_JSX_TEXT */4) {
      var match$4 = token$5._0;
      var loc$2 = match$4[0];
      token$4(env, token$5);
      var value = {
        TAG: /* String */0,
        _0: match$4[1]
      };
      match$1 = [
        loc$2,
        {
          TAG: /* Literal */0,
          _0: loc$2,
          _1: {
            value: value,
            raw: match$4[2]
          }
        }
      ];
    } else {
      exit = 1;
    }
    if (exit === 1) {
      error$1(env, /* InvalidJSXAttributeValue */41);
      var loc$3 = Curry._2(Parser_env_Peek.loc, undefined, env);
      match$1 = [
        loc$3,
        {
          TAG: /* Literal */0,
          _0: loc$3,
          _1: {
            value: {
              TAG: /* String */0,
              _0: ""
            },
            raw: ""
          }
        }
      ];
    }
    
  } else {
    match$1 = [
      match[0],
      undefined
    ];
  }
  return [
          btwn(start_loc, match$1[0]),
          {
            name: match[1],
            value: match$1[1]
          }
        ];
}

function attributes(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      if (match >= 91) {
        if (match === 96) {
          return List.rev(acc);
        }
        if (match === 105) {
          return List.rev(acc);
        }
        
      } else if (match !== 1) {
        if (match >= 90) {
          return List.rev(acc);
        }
        
      } else {
        var attribute$1 = {
          TAG: /* SpreadAttribute */1,
          _0: spread_attribute(env)
        };
        _acc = {
          hd: attribute$1,
          tl: acc
        };
        continue ;
      }
    }
    var attribute$2 = {
      TAG: /* Attribute */0,
      _0: attribute(env)
    };
    _acc = {
      hd: attribute$2,
      tl: acc
    };
    continue ;
  };
}

function opening_element_without_lt(env, start_loc) {
  var name$1 = name(env);
  var attributes$1 = attributes(env, /* [] */0);
  var selfClosing = Curry._2(Parser_env_Peek.token, undefined, env) === /* T_DIV */96;
  if (selfClosing) {
    token$4(env, /* T_DIV */96);
  }
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_GREATER_THAN */90);
  pop_lex_mode(env);
  return [
          btwn(start_loc, end_loc),
          {
            name: name$1,
            selfClosing: selfClosing,
            attributes: attributes$1
          }
        ];
}

function closing_element_without_lt(env, start_loc) {
  token$4(env, /* T_DIV */96);
  var name$1 = name(env);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_GREATER_THAN */90);
  double_pop_lex_mode(env);
  return [
          btwn(start_loc, end_loc),
          {
            name: name$1
          }
        ];
}

function child(env) {
  var token$5 = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof token$5 === "number") {
    if (token$5 === /* T_LCURLY */1) {
      var expression_container$1 = expression_container(env);
      return [
              expression_container$1[0],
              {
                TAG: /* ExpressionContainer */1,
                _0: expression_container$1[1]
              }
            ];
    }
    
  } else if (token$5.TAG === /* T_JSX_TEXT */4) {
    var match = token$5._0;
    token$4(env, token$5);
    return [
            match[0],
            {
              TAG: /* Text */2,
              _0: {
                value: match[1],
                raw: match[2]
              }
            }
          ];
  }
  var element$1 = element(env);
  return [
          element$1[0],
          {
            TAG: /* Element */0,
            _0: element$1[1]
          }
        ];
}

function element(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  push_lex_mode(env, /* JSX_TAG */2);
  token$4(env, /* T_LESS_THAN */89);
  return Curry._2(element_without_lt, env, start_loc);
}

function element_or_closing(env) {
  push_lex_mode(env, /* JSX_TAG */2);
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_LESS_THAN */89);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "number" && !(match !== 96 && match !== 105)) {
    return {
            TAG: /* Closing */0,
            _0: closing_element_without_lt(env, start_loc)
          };
  } else {
    return {
            TAG: /* ChildElement */1,
            _0: Curry._2(element_without_lt, env, start_loc)
          };
  }
}

function children_and_closing(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "number") {
      if (match !== 89) {
        if (match !== 105) {
          _acc = {
            hd: child(env),
            tl: acc
          };
          continue ;
        }
        error_unexpected(env);
        return [
                List.rev(acc),
                undefined
              ];
      }
      var closingElement = element_or_closing(env);
      if (closingElement.TAG === /* Closing */0) {
        return [
                List.rev(acc),
                closingElement._0
              ];
      }
      var element = closingElement._0;
      var element_0 = element[0];
      var element_1 = {
        TAG: /* Element */0,
        _0: element[1]
      };
      var element$1 = [
        element_0,
        element_1
      ];
      _acc = {
        hd: element$1,
        tl: acc
      };
      continue ;
    }
    _acc = {
      hd: child(env),
      tl: acc
    };
    continue ;
  };
}

function normalize(name) {
  switch (name.TAG | 0) {
    case /* Identifier */0 :
        return name._0[1].name;
    case /* NamespacedName */1 :
        var match = name._0[1];
        return match.namespace[1].name + (":" + match.name[1].name);
    case /* MemberExpression */2 :
        var match$1 = name._0[1];
        var _object = match$1._object;
        var _object$1;
        _object$1 = _object.TAG === /* Identifier */0 ? _object._0[1].name : normalize({
                TAG: /* MemberExpression */2,
                _0: _object._0
              });
        return _object$1 + ("." + match$1.property[1].name);
    
  }
}

function element_without_lt(env, start_loc) {
  var openingElement = opening_element_without_lt(env, start_loc);
  var match = openingElement[1].selfClosing ? [
      /* [] */0,
      undefined
    ] : (push_lex_mode(env, /* JSX_CHILD */3), children_and_closing(env, /* [] */0));
  var closingElement = match[1];
  var end_loc;
  if (closingElement !== undefined) {
    var opening_name = normalize(openingElement[1].name);
    if (normalize(closingElement[1].name) !== opening_name) {
      error$1(env, {
            TAG: /* ExpectedJSXClosingTag */6,
            _0: opening_name
          });
    }
    end_loc = closingElement[0];
  } else {
    end_loc = openingElement[0];
  }
  return [
          btwn(openingElement[0], end_loc),
          {
            openingElement: openingElement,
            closingElement: closingElement,
            children: match[0]
          }
        ];
}

function statement(env) {
  while(true) {
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    var exit = 0;
    if (typeof match === "number") {
      if (match !== 105) {
        if (match >= 58) {
          exit = 2;
        } else {
          switch (match) {
            case /* T_LCURLY */1 :
                var match$1 = Curry._1(Parse.block_body, env);
                return [
                        match$1[0],
                        {
                          TAG: /* Block */0,
                          _0: match$1[1]
                        }
                      ];
            case /* T_SEMICOLON */7 :
                var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_SEMICOLON */7);
                return [
                        loc,
                        /* Empty */0
                      ];
            case /* T_IF */14 :
                return _if(env);
            case /* T_RETURN */17 :
                if (!env.in_function) {
                  error$1(env, /* IllegalReturn */23);
                }
                var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_RETURN */17);
                var argument = Curry._2(Parser_env_Peek.token, undefined, env) === /* T_SEMICOLON */7 || Curry._1(Parser_env_Peek.is_implicit_semicolon, env) ? undefined : Curry._1(Parse.expression, env);
                var loc$1 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
                var end_loc = loc$1 !== undefined ? loc$1 : (
                    argument !== undefined ? argument[0] : start_loc
                  );
                semicolon(env);
                return [
                        btwn(start_loc, end_loc),
                        {
                          TAG: /* Return */9,
                          _0: {
                            argument: argument
                          }
                        }
                      ];
            case /* T_SWITCH */18 :
                var start_loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_SWITCH */18);
                token$4(env, /* T_LPAREN */3);
                var discriminant = Curry._1(Parse.expression, env);
                token$4(env, /* T_RPAREN */4);
                token$4(env, /* T_LCURLY */1);
                var cases = case_list(env, [
                      false,
                      /* [] */0
                    ]);
                var end_loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_RCURLY */2);
                return [
                        btwn(start_loc$1, end_loc$1),
                        {
                          TAG: /* Switch */8,
                          _0: {
                            discriminant: discriminant,
                            cases: cases,
                            lexical: false
                          }
                        }
                      ];
            case /* T_THROW */20 :
                var start_loc$2 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_THROW */20);
                if (Curry._1(Parser_env_Peek.is_line_terminator, env)) {
                  error_at(env, [
                        start_loc$2,
                        /* NewlineAfterThrow */11
                      ]);
                }
                var argument$1 = Curry._1(Parse.expression, env);
                var loc$2 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
                var end_loc$2 = loc$2 !== undefined ? loc$2 : argument$1[0];
                semicolon(env);
                return [
                        btwn(start_loc$2, end_loc$2),
                        {
                          TAG: /* Throw */10,
                          _0: {
                            argument: argument$1
                          }
                        }
                      ];
            case /* T_TRY */21 :
                var start_loc$3 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_TRY */21);
                var block = Curry._1(Parse.block_body, env);
                var match$2 = Curry._2(Parser_env_Peek.token, undefined, env);
                var handler;
                if (typeof match$2 === "number" && match$2 === 32) {
                  var start_loc$4 = Curry._2(Parser_env_Peek.loc, undefined, env);
                  token$4(env, /* T_CATCH */32);
                  token$4(env, /* T_LPAREN */3);
                  var id = Curry._2(Parse.identifier, /* StrictCatchVariable */26, env);
                  var param_0 = id[0];
                  var param_1 = {
                    TAG: /* Identifier */3,
                    _0: id
                  };
                  var param = [
                    param_0,
                    param_1
                  ];
                  token$4(env, /* T_RPAREN */4);
                  var body = Curry._1(Parse.block_body, env);
                  var loc$3 = btwn(start_loc$4, body[0]);
                  handler = [
                    loc$3,
                    {
                      param: param,
                      guard: undefined,
                      body: body
                    }
                  ];
                } else {
                  handler = undefined;
                }
                var match$3 = Curry._2(Parser_env_Peek.token, undefined, env);
                var finalizer = typeof match$3 === "number" && match$3 === 36 ? (token$4(env, /* T_FINALLY */36), Curry._1(Parse.block_body, env)) : undefined;
                var end_loc$3 = finalizer !== undefined ? finalizer[0] : (
                    handler !== undefined ? handler[0] : (error_at(env, [
                              block[0],
                              /* NoCatchOrFinally */20
                            ]), block[0])
                  );
                return [
                        btwn(start_loc$3, end_loc$3),
                        {
                          TAG: /* Try */11,
                          _0: {
                            block: block,
                            handler: handler,
                            guardedHandlers: /* [] */0,
                            finalizer: finalizer
                          }
                        }
                      ];
            case /* T_VAR */22 :
                return var_or_const(env);
            case /* T_WHILE */23 :
                var start_loc$5 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_WHILE */23);
                token$4(env, /* T_LPAREN */3);
                var test = Curry._1(Parse.expression, env);
                token$4(env, /* T_RPAREN */4);
                var body$1 = Curry._1(Parse.statement, with_in_loop(true, env));
                return [
                        btwn(start_loc$5, body$1[0]),
                        {
                          TAG: /* While */12,
                          _0: {
                            test: test,
                            body: body$1
                          }
                        }
                      ];
            case /* T_WITH */24 :
                var start_loc$6 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_WITH */24);
                token$4(env, /* T_LPAREN */3);
                var _object = Curry._1(Parse.expression, env);
                token$4(env, /* T_RPAREN */4);
                var body$2 = Curry._1(Parse.statement, env);
                var loc$4 = btwn(start_loc$6, body$2[0]);
                strict_error_at(env, [
                      loc$4,
                      /* StrictModeWith */25
                    ]);
                return [
                        loc$4,
                        {
                          TAG: /* With */6,
                          _0: {
                            _object: _object,
                            body: body$2
                          }
                        }
                      ];
            case /* T_BREAK */30 :
                var start_loc$7 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_BREAK */30);
                var label;
                if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_SEMICOLON */7 || Curry._1(Parser_env_Peek.is_implicit_semicolon, env)) {
                  label = undefined;
                } else {
                  var label$1 = Curry._2(Parse.identifier, undefined, env);
                  var name = label$1[1].name;
                  if (!mem$1(name, env.labels)) {
                    error$1(env, {
                          TAG: /* UnknownLabel */4,
                          _0: name
                        });
                  }
                  label = label$1;
                }
                var loc$5 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
                var end_loc$4 = loc$5 !== undefined ? loc$5 : (
                    label !== undefined ? label[0] : start_loc$7
                  );
                var loc$6 = btwn(start_loc$7, end_loc$4);
                if (label === undefined && !(env.in_loop || env.in_switch)) {
                  error_at(env, [
                        loc$6,
                        /* IllegalBreak */22
                      ]);
                }
                semicolon(env);
                return [
                        loc$6,
                        {
                          TAG: /* Break */4,
                          _0: {
                            label: label
                          }
                        }
                      ];
            case /* T_CONTINUE */33 :
                var start_loc$8 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_CONTINUE */33);
                var label$2;
                if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_SEMICOLON */7 || Curry._1(Parser_env_Peek.is_implicit_semicolon, env)) {
                  label$2 = undefined;
                } else {
                  var label$3 = Curry._2(Parse.identifier, undefined, env);
                  var name$1 = label$3[1].name;
                  if (!mem$1(name$1, env.labels)) {
                    error$1(env, {
                          TAG: /* UnknownLabel */4,
                          _0: name$1
                        });
                  }
                  label$2 = label$3;
                }
                var loc$7 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
                var end_loc$5 = loc$7 !== undefined ? loc$7 : (
                    label$2 !== undefined ? label$2[0] : start_loc$8
                  );
                var loc$8 = btwn(start_loc$8, end_loc$5);
                if (!env.in_loop) {
                  error_at(env, [
                        loc$8,
                        /* IllegalContinue */21
                      ]);
                }
                semicolon(env);
                return [
                        loc$8,
                        {
                          TAG: /* Continue */5,
                          _0: {
                            label: label$2
                          }
                        }
                      ];
            case /* T_DO */35 :
                var start_loc$9 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_DO */35);
                var body$3 = Curry._1(Parse.statement, with_in_loop(true, env));
                token$4(env, /* T_WHILE */23);
                token$4(env, /* T_LPAREN */3);
                var test$1 = Curry._1(Parse.expression, env);
                var end_loc$6 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_RPAREN */4);
                var loc$9 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
                var end_loc$7 = loc$9 !== undefined ? loc$9 : end_loc$6;
                if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_SEMICOLON */7) {
                  semicolon(env);
                }
                return [
                        btwn(start_loc$9, end_loc$7),
                        {
                          TAG: /* DoWhile */13,
                          _0: {
                            body: body$3,
                            test: test$1
                          }
                        }
                      ];
            case /* T_FOR */37 :
                var start_loc$10 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_FOR */37);
                token$4(env, /* T_LPAREN */3);
                var match$4 = Curry._2(Parser_env_Peek.token, undefined, env);
                var match$5;
                var exit$1 = 0;
                if (typeof match$4 === "number") {
                  if (match$4 >= 22) {
                    if (match$4 >= 27) {
                      exit$1 = 1;
                    } else {
                      switch (match$4 - 22 | 0) {
                        case /* T_IDENTIFIER */0 :
                            var match$6 = declarations(/* T_VAR */22, /* Var */0, with_no_in(true, env));
                            match$5 = [
                              {
                                TAG: /* InitDeclaration */0,
                                _0: match$6[0]
                              },
                              match$6[1]
                            ];
                            break;
                        case /* T_LCURLY */1 :
                        case /* T_RCURLY */2 :
                            exit$1 = 1;
                            break;
                        case /* T_LPAREN */3 :
                            var match$7 = $$const(with_no_in(true, env));
                            match$5 = [
                              {
                                TAG: /* InitDeclaration */0,
                                _0: match$7[0]
                              },
                              match$7[1]
                            ];
                            break;
                        case /* T_RPAREN */4 :
                            var match$8 = _let(with_no_in(true, env));
                            match$5 = [
                              {
                                TAG: /* InitDeclaration */0,
                                _0: match$8[0]
                              },
                              match$8[1]
                            ];
                            break;
                        
                      }
                    }
                  } else if (match$4 !== 7) {
                    exit$1 = 1;
                  } else {
                    match$5 = [
                      undefined,
                      /* [] */0
                    ];
                  }
                } else {
                  exit$1 = 1;
                }
                if (exit$1 === 1) {
                  var expr = Curry._1(Parse.expression, with_no_let(true, with_no_in(true, env)));
                  match$5 = [
                    {
                      TAG: /* InitExpression */1,
                      _0: expr
                    },
                    /* [] */0
                  ];
                }
                var init = match$5[0];
                var match$9 = Curry._2(Parser_env_Peek.token, undefined, env);
                if (typeof match$9 === "number") {
                  if (match$9 !== 15) {
                    if (match$9 === 60) {
                      assert_can_be_forin_or_forof(env, /* InvalidLHSInForOf */17, init);
                      var left;
                      if (init !== undefined) {
                        left = init.TAG === /* InitDeclaration */0 ? ({
                              TAG: /* LeftDeclaration */0,
                              _0: init._0
                            }) : ({
                              TAG: /* LeftExpression */1,
                              _0: init._0
                            });
                      } else {
                        throw {
                              RE_EXN_ID: "Assert_failure",
                              _1: [
                                "parser_flow.ml",
                                2573,
                                22
                              ],
                              Error: new Error()
                            };
                      }
                      token$4(env, /* T_OF */60);
                      var right = Curry._1(Parse.assignment, env);
                      token$4(env, /* T_RPAREN */4);
                      var body$4 = Curry._1(Parse.statement, with_in_loop(true, env));
                      return [
                              btwn(start_loc$10, body$4[0]),
                              {
                                TAG: /* ForOf */16,
                                _0: {
                                  left: left,
                                  right: right,
                                  body: body$4
                                }
                              }
                            ];
                    }
                    
                  } else {
                    assert_can_be_forin_or_forof(env, /* InvalidLHSInForIn */16, init);
                    var left$1;
                    if (init !== undefined) {
                      left$1 = init.TAG === /* InitDeclaration */0 ? ({
                            TAG: /* LeftDeclaration */0,
                            _0: init._0
                          }) : ({
                            TAG: /* LeftExpression */1,
                            _0: init._0
                          });
                    } else {
                      throw {
                            RE_EXN_ID: "Assert_failure",
                            _1: [
                              "parser_flow.ml",
                              2556,
                              22
                            ],
                            Error: new Error()
                          };
                    }
                    token$4(env, /* T_IN */15);
                    var right$1 = Curry._1(Parse.expression, env);
                    token$4(env, /* T_RPAREN */4);
                    var body$5 = Curry._1(Parse.statement, with_in_loop(true, env));
                    return [
                            btwn(start_loc$10, body$5[0]),
                            {
                              TAG: /* ForIn */15,
                              _0: {
                                left: left$1,
                                right: right$1,
                                body: body$5,
                                each: false
                              }
                            }
                          ];
                  }
                }
                List.iter((function (param) {
                        return error_at(env, param);
                      }), match$5[1]);
                token$4(env, /* T_SEMICOLON */7);
                var match$10 = Curry._2(Parser_env_Peek.token, undefined, env);
                var test$2 = typeof match$10 === "number" && match$10 === 7 ? undefined : Curry._1(Parse.expression, env);
                token$4(env, /* T_SEMICOLON */7);
                var match$11 = Curry._2(Parser_env_Peek.token, undefined, env);
                var update = typeof match$11 === "number" && match$11 === 4 ? undefined : Curry._1(Parse.expression, env);
                token$4(env, /* T_RPAREN */4);
                var body$6 = Curry._1(Parse.statement, with_in_loop(true, env));
                return [
                        btwn(start_loc$10, body$6[0]),
                        {
                          TAG: /* For */14,
                          _0: {
                            init: init,
                            test: test$2,
                            update: update,
                            body: body$6
                          }
                        }
                      ];
            case /* T_IDENTIFIER */0 :
            case /* T_RCURLY */2 :
            case /* T_LPAREN */3 :
            case /* T_RPAREN */4 :
            case /* T_LBRACKET */5 :
            case /* T_RBRACKET */6 :
            case /* T_COMMA */8 :
            case /* T_PERIOD */9 :
            case /* T_ARROW */10 :
            case /* T_ELLIPSIS */11 :
            case /* T_AT */12 :
            case /* T_FUNCTION */13 :
            case /* T_IN */15 :
            case /* T_INSTANCEOF */16 :
            case /* T_THIS */19 :
            case /* T_CONST */25 :
            case /* T_LET */26 :
            case /* T_NULL */27 :
            case /* T_FALSE */28 :
            case /* T_TRUE */29 :
            case /* T_CASE */31 :
            case /* T_CATCH */32 :
            case /* T_DEFAULT */34 :
            case /* T_FINALLY */36 :
            case /* T_CLASS */38 :
            case /* T_EXTENDS */39 :
            case /* T_STATIC */40 :
            case /* T_ELSE */41 :
            case /* T_NEW */42 :
            case /* T_DELETE */43 :
            case /* T_TYPEOF */44 :
            case /* T_VOID */45 :
            case /* T_ENUM */46 :
            case /* T_EXPORT */47 :
            case /* T_IMPORT */48 :
            case /* T_SUPER */49 :
            case /* T_IMPLEMENTS */50 :
            case /* T_INTERFACE */51 :
            case /* T_PACKAGE */52 :
            case /* T_PRIVATE */53 :
            case /* T_PROTECTED */54 :
            case /* T_PUBLIC */55 :
            case /* T_YIELD */56 :
                exit = 2;
                break;
            case /* T_DEBUGGER */57 :
                var start_loc$11 = Curry._2(Parser_env_Peek.loc, undefined, env);
                token$4(env, /* T_DEBUGGER */57);
                var loc$10 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
                var end_loc$8 = loc$10 !== undefined ? loc$10 : start_loc$11;
                semicolon(env);
                return [
                        btwn(start_loc$11, end_loc$8),
                        /* Debugger */1
                      ];
            
          }
        }
      } else {
        error_unexpected(env);
        return [
                Curry._2(Parser_env_Peek.loc, undefined, env),
                /* Empty */0
              ];
      }
    } else {
      exit = 2;
    }
    if (exit === 2) {
      if (Curry._2(Parser_env_Peek.is_identifier, undefined, env)) {
        var expr$1 = Curry._1(Parse.expression, env);
        var match$12 = Curry._2(Parser_env_Peek.token, undefined, env);
        var label$4 = expr$1[1];
        var loc$11 = expr$1[0];
        if (typeof label$4 !== "number" && label$4.TAG === /* Identifier */18 && typeof match$12 === "number" && match$12 === 77) {
          var label$5 = label$4._0;
          var match$13 = label$5[1];
          var name$2 = match$13.name;
          token$4(env, /* T_COLON */77);
          if (mem$1(name$2, env.labels)) {
            error_at(env, [
                  loc$11,
                  {
                    TAG: /* Redeclaration */5,
                    _0: "Label",
                    _1: name$2
                  }
                ]);
          }
          var env$1 = add_label(env, name$2);
          var labeled_stmt = Curry._1(Parse.statement, env$1);
          return [
                  btwn(loc$11, labeled_stmt[0]),
                  {
                    TAG: /* Labeled */3,
                    _0: {
                      label: label$5,
                      body: labeled_stmt
                    }
                  }
                ];
        }
        var loc$12 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
        var end_loc$9 = loc$12 !== undefined ? loc$12 : expr$1[0];
        semicolon(env);
        return [
                btwn(expr$1[0], end_loc$9),
                {
                  TAG: /* Expression */1,
                  _0: {
                    expression: expr$1
                  }
                }
              ];
      }
      if (typeof match !== "number") {
        return expression(env);
      }
      if (match !== 77) {
        if (match >= 49) {
          return expression(env);
        }
        switch (match) {
          case /* T_ELSE */41 :
              return _if(env);
          case /* T_IDENTIFIER */0 :
          case /* T_LCURLY */1 :
          case /* T_LPAREN */3 :
          case /* T_LBRACKET */5 :
          case /* T_SEMICOLON */7 :
          case /* T_AT */12 :
          case /* T_FUNCTION */13 :
          case /* T_IF */14 :
          case /* T_RETURN */17 :
          case /* T_SWITCH */18 :
          case /* T_THIS */19 :
          case /* T_THROW */20 :
          case /* T_TRY */21 :
          case /* T_VAR */22 :
          case /* T_WHILE */23 :
          case /* T_WITH */24 :
          case /* T_CONST */25 :
          case /* T_LET */26 :
          case /* T_NULL */27 :
          case /* T_FALSE */28 :
          case /* T_TRUE */29 :
          case /* T_BREAK */30 :
          case /* T_CONTINUE */33 :
          case /* T_DO */35 :
          case /* T_FOR */37 :
          case /* T_CLASS */38 :
          case /* T_NEW */42 :
          case /* T_DELETE */43 :
          case /* T_TYPEOF */44 :
          case /* T_VOID */45 :
          case /* T_ENUM */46 :
              return expression(env);
          case /* T_RCURLY */2 :
          case /* T_RPAREN */4 :
          case /* T_RBRACKET */6 :
          case /* T_COMMA */8 :
          case /* T_PERIOD */9 :
          case /* T_ARROW */10 :
          case /* T_ELLIPSIS */11 :
          case /* T_IN */15 :
          case /* T_INSTANCEOF */16 :
          case /* T_CASE */31 :
          case /* T_CATCH */32 :
          case /* T_DEFAULT */34 :
          case /* T_FINALLY */36 :
          case /* T_EXTENDS */39 :
          case /* T_STATIC */40 :
          case /* T_EXPORT */47 :
          case /* T_IMPORT */48 :
              break;
          
        }
      }
      
    }
    error_unexpected(env);
    token$3(env);
    continue ;
  };
}

function module_item(env) {
  var decorators = decorator_list(env);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match !== "number") {
    return statement_list_item(decorators, env);
  }
  switch (match) {
    case /* T_EXPORT */47 :
        var env$1 = with_in_export(true, with_strict(true, env));
        var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
        token$4(env$1, /* T_EXPORT */47);
        var match$1 = Curry._2(Parser_env_Peek.token, undefined, env$1);
        var exit = 0;
        if (typeof match$1 === "number") {
          if (match$1 >= 51) {
            if (match$1 !== 97) {
              if (match$1 >= 62) {
                exit = 1;
              } else {
                switch (match$1 - 51 | 0) {
                  case /* T_IDENTIFIER */0 :
                      if (!env$1.parse_options.types) {
                        error$1(env$1, /* UnexpectedTypeExport */9);
                      }
                      var $$interface$1 = $$interface(env$1);
                      var match$2 = $$interface$1[1];
                      if (typeof match$2 === "number") {
                        throw {
                              RE_EXN_ID: "Failure",
                              _1: "Internal Flow Error! Parsed `export interface` into something other than an interface declaration!",
                              Error: new Error()
                            };
                      }
                      if (match$2.TAG === /* InterfaceDeclaration */21) {
                        record_export(env$1, [
                              $$interface$1[0],
                              extract_ident_name(match$2._0.id)
                            ]);
                      } else {
                        throw {
                              RE_EXN_ID: "Failure",
                              _1: "Internal Flow Error! Parsed `export interface` into something other than an interface declaration!",
                              Error: new Error()
                            };
                      }
                      var end_loc = $$interface$1[0];
                      return [
                              btwn(start_loc, end_loc),
                              {
                                TAG: /* ExportDeclaration */28,
                                _0: {
                                  default: false,
                                  declaration: {
                                    TAG: /* Declaration */0,
                                    _0: $$interface$1
                                  },
                                  specifiers: undefined,
                                  source: undefined,
                                  exportKind: /* ExportType */0
                                }
                              }
                            ];
                  case /* T_COMMA */8 :
                      if (Curry._2(Parser_env_Peek.token, 1, env$1) !== /* T_LCURLY */1) {
                        if (!env$1.parse_options.types) {
                          error$1(env$1, /* UnexpectedTypeExport */9);
                        }
                        var type_alias$1 = type_alias(env$1);
                        var match$3 = type_alias$1[1];
                        if (typeof match$3 === "number") {
                          throw {
                                RE_EXN_ID: "Failure",
                                _1: "Internal Flow Error! Parsed `export type` into something other than a type alias!",
                                Error: new Error()
                              };
                        }
                        if (match$3.TAG === /* TypeAlias */7) {
                          record_export(env$1, [
                                type_alias$1[0],
                                extract_ident_name(match$3._0.id)
                              ]);
                        } else {
                          throw {
                                RE_EXN_ID: "Failure",
                                _1: "Internal Flow Error! Parsed `export type` into something other than a type alias!",
                                Error: new Error()
                              };
                        }
                        var end_loc$1 = type_alias$1[0];
                        return [
                                btwn(start_loc, end_loc$1),
                                {
                                  TAG: /* ExportDeclaration */28,
                                  _0: {
                                    default: false,
                                    declaration: {
                                      TAG: /* Declaration */0,
                                      _0: type_alias$1
                                    },
                                    specifiers: undefined,
                                    source: undefined,
                                    exportKind: /* ExportType */0
                                  }
                                }
                              ];
                      }
                      exit = 1;
                      break;
                  case /* T_LCURLY */1 :
                  case /* T_RCURLY */2 :
                  case /* T_LPAREN */3 :
                  case /* T_RPAREN */4 :
                  case /* T_LBRACKET */5 :
                  case /* T_RBRACKET */6 :
                  case /* T_SEMICOLON */7 :
                  case /* T_PERIOD */9 :
                      exit = 1;
                      break;
                  case /* T_ARROW */10 :
                      exit = 2;
                      break;
                  
                }
              }
            } else {
              var loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
              token$4(env$1, /* T_MULT */97);
              var parse_export_star_as = env$1.parse_options.esproposal_export_star_as;
              var local_name = Curry._2(Parser_env_Peek.value, undefined, env$1) === "as" ? (contextual(env$1, "as"), parse_export_star_as ? Curry._2(Parse.identifier, undefined, env$1) : (error$1(env$1, /* UnexpectedTypeDeclaration */7), undefined)) : undefined;
              var specifiers = {
                TAG: /* ExportBatchSpecifier */1,
                _0: loc,
                _1: local_name
              };
              var source$1 = export_source(env$1);
              var loc$1 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
              var end_loc$2 = loc$1 !== undefined ? loc$1 : source$1[0];
              var source$2 = source$1;
              semicolon(env$1);
              return [
                      btwn(start_loc, end_loc$2),
                      {
                        TAG: /* ExportDeclaration */28,
                        _0: {
                          default: false,
                          declaration: undefined,
                          specifiers: specifiers,
                          source: source$2,
                          exportKind: /* ExportValue */1
                        }
                      }
                    ];
            }
          } else {
            switch (match$1) {
              case /* T_DEFAULT */34 :
                  token$4(env$1, /* T_DEFAULT */34);
                  record_export(env$1, [
                        btwn(start_loc, Curry._2(Parser_env_Peek.loc, undefined, env$1)),
                        "default"
                      ]);
                  var match$4 = Curry._2(Parser_env_Peek.token, undefined, env$1);
                  var match$5;
                  var exit$1 = 0;
                  if (typeof match$4 === "number" && match$4 === 13) {
                    var fn = _function(env$1);
                    match$5 = [
                      fn[0],
                      {
                        TAG: /* Declaration */0,
                        _0: fn
                      }
                    ];
                  } else {
                    exit$1 = 3;
                  }
                  if (exit$1 === 3) {
                    if (Curry._2(Parser_env_Peek.is_class, undefined, env$1)) {
                      var _class = class_declaration(env$1, decorators);
                      match$5 = [
                        _class[0],
                        {
                          TAG: /* Declaration */0,
                          _0: _class
                        }
                      ];
                    } else {
                      var expr = Curry._1(Parse.assignment, env$1);
                      var loc$2 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
                      var end_loc$3 = loc$2 !== undefined ? loc$2 : expr[0];
                      semicolon(env$1);
                      match$5 = [
                        end_loc$3,
                        {
                          TAG: /* Expression */1,
                          _0: expr
                        }
                      ];
                    }
                  }
                  return [
                          btwn(start_loc, match$5[0]),
                          {
                            TAG: /* ExportDeclaration */28,
                            _0: {
                              default: true,
                              declaration: match$5[1],
                              specifiers: undefined,
                              source: undefined,
                              exportKind: /* ExportValue */1
                            }
                          }
                        ];
              case /* T_IF */14 :
              case /* T_IN */15 :
              case /* T_INSTANCEOF */16 :
              case /* T_RETURN */17 :
              case /* T_SWITCH */18 :
              case /* T_THIS */19 :
              case /* T_THROW */20 :
              case /* T_TRY */21 :
              case /* T_WHILE */23 :
              case /* T_WITH */24 :
              case /* T_NULL */27 :
              case /* T_FALSE */28 :
              case /* T_TRUE */29 :
              case /* T_BREAK */30 :
              case /* T_CASE */31 :
              case /* T_CATCH */32 :
              case /* T_CONTINUE */33 :
              case /* T_DO */35 :
              case /* T_FINALLY */36 :
              case /* T_FOR */37 :
                  exit = 1;
                  break;
              case /* T_AT */12 :
              case /* T_FUNCTION */13 :
              case /* T_VAR */22 :
              case /* T_CONST */25 :
              case /* T_LET */26 :
              case /* T_CLASS */38 :
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
              var match$6 = Curry._2(Parser_env_Peek.token, undefined, env$1);
              var exportKind = typeof match$6 === "number" && match$6 === 59 ? (token$3(env$1), /* ExportType */0) : /* ExportValue */1;
              token$4(env$1, /* T_LCURLY */1);
              var match$7 = export_specifiers_and_errs(env$1, /* [] */0, /* [] */0);
              var specifiers$1 = {
                TAG: /* ExportSpecifiers */0,
                _0: match$7[0]
              };
              var end_loc$4 = Curry._2(Parser_env_Peek.loc, undefined, env$1);
              token$4(env$1, /* T_RCURLY */2);
              var source$3 = Curry._2(Parser_env_Peek.value, undefined, env$1) === "from" ? export_source(env$1) : (List.iter((function (param) {
                          return error_at(env$1, param);
                        }), match$7[1]), undefined);
              var loc$3 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
              var end_loc$5 = loc$3 !== undefined ? loc$3 : (
                  source$3 !== undefined ? source$3[0] : end_loc$4
                );
              semicolon(env$1);
              return [
                      btwn(start_loc, end_loc$5),
                      {
                        TAG: /* ExportDeclaration */28,
                        _0: {
                          default: false,
                          declaration: undefined,
                          specifiers: specifiers$1,
                          source: source$3,
                          exportKind: exportKind
                        }
                      }
                    ];
          case 2 :
              var stmt = Curry._2(Parse.statement_list_item, decorators, env$1);
              var match$8 = stmt[1];
              var loc$4 = stmt[0];
              var names;
              if (typeof match$8 === "number") {
                throw {
                      RE_EXN_ID: "Failure",
                      _1: "Internal Flow Error! Unexpected export statement declaration!",
                      Error: new Error()
                    };
              }
              switch (match$8.TAG | 0) {
                case /* FunctionDeclaration */18 :
                    var id = match$8._0.id;
                    if (id !== undefined) {
                      names = {
                        hd: [
                          loc$4,
                          extract_ident_name(id)
                        ],
                        tl: /* [] */0
                      };
                    } else {
                      error_at(env$1, [
                            loc$4,
                            /* ExportNamelessFunction */56
                          ]);
                      names = /* [] */0;
                    }
                    break;
                case /* VariableDeclaration */19 :
                    names = List.fold_left((function (names, param) {
                            var id = param[1].id;
                            var param$1 = {
                              hd: id,
                              tl: /* [] */0
                            };
                            return List.fold_left(fold, names, param$1);
                          }), /* [] */0, match$8._0.declarations);
                    break;
                case /* ClassDeclaration */20 :
                    var id$1 = match$8._0.id;
                    if (id$1 !== undefined) {
                      names = {
                        hd: [
                          loc$4,
                          extract_ident_name(id$1)
                        ],
                        tl: /* [] */0
                      };
                    } else {
                      error_at(env$1, [
                            loc$4,
                            /* ExportNamelessClass */55
                          ]);
                      names = /* [] */0;
                    }
                    break;
                default:
                  throw {
                        RE_EXN_ID: "Failure",
                        _1: "Internal Flow Error! Unexpected export statement declaration!",
                        Error: new Error()
                      };
              }
              List.iter((function (param) {
                      return record_export(env$1, param);
                    }), names);
              var declaration = {
                TAG: /* Declaration */0,
                _0: stmt
              };
              return [
                      btwn(start_loc, stmt[0]),
                      {
                        TAG: /* ExportDeclaration */28,
                        _0: {
                          default: false,
                          declaration: declaration,
                          specifiers: undefined,
                          source: undefined,
                          exportKind: /* ExportValue */1
                        }
                      }
                    ];
          
        }
    case /* T_IMPORT */48 :
        error_on_decorators(env)(decorators);
        var env$2 = with_strict(true, env);
        var start_loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env$2);
        token$4(env$2, /* T_IMPORT */48);
        var match$9 = Curry._2(Parser_env_Peek.token, undefined, env$2);
        var match$10 = typeof match$9 === "number" ? (
            match$9 !== 44 ? (
                match$9 !== 59 ? [
                    /* ImportValue */2,
                    undefined
                  ] : (!env$2.parse_options.types ? error$1(env$2, /* UnexpectedTypeImport */8) : undefined, [
                      /* ImportType */0,
                      Curry._2(Parse.identifier, undefined, env$2)
                    ])
              ) : (!env$2.parse_options.types ? error$1(env$2, /* UnexpectedTypeImport */8) : undefined, token$4(env$2, /* T_TYPEOF */44), [
                  /* ImportTypeof */1,
                  undefined
                ])
          ) : [
            /* ImportValue */2,
            undefined
          ];
        var type_ident = match$10[1];
        var importKind = match$10[0];
        var match$11 = Curry._2(Parser_env_Peek.token, undefined, env$2);
        var match$12 = Curry._2(Parser_env_Peek.is_identifier, undefined, env$2);
        var exit$2 = 0;
        var exit$3 = 0;
        if (typeof match$11 === "number") {
          if (match$11 === /* T_COMMA */8) {
            exit$2 = 1;
          } else {
            exit$3 = 2;
          }
        } else if (match$11.TAG === /* T_STRING */1) {
          if (importKind === /* ImportValue */2) {
            var match$13 = match$11._0;
            var octal = match$13[3];
            var raw = match$13[2];
            var value = match$13[1];
            var str_loc = match$13[0];
            if (octal) {
              strict_error(env$2, /* StrictOctalLiteral */31);
            }
            token$4(env$2, {
                  TAG: /* T_STRING */1,
                  _0: [
                    str_loc,
                    value,
                    raw,
                    octal
                  ]
                });
            var value$1 = {
              TAG: /* String */0,
              _0: value
            };
            var source_1 = {
              value: value$1,
              raw: raw
            };
            var source$4 = [
              str_loc,
              source_1
            ];
            var loc$5 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$2);
            var end_loc$6 = loc$5 !== undefined ? loc$5 : str_loc;
            semicolon(env$2);
            return [
                    btwn(start_loc$1, end_loc$6),
                    {
                      TAG: /* ImportDeclaration */29,
                      _0: {
                        importKind: importKind,
                        source: source$4,
                        specifiers: /* [] */0
                      }
                    }
                  ];
          }
          exit$3 = 2;
        } else {
          exit$3 = 2;
        }
        if (exit$3 === 2) {
          if (match$12) {
            exit$2 = 1;
          } else {
            var specifiers$2 = named_or_namespace_specifier(env$2);
            var source$5 = source(env$2);
            var loc$6 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$2);
            var end_loc$7 = loc$6 !== undefined ? loc$6 : source$5[0];
            semicolon(env$2);
            return [
                    btwn(start_loc$1, end_loc$7),
                    {
                      TAG: /* ImportDeclaration */29,
                      _0: {
                        importKind: importKind,
                        source: source$5,
                        specifiers: specifiers$2
                      }
                    }
                  ];
          }
        }
        if (exit$2 === 1) {
          var match$14 = Curry._2(Parser_env_Peek.token, undefined, env$2);
          var match$15 = Curry._2(Parser_env_Peek.value, undefined, env$2);
          var match$16;
          var exit$4 = 0;
          if (type_ident !== undefined && typeof match$14 === "number" && !(match$14 !== 8 && (match$14 !== 0 || match$15 !== "from"))) {
            match$16 = [
              /* ImportValue */2,
              {
                TAG: /* ImportDefaultSpecifier */1,
                _0: type_ident
              }
            ];
          } else {
            exit$4 = 2;
          }
          if (exit$4 === 2) {
            match$16 = [
              importKind,
              {
                TAG: /* ImportDefaultSpecifier */1,
                _0: Curry._2(Parse.identifier, undefined, env$2)
              }
            ];
          }
          var match$17 = Curry._2(Parser_env_Peek.token, undefined, env$2);
          var additional_specifiers = typeof match$17 === "number" && match$17 === 8 ? (token$4(env$2, /* T_COMMA */8), named_or_namespace_specifier(env$2)) : /* [] */0;
          var source$6 = source(env$2);
          var loc$7 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$2);
          var end_loc$8 = loc$7 !== undefined ? loc$7 : source$6[0];
          semicolon(env$2);
          return [
                  btwn(start_loc$1, end_loc$8),
                  {
                    TAG: /* ImportDeclaration */29,
                    _0: {
                      importKind: match$16[0],
                      source: source$6,
                      specifiers: {
                        hd: match$16[1],
                        tl: additional_specifiers
                      }
                    }
                  }
                ];
        }
        case /* T_SUPER */49 :
    case /* T_IMPLEMENTS */50 :
    case /* T_INTERFACE */51 :
    case /* T_PACKAGE */52 :
    case /* T_PRIVATE */53 :
    case /* T_PROTECTED */54 :
    case /* T_PUBLIC */55 :
    case /* T_YIELD */56 :
    case /* T_DEBUGGER */57 :
        return statement_list_item(decorators, env);
    case /* T_DECLARE */58 :
        if (Curry._2(Parser_env_Peek.token, 1, env) === /* T_EXPORT */47) {
          error_on_decorators(env)(decorators);
          return declare_export_declaration(undefined, env);
        } else {
          return statement_list_item(decorators, env);
        }
    default:
      return statement_list_item(decorators, env);
  }
}

function statement_list_item(decoratorsOpt, env) {
  var decorators = decoratorsOpt !== undefined ? decoratorsOpt : /* [] */0;
  if (!Curry._2(Parser_env_Peek.is_class, undefined, env)) {
    error_on_decorators(env)(decorators);
  }
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "number") {
    if (match === 25) {
      return var_or_const(env);
    }
    if (match === 26) {
      var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, /* T_LET */26);
      if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_LPAREN */3) {
        token$4(env, /* T_LPAREN */3);
        var match$1 = helper(with_no_let(true, env), /* [] */0, /* [] */0);
        var head = List.map((function (param) {
                var match = param[1];
                return {
                        id: match.id,
                        init: match.init
                      };
              }), match$1[1]);
        token$4(env, /* T_RPAREN */4);
        var body = Curry._1(Parse.statement, env);
        var end_loc = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
        var end_loc$1 = end_loc !== undefined ? end_loc : match$1[0];
        semicolon(env);
        List.iter((function (param) {
                return error_at(env, param);
              }), match$1[2]);
        return [
                btwn(start_loc, end_loc$1),
                {
                  TAG: /* Let */17,
                  _0: {
                    head: head,
                    body: body
                  }
                }
              ];
      }
      var match$2 = helper(with_no_let(true, env), /* [] */0, /* [] */0);
      var declaration = {
        TAG: /* VariableDeclaration */19,
        _0: {
          declarations: match$2[1],
          kind: /* Let */1
        }
      };
      var end_loc$2 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
      var end_loc$3 = end_loc$2 !== undefined ? end_loc$2 : match$2[0];
      semicolon(env);
      List.iter((function (param) {
              return error_at(env, param);
            }), match$2[2]);
      return [
              btwn(start_loc, end_loc$3),
              declaration
            ];
    }
    
  }
  if (Curry._2(Parser_env_Peek.is_function, undefined, env)) {
    return _function(env);
  }
  if (Curry._2(Parser_env_Peek.is_class, undefined, env)) {
    return class_declaration$1(env, decorators);
  }
  if (typeof match !== "number") {
    return statement(env);
  }
  switch (match) {
    case /* T_INTERFACE */51 :
        return $$interface(env);
    case /* T_PACKAGE */52 :
    case /* T_PRIVATE */53 :
    case /* T_PROTECTED */54 :
    case /* T_PUBLIC */55 :
    case /* T_YIELD */56 :
    case /* T_DEBUGGER */57 :
        return statement(env);
    case /* T_DECLARE */58 :
        return declare(undefined, env);
    case /* T_TYPE */59 :
        return type_alias(env);
    default:
      return statement(env);
  }
}

var class_declaration$1 = class_declaration;

function statement_list(_env, term_fn, item_fn, _param) {
  while(true) {
    var param = _param;
    var env = _env;
    var stmts = param[1];
    var string_tokens = param[0];
    var t = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof t === "number" && t === 105) {
      return [
              env,
              string_tokens,
              stmts
            ];
    }
    if (Curry._1(term_fn, t)) {
      return [
              env,
              string_tokens,
              stmts
            ];
    }
    var string_token_0 = Curry._2(Parser_env_Peek.loc, undefined, env);
    var string_token_1 = Curry._2(Parser_env_Peek.token, undefined, env);
    var string_token = [
      string_token_0,
      string_token_1
    ];
    var possible_directive = Curry._1(item_fn, env);
    var stmts$1 = {
      hd: possible_directive,
      tl: stmts
    };
    var match = possible_directive[1];
    if (typeof match === "number") {
      return [
              env,
              string_tokens,
              stmts$1
            ];
    }
    if (match.TAG !== /* Expression */1) {
      return [
              env,
              string_tokens,
              stmts$1
            ];
    }
    var match$1 = match._0.expression;
    var match$2 = match$1[1];
    if (typeof match$2 === "number") {
      return [
              env,
              string_tokens,
              stmts$1
            ];
    }
    if (match$2.TAG !== /* Literal */19) {
      return [
              env,
              string_tokens,
              stmts$1
            ];
    }
    var str = match$2._0.value;
    if (typeof str === "number") {
      return [
              env,
              string_tokens,
              stmts$1
            ];
    }
    if (str.TAG !== /* String */0) {
      return [
              env,
              string_tokens,
              stmts$1
            ];
    }
    var loc = match$1[0];
    var len = loc._end.column - loc.start.column | 0;
    var strict = env.in_strict_mode || str._0 === "use strict" && len === 12;
    var string_tokens$1 = {
      hd: string_token,
      tl: string_tokens
    };
    _param = [
      string_tokens$1,
      stmts$1
    ];
    _env = with_strict(strict, env);
    continue ;
  };
}

function directives(env, term_fn, item_fn) {
  var match = statement_list(env, term_fn, item_fn, [
        /* [] */0,
        /* [] */0
      ]);
  var env$1 = match[0];
  List.iter((function (param) {
          var token = param[1];
          if (typeof token !== "number" && token.TAG === /* T_STRING */1) {
            if (token._0[3]) {
              return strict_error_at(env$1, [
                          param[0],
                          /* StrictOctalLiteral */31
                        ]);
            } else {
              return ;
            }
          }
          var s = "Nooo: " + (token_to_string(token) + "\n");
          throw {
                RE_EXN_ID: "Failure",
                _1: s,
                Error: new Error()
              };
        }), List.rev(match[1]));
  return [
          env$1,
          match[2]
        ];
}

function statement_list$1(term_fn, env) {
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var t = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof t === "number" && t === 105) {
      return List.rev(acc);
    }
    if (Curry._1(term_fn, t)) {
      return List.rev(acc);
    }
    _acc = {
      hd: statement_list_item(undefined, env),
      tl: acc
    };
    continue ;
  };
}

function module_body(term_fn, env) {
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var t = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof t === "number" && t === 105) {
      return List.rev(acc);
    }
    if (Curry._1(term_fn, t)) {
      return List.rev(acc);
    }
    _acc = {
      hd: module_item(env),
      tl: acc
    };
    continue ;
  };
}

function module_body_with_directives(env, term_fn) {
  var match = Curry._3(directives, env, term_fn, module_item);
  var stmts = Curry._2(module_body, term_fn, match[0]);
  return List.fold_left((function (acc, stmt) {
                return {
                        hd: stmt,
                        tl: acc
                      };
              }), stmts, match[1]);
}

function identifier$2(restricted_error, env) {
  var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var name = Curry._2(Parser_env_Peek.value, undefined, env);
  var t = Curry._2(Parser_env_Peek.token, undefined, env);
  var exit = 0;
  if (typeof t === "number" && t === 26) {
    if (env.in_strict_mode) {
      strict_error(env, /* StrictReservedWord */39);
    } else if (env.no_let) {
      error$1(env, {
            TAG: /* UnexpectedToken */1,
            _0: name
          });
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
  if (restricted_error !== undefined && is_restricted(name)) {
    strict_error_at(env, [
          loc,
          restricted_error
        ]);
  }
  return [
          loc,
          {
            name: name,
            typeAnnotation: undefined,
            optional: false
          }
        ];
}

function statement_list_with_directives(term_fn, env) {
  var match = Curry._3(directives, env, term_fn, (function (eta) {
          return statement_list_item(undefined, eta);
        }));
  var env$1 = match[0];
  var stmts = Curry._2(statement_list$1, term_fn, env$1);
  var stmts$1 = List.fold_left((function (acc, stmt) {
          return {
                  hd: stmt,
                  tl: acc
                };
        }), stmts, match[1]);
  return [
          stmts$1,
          env$1.in_strict_mode
        ];
}

function program(env) {
  var stmts = module_body_with_directives(env, (function (param) {
          return false;
        }));
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_EOF */105);
  var loc = stmts ? btwn(List.hd(stmts)[0], List.hd(List.rev(stmts))[0]) : end_loc;
  var comments = List.rev(env.comments.contents);
  return [
          loc,
          stmts,
          comments
        ];
}

function expression$1(env) {
  var expr = Curry._1(assignment, env);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "number" && match === 8) {
    return sequence(env, {
                hd: expr,
                tl: /* [] */0
              });
  } else {
    return expr;
  }
}

function identifier_with_type(env, restricted_error) {
  var match = identifier$2(restricted_error, env);
  var id = match[1];
  var loc = match[0];
  var match$1;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === /* T_PLING */76) {
    if (!env.parse_options.types) {
      error$1(env, /* UnexpectedTypeAnnotation */6);
    }
    var loc$1 = btwn(loc, Curry._2(Parser_env_Peek.loc, undefined, env));
    token$4(env, /* T_PLING */76);
    match$1 = [
      loc$1,
      {
        name: id.name,
        typeAnnotation: id.typeAnnotation,
        optional: true
      }
    ];
  } else {
    match$1 = [
      loc,
      id
    ];
  }
  var id$1 = match$1[1];
  var loc$2 = match$1[0];
  if (Curry._2(Parser_env_Peek.token, undefined, env) !== /* T_COLON */77) {
    return [
            loc$2,
            id$1
          ];
  }
  var typeAnnotation = wrap(annotation, env);
  var loc$3 = btwn(loc$2, typeAnnotation[0]);
  var typeAnnotation$1 = typeAnnotation;
  return [
          loc$3,
          {
            name: id$1.name,
            typeAnnotation: typeAnnotation$1,
            optional: id$1.optional
          }
        ];
}

function block_body(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_LCURLY */1);
  var term_fn = function (t) {
    return t === /* T_RCURLY */2;
  };
  var body = Curry._2(statement_list$1, term_fn, env);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_RCURLY */2);
  return [
          btwn(start_loc, end_loc),
          {
            body: body
          }
        ];
}

function function_block_body(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_LCURLY */1);
  var term_fn = function (t) {
    return t === /* T_RCURLY */2;
  };
  var match = statement_list_with_directives(term_fn, env);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_RCURLY */2);
  return [
          btwn(start_loc, end_loc),
          {
            body: match[0]
          },
          match[1]
        ];
}

function predicate(env) {
  var checks_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (!(Curry._2(Parser_env_Peek.token, undefined, env) === /* T_IDENTIFIER */0 && Curry._2(Parser_env_Peek.value, undefined, env) === "checks")) {
    return ;
  }
  token$4(env, /* T_IDENTIFIER */0);
  if (!maybe(env, /* T_LPAREN */3)) {
    return [
            checks_loc,
            /* Inferred */0
          ];
  }
  var exp = Curry._1(Parse.expression, env);
  var rparen_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, /* T_RPAREN */4);
  var loc = btwn(checks_loc, rparen_loc);
  return [
          loc,
          /* Declared */{
            _0: exp
          }
        ];
}

Caml_module.update_mod({
      TAG: /* Module */0,
      _0: [
        [
          /* Function */0,
          "program"
        ],
        [
          /* Function */0,
          "statement"
        ],
        [
          /* Function */0,
          "statement_list_item"
        ],
        [
          /* Function */0,
          "statement_list"
        ],
        [
          /* Function */0,
          "statement_list_with_directives"
        ],
        [
          /* Function */0,
          "module_body"
        ],
        [
          /* Function */0,
          "expression"
        ],
        [
          /* Function */0,
          "assignment"
        ],
        [
          /* Function */0,
          "object_initializer"
        ],
        [
          /* Function */0,
          "array_initializer"
        ],
        [
          /* Function */0,
          "identifier"
        ],
        [
          /* Function */0,
          "identifier_or_reserved_keyword"
        ],
        [
          /* Function */0,
          "identifier_with_type"
        ],
        [
          /* Function */0,
          "block_body"
        ],
        [
          /* Function */0,
          "function_block_body"
        ],
        [
          /* Function */0,
          "jsx_element"
        ],
        [
          /* Function */0,
          "pattern"
        ],
        [
          /* Function */0,
          "pattern_from_expr"
        ],
        [
          /* Function */0,
          "object_key"
        ],
        [
          /* Function */0,
          "class_declaration"
        ],
        [
          /* Function */0,
          "class_expression"
        ],
        [
          /* Function */0,
          "is_assignable_lhs"
        ],
        [
          /* Function */0,
          "predicate"
        ]
      ]
    }, Parse, {
      program: program,
      statement: statement,
      statement_list_item: statement_list_item,
      statement_list: statement_list$1,
      statement_list_with_directives: statement_list_with_directives,
      module_body: module_body,
      expression: expression$1,
      assignment: assignment,
      object_initializer: _initializer,
      array_initializer: array_initializer,
      identifier: identifier$2,
      identifier_or_reserved_keyword: identifier_or_reserved_keyword,
      identifier_with_type: identifier_with_type,
      block_body: block_body,
      function_block_body: function_block_body,
      jsx_element: element,
      pattern: pattern$1,
      pattern_from_expr: from_expr,
      object_key: key,
      class_declaration: class_declaration$1,
      class_expression: class_expression,
      is_assignable_lhs: is_assignable_lhs,
      predicate: predicate
    });

function program$1(failOpt, token_sinkOpt, parse_optionsOpt, content) {
  var fail = failOpt !== undefined ? failOpt : true;
  var token_sink = token_sinkOpt !== undefined ? Caml_option.valFromOption(token_sinkOpt) : undefined;
  var parse_options = parse_optionsOpt !== undefined ? Caml_option.valFromOption(parse_optionsOpt) : undefined;
  var token_sinkOpt$1 = Caml_option.some(token_sink);
  var parse_optionsOpt$1 = Caml_option.some(parse_options);
  var filename;
  var token_sink$1 = token_sinkOpt$1 !== undefined ? Caml_option.valFromOption(token_sinkOpt$1) : undefined;
  var parse_options$1 = parse_optionsOpt$1 !== undefined ? Caml_option.valFromOption(parse_optionsOpt$1) : undefined;
  var env = init_env(Caml_option.some(token_sink$1), Caml_option.some(parse_options$1), filename, content);
  var parser = Parse.program;
  var ast = Curry._1(parser, env);
  var error_list = filter_duplicate_errors(env.errors.contents);
  if (fail && error_list !== /* [] */0) {
    throw {
          RE_EXN_ID: $$Error,
          _1: error_list,
          Error: new Error()
        };
  }
  return [
          ast,
          error_list
        ];
}

var translation_errors = {
  contents: /* [] */0
};

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
    translation_errors.contents = {
      hd: [
        loc,
        /* InvalidRegExp */12
      ],
      tl: translation_errors.contents
    };
    return new RegExp("", flags);
  }
}

function parse(content, options) {
  try {
    var match = program$1(false, undefined, Caml_option.some(undefined), content);
    translation_errors.contents = /* [] */0;
    var array_of_list = function (fn, list) {
      return array($$Array.of_list(List.map(fn, list)));
    };
    var option = function (f, v) {
      if (v !== undefined) {
        return Curry._1(f, Caml_option.valFromOption(v));
      } else {
        return $$null;
      }
    };
    var position = function (p) {
      return obj([
                  [
                    "line",
                    number$1(p.line)
                  ],
                  [
                    "column",
                    number$1(p.column)
                  ]
                ]);
    };
    var loc = function ($$location) {
      var match = $$location.source;
      var source = match !== undefined ? (
          typeof match === "number" ? string("(global)") : string(match._0)
        ) : $$null;
      return obj([
                  [
                    "source",
                    source
                  ],
                  [
                    "start",
                    position($$location.start)
                  ],
                  [
                    "end",
                    position($$location._end)
                  ]
                ]);
    };
    var range = function ($$location) {
      return array([
                  number$1($$location.start.offset),
                  number$1($$location._end.offset)
                ]);
    };
    var node = function (_type, $$location, props) {
      return obj($$Array.append([
                      [
                        "type",
                        string(_type)
                      ],
                      [
                        "loc",
                        loc($$location)
                      ],
                      [
                        "range",
                        range($$location)
                      ]
                    ], props));
    };
    var errors = function (l) {
      var error$2 = function (param) {
        return obj([
                    [
                      "loc",
                      loc(param[0])
                    ],
                    [
                      "message",
                      string(error(param[1]))
                    ]
                  ]);
      };
      return array_of_list(error$2, l);
    };
    var _type = function (param) {
      var t = param[1];
      var loc = param[0];
      if (typeof t === "number") {
        switch (t) {
          case /* Any */0 :
              return node("AnyTypeAnnotation", loc, []);
          case /* Void */1 :
              return node("VoidTypeAnnotation", loc, []);
          case /* Null */2 :
              return node("NullTypeAnnotation", loc, []);
          case /* Number */3 :
              return node("NumberTypeAnnotation", loc, []);
          case /* String */4 :
              return node("StringTypeAnnotation", loc, []);
          case /* Boolean */5 :
              return node("BooleanTypeAnnotation", loc, []);
          case /* Exists */6 :
              return node("ExistsTypeAnnotation", loc, []);
          
        }
      } else {
        switch (t.TAG | 0) {
          case /* Nullable */0 :
              var t$1 = t._0;
              return node("NullableTypeAnnotation", loc, [[
                            "typeAnnotation",
                            _type(t$1)
                          ]]);
          case /* Function */1 :
              return function_type([
                          loc,
                          t._0
                        ]);
          case /* Object */2 :
              return object_type([
                          loc,
                          t._0
                        ]);
          case /* Array */3 :
              var t$2 = t._0;
              return node("ArrayTypeAnnotation", loc, [[
                            "elementType",
                            _type(t$2)
                          ]]);
          case /* Generic */4 :
              var param$1 = [
                loc,
                t._0
              ];
              var g = param$1[1];
              var id = g.id;
              var id$1;
              id$1 = id.TAG === /* Unqualified */0 ? identifier(id._0) : generic_type_qualified_identifier(id._0);
              return node("GenericTypeAnnotation", param$1[0], [
                          [
                            "id",
                            id$1
                          ],
                          [
                            "typeParameters",
                            option(type_parameter_instantiation, g.typeParameters)
                          ]
                        ]);
          case /* Union */5 :
              var param$2 = [
                loc,
                t._0
              ];
              return node("UnionTypeAnnotation", param$2[0], [[
                            "types",
                            array_of_list(_type, param$2[1])
                          ]]);
          case /* Intersection */6 :
              var param$3 = [
                loc,
                t._0
              ];
              return node("IntersectionTypeAnnotation", param$3[0], [[
                            "types",
                            array_of_list(_type, param$3[1])
                          ]]);
          case /* Typeof */7 :
              var param$4 = [
                loc,
                t._0
              ];
              return node("TypeofTypeAnnotation", param$4[0], [[
                            "argument",
                            _type(param$4[1])
                          ]]);
          case /* Tuple */8 :
              var param$5 = [
                loc,
                t._0
              ];
              return node("TupleTypeAnnotation", param$5[0], [[
                            "types",
                            array_of_list(_type, param$5[1])
                          ]]);
          case /* StringLiteral */9 :
              var param$6 = [
                loc,
                t._0
              ];
              var s = param$6[1];
              return node("StringLiteralTypeAnnotation", param$6[0], [
                          [
                            "value",
                            string(s.value)
                          ],
                          [
                            "raw",
                            string(s.raw)
                          ]
                        ]);
          case /* NumberLiteral */10 :
              var param$7 = [
                loc,
                t._0
              ];
              var s$1 = param$7[1];
              return node("NumberLiteralTypeAnnotation", param$7[0], [
                          [
                            "value",
                            number$1(s$1.value)
                          ],
                          [
                            "raw",
                            string(s$1.raw)
                          ]
                        ]);
          case /* BooleanLiteral */11 :
              var param$8 = [
                loc,
                t._0
              ];
              var s$2 = param$8[1];
              return node("BooleanLiteralTypeAnnotation", param$8[0], [
                          [
                            "value",
                            bool(s$2.value)
                          ],
                          [
                            "raw",
                            string(s$2.raw)
                          ]
                        ]);
          
        }
      }
    };
    var literal = function (param) {
      var lit = param[1];
      var raw = lit.raw;
      var value = lit.value;
      var loc = param[0];
      var value_;
      if (typeof value === "number") {
        value_ = $$null;
      } else {
        switch (value.TAG | 0) {
          case /* String */0 :
              value_ = string(value._0);
              break;
          case /* Boolean */1 :
              value_ = bool(value._0);
              break;
          case /* Number */2 :
              value_ = number$1(value._0);
              break;
          case /* RegExp */3 :
              var match = value._0;
              value_ = regexp$1(loc, match.pattern, match.flags);
              break;
          
        }
      }
      var props;
      var exit = 0;
      if (typeof value === "number" || value.TAG !== /* RegExp */3) {
        exit = 1;
      } else {
        var match$1 = value._0;
        var regex = obj([
              [
                "pattern",
                string(match$1.pattern)
              ],
              [
                "flags",
                string(match$1.flags)
              ]
            ]);
        props = [
          [
            "value",
            value_
          ],
          [
            "raw",
            string(raw)
          ],
          [
            "regex",
            regex
          ]
        ];
      }
      if (exit === 1) {
        props = [
          [
            "value",
            value_
          ],
          [
            "raw",
            string(raw)
          ]
        ];
      }
      return node("Literal", loc, props);
    };
    var identifier = function (param) {
      var id = param[1];
      return node("Identifier", param[0], [
                  [
                    "name",
                    string(id.name)
                  ],
                  [
                    "typeAnnotation",
                    option(type_annotation, id.typeAnnotation)
                  ],
                  [
                    "optional",
                    bool(id.optional)
                  ]
                ]);
    };
    var jsx_opening = function (param) {
      var opening = param[1];
      return node("JSXOpeningElement", param[0], [
                  [
                    "name",
                    jsx_name(opening.name)
                  ],
                  [
                    "attributes",
                    array_of_list(jsx_opening_attribute, opening.attributes)
                  ],
                  [
                    "selfClosing",
                    bool(opening.selfClosing)
                  ]
                ]);
    };
    var jsx_closing = function (param) {
      return node("JSXClosingElement", param[0], [[
                    "name",
                    jsx_name(param[1].name)
                  ]]);
    };
    var jsx_child = function (param) {
      var element = param[1];
      var loc = param[0];
      switch (element.TAG | 0) {
        case /* Element */0 :
            return jsx_element([
                        loc,
                        element._0
                      ]);
        case /* ExpressionContainer */1 :
            return jsx_expression_container([
                        loc,
                        element._0
                      ]);
        case /* Text */2 :
            var param$1 = [
              loc,
              element._0
            ];
            var text = param$1[1];
            return node("JSXText", param$1[0], [
                        [
                          "value",
                          string(text.value)
                        ],
                        [
                          "raw",
                          string(text.raw)
                        ]
                      ]);
        
      }
    };
    var variable_declarator = function (param) {
      var declarator = param[1];
      return node("VariableDeclarator", param[0], [
                  [
                    "id",
                    pattern(declarator.id)
                  ],
                  [
                    "init",
                    option(expression, declarator.init)
                  ]
                ]);
    };
    var type_annotation = function (param) {
      return node("TypeAnnotation", param[0], [[
                    "typeAnnotation",
                    _type(param[1])
                  ]]);
    };
    var class_element = function (m) {
      if (m.TAG === /* Method */0) {
        var param = m._0;
        var method_ = param[1];
        var key = method_.key;
        var match;
        switch (key.TAG | 0) {
          case /* Literal */0 :
              match = [
                literal(key._0),
                false
              ];
              break;
          case /* Identifier */1 :
              match = [
                identifier(key._0),
                false
              ];
              break;
          case /* Computed */2 :
              match = [
                expression(key._0),
                true
              ];
              break;
          
        }
        var kind;
        switch (method_.kind) {
          case /* Constructor */0 :
              kind = "constructor";
              break;
          case /* Method */1 :
              kind = "method";
              break;
          case /* Get */2 :
              kind = "get";
              break;
          case /* Set */3 :
              kind = "set";
              break;
          
        }
        return node("MethodDefinition", param[0], [
                    [
                      "key",
                      match[0]
                    ],
                    [
                      "value",
                      function_expression(method_.value)
                    ],
                    [
                      "kind",
                      string(kind)
                    ],
                    [
                      "static",
                      bool(method_.static)
                    ],
                    [
                      "computed",
                      bool(match[1])
                    ],
                    [
                      "decorators",
                      array_of_list(expression, method_.decorators)
                    ]
                  ]);
      } else {
        var param$1 = m._0;
        var prop = param$1[1];
        var lit = prop.key;
        var match$1;
        switch (lit.TAG | 0) {
          case /* Literal */0 :
              match$1 = [
                literal(lit._0),
                false
              ];
              break;
          case /* Identifier */1 :
              match$1 = [
                identifier(lit._0),
                false
              ];
              break;
          case /* Computed */2 :
              match$1 = [
                expression(lit._0),
                true
              ];
              break;
          
        }
        return node("ClassProperty", param$1[0], [
                    [
                      "key",
                      match$1[0]
                    ],
                    [
                      "value",
                      option(expression, prop.value)
                    ],
                    [
                      "typeAnnotation",
                      option(type_annotation, prop.typeAnnotation)
                    ],
                    [
                      "computed",
                      bool(match$1[1])
                    ],
                    [
                      "static",
                      bool(prop.static)
                    ]
                  ]);
      }
    };
    var expression = function (param) {
      var arr = param[1];
      var loc = param[0];
      if (typeof arr === "number") {
        return node("ThisExpression", loc, []);
      }
      switch (arr.TAG | 0) {
        case /* Array */0 :
            return node("ArrayExpression", loc, [[
                          "elements",
                          array_of_list((function (param) {
                                  return option(expression_or_spread, param);
                                }), arr._0.elements)
                        ]]);
        case /* Object */1 :
            return node("ObjectExpression", loc, [[
                          "properties",
                          array_of_list(object_property, arr._0.properties)
                        ]]);
        case /* Function */2 :
            return function_expression([
                        loc,
                        arr._0
                      ]);
        case /* ArrowFunction */3 :
            var arrow = arr._0;
            var b = arrow.body;
            var body;
            body = b.TAG === /* BodyBlock */0 ? block(b._0) : expression(b._0);
            return node("ArrowFunctionExpression", loc, [
                        [
                          "id",
                          option(identifier, arrow.id)
                        ],
                        [
                          "params",
                          array_of_list(pattern, arrow.params)
                        ],
                        [
                          "defaults",
                          array_of_list((function (param) {
                                  return option(expression, param);
                                }), arrow.defaults)
                        ],
                        [
                          "rest",
                          option(identifier, arrow.rest)
                        ],
                        [
                          "body",
                          body
                        ],
                        [
                          "async",
                          bool(arrow.async)
                        ],
                        [
                          "generator",
                          bool(arrow.generator)
                        ],
                        [
                          "expression",
                          bool(arrow.expression)
                        ],
                        [
                          "returnType",
                          option(type_annotation, arrow.returnType)
                        ],
                        [
                          "typeParameters",
                          option(type_parameter_declaration, arrow.typeParameters)
                        ]
                      ]);
        case /* Sequence */4 :
            return node("SequenceExpression", loc, [[
                          "expressions",
                          array_of_list(expression, arr._0.expressions)
                        ]]);
        case /* Unary */5 :
            var unary = arr._0;
            var match = unary.operator;
            if (match >= 7) {
              return node("AwaitExpression", loc, [[
                            "argument",
                            expression(unary.argument)
                          ]]);
            }
            var match$1 = unary.operator;
            var operator;
            switch (match$1) {
              case /* Minus */0 :
                  operator = "-";
                  break;
              case /* Plus */1 :
                  operator = "+";
                  break;
              case /* Not */2 :
                  operator = "!";
                  break;
              case /* BitNot */3 :
                  operator = "~";
                  break;
              case /* Typeof */4 :
                  operator = "typeof";
                  break;
              case /* Void */5 :
                  operator = "void";
                  break;
              case /* Delete */6 :
                  operator = "delete";
                  break;
              case /* Await */7 :
                  throw {
                        RE_EXN_ID: "Failure",
                        _1: "matched above",
                        Error: new Error()
                      };
              
            }
            return node("UnaryExpression", loc, [
                        [
                          "operator",
                          string(operator)
                        ],
                        [
                          "prefix",
                          bool(unary.prefix)
                        ],
                        [
                          "argument",
                          expression(unary.argument)
                        ]
                      ]);
        case /* Binary */6 :
            var binary = arr._0;
            var match$2 = binary.operator;
            var operator$1;
            switch (match$2) {
              case /* Equal */0 :
                  operator$1 = "==";
                  break;
              case /* NotEqual */1 :
                  operator$1 = "!=";
                  break;
              case /* StrictEqual */2 :
                  operator$1 = "===";
                  break;
              case /* StrictNotEqual */3 :
                  operator$1 = "!==";
                  break;
              case /* LessThan */4 :
                  operator$1 = "<";
                  break;
              case /* LessThanEqual */5 :
                  operator$1 = "<=";
                  break;
              case /* GreaterThan */6 :
                  operator$1 = ">";
                  break;
              case /* GreaterThanEqual */7 :
                  operator$1 = ">=";
                  break;
              case /* LShift */8 :
                  operator$1 = "<<";
                  break;
              case /* RShift */9 :
                  operator$1 = ">>";
                  break;
              case /* RShift3 */10 :
                  operator$1 = ">>>";
                  break;
              case /* Plus */11 :
                  operator$1 = "+";
                  break;
              case /* Minus */12 :
                  operator$1 = "-";
                  break;
              case /* Mult */13 :
                  operator$1 = "*";
                  break;
              case /* Exp */14 :
                  operator$1 = "**";
                  break;
              case /* Div */15 :
                  operator$1 = "/";
                  break;
              case /* Mod */16 :
                  operator$1 = "%";
                  break;
              case /* BitOr */17 :
                  operator$1 = "|";
                  break;
              case /* Xor */18 :
                  operator$1 = "^";
                  break;
              case /* BitAnd */19 :
                  operator$1 = "&";
                  break;
              case /* In */20 :
                  operator$1 = "in";
                  break;
              case /* Instanceof */21 :
                  operator$1 = "instanceof";
                  break;
              
            }
            return node("BinaryExpression", loc, [
                        [
                          "operator",
                          string(operator$1)
                        ],
                        [
                          "left",
                          expression(binary.left)
                        ],
                        [
                          "right",
                          expression(binary.right)
                        ]
                      ]);
        case /* Assignment */7 :
            var assignment = arr._0;
            var match$3 = assignment.operator;
            var operator$2;
            switch (match$3) {
              case /* Assign */0 :
                  operator$2 = "=";
                  break;
              case /* PlusAssign */1 :
                  operator$2 = "+=";
                  break;
              case /* MinusAssign */2 :
                  operator$2 = "-=";
                  break;
              case /* MultAssign */3 :
                  operator$2 = "*=";
                  break;
              case /* ExpAssign */4 :
                  operator$2 = "**=";
                  break;
              case /* DivAssign */5 :
                  operator$2 = "/=";
                  break;
              case /* ModAssign */6 :
                  operator$2 = "%=";
                  break;
              case /* LShiftAssign */7 :
                  operator$2 = "<<=";
                  break;
              case /* RShiftAssign */8 :
                  operator$2 = ">>=";
                  break;
              case /* RShift3Assign */9 :
                  operator$2 = ">>>=";
                  break;
              case /* BitOrAssign */10 :
                  operator$2 = "|=";
                  break;
              case /* BitXorAssign */11 :
                  operator$2 = "^=";
                  break;
              case /* BitAndAssign */12 :
                  operator$2 = "&=";
                  break;
              
            }
            return node("AssignmentExpression", loc, [
                        [
                          "operator",
                          string(operator$2)
                        ],
                        [
                          "left",
                          pattern(assignment.left)
                        ],
                        [
                          "right",
                          expression(assignment.right)
                        ]
                      ]);
        case /* Update */8 :
            var update = arr._0;
            var match$4 = update.operator;
            var operator$3 = match$4 ? "--" : "++";
            return node("UpdateExpression", loc, [
                        [
                          "operator",
                          string(operator$3)
                        ],
                        [
                          "argument",
                          expression(update.argument)
                        ],
                        [
                          "prefix",
                          bool(update.prefix)
                        ]
                      ]);
        case /* Logical */9 :
            var logical = arr._0;
            var match$5 = logical.operator;
            var operator$4 = match$5 ? "&&" : "||";
            return node("LogicalExpression", loc, [
                        [
                          "operator",
                          string(operator$4)
                        ],
                        [
                          "left",
                          expression(logical.left)
                        ],
                        [
                          "right",
                          expression(logical.right)
                        ]
                      ]);
        case /* Conditional */10 :
            var conditional = arr._0;
            return node("ConditionalExpression", loc, [
                        [
                          "test",
                          expression(conditional.test)
                        ],
                        [
                          "consequent",
                          expression(conditional.consequent)
                        ],
                        [
                          "alternate",
                          expression(conditional.alternate)
                        ]
                      ]);
        case /* New */11 :
            var _new = arr._0;
            return node("NewExpression", loc, [
                        [
                          "callee",
                          expression(_new.callee)
                        ],
                        [
                          "arguments",
                          array_of_list(expression_or_spread, _new.arguments)
                        ]
                      ]);
        case /* Call */12 :
            var call = arr._0;
            return node("CallExpression", loc, [
                        [
                          "callee",
                          expression(call.callee)
                        ],
                        [
                          "arguments",
                          array_of_list(expression_or_spread, call.arguments)
                        ]
                      ]);
        case /* Member */13 :
            var member = arr._0;
            var id = member.property;
            var property;
            property = id.TAG === /* PropertyIdentifier */0 ? identifier(id._0) : expression(id._0);
            return node("MemberExpression", loc, [
                        [
                          "object",
                          expression(member._object)
                        ],
                        [
                          "property",
                          property
                        ],
                        [
                          "computed",
                          bool(member.computed)
                        ]
                      ]);
        case /* Yield */14 :
            var $$yield = arr._0;
            return node("YieldExpression", loc, [
                        [
                          "argument",
                          option(expression, $$yield.argument)
                        ],
                        [
                          "delegate",
                          bool($$yield.delegate)
                        ]
                      ]);
        case /* Comprehension */15 :
            var comp = arr._0;
            return node("ComprehensionExpression", loc, [
                        [
                          "blocks",
                          array_of_list(comprehension_block, comp.blocks)
                        ],
                        [
                          "filter",
                          option(expression, comp.filter)
                        ]
                      ]);
        case /* Generator */16 :
            var gen = arr._0;
            return node("GeneratorExpression", loc, [
                        [
                          "blocks",
                          array_of_list(comprehension_block, gen.blocks)
                        ],
                        [
                          "filter",
                          option(expression, gen.filter)
                        ]
                      ]);
        case /* Let */17 :
            var _let = arr._0;
            return node("LetExpression", loc, [
                        [
                          "head",
                          array_of_list(let_assignment, _let.head)
                        ],
                        [
                          "body",
                          expression(_let.body)
                        ]
                      ]);
        case /* Identifier */18 :
            return identifier(arr._0);
        case /* Literal */19 :
            return literal([
                        loc,
                        arr._0
                      ]);
        case /* TemplateLiteral */20 :
            return template_literal([
                        loc,
                        arr._0
                      ]);
        case /* TaggedTemplate */21 :
            var param$1 = [
              loc,
              arr._0
            ];
            var tagged = param$1[1];
            return node("TaggedTemplateExpression", param$1[0], [
                        [
                          "tag",
                          expression(tagged.tag)
                        ],
                        [
                          "quasi",
                          template_literal(tagged.quasi)
                        ]
                      ]);
        case /* JSXElement */22 :
            return jsx_element([
                        loc,
                        arr._0
                      ]);
        case /* Class */23 :
            var param$2 = [
              loc,
              arr._0
            ];
            var c = param$2[1];
            return node("ClassExpression", param$2[0], [
                        [
                          "id",
                          option(identifier, c.id)
                        ],
                        [
                          "body",
                          class_body(c.body)
                        ],
                        [
                          "superClass",
                          option(expression, c.superClass)
                        ],
                        [
                          "typeParameters",
                          option(type_parameter_declaration, c.typeParameters)
                        ],
                        [
                          "superTypeParameters",
                          option(type_parameter_instantiation, c.superTypeParameters)
                        ],
                        [
                          "implements",
                          array_of_list(class_implements, c.implements)
                        ],
                        [
                          "decorators",
                          array_of_list(expression, c.classDecorators)
                        ]
                      ]);
        case /* TypeCast */24 :
            var typecast = arr._0;
            return node("TypeCastExpression", loc, [
                        [
                          "expression",
                          expression(typecast.expression)
                        ],
                        [
                          "typeAnnotation",
                          type_annotation(typecast.typeAnnotation)
                        ]
                      ]);
        
      }
    };
    var template_literal = function (param) {
      var value = param[1];
      return node("TemplateLiteral", param[0], [
                  [
                    "quasis",
                    array_of_list(template_element, value.quasis)
                  ],
                  [
                    "expressions",
                    array_of_list(expression, value.expressions)
                  ]
                ]);
    };
    var object_type = function (param) {
      var o = param[1];
      return node("ObjectTypeAnnotation", param[0], [
                  [
                    "properties",
                    array_of_list(object_type_property, o.properties)
                  ],
                  [
                    "indexers",
                    array_of_list(object_type_indexer, o.indexers)
                  ],
                  [
                    "callProperties",
                    array_of_list(object_type_call_property, o.callProperties)
                  ]
                ]);
    };
    var interface_extends = function (param) {
      var g = param[1];
      var id = g.id;
      var id$1;
      id$1 = id.TAG === /* Unqualified */0 ? identifier(id._0) : generic_type_qualified_identifier(id._0);
      return node("InterfaceExtends", param[0], [
                  [
                    "id",
                    id$1
                  ],
                  [
                    "typeParameters",
                    option(type_parameter_instantiation, g.typeParameters)
                  ]
                ]);
    };
    var type_parameter_declaration = function (param) {
      return node("TypeParameterDeclaration", param[0], [[
                    "params",
                    array_of_list(type_param, param[1].params)
                  ]]);
    };
    var pattern = function (param) {
      var obj = param[1];
      var loc = param[0];
      switch (obj.TAG | 0) {
        case /* Object */0 :
            var obj$1 = obj._0;
            return node("ObjectPattern", loc, [
                        [
                          "properties",
                          array_of_list(object_pattern_property, obj$1.properties)
                        ],
                        [
                          "typeAnnotation",
                          option(type_annotation, obj$1.typeAnnotation)
                        ]
                      ]);
        case /* Array */1 :
            var arr = obj._0;
            return node("ArrayPattern", loc, [
                        [
                          "elements",
                          array_of_list((function (param) {
                                  return option(array_pattern_element, param);
                                }), arr.elements)
                        ],
                        [
                          "typeAnnotation",
                          option(type_annotation, arr.typeAnnotation)
                        ]
                      ]);
        case /* Assignment */2 :
            var match = obj._0;
            return node("AssignmentPattern", loc, [
                        [
                          "left",
                          pattern(match.left)
                        ],
                        [
                          "right",
                          expression(match.right)
                        ]
                      ]);
        case /* Identifier */3 :
            return identifier(obj._0);
        case /* Expression */4 :
            return expression(obj._0);
        
      }
    };
    var template_element = function (param) {
      var element = param[1];
      var value = obj([
            [
              "raw",
              string(element.value.raw)
            ],
            [
              "cooked",
              string(element.value.cooked)
            ]
          ]);
      return node("TemplateElement", param[0], [
                  [
                    "value",
                    value
                  ],
                  [
                    "tail",
                    bool(element.tail)
                  ]
                ]);
    };
    var export_specifier = function (param) {
      var specifier = param[1];
      return node("ExportSpecifier", param[0], [
                  [
                    "id",
                    identifier(specifier.id)
                  ],
                  [
                    "name",
                    option(identifier, specifier.name)
                  ]
                ]);
    };
    var function_type_param = function (param) {
      var param$1 = param[1];
      return node("FunctionTypeParam", param[0], [
                  [
                    "name",
                    identifier(param$1.name)
                  ],
                  [
                    "typeAnnotation",
                    _type(param$1.typeAnnotation)
                  ],
                  [
                    "optional",
                    bool(param$1.optional)
                  ]
                ]);
    };
    var jsx_name = function (id) {
      switch (id.TAG | 0) {
        case /* Identifier */0 :
            return jsx_identifier(id._0);
        case /* NamespacedName */1 :
            return jsx_namespaced_name(id._0);
        case /* MemberExpression */2 :
            return jsx_member_expression(id._0);
        
      }
    };
    var jsx_expression_container = function (param) {
      var expr = param[1].expression;
      var expression$1;
      expression$1 = expr.TAG === /* Expression */0 ? expression(expr._0) : node("JSXEmptyExpression", expr._0, []);
      return node("JSXExpressionContainer", param[0], [[
                    "expression",
                    expression$1
                  ]]);
    };
    var jsx_opening_attribute = function (attribute) {
      if (attribute.TAG === /* Attribute */0) {
        var param = attribute._0;
        var attribute$1 = param[1];
        var id = attribute$1.name;
        var name;
        name = id.TAG === /* Identifier */0 ? jsx_identifier(id._0) : jsx_namespaced_name(id._0);
        return node("JSXAttribute", param[0], [
                    [
                      "name",
                      name
                    ],
                    [
                      "value",
                      option(jsx_attribute_value, attribute$1.value)
                    ]
                  ]);
      } else {
        var param$1 = attribute._0;
        return node("JSXSpreadAttribute", param$1[0], [[
                      "argument",
                      expression(param$1[1].argument)
                    ]]);
      }
    };
    var comment = function (param) {
      var c = param[1];
      var match;
      match = c.TAG === /* Block */0 ? [
          "Block",
          c._0
        ] : [
          "Line",
          c._0
        ];
      return node(match[0], param[0], [[
                    "value",
                    string(match[1])
                  ]]);
    };
    var class_body = function (param) {
      return node("ClassBody", param[0], [[
                    "body",
                    array_of_list(class_element, param[1].body)
                  ]]);
    };
    var class_implements = function (param) {
      var $$implements = param[1];
      return node("ClassImplements", param[0], [
                  [
                    "id",
                    identifier($$implements.id)
                  ],
                  [
                    "typeParameters",
                    option(type_parameter_instantiation, $$implements.typeParameters)
                  ]
                ]);
    };
    var type_parameter_instantiation = function (param) {
      return node("TypeParameterInstantiation", param[0], [[
                    "params",
                    array_of_list(_type, param[1].params)
                  ]]);
    };
    var statement = function (param) {
      var b = param[1];
      var loc = param[0];
      if (typeof b === "number") {
        if (b === /* Empty */0) {
          return node("EmptyStatement", loc, []);
        } else {
          return node("DebuggerStatement", loc, []);
        }
      }
      switch (b.TAG | 0) {
        case /* Block */0 :
            return block([
                        loc,
                        b._0
                      ]);
        case /* Expression */1 :
            return node("ExpressionStatement", loc, [[
                          "expression",
                          expression(b._0.expression)
                        ]]);
        case /* If */2 :
            var _if = b._0;
            return node("IfStatement", loc, [
                        [
                          "test",
                          expression(_if.test)
                        ],
                        [
                          "consequent",
                          statement(_if.consequent)
                        ],
                        [
                          "alternate",
                          option(statement, _if.alternate)
                        ]
                      ]);
        case /* Labeled */3 :
            var labeled = b._0;
            return node("LabeledStatement", loc, [
                        [
                          "label",
                          identifier(labeled.label)
                        ],
                        [
                          "body",
                          statement(labeled.body)
                        ]
                      ]);
        case /* Break */4 :
            return node("BreakStatement", loc, [[
                          "label",
                          option(identifier, b._0.label)
                        ]]);
        case /* Continue */5 :
            return node("ContinueStatement", loc, [[
                          "label",
                          option(identifier, b._0.label)
                        ]]);
        case /* With */6 :
            var _with = b._0;
            return node("WithStatement", loc, [
                        [
                          "object",
                          expression(_with._object)
                        ],
                        [
                          "body",
                          statement(_with.body)
                        ]
                      ]);
        case /* TypeAlias */7 :
            return type_alias([
                        loc,
                        b._0
                      ]);
        case /* Switch */8 :
            var $$switch = b._0;
            return node("SwitchStatement", loc, [
                        [
                          "discriminant",
                          expression($$switch.discriminant)
                        ],
                        [
                          "cases",
                          array_of_list($$case, $$switch.cases)
                        ],
                        [
                          "lexical",
                          bool($$switch.lexical)
                        ]
                      ]);
        case /* Return */9 :
            return node("ReturnStatement", loc, [[
                          "argument",
                          option(expression, b._0.argument)
                        ]]);
        case /* Throw */10 :
            return node("ThrowStatement", loc, [[
                          "argument",
                          expression(b._0.argument)
                        ]]);
        case /* Try */11 :
            var _try = b._0;
            return node("TryStatement", loc, [
                        [
                          "block",
                          block(_try.block)
                        ],
                        [
                          "handler",
                          option($$catch, _try.handler)
                        ],
                        [
                          "guardedHandlers",
                          array_of_list($$catch, _try.guardedHandlers)
                        ],
                        [
                          "finalizer",
                          option(block, _try.finalizer)
                        ]
                      ]);
        case /* While */12 :
            var _while = b._0;
            return node("WhileStatement", loc, [
                        [
                          "test",
                          expression(_while.test)
                        ],
                        [
                          "body",
                          statement(_while.body)
                        ]
                      ]);
        case /* DoWhile */13 :
            var dowhile = b._0;
            return node("DoWhileStatement", loc, [
                        [
                          "body",
                          statement(dowhile.body)
                        ],
                        [
                          "test",
                          expression(dowhile.test)
                        ]
                      ]);
        case /* For */14 :
            var _for = b._0;
            var init = function (init$1) {
              if (init$1.TAG === /* InitDeclaration */0) {
                return variable_declaration(init$1._0);
              } else {
                return expression(init$1._0);
              }
            };
            return node("ForStatement", loc, [
                        [
                          "init",
                          option(init, _for.init)
                        ],
                        [
                          "test",
                          option(expression, _for.test)
                        ],
                        [
                          "update",
                          option(expression, _for.update)
                        ],
                        [
                          "body",
                          statement(_for.body)
                        ]
                      ]);
        case /* ForIn */15 :
            var forin = b._0;
            var left = forin.left;
            var left$1;
            left$1 = left.TAG === /* LeftDeclaration */0 ? variable_declaration(left._0) : expression(left._0);
            return node("ForInStatement", loc, [
                        [
                          "left",
                          left$1
                        ],
                        [
                          "right",
                          expression(forin.right)
                        ],
                        [
                          "body",
                          statement(forin.body)
                        ],
                        [
                          "each",
                          bool(forin.each)
                        ]
                      ]);
        case /* ForOf */16 :
            var forof = b._0;
            var left$2 = forof.left;
            var left$3;
            left$3 = left$2.TAG === /* LeftDeclaration */0 ? variable_declaration(left$2._0) : expression(left$2._0);
            return node("ForOfStatement", loc, [
                        [
                          "left",
                          left$3
                        ],
                        [
                          "right",
                          expression(forof.right)
                        ],
                        [
                          "body",
                          statement(forof.body)
                        ]
                      ]);
        case /* Let */17 :
            var _let = b._0;
            return node("LetStatement", loc, [
                        [
                          "head",
                          array_of_list(let_assignment, _let.head)
                        ],
                        [
                          "body",
                          statement(_let.body)
                        ]
                      ]);
        case /* FunctionDeclaration */18 :
            var fn = b._0;
            var id = fn.id;
            var match = id !== undefined ? [
                "FunctionDeclaration",
                identifier(id)
              ] : [
                "FunctionExpression",
                $$null
              ];
            var b$1 = fn.body;
            var body;
            body = b$1.TAG === /* BodyBlock */0 ? block(b$1._0) : expression(b$1._0);
            return node(match[0], loc, [
                        [
                          "id",
                          match[1]
                        ],
                        [
                          "params",
                          array_of_list(pattern, fn.params)
                        ],
                        [
                          "defaults",
                          array_of_list((function (param) {
                                  return option(expression, param);
                                }), fn.defaults)
                        ],
                        [
                          "rest",
                          option(identifier, fn.rest)
                        ],
                        [
                          "body",
                          body
                        ],
                        [
                          "async",
                          bool(fn.async)
                        ],
                        [
                          "generator",
                          bool(fn.generator)
                        ],
                        [
                          "expression",
                          bool(fn.expression)
                        ],
                        [
                          "returnType",
                          option(type_annotation, fn.returnType)
                        ],
                        [
                          "typeParameters",
                          option(type_parameter_declaration, fn.typeParameters)
                        ]
                      ]);
        case /* VariableDeclaration */19 :
            return variable_declaration([
                        loc,
                        b._0
                      ]);
        case /* ClassDeclaration */20 :
            var param$1 = [
              loc,
              b._0
            ];
            var c = param$1[1];
            var id$1 = c.id;
            var match$1 = id$1 !== undefined ? [
                "ClassDeclaration",
                identifier(id$1)
              ] : [
                "ClassExpression",
                $$null
              ];
            return node(match$1[0], param$1[0], [
                        [
                          "id",
                          match$1[1]
                        ],
                        [
                          "body",
                          class_body(c.body)
                        ],
                        [
                          "superClass",
                          option(expression, c.superClass)
                        ],
                        [
                          "typeParameters",
                          option(type_parameter_declaration, c.typeParameters)
                        ],
                        [
                          "superTypeParameters",
                          option(type_parameter_instantiation, c.superTypeParameters)
                        ],
                        [
                          "implements",
                          array_of_list(class_implements, c.implements)
                        ],
                        [
                          "decorators",
                          array_of_list(expression, c.classDecorators)
                        ]
                      ]);
        case /* InterfaceDeclaration */21 :
            return interface_declaration([
                        loc,
                        b._0
                      ]);
        case /* DeclareVariable */22 :
            return declare_variable([
                        loc,
                        b._0
                      ]);
        case /* DeclareFunction */23 :
            return declare_function([
                        loc,
                        b._0
                      ]);
        case /* DeclareClass */24 :
            return declare_class([
                        loc,
                        b._0
                      ]);
        case /* DeclareModule */25 :
            var m = b._0;
            var lit = m.id;
            var id$2;
            id$2 = lit.TAG === /* Identifier */0 ? identifier(lit._0) : literal(lit._0);
            var match$2 = m.kind;
            var tmp;
            tmp = match$2.TAG === /* CommonJS */0 ? string("CommonJS") : string("ES");
            return node("DeclareModule", loc, [
                        [
                          "id",
                          id$2
                        ],
                        [
                          "body",
                          block(m.body)
                        ],
                        [
                          "kind",
                          tmp
                        ]
                      ]);
        case /* DeclareModuleExports */26 :
            return node("DeclareModuleExports", loc, [[
                          "typeAnnotation",
                          type_annotation(b._0)
                        ]]);
        case /* DeclareExportDeclaration */27 :
            var $$export = b._0;
            var match$3 = $$export.declaration;
            var declaration;
            if (match$3 !== undefined) {
              switch (match$3.TAG | 0) {
                case /* Variable */0 :
                    declaration = declare_variable(match$3._0);
                    break;
                case /* Function */1 :
                    declaration = declare_function(match$3._0);
                    break;
                case /* Class */2 :
                    declaration = declare_class(match$3._0);
                    break;
                case /* DefaultType */3 :
                    declaration = _type(match$3._0);
                    break;
                case /* NamedType */4 :
                    declaration = type_alias(match$3._0);
                    break;
                case /* Interface */5 :
                    declaration = interface_declaration(match$3._0);
                    break;
                
              }
            } else {
              declaration = $$null;
            }
            return node("DeclareExportDeclaration", loc, [
                        [
                          "default",
                          bool($$export.default)
                        ],
                        [
                          "declaration",
                          declaration
                        ],
                        [
                          "specifiers",
                          export_specifiers($$export.specifiers)
                        ],
                        [
                          "source",
                          option(literal, $$export.source)
                        ]
                      ]);
        case /* ExportDeclaration */28 :
            var $$export$1 = b._0;
            var match$4 = $$export$1.declaration;
            var declaration$1 = match$4 !== undefined ? (
                match$4.TAG === /* Declaration */0 ? statement(match$4._0) : expression(match$4._0)
              ) : $$null;
            return node("ExportDeclaration", loc, [
                        [
                          "default",
                          bool($$export$1.default)
                        ],
                        [
                          "declaration",
                          declaration$1
                        ],
                        [
                          "specifiers",
                          export_specifiers($$export$1.specifiers)
                        ],
                        [
                          "source",
                          option(literal, $$export$1.source)
                        ],
                        [
                          "exportKind",
                          string($$export$1.exportKind ? "value" : "type")
                        ]
                      ]);
        case /* ImportDeclaration */29 :
            var $$import = b._0;
            var specifiers = List.map((function (id) {
                    switch (id.TAG | 0) {
                      case /* ImportNamedSpecifier */0 :
                          var match = id._0;
                          var local_id = match.local;
                          var remote_id = match.remote;
                          var span_loc = local_id !== undefined ? btwn(remote_id[0], local_id[0]) : remote_id[0];
                          return node("ImportSpecifier", span_loc, [
                                      [
                                        "id",
                                        identifier(remote_id)
                                      ],
                                      [
                                        "name",
                                        option(identifier, local_id)
                                      ]
                                    ]);
                      case /* ImportDefaultSpecifier */1 :
                          var id$1 = id._0;
                          return node("ImportDefaultSpecifier", id$1[0], [[
                                        "id",
                                        identifier(id$1)
                                      ]]);
                      case /* ImportNamespaceSpecifier */2 :
                          var param = id._0;
                          return node("ImportNamespaceSpecifier", param[0], [[
                                        "id",
                                        identifier(param[1])
                                      ]]);
                      
                    }
                  }), $$import.specifiers);
            var match$5 = $$import.importKind;
            var import_kind;
            switch (match$5) {
              case /* ImportType */0 :
                  import_kind = "type";
                  break;
              case /* ImportTypeof */1 :
                  import_kind = "typeof";
                  break;
              case /* ImportValue */2 :
                  import_kind = "value";
                  break;
              
            }
            return node("ImportDeclaration", loc, [
                        [
                          "specifiers",
                          array($$Array.of_list(specifiers))
                        ],
                        [
                          "source",
                          literal($$import.source)
                        ],
                        [
                          "importKind",
                          string(import_kind)
                        ]
                      ]);
        
      }
    };
    var jsx_identifier = function (param) {
      return node("JSXIdentifier", param[0], [[
                    "name",
                    string(param[1].name)
                  ]]);
    };
    var jsx_member_expression = function (param) {
      var member_expression = param[1];
      var id = member_expression._object;
      var _object;
      _object = id.TAG === /* Identifier */0 ? jsx_identifier(id._0) : jsx_member_expression(id._0);
      return node("JSXMemberExpression", param[0], [
                  [
                    "object",
                    _object
                  ],
                  [
                    "property",
                    jsx_identifier(member_expression.property)
                  ]
                ]);
    };
    var block = function (param) {
      return node("BlockStatement", param[0], [[
                    "body",
                    array_of_list(statement, param[1].body)
                  ]]);
    };
    var function_type = function (param) {
      var fn = param[1];
      return node("FunctionTypeAnnotation", param[0], [
                  [
                    "params",
                    array_of_list(function_type_param, fn.params)
                  ],
                  [
                    "returnType",
                    _type(fn.returnType)
                  ],
                  [
                    "rest",
                    option(function_type_param, fn.rest)
                  ],
                  [
                    "typeParameters",
                    option(type_parameter_declaration, fn.typeParameters)
                  ]
                ]);
    };
    var generic_type_qualified_identifier = function (param) {
      var q = param[1];
      var id = q.qualification;
      var qualification;
      qualification = id.TAG === /* Unqualified */0 ? identifier(id._0) : generic_type_qualified_identifier(id._0);
      return node("QualifiedTypeIdentifier", param[0], [
                  [
                    "qualification",
                    qualification
                  ],
                  [
                    "id",
                    identifier(q.id)
                  ]
                ]);
    };
    var jsx_element = function (param) {
      var element = param[1];
      return node("JSXElement", param[0], [
                  [
                    "openingElement",
                    jsx_opening(element.openingElement)
                  ],
                  [
                    "closingElement",
                    option(jsx_closing, element.closingElement)
                  ],
                  [
                    "children",
                    array_of_list(jsx_child, element.children)
                  ]
                ]);
    };
    var jsx_namespaced_name = function (param) {
      var namespaced_name = param[1];
      return node("JSXNamespacedName", param[0], [
                  [
                    "namespace",
                    jsx_identifier(namespaced_name.namespace)
                  ],
                  [
                    "name",
                    jsx_identifier(namespaced_name.name)
                  ]
                ]);
    };
    var type_param = function (param) {
      var tp = param[1];
      var variance = function (param) {
        if (param) {
          return string("minus");
        } else {
          return string("plus");
        }
      };
      return node("TypeParameter", param[0], [
                  [
                    "name",
                    string(tp.name)
                  ],
                  [
                    "bound",
                    option(type_annotation, tp.bound)
                  ],
                  [
                    "variance",
                    option(variance, tp.variance)
                  ],
                  [
                    "default",
                    option(_type, tp.default)
                  ]
                ]);
    };
    var object_type_indexer = function (param) {
      var indexer = param[1];
      return node("ObjectTypeIndexer", param[0], [
                  [
                    "id",
                    identifier(indexer.id)
                  ],
                  [
                    "key",
                    _type(indexer.key)
                  ],
                  [
                    "value",
                    _type(indexer.value)
                  ],
                  [
                    "static",
                    bool(indexer.static)
                  ]
                ]);
    };
    var object_type_property = function (param) {
      var prop = param[1];
      var lit = prop.key;
      var key;
      switch (lit.TAG | 0) {
        case /* Literal */0 :
            key = literal(lit._0);
            break;
        case /* Identifier */1 :
            key = identifier(lit._0);
            break;
        case /* Computed */2 :
            throw {
                  RE_EXN_ID: "Failure",
                  _1: "There should not be computed object type property keys",
                  Error: new Error()
                };
        
      }
      return node("ObjectTypeProperty", param[0], [
                  [
                    "key",
                    key
                  ],
                  [
                    "value",
                    _type(prop.value)
                  ],
                  [
                    "optional",
                    bool(prop.optional)
                  ],
                  [
                    "static",
                    bool(prop.static)
                  ]
                ]);
    };
    var object_type_call_property = function (param) {
      var callProperty = param[1];
      return node("ObjectTypeCallProperty", param[0], [
                  [
                    "value",
                    function_type(callProperty.value)
                  ],
                  [
                    "static",
                    bool(callProperty.static)
                  ]
                ]);
    };
    var object_property = function (param) {
      if (param.TAG === /* Property */0) {
        var match = param._0;
        var prop = match[1];
        var lit = prop.key;
        var match$1;
        switch (lit.TAG | 0) {
          case /* Literal */0 :
              match$1 = [
                literal(lit._0),
                false
              ];
              break;
          case /* Identifier */1 :
              match$1 = [
                identifier(lit._0),
                false
              ];
              break;
          case /* Computed */2 :
              match$1 = [
                expression(lit._0),
                true
              ];
              break;
          
        }
        var match$2 = prop.kind;
        var kind;
        switch (match$2) {
          case /* Init */0 :
              kind = "init";
              break;
          case /* Get */1 :
              kind = "get";
              break;
          case /* Set */2 :
              kind = "set";
              break;
          
        }
        return node("Property", match[0], [
                    [
                      "key",
                      match$1[0]
                    ],
                    [
                      "value",
                      expression(prop.value)
                    ],
                    [
                      "kind",
                      string(kind)
                    ],
                    [
                      "method",
                      bool(prop._method)
                    ],
                    [
                      "shorthand",
                      bool(prop.shorthand)
                    ],
                    [
                      "computed",
                      bool(match$1[1])
                    ]
                  ]);
      }
      var match$3 = param._0;
      return node("SpreadProperty", match$3[0], [[
                    "argument",
                    expression(match$3[1].argument)
                  ]]);
    };
    var expression_or_spread = function (expr) {
      if (expr.TAG === /* Expression */0) {
        return expression(expr._0);
      }
      var match = expr._0;
      return node("SpreadElement", match[0], [[
                    "argument",
                    expression(match[1].argument)
                  ]]);
    };
    var let_assignment = function (assignment) {
      return obj([
                  [
                    "id",
                    pattern(assignment.id)
                  ],
                  [
                    "init",
                    option(expression, assignment.init)
                  ]
                ]);
    };
    var function_expression = function (param) {
      var _function = param[1];
      var b = _function.body;
      var body;
      body = b.TAG === /* BodyBlock */0 ? block(b._0) : expression(b._0);
      return node("FunctionExpression", param[0], [
                  [
                    "id",
                    option(identifier, _function.id)
                  ],
                  [
                    "params",
                    array_of_list(pattern, _function.params)
                  ],
                  [
                    "defaults",
                    array_of_list((function (param) {
                            return option(expression, param);
                          }), _function.defaults)
                  ],
                  [
                    "rest",
                    option(identifier, _function.rest)
                  ],
                  [
                    "body",
                    body
                  ],
                  [
                    "async",
                    bool(_function.async)
                  ],
                  [
                    "generator",
                    bool(_function.generator)
                  ],
                  [
                    "expression",
                    bool(_function.expression)
                  ],
                  [
                    "returnType",
                    option(type_annotation, _function.returnType)
                  ],
                  [
                    "typeParameters",
                    option(type_parameter_declaration, _function.typeParameters)
                  ]
                ]);
    };
    var comprehension_block = function (param) {
      var b = param[1];
      return node("ComprehensionBlock", param[0], [
                  [
                    "left",
                    pattern(b.left)
                  ],
                  [
                    "right",
                    expression(b.right)
                  ],
                  [
                    "each",
                    bool(b.each)
                  ]
                ]);
    };
    var jsx_attribute_value = function (param) {
      if (param.TAG === /* Literal */0) {
        return literal([
                    param._0,
                    param._1
                  ]);
      } else {
        return jsx_expression_container([
                    param._0,
                    param._1
                  ]);
      }
    };
    var export_specifiers = function (param) {
      if (param !== undefined) {
        if (param.TAG === /* ExportSpecifiers */0) {
          return array_of_list(export_specifier, param._0);
        } else {
          return array([node("ExportBatchSpecifier", param._0, [[
                              "name",
                              option(identifier, param._1)
                            ]])]);
        }
      } else {
        return array([]);
      }
    };
    var declare_class = function (param) {
      var d = param[1];
      return node("DeclareClass", param[0], [
                  [
                    "id",
                    identifier(d.id)
                  ],
                  [
                    "typeParameters",
                    option(type_parameter_declaration, d.typeParameters)
                  ],
                  [
                    "body",
                    object_type(d.body)
                  ],
                  [
                    "extends",
                    array_of_list(interface_extends, d.extends)
                  ]
                ]);
    };
    var interface_declaration = function (param) {
      var i = param[1];
      return node("InterfaceDeclaration", param[0], [
                  [
                    "id",
                    identifier(i.id)
                  ],
                  [
                    "typeParameters",
                    option(type_parameter_declaration, i.typeParameters)
                  ],
                  [
                    "body",
                    object_type(i.body)
                  ],
                  [
                    "extends",
                    array_of_list(interface_extends, i.extends)
                  ]
                ]);
    };
    var $$catch = function (param) {
      var c = param[1];
      return node("CatchClause", param[0], [
                  [
                    "param",
                    pattern(c.param)
                  ],
                  [
                    "guard",
                    option(expression, c.guard)
                  ],
                  [
                    "body",
                    block(c.body)
                  ]
                ]);
    };
    var type_alias = function (param) {
      var alias = param[1];
      return node("TypeAlias", param[0], [
                  [
                    "id",
                    identifier(alias.id)
                  ],
                  [
                    "typeParameters",
                    option(type_parameter_declaration, alias.typeParameters)
                  ],
                  [
                    "right",
                    _type(alias.right)
                  ]
                ]);
    };
    var $$case = function (param) {
      var c = param[1];
      return node("SwitchCase", param[0], [
                  [
                    "test",
                    option(expression, c.test)
                  ],
                  [
                    "consequent",
                    array_of_list(statement, c.consequent)
                  ]
                ]);
    };
    var variable_declaration = function (param) {
      var $$var = param[1];
      var match = $$var.kind;
      var kind;
      switch (match) {
        case /* Var */0 :
            kind = "var";
            break;
        case /* Let */1 :
            kind = "let";
            break;
        case /* Const */2 :
            kind = "const";
            break;
        
      }
      return node("VariableDeclaration", param[0], [
                  [
                    "declarations",
                    array_of_list(variable_declarator, $$var.declarations)
                  ],
                  [
                    "kind",
                    string(kind)
                  ]
                ]);
    };
    var declare_variable = function (param) {
      return node("DeclareVariable", param[0], [[
                    "id",
                    identifier(param[1].id)
                  ]]);
    };
    var declare_function = function (param) {
      return node("DeclareFunction", param[0], [[
                    "id",
                    identifier(param[1].id)
                  ]]);
    };
    var array_pattern_element = function (p) {
      if (p.TAG === /* Element */0) {
        return pattern(p._0);
      }
      var match = p._0;
      return node("SpreadElementPattern", match[0], [[
                    "argument",
                    pattern(match[1].argument)
                  ]]);
    };
    var object_pattern_property = function (param) {
      if (param.TAG === /* Property */0) {
        var match = param._0;
        var prop = match[1];
        var lit = prop.key;
        var match$1;
        switch (lit.TAG | 0) {
          case /* Literal */0 :
              match$1 = [
                literal(lit._0),
                false
              ];
              break;
          case /* Identifier */1 :
              match$1 = [
                identifier(lit._0),
                false
              ];
              break;
          case /* Computed */2 :
              match$1 = [
                expression(lit._0),
                true
              ];
              break;
          
        }
        return node("PropertyPattern", match[0], [
                    [
                      "key",
                      match$1[0]
                    ],
                    [
                      "pattern",
                      pattern(prop.pattern)
                    ],
                    [
                      "computed",
                      bool(match$1[1])
                    ],
                    [
                      "shorthand",
                      bool(prop.shorthand)
                    ]
                  ]);
      }
      var match$2 = param._0;
      return node("SpreadPropertyPattern", match$2[0], [[
                    "argument",
                    pattern(match$2[1].argument)
                  ]]);
    };
    var program$2 = function (param) {
      return node("Program", param[0], [
                  [
                    "body",
                    array_of_list(statement, param[1])
                  ],
                  [
                    "comments",
                    array_of_list(comment, param[2])
                  ]
                ]);
    };
    var ret = program$2(match[0]);
    var translation_errors$1 = translation_errors.contents;
    ret["errors"] = errors(Pervasives.$at(match[1], translation_errors$1));
    return ret;
  }
  catch (raw_l){
    var l = Caml_js_exceptions.internalToOCamlException(raw_l);
    if (l.RE_EXN_ID === $$Error) {
      var e = new Error(String(List.length(l._1)) + " errors");
      e["name"] = "Parse Error";
      throw(e);
      return {};
    }
    throw l;
  }
}

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

var f = typeof __dirname === "undefined" ? undefined : __dirname;

if (f !== undefined) {
  var f$1 = Path.join(f, "flow_parser_sample.js");
  var v = parse(Fs.readFileSync(f$1, "utf8"), undefined);
  eq("File \"runParser.ml\", line 14, characters 7-14", [
        0,
        2842
      ], v.range);
} else {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "runParser.ml",
          15,
          12
        ],
        Error: new Error()
      };
}

Mt.from_pair_suites("Flow_parser_reg_test", suites.contents);

/* Literal Not a pure module */
