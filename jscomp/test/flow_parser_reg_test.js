'use strict';

var Mt = require("./mt.js");
var Fs = require("fs");
var Sys = require("../../lib/js/sys.js");
var Char = require("../../lib/js/char.js");
var List = require("../../lib/js/list.js");
var Path = require("path");
var $$Array = require("../../lib/js/array.js");
var Bytes = require("../../lib/js/bytes.js");
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
  if (typeof param === "string") {
    return "(global)";
  } else {
    return param.Arg0;
  }
}

function order_of_filename(param) {
  if (typeof param === "string") {
    return 1;
  } else {
    switch (/* XXX */param.tag) {
      case "LibFile" :
          return 2;
      case "SourceFile" :
      case "JsonFile" :
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
  if (typeof param === "string") {
    switch (param) {
      case "UnexpectedNumber" :
          return "Unexpected number";
      case "UnexpectedString" :
          return "Unexpected string";
      case "UnexpectedIdentifier" :
          return "Unexpected identifier";
      case "UnexpectedReserved" :
          return "Unexpected reserved word";
      case "UnexpectedEOS" :
          return "Unexpected end of input";
      case "UnexpectedTypeAlias" :
          return "Type aliases are not allowed in untyped mode";
      case "UnexpectedTypeAnnotation" :
          return "Type annotations are not allowed in untyped mode";
      case "UnexpectedTypeDeclaration" :
          return "Type declarations are not allowed in untyped mode";
      case "UnexpectedTypeImport" :
          return "Type imports are not allowed in untyped mode";
      case "UnexpectedTypeExport" :
          return "Type exports are not allowed in untyped mode";
      case "UnexpectedTypeInterface" :
          return "Interfaces are not allowed in untyped mode";
      case "NewlineAfterThrow" :
          return "Illegal newline after throw";
      case "InvalidRegExp" :
          return "Invalid regular expression";
      case "UnterminatedRegExp" :
          return "Invalid regular expression: missing /";
      case "InvalidLHSInAssignment" :
          return "Invalid left-hand side in assignment";
      case "InvalidLHSInExponentiation" :
          return "Invalid left-hand side in exponentiation expression";
      case "InvalidLHSInForIn" :
          return "Invalid left-hand side in for-in";
      case "InvalidLHSInForOf" :
          return "Invalid left-hand side in for-of";
      case "ExpectedPatternFoundExpression" :
          return "Expected an object pattern, array pattern, or an identifier but found an expression instead";
      case "MultipleDefaultsInSwitch" :
          return "More than one default clause in switch statement";
      case "NoCatchOrFinally" :
          return "Missing catch or finally after try";
      case "IllegalContinue" :
          return "Illegal continue statement";
      case "IllegalBreak" :
          return "Illegal break statement";
      case "IllegalReturn" :
          return "Illegal return statement";
      case "IllegalYield" :
          return "Illegal yield expression";
      case "StrictModeWith" :
          return "Strict mode code may not include a with statement";
      case "StrictCatchVariable" :
          return "Catch variable may not be eval or arguments in strict mode";
      case "StrictVarName" :
          return "Variable name may not be eval or arguments in strict mode";
      case "StrictParamName" :
          return "Parameter name eval or arguments is not allowed in strict mode";
      case "StrictParamDupe" :
          return "Strict mode function may not have duplicate parameter names";
      case "StrictFunctionName" :
          return "Function name may not be eval or arguments in strict mode";
      case "StrictOctalLiteral" :
          return "Octal literals are not allowed in strict mode.";
      case "StrictDelete" :
          return "Delete of an unqualified identifier in strict mode.";
      case "StrictDuplicateProperty" :
          return "Duplicate data property in object literal not allowed in strict mode";
      case "AccessorDataProperty" :
          return "Object literal may not have data and accessor property with the same name";
      case "AccessorGetSet" :
          return "Object literal may not have multiple get/set accessors with the same name";
      case "StrictLHSAssignment" :
          return "Assignment to eval or arguments is not allowed in strict mode";
      case "StrictLHSPostfix" :
          return "Postfix increment/decrement may not have eval or arguments operand in strict mode";
      case "StrictLHSPrefix" :
          return "Prefix increment/decrement may not have eval or arguments operand in strict mode";
      case "StrictReservedWord" :
          return "Use of future reserved word in strict mode";
      case "JSXAttributeValueEmptyExpression" :
          return "JSX attributes must only be assigned a non-empty expression";
      case "InvalidJSXAttributeValue" :
          return "JSX value should be either an expression or a quoted JSX text";
      case "NoUninitializedConst" :
          return "Const must be initialized";
      case "NoUninitializedDestructuring" :
          return "Destructuring assignment must be initialized";
      case "NewlineBeforeArrow" :
          return "Illegal newline before arrow";
      case "StrictFunctionStatement" :
          return "In strict mode code, functions can only be declared at top level or immediately within another function.";
      case "AdjacentJSXElements" :
          return "Unexpected token <. Remember, adjacent JSX elements must be wrapped in an enclosing parent tag";
      case "ParameterAfterRestParameter" :
          return "Rest parameter must be final parameter of an argument list";
      case "AsyncGenerator" :
          return "A function may not be both async and a generator";
      case "DeclareAsync" :
          return "async is an implementation detail and isn't necessary for your declare function statement. It is sufficient for your declare function to just have a Promise return type.";
      case "DeclareExportLet" :
          return "`declare export let` is not supported. Use `declare export var` instead.";
      case "DeclareExportConst" :
          return "`declare export const` is not supported. Use `declare export var` instead.";
      case "DeclareExportType" :
          return "`declare export type` is not supported. Use `export type` instead.";
      case "DeclareExportInterface" :
          return "`declare export interface` is not supported. Use `export interface` instead.";
      case "UnexpectedExportStarAs" :
          return "`export * as` is an early-stage proposal and is not enabled by default. To enable support in the parser, use the `esproposal_export_star_as` option";
      case "ExportNamelessClass" :
          return "When exporting a class as a named export, you must specify a class name. Did you mean `export default class ...`?";
      case "ExportNamelessFunction" :
          return "When exporting a function as a named export, you must specify a function name. Did you mean `export default function ...`?";
      case "UnsupportedDecorator" :
          return "Found a decorator in an unsupported position.";
      case "MissingTypeParamDefault" :
          return "Type parameter declaration needs a default, since a preceding type parameter declaration has a default.";
      case "WindowsFloatOfString" :
          return "The Windows version of OCaml has a bug in how it parses hexidecimal numbers. It is fixed in OCaml 4.03.0. Until we can switch to 4.03.0, please avoid either hexidecimal notation or Windows.";
      case "DuplicateDeclareModuleExports" :
          return "Duplicate `declare module.exports` statement!";
      case "AmbiguousDeclareModuleKind" :
          return "Found both `declare module.exports` and `declare export` in the same module. Modules can only have 1 since they are either an ES module xor they are a CommonJS module.";
      
    }
  } else {
    switch (/* XXX */param.tag) {
      case "Assertion" :
          return "Unexpected parser state: " + param.Arg0;
      case "UnexpectedToken" :
          return "Unexpected token " + param.Arg0;
      case "UnexpectedTokenWithSuggestion" :
          return Curry._2(Printf.sprintf(/* constructor */{
                          tag: "Format",
                          Arg0: /* constructor */{
                            tag: "String_literal",
                            Arg0: "Unexpected token `",
                            Arg1: /* constructor */{
                              tag: "String",
                              Arg0: "No_padding",
                              Arg1: /* constructor */{
                                tag: "String_literal",
                                Arg0: "`. Did you mean `",
                                Arg1: /* constructor */{
                                  tag: "String",
                                  Arg0: "No_padding",
                                  Arg1: /* constructor */{
                                    tag: "String_literal",
                                    Arg0: "`?",
                                    Arg1: "End_of_format"
                                  }
                                }
                              }
                            }
                          },
                          Arg1: "Unexpected token `%s`. Did you mean `%s`?"
                        }), param.Arg0, param.Arg1);
      case "InvalidRegExpFlags" :
          return "Invalid flags supplied to RegExp constructor '" + (param.Arg0 + "'");
      case "UnknownLabel" :
          return "Undefined label '" + (param.Arg0 + "'");
      case "Redeclaration" :
          return param.Arg0 + (" '" + (param.Arg1 + "' has already been declared"));
      case "ExpectedJSXClosingTag" :
          return "Expected corresponding JSX closing tag for " + param.Arg0;
      case "DuplicateExport" :
          return Curry._1(Printf.sprintf(/* constructor */{
                          tag: "Format",
                          Arg0: /* constructor */{
                            tag: "String_literal",
                            Arg0: "Duplicate export for `",
                            Arg1: /* constructor */{
                              tag: "String",
                              Arg0: "No_padding",
                              Arg1: /* constructor */{
                                tag: "Char_literal",
                                Arg0: /* "`" */96,
                                Arg1: "End_of_format"
                              }
                            }
                          },
                          Arg1: "Duplicate export for `%s`"
                        }), param.Arg0);
      
    }
  }
}

var Literal = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      44,
      6
    ], /* constructor */{
      tag: "Module",
      Arg0: [[
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "RegExp"
        ]]
    });

var Type = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      191,
      6
    ], /* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Param"
              ]]
          },
          "Function"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Property"
              ],
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Indexer"
              ],
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "CallProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Identifier"
              ]]
          },
          "Generic"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "StringLiteral"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "NumberLiteral"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "BooleanLiteral"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: [[
                      /* constructor */{
                        tag: "Module",
                        Arg0: []
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
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "ParameterInstantiation"
        ]
      ]
    });

var Statement = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      493,
      6
    ], /* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Block"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "If"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Labeled"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Break"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Continue"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "With"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "TypeAlias"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Case"
              ]]
          },
          "Switch"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Return"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Throw"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "CatchClause"
              ]]
          },
          "Try"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Declarator"
              ]]
          },
          "VariableDeclaration"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "While"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "DoWhile"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "For"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "ForIn"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "ForOf"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Let"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Interface"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "DeclareVariable"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "DeclareFunction"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "DeclareModule"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Specifier"
              ]]
          },
          "ExportDeclaration"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "DeclareExportDeclaration"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "NamedSpecifier"
              ]]
          },
          "ImportDeclaration"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Expression"
        ]
      ]
    });

var Expression = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      758,
      6
    ], /* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "SpreadElement"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Array"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Element"
              ]]
          },
          "TemplateLiteral"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "TaggedTemplate"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Property"
              ],
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "SpreadProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Sequence"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Unary"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Binary"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Assignment"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Update"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Logical"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Conditional"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "New"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Call"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Member"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Yield"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Block"
              ]]
          },
          "Comprehension"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Generator"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Let"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "TypeCast"
        ]
      ]
    });

var JSX = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      861,
      6
    ], /* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Identifier"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "NamespacedName"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "ExpressionContainer"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Text"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Attribute"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "SpreadAttribute"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "MemberExpression"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Opening"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Closing"
        ]
      ]
    });

var Pattern = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      919,
      6
    ], /* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: [
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Property"
              ],
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "SpreadProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "SpreadElement"
              ]]
          },
          "Array"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Assignment"
        ]
      ]
    });

var Class = Caml_module.init_mod([
      "spider_monkey_ast.ml",
      978,
      6
    ], /* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Method"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Property"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Implements"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Body"
        ]
      ]
    });

Caml_module.update_mod(/* constructor */{
      tag: "Module",
      Arg0: [[
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "RegExp"
        ]]
    }, Literal, Literal);

Caml_module.update_mod(/* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Param"
              ]]
          },
          "Function"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Property"
              ],
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Indexer"
              ],
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "CallProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Identifier"
              ]]
          },
          "Generic"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "StringLiteral"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "NumberLiteral"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "BooleanLiteral"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: [[
                      /* constructor */{
                        tag: "Module",
                        Arg0: []
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
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "ParameterInstantiation"
        ]
      ]
    }, Type, Type);

Caml_module.update_mod(/* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Block"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "If"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Labeled"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Break"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Continue"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "With"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "TypeAlias"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Case"
              ]]
          },
          "Switch"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Return"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Throw"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "CatchClause"
              ]]
          },
          "Try"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Declarator"
              ]]
          },
          "VariableDeclaration"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "While"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "DoWhile"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "For"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "ForIn"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "ForOf"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Let"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Interface"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "DeclareVariable"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "DeclareFunction"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "DeclareModule"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Specifier"
              ]]
          },
          "ExportDeclaration"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "DeclareExportDeclaration"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "NamedSpecifier"
              ]]
          },
          "ImportDeclaration"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Expression"
        ]
      ]
    }, Statement, Statement);

Caml_module.update_mod(/* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "SpreadElement"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Array"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Element"
              ]]
          },
          "TemplateLiteral"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "TaggedTemplate"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Property"
              ],
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "SpreadProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Sequence"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Unary"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Binary"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Assignment"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Update"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Logical"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Conditional"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "New"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Call"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Member"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Yield"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Block"
              ]]
          },
          "Comprehension"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Generator"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Let"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "TypeCast"
        ]
      ]
    }, Expression, Expression);

Caml_module.update_mod(/* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Identifier"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "NamespacedName"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "ExpressionContainer"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Text"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Attribute"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "SpreadAttribute"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "MemberExpression"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Opening"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Closing"
        ]
      ]
    }, JSX, JSX);

Caml_module.update_mod(/* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: [
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "Property"
              ],
              [
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "SpreadProperty"
              ]
            ]
          },
          "Object"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: [[
                /* constructor */{
                  tag: "Module",
                  Arg0: []
                },
                "SpreadElement"
              ]]
          },
          "Array"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Assignment"
        ]
      ]
    }, Pattern, Pattern);

Caml_module.update_mod(/* constructor */{
      tag: "Module",
      Arg0: [
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Method"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Property"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Implements"
        ],
        [
          /* constructor */{
            tag: "Module",
            Arg0: []
          },
          "Body"
        ]
      ]
    }, Class, Class);

function token_to_string(param) {
  if (typeof param === "string") {
    switch (param) {
      case "T_IDENTIFIER" :
          return "T_IDENTIFIER";
      case "T_LCURLY" :
          return "T_LCURLY";
      case "T_RCURLY" :
          return "T_RCURLY";
      case "T_LPAREN" :
          return "T_LPAREN";
      case "T_RPAREN" :
          return "T_RPAREN";
      case "T_LBRACKET" :
          return "T_LBRACKET";
      case "T_RBRACKET" :
          return "T_RBRACKET";
      case "T_SEMICOLON" :
          return "T_SEMICOLON";
      case "T_COMMA" :
          return "T_COMMA";
      case "T_PERIOD" :
          return "T_PERIOD";
      case "T_ARROW" :
          return "T_ARROW";
      case "T_ELLIPSIS" :
          return "T_ELLIPSIS";
      case "T_AT" :
          return "T_AT";
      case "T_FUNCTION" :
          return "T_FUNCTION";
      case "T_IF" :
          return "T_IF";
      case "T_IN" :
          return "T_IN";
      case "T_INSTANCEOF" :
          return "T_INSTANCEOF";
      case "T_RETURN" :
          return "T_RETURN";
      case "T_SWITCH" :
          return "T_SWITCH";
      case "T_THIS" :
          return "T_THIS";
      case "T_THROW" :
          return "T_THROW";
      case "T_TRY" :
          return "T_TRY";
      case "T_VAR" :
          return "T_VAR";
      case "T_WHILE" :
          return "T_WHILE";
      case "T_WITH" :
          return "T_WITH";
      case "T_CONST" :
          return "T_CONST";
      case "T_LET" :
          return "T_LET";
      case "T_NULL" :
          return "T_NULL";
      case "T_FALSE" :
          return "T_FALSE";
      case "T_TRUE" :
          return "T_TRUE";
      case "T_BREAK" :
          return "T_BREAK";
      case "T_CASE" :
          return "T_CASE";
      case "T_CATCH" :
          return "T_CATCH";
      case "T_CONTINUE" :
          return "T_CONTINUE";
      case "T_DEFAULT" :
          return "T_DEFAULT";
      case "T_DO" :
          return "T_DO";
      case "T_FINALLY" :
          return "T_FINALLY";
      case "T_FOR" :
          return "T_FOR";
      case "T_CLASS" :
          return "T_CLASS";
      case "T_EXTENDS" :
          return "T_EXTENDS";
      case "T_STATIC" :
          return "T_STATIC";
      case "T_ELSE" :
          return "T_ELSE";
      case "T_NEW" :
          return "T_NEW";
      case "T_DELETE" :
          return "T_DELETE";
      case "T_TYPEOF" :
          return "T_TYPEOF";
      case "T_VOID" :
          return "T_VOID";
      case "T_ENUM" :
          return "T_ENUM";
      case "T_EXPORT" :
          return "T_EXPORT";
      case "T_IMPORT" :
          return "T_IMPORT";
      case "T_SUPER" :
          return "T_SUPER";
      case "T_IMPLEMENTS" :
          return "T_IMPLEMENTS";
      case "T_INTERFACE" :
          return "T_INTERFACE";
      case "T_PACKAGE" :
          return "T_PACKAGE";
      case "T_PRIVATE" :
          return "T_PRIVATE";
      case "T_PROTECTED" :
          return "T_PROTECTED";
      case "T_PUBLIC" :
          return "T_PUBLIC";
      case "T_YIELD" :
          return "T_YIELD";
      case "T_DEBUGGER" :
          return "T_DEBUGGER";
      case "T_DECLARE" :
          return "T_DECLARE";
      case "T_TYPE" :
          return "T_TYPE";
      case "T_OF" :
          return "T_OF";
      case "T_ASYNC" :
          return "T_ASYNC";
      case "T_AWAIT" :
          return "T_AWAIT";
      case "T_RSHIFT3_ASSIGN" :
          return "T_RSHIFT3_ASSIGN";
      case "T_RSHIFT_ASSIGN" :
          return "T_RSHIFT_ASSIGN";
      case "T_LSHIFT_ASSIGN" :
          return "T_LSHIFT_ASSIGN";
      case "T_BIT_XOR_ASSIGN" :
          return "T_BIT_XOR_ASSIGN";
      case "T_BIT_OR_ASSIGN" :
          return "T_BIT_OR_ASSIGN";
      case "T_BIT_AND_ASSIGN" :
          return "T_BIT_AND_ASSIGN";
      case "T_MOD_ASSIGN" :
          return "T_MOD_ASSIGN";
      case "T_DIV_ASSIGN" :
          return "T_DIV_ASSIGN";
      case "T_MULT_ASSIGN" :
          return "T_MULT_ASSIGN";
      case "T_EXP_ASSIGN" :
          return "T_EXP_ASSIGN";
      case "T_MINUS_ASSIGN" :
          return "T_MINUS_ASSIGN";
      case "T_PLUS_ASSIGN" :
          return "T_PLUS_ASSIGN";
      case "T_ASSIGN" :
          return "T_ASSIGN";
      case "T_PLING" :
          return "T_PLING";
      case "T_COLON" :
          return "T_COLON";
      case "T_OR" :
          return "T_OR";
      case "T_AND" :
          return "T_AND";
      case "T_BIT_OR" :
          return "T_BIT_OR";
      case "T_BIT_XOR" :
          return "T_BIT_XOR";
      case "T_BIT_AND" :
          return "T_BIT_AND";
      case "T_EQUAL" :
          return "T_EQUAL";
      case "T_NOT_EQUAL" :
          return "T_NOT_EQUAL";
      case "T_STRICT_EQUAL" :
          return "T_STRICT_EQUAL";
      case "T_STRICT_NOT_EQUAL" :
          return "T_STRICT_NOT_EQUAL";
      case "T_LESS_THAN_EQUAL" :
          return "T_LESS_THAN_EQUAL";
      case "T_GREATER_THAN_EQUAL" :
          return "T_GREATER_THAN_EQUAL";
      case "T_LESS_THAN" :
          return "T_LESS_THAN";
      case "T_GREATER_THAN" :
          return "T_GREATER_THAN";
      case "T_LSHIFT" :
          return "T_LSHIFT";
      case "T_RSHIFT" :
          return "T_RSHIFT";
      case "T_RSHIFT3" :
          return "T_RSHIFT3";
      case "T_PLUS" :
          return "T_PLUS";
      case "T_MINUS" :
          return "T_MINUS";
      case "T_DIV" :
          return "T_DIV";
      case "T_MULT" :
          return "T_MULT";
      case "T_EXP" :
          return "T_EXP";
      case "T_MOD" :
          return "T_MOD";
      case "T_NOT" :
          return "T_NOT";
      case "T_BIT_NOT" :
          return "T_BIT_NOT";
      case "T_INCR" :
          return "T_INCR";
      case "T_DECR" :
          return "T_DECR";
      case "T_ERROR" :
          return "T_ERROR";
      case "T_EOF" :
          return "T_EOF";
      case "T_JSX_IDENTIFIER" :
          return "T_JSX_IDENTIFIER";
      case "T_ANY_TYPE" :
          return "T_ANY_TYPE";
      case "T_BOOLEAN_TYPE" :
          return "T_BOOLEAN_TYPE";
      case "T_NUMBER_TYPE" :
          return "T_NUMBER_TYPE";
      case "T_STRING_TYPE" :
          return "T_STRING_TYPE";
      case "T_VOID_TYPE" :
          return "T_VOID_TYPE";
      
    }
  } else {
    switch (/* XXX */param.tag) {
      case "T_NUMBER" :
          return "T_NUMBER";
      case "T_STRING" :
          return "T_STRING";
      case "T_TEMPLATE_PART" :
          return "T_TEMPLATE_PART";
      case "T_REGEXP" :
          return "T_REGEXP";
      case "T_JSX_TEXT" :
          return "T_JSX_TEXT";
      case "T_NUMBER_SINGLETON_TYPE" :
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
  /* lex_errors_acc */"[]",
  /* lex_comments_acc */"[]"
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
  if (typeof lex_token === "string") {
    exit = 2;
  } else {
    switch (/* XXX */lex_token.tag) {
      case "T_TEMPLATE_PART" :
          var match$2 = lex_token.Arg0;
          match$1 = /* tuple */[
            match$2[0],
            match$2[1][/* literal */2]
          ];
          break;
      case "T_REGEXP" :
          var match$3 = lex_token.Arg0;
          match$1 = /* tuple */[
            match$3[0],
            "/" + (match$3[1] + ("/" + match$3[2]))
          ];
          break;
      case "T_STRING" :
      case "T_JSX_TEXT" :
          exit = 1;
          break;
      default:
        exit = 2;
    }
  }
  switch (exit) {
    case 1 :
        var match$4 = lex_token.Arg0;
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
  var lex_errors_acc = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc,
      err
    ],
    Arg1: env[/* lex_state */4][/* lex_errors_acc */0]
  };
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
  return lex_error(env, loc, /* constructor */{
              tag: "UnexpectedToken",
              Arg0: value
            });
}

function unexpected_error_w_suggest(env, loc, value, suggest) {
  return lex_error(env, loc, /* constructor */{
              tag: "UnexpectedTokenWithSuggestion",
              Arg0: value,
              Arg1: suggest
            });
}

function illegal_number(env, lexbuf, word, token) {
  var loc = from_lb(env[/* lex_source */0], lexbuf);
  yyback(word.length, lexbuf);
  var env$1 = lex_error(env, loc, /* constructor */{
        tag: "UnexpectedToken",
        Arg0: "ILLEGAL"
      });
  return /* tuple */[
          env$1,
          token
        ];
}

var No_good = Caml_exceptions.create("Flow_parser_reg_test.Lexer_flow.FloatOfString.No_good");

function eat(f) {
  var match = f[/* todo */4];
  if (match !== "[]") {
    return /* record */[
            /* negative */f[/* negative */0],
            /* mantissa */f[/* mantissa */1],
            /* exponent */f[/* exponent */2],
            /* decimal_exponent */f[/* decimal_exponent */3],
            /* todo */match.Arg1
          ];
  } else {
    throw No_good;
  }
}

function start(str) {
  var todo = /* record */[/* contents */"[]"];
  Bytes.iter((function (c) {
          todo[0] = /* constructor */{
            tag: "::",
            Arg0: c,
            Arg1: todo[0]
          };
          return /* () */0;
        }), Caml_bytes.bytes_of_string(str));
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
  if (match !== "[]") {
    switch (match.Arg0) {
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
  if (match !== "[]") {
    if (match.Arg0 !== 48) {
      throw No_good;
    }
    var match$1 = match.Arg1;
    if (match$1 !== "[]") {
      var match$2 = match$1.Arg0;
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
          /* todo */"[]"
        ];
}

function parse_body(_f) {
  while(true) {
    var f = _f;
    var match = f[/* todo */4];
    if (match !== "[]") {
      var c = match.Arg0;
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
        if (f[/* todo */4] !== "[]") {
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
  var c = multiline ? /* constructor */({
        tag: "Block",
        Arg0: s
      }) : /* constructor */({
        tag: "Line",
        Arg0: s
      });
  var lex_comments_acc = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc,
      c
    ],
    Arg1: env[/* lex_state */4][/* lex_comments_acc */1]
  };
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
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "lexer_flow.mll",
          610,
          11
        ]
      ];
}

function utf16to8(code) {
  if (code >= 65536) {
    return /* constructor */{
            tag: "::",
            Arg0: Char.chr(240 | (code >>> 18)),
            Arg1: /* constructor */{
              tag: "::",
              Arg0: Char.chr(128 | (code >>> 12) & 63),
              Arg1: /* constructor */{
                tag: "::",
                Arg0: Char.chr(128 | (code >>> 6) & 63),
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: Char.chr(128 | code & 63),
                  Arg1: "[]"
                }
              }
            }
          };
  } else if (code >= 2048) {
    return /* constructor */{
            tag: "::",
            Arg0: Char.chr(224 | (code >>> 12)),
            Arg1: /* constructor */{
              tag: "::",
              Arg0: Char.chr(128 | (code >>> 6) & 63),
              Arg1: /* constructor */{
                tag: "::",
                Arg0: Char.chr(128 | code & 63),
                Arg1: "[]"
              }
            }
          };
  } else if (code >= 128) {
    return /* constructor */{
            tag: "::",
            Arg0: Char.chr(192 | (code >>> 6)),
            Arg1: /* constructor */{
              tag: "::",
              Arg0: Char.chr(128 | code & 63),
              Arg1: "[]"
            }
          };
  } else {
    return /* constructor */{
            tag: "::",
            Arg0: Char.chr(code),
            Arg1: "[]"
          };
  }
}

function mk_num_singleton(number_type, num, neg) {
  var value;
  switch (number_type) {
    case "LEGACY_OCTAL" :
        value = Caml_format.caml_int_of_string("0o" + num);
        break;
    case "BINARY" :
    case "OCTAL" :
        value = Caml_format.caml_int_of_string(num);
        break;
    case "NORMAL" :
        value = float_of_string(num);
        break;
    
  }
  var value$1 = neg === "" ? value : -value;
  return /* constructor */{
          tag: "T_NUMBER_SINGLETON_TYPE",
          Arg0: number_type,
          Arg1: value$1
        };
}

var keywords = Hashtbl.create(undefined, 53);

var type_keywords = Hashtbl.create(undefined, 53);

List.iter((function (param) {
        return Hashtbl.add(keywords, param[0], param[1]);
      }), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "function",
        "T_FUNCTION"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "if",
          "T_IF"
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "in",
            "T_IN"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "instanceof",
              "T_INSTANCEOF"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "return",
                "T_RETURN"
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "switch",
                  "T_SWITCH"
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "this",
                    "T_THIS"
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "throw",
                      "T_THROW"
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "try",
                        "T_TRY"
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "var",
                          "T_VAR"
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "while",
                            "T_WHILE"
                          ],
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              "with",
                              "T_WITH"
                            ],
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: /* tuple */[
                                "const",
                                "T_CONST"
                              ],
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: /* tuple */[
                                  "let",
                                  "T_LET"
                                ],
                                Arg1: /* constructor */{
                                  tag: "::",
                                  Arg0: /* tuple */[
                                    "null",
                                    "T_NULL"
                                  ],
                                  Arg1: /* constructor */{
                                    tag: "::",
                                    Arg0: /* tuple */[
                                      "false",
                                      "T_FALSE"
                                    ],
                                    Arg1: /* constructor */{
                                      tag: "::",
                                      Arg0: /* tuple */[
                                        "true",
                                        "T_TRUE"
                                      ],
                                      Arg1: /* constructor */{
                                        tag: "::",
                                        Arg0: /* tuple */[
                                          "break",
                                          "T_BREAK"
                                        ],
                                        Arg1: /* constructor */{
                                          tag: "::",
                                          Arg0: /* tuple */[
                                            "case",
                                            "T_CASE"
                                          ],
                                          Arg1: /* constructor */{
                                            tag: "::",
                                            Arg0: /* tuple */[
                                              "catch",
                                              "T_CATCH"
                                            ],
                                            Arg1: /* constructor */{
                                              tag: "::",
                                              Arg0: /* tuple */[
                                                "continue",
                                                "T_CONTINUE"
                                              ],
                                              Arg1: /* constructor */{
                                                tag: "::",
                                                Arg0: /* tuple */[
                                                  "default",
                                                  "T_DEFAULT"
                                                ],
                                                Arg1: /* constructor */{
                                                  tag: "::",
                                                  Arg0: /* tuple */[
                                                    "do",
                                                    "T_DO"
                                                  ],
                                                  Arg1: /* constructor */{
                                                    tag: "::",
                                                    Arg0: /* tuple */[
                                                      "finally",
                                                      "T_FINALLY"
                                                    ],
                                                    Arg1: /* constructor */{
                                                      tag: "::",
                                                      Arg0: /* tuple */[
                                                        "for",
                                                        "T_FOR"
                                                      ],
                                                      Arg1: /* constructor */{
                                                        tag: "::",
                                                        Arg0: /* tuple */[
                                                          "class",
                                                          "T_CLASS"
                                                        ],
                                                        Arg1: /* constructor */{
                                                          tag: "::",
                                                          Arg0: /* tuple */[
                                                            "extends",
                                                            "T_EXTENDS"
                                                          ],
                                                          Arg1: /* constructor */{
                                                            tag: "::",
                                                            Arg0: /* tuple */[
                                                              "static",
                                                              "T_STATIC"
                                                            ],
                                                            Arg1: /* constructor */{
                                                              tag: "::",
                                                              Arg0: /* tuple */[
                                                                "else",
                                                                "T_ELSE"
                                                              ],
                                                              Arg1: /* constructor */{
                                                                tag: "::",
                                                                Arg0: /* tuple */[
                                                                  "new",
                                                                  "T_NEW"
                                                                ],
                                                                Arg1: /* constructor */{
                                                                  tag: "::",
                                                                  Arg0: /* tuple */[
                                                                    "delete",
                                                                    "T_DELETE"
                                                                  ],
                                                                  Arg1: /* constructor */{
                                                                    tag: "::",
                                                                    Arg0: /* tuple */[
                                                                      "typeof",
                                                                      "T_TYPEOF"
                                                                    ],
                                                                    Arg1: /* constructor */{
                                                                      tag: "::",
                                                                      Arg0: /* tuple */[
                                                                        "void",
                                                                        "T_VOID"
                                                                      ],
                                                                      Arg1: /* constructor */{
                                                                        tag: "::",
                                                                        Arg0: /* tuple */[
                                                                          "enum",
                                                                          "T_ENUM"
                                                                        ],
                                                                        Arg1: /* constructor */{
                                                                          tag: "::",
                                                                          Arg0: /* tuple */[
                                                                            "export",
                                                                            "T_EXPORT"
                                                                          ],
                                                                          Arg1: /* constructor */{
                                                                            tag: "::",
                                                                            Arg0: /* tuple */[
                                                                              "import",
                                                                              "T_IMPORT"
                                                                            ],
                                                                            Arg1: /* constructor */{
                                                                              tag: "::",
                                                                              Arg0: /* tuple */[
                                                                                "super",
                                                                                "T_SUPER"
                                                                              ],
                                                                              Arg1: /* constructor */{
                                                                                tag: "::",
                                                                                Arg0: /* tuple */[
                                                                                  "implements",
                                                                                  "T_IMPLEMENTS"
                                                                                ],
                                                                                Arg1: /* constructor */{
                                                                                  tag: "::",
                                                                                  Arg0: /* tuple */[
                                                                                    "interface",
                                                                                    "T_INTERFACE"
                                                                                  ],
                                                                                  Arg1: /* constructor */{
                                                                                    tag: "::",
                                                                                    Arg0: /* tuple */[
                                                                                      "package",
                                                                                      "T_PACKAGE"
                                                                                    ],
                                                                                    Arg1: /* constructor */{
                                                                                      tag: "::",
                                                                                      Arg0: /* tuple */[
                                                                                        "private",
                                                                                        "T_PRIVATE"
                                                                                      ],
                                                                                      Arg1: /* constructor */{
                                                                                        tag: "::",
                                                                                        Arg0: /* tuple */[
                                                                                          "protected",
                                                                                          "T_PROTECTED"
                                                                                        ],
                                                                                        Arg1: /* constructor */{
                                                                                          tag: "::",
                                                                                          Arg0: /* tuple */[
                                                                                            "public",
                                                                                            "T_PUBLIC"
                                                                                          ],
                                                                                          Arg1: /* constructor */{
                                                                                            tag: "::",
                                                                                            Arg0: /* tuple */[
                                                                                              "yield",
                                                                                              "T_YIELD"
                                                                                            ],
                                                                                            Arg1: /* constructor */{
                                                                                              tag: "::",
                                                                                              Arg0: /* tuple */[
                                                                                                "debugger",
                                                                                                "T_DEBUGGER"
                                                                                              ],
                                                                                              Arg1: /* constructor */{
                                                                                                tag: "::",
                                                                                                Arg0: /* tuple */[
                                                                                                  "declare",
                                                                                                  "T_DECLARE"
                                                                                                ],
                                                                                                Arg1: /* constructor */{
                                                                                                  tag: "::",
                                                                                                  Arg0: /* tuple */[
                                                                                                    "type",
                                                                                                    "T_TYPE"
                                                                                                  ],
                                                                                                  Arg1: /* constructor */{
                                                                                                    tag: "::",
                                                                                                    Arg0: /* tuple */[
                                                                                                      "of",
                                                                                                      "T_OF"
                                                                                                    ],
                                                                                                    Arg1: /* constructor */{
                                                                                                      tag: "::",
                                                                                                      Arg0: /* tuple */[
                                                                                                        "async",
                                                                                                        "T_ASYNC"
                                                                                                      ],
                                                                                                      Arg1: /* constructor */{
                                                                                                        tag: "::",
                                                                                                        Arg0: /* tuple */[
                                                                                                          "await",
                                                                                                          "T_AWAIT"
                                                                                                        ],
                                                                                                        Arg1: "[]"
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
      }), /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "static",
        "T_STATIC"
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "typeof",
          "T_TYPEOF"
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "any",
            "T_ANY_TYPE"
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "bool",
              "T_BOOLEAN_TYPE"
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "boolean",
                "T_BOOLEAN_TYPE"
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "true",
                  "T_TRUE"
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "false",
                    "T_FALSE"
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "number",
                      "T_NUMBER_TYPE"
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "string",
                        "T_STRING_TYPE"
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "void",
                          "T_VOID_TYPE"
                        ],
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: /* tuple */[
                            "null",
                            "T_NULL"
                          ],
                          Arg1: "[]"
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
    switch (__ocaml_lex_state$1) {
      case 0 :
          Lexing.new_line(lexbuf$1);
          return token(env$1, lexbuf$1);
      case 1 :
          var env$2 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* constructor */{
                tag: "UnexpectedToken",
                Arg0: "ILLEGAL"
              });
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
                      "T_COLON"
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
                    "T_MULT"
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
                    "T_ERROR"
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
                  /* constructor */{
                    tag: "T_STRING",
                    Arg0: /* tuple */[
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
          $$Buffer.add_string(literal, Lexing.lexeme(lexbuf$1));
          var start$4 = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var match$5 = template_part(env$1, start$4, cooked, raw$1, literal, lexbuf$1);
          return /* tuple */[
                  match$5[0],
                  /* constructor */{
                    tag: "T_TEMPLATE_PART",
                    Arg0: /* tuple */[
                      match$5[1],
                      /* record */[
                        /* cooked */$$Buffer.contents(cooked),
                        /* raw */$$Buffer.contents(raw$1),
                        /* literal */$$Buffer.contents(literal)
                      ],
                      match$5[2]
                    ]
                  }
                ];
      case 10 :
          var w = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w, /* constructor */{
                      tag: "T_NUMBER",
                      Arg0: "BINARY"
                    });
      case 11 :
          return /* tuple */[
                  env$1,
                  /* constructor */{
                    tag: "T_NUMBER",
                    Arg0: "BINARY"
                  }
                ];
      case 12 :
          var w$1 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$1, /* constructor */{
                      tag: "T_NUMBER",
                      Arg0: "OCTAL"
                    });
      case 13 :
          return /* tuple */[
                  env$1,
                  /* constructor */{
                    tag: "T_NUMBER",
                    Arg0: "OCTAL"
                  }
                ];
      case 14 :
          var w$2 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$2, /* constructor */{
                      tag: "T_NUMBER",
                      Arg0: "LEGACY_OCTAL"
                    });
      case 15 :
          return /* tuple */[
                  env$1,
                  /* constructor */{
                    tag: "T_NUMBER",
                    Arg0: "LEGACY_OCTAL"
                  }
                ];
      case 16 :
      case 18 :
      case 20 :
          break;
      case 17 :
      case 19 :
      case 21 :
          return /* tuple */[
                  env$1,
                  /* constructor */{
                    tag: "T_NUMBER",
                    Arg0: "NORMAL"
                  }
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
                      "T_IDENTIFIER"
                    ];
            } else {
              throw exn;
            }
          }
      case 23 :
          return /* tuple */[
                  env$1,
                  "T_LCURLY"
                ];
      case 24 :
          return /* tuple */[
                  env$1,
                  "T_RCURLY"
                ];
      case 25 :
          return /* tuple */[
                  env$1,
                  "T_LPAREN"
                ];
      case 26 :
          return /* tuple */[
                  env$1,
                  "T_RPAREN"
                ];
      case 27 :
          return /* tuple */[
                  env$1,
                  "T_LBRACKET"
                ];
      case 28 :
          return /* tuple */[
                  env$1,
                  "T_RBRACKET"
                ];
      case 29 :
          return /* tuple */[
                  env$1,
                  "T_ELLIPSIS"
                ];
      case 30 :
          return /* tuple */[
                  env$1,
                  "T_PERIOD"
                ];
      case 31 :
          return /* tuple */[
                  env$1,
                  "T_SEMICOLON"
                ];
      case 32 :
          return /* tuple */[
                  env$1,
                  "T_COMMA"
                ];
      case 33 :
          return /* tuple */[
                  env$1,
                  "T_COLON"
                ];
      case 34 :
          return /* tuple */[
                  env$1,
                  "T_PLING"
                ];
      case 35 :
          return /* tuple */[
                  env$1,
                  "T_AND"
                ];
      case 36 :
          return /* tuple */[
                  env$1,
                  "T_OR"
                ];
      case 37 :
          return /* tuple */[
                  env$1,
                  "T_STRICT_EQUAL"
                ];
      case 38 :
          return /* tuple */[
                  env$1,
                  "T_STRICT_NOT_EQUAL"
                ];
      case 39 :
          return /* tuple */[
                  env$1,
                  "T_LESS_THAN_EQUAL"
                ];
      case 40 :
          return /* tuple */[
                  env$1,
                  "T_GREATER_THAN_EQUAL"
                ];
      case 41 :
          return /* tuple */[
                  env$1,
                  "T_EQUAL"
                ];
      case 42 :
          return /* tuple */[
                  env$1,
                  "T_NOT_EQUAL"
                ];
      case 43 :
          return /* tuple */[
                  env$1,
                  "T_INCR"
                ];
      case 44 :
          return /* tuple */[
                  env$1,
                  "T_DECR"
                ];
      case 45 :
          return /* tuple */[
                  env$1,
                  "T_LSHIFT_ASSIGN"
                ];
      case 46 :
          return /* tuple */[
                  env$1,
                  "T_LSHIFT"
                ];
      case 47 :
          return /* tuple */[
                  env$1,
                  "T_RSHIFT_ASSIGN"
                ];
      case 48 :
          return /* tuple */[
                  env$1,
                  "T_RSHIFT3_ASSIGN"
                ];
      case 49 :
          return /* tuple */[
                  env$1,
                  "T_RSHIFT3"
                ];
      case 50 :
          return /* tuple */[
                  env$1,
                  "T_RSHIFT"
                ];
      case 51 :
          return /* tuple */[
                  env$1,
                  "T_PLUS_ASSIGN"
                ];
      case 52 :
          return /* tuple */[
                  env$1,
                  "T_MINUS_ASSIGN"
                ];
      case 53 :
          return /* tuple */[
                  env$1,
                  "T_MULT_ASSIGN"
                ];
      case 54 :
          return /* tuple */[
                  env$1,
                  "T_EXP_ASSIGN"
                ];
      case 55 :
          return /* tuple */[
                  env$1,
                  "T_MOD_ASSIGN"
                ];
      case 56 :
          return /* tuple */[
                  env$1,
                  "T_BIT_AND_ASSIGN"
                ];
      case 57 :
          return /* tuple */[
                  env$1,
                  "T_BIT_OR_ASSIGN"
                ];
      case 58 :
          return /* tuple */[
                  env$1,
                  "T_BIT_XOR_ASSIGN"
                ];
      case 59 :
          return /* tuple */[
                  env$1,
                  "T_LESS_THAN"
                ];
      case 60 :
          return /* tuple */[
                  env$1,
                  "T_GREATER_THAN"
                ];
      case 61 :
          return /* tuple */[
                  env$1,
                  "T_PLUS"
                ];
      case 62 :
          return /* tuple */[
                  env$1,
                  "T_MINUS"
                ];
      case 63 :
          return /* tuple */[
                  env$1,
                  "T_MULT"
                ];
      case 64 :
          return /* tuple */[
                  env$1,
                  "T_EXP"
                ];
      case 65 :
          return /* tuple */[
                  env$1,
                  "T_MOD"
                ];
      case 66 :
          return /* tuple */[
                  env$1,
                  "T_BIT_OR"
                ];
      case 67 :
          return /* tuple */[
                  env$1,
                  "T_BIT_AND"
                ];
      case 68 :
          return /* tuple */[
                  env$1,
                  "T_BIT_XOR"
                ];
      case 69 :
          return /* tuple */[
                  env$1,
                  "T_NOT"
                ];
      case 70 :
          return /* tuple */[
                  env$1,
                  "T_BIT_NOT"
                ];
      case 71 :
          return /* tuple */[
                  env$1,
                  "T_ASSIGN"
                ];
      case 72 :
          return /* tuple */[
                  env$1,
                  "T_ARROW"
                ];
      case 73 :
          return /* tuple */[
                  env$1,
                  "T_DIV_ASSIGN"
                ];
      case 74 :
          return /* tuple */[
                  env$1,
                  "T_DIV"
                ];
      case 75 :
          return /* tuple */[
                  env$1,
                  "T_AT"
                ];
      case 76 :
          var env$9;
          if (env$1[/* lex_in_comment_syntax */2]) {
            var loc$1 = from_lb(env$1[/* lex_source */0], lexbuf$1);
            env$9 = lex_error(env$1, loc$1, "UnexpectedEOS");
          } else {
            env$9 = env$1;
          }
          return /* tuple */[
                  env$9,
                  "T_EOF"
                ];
      case 77 :
          var env$10 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* constructor */{
                tag: "UnexpectedToken",
                Arg0: "ILLEGAL"
              });
          return /* tuple */[
                  env$10,
                  "T_ERROR"
                ];
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
        ___ocaml_lex_state = __ocaml_lex_state$1;
        continue ;
    }
    var w$3 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
    return illegal_number(env$1, lexbuf$1, w$3, /* constructor */{
                tag: "T_NUMBER",
                Arg0: "NORMAL"
              });
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
          switch (mode$1) {
            case "JSX_SINGLE_QUOTED_TEXT" :
                if (c === 39) {
                  return /* tuple */[
                          env$1,
                          from_lb(env$1[/* lex_source */0], lexbuf$1)
                        ];
                }
                break;
            case "JSX_DOUBLE_QUOTED_TEXT" :
                if (c === 34) {
                  return /* tuple */[
                          env$1,
                          from_lb(env$1[/* lex_source */0], lexbuf$1)
                        ];
                }
                break;
            case "JSX_CHILD_TEXT" :
                var exit = 0;
                if (!(c !== 60 && c !== 123)) {
                  exit = 2;
                }
                if (exit === 2) {
                  back(lexbuf$1);
                  return /* tuple */[
                          env$1,
                          from_lb(env$1[/* lex_source */0], lexbuf$1)
                        ];
                }
                break;
            
          }
          $$Buffer.add_char(raw$1, c);
          $$Buffer.add_char(buf$1, c);
          return jsx_text(env$1, mode$1, buf$1, raw$1, lexbuf$1);
      case 1 :
          var env$2 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* constructor */{
                tag: "UnexpectedToken",
                Arg0: "ILLEGAL"
              });
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
                  /* constructor */{
                    tag: "T_TEMPLATE_PART",
                    Arg0: /* tuple */[
                      match$2[1],
                      /* record */[
                        /* cooked */$$Buffer.contents(cooked),
                        /* raw */$$Buffer.contents(raw),
                        /* literal */$$Buffer.contents(literal)
                      ],
                      match$2[2]
                    ]
                  }
                ];
      case 5 :
          var env$3 = lex_error(env, from_lb(env[/* lex_source */0], lexbuf), /* constructor */{
                tag: "UnexpectedToken",
                Arg0: "ILLEGAL"
              });
          return /* tuple */[
                  env$3,
                  /* constructor */{
                    tag: "T_TEMPLATE_PART",
                    Arg0: /* tuple */[
                      from_lb(env$3[/* lex_source */0], lexbuf),
                      /* record */[
                        /* cooked */"",
                        /* raw */"",
                        /* literal */""
                      ],
                      true
                    ]
                  }
                ];
      default:
        Curry._1(lexbuf[/* refill_buff */0], lexbuf);
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
          var env$2 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* constructor */{
                tag: "UnexpectedToken",
                Arg0: "ILLEGAL"
              });
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
          var env$2 = code$6 > 1114111 ? lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* constructor */{
                  tag: "UnexpectedToken",
                  Arg0: "ILLEGAL"
                }) : env$1;
          List.iter((function (param) {
                  return $$Buffer.add_char(buf$1, param);
                }), utf16to8(code$6));
          return /* tuple */[
                  env$2,
                  false
                ];
      case 15 :
          var c$2 = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          var env$3 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* constructor */{
                tag: "UnexpectedToken",
                Arg0: "ILLEGAL"
              });
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

function regexp_class(env, buf, lexbuf) {
  var env$1 = env;
  var buf$1 = buf;
  var lexbuf$1 = lexbuf;
  var ___ocaml_lex_state = 326;
  while(true) {
    var __ocaml_lex_state = ___ocaml_lex_state;
    var __ocaml_lex_state$1 = Lexing.engine(__ocaml_lex_tables, __ocaml_lex_state, lexbuf$1);
    switch (__ocaml_lex_state$1) {
      case 0 :
          return env$1;
      case 1 :
      case 2 :
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
    var s = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], lexbuf$1[/* lex_start_pos */4] + 2 | 0);
    $$Buffer.add_string(buf$1, s);
    return regexp_class(env$1, buf$1, lexbuf$1);
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
          var env$2 = lex_error(env$1, loc, "UnterminatedRegExp");
          return /* tuple */[
                  env$2,
                  ""
                ];
      case 1 :
          var loc$1 = from_lb(env$1[/* lex_source */0], lexbuf$1);
          var env$3 = lex_error(env$1, loc$1, "UnterminatedRegExp");
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
          var env$5 = lex_error(env$1, loc$2, "UnterminatedRegExp");
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
          var env$2 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* constructor */{
                tag: "UnexpectedToken",
                Arg0: "ILLEGAL"
              });
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
          var env$2 = lex_error(env$1, from_lb(env$1[/* lex_source */0], lexbuf$1), /* constructor */{
                tag: "UnexpectedToken",
                Arg0: "ILLEGAL"
              });
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
                      "T_COLON"
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
                    "T_MULT"
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
                  /* constructor */{
                    tag: "T_STRING",
                    Arg0: /* tuple */[
                      btwn(start$3, match$3[1]),
                      $$Buffer.contents(buf$3),
                      $$Buffer.contents(raw),
                      match$3[2]
                    ]
                  }
                ];
      case 7 :
          var neg = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w, mk_num_singleton("BINARY", num, neg));
      case 8 :
          var neg$1 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$1 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return /* tuple */[
                  env$1,
                  mk_num_singleton("BINARY", num$1, neg$1)
                ];
      case 9 :
          var neg$2 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$2 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w$1 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$1, mk_num_singleton("OCTAL", num$2, neg$2));
      case 10 :
          var neg$3 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$3 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return /* tuple */[
                  env$1,
                  mk_num_singleton("OCTAL", num$3, neg$3)
                ];
      case 11 :
          var neg$4 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$4 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w$2 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$2, mk_num_singleton("LEGACY_OCTAL", num$4, neg$4));
      case 12 :
          var neg$5 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$5 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return /* tuple */[
                  env$1,
                  mk_num_singleton("LEGACY_OCTAL", num$5, neg$5)
                ];
      case 13 :
          var neg$6 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$6 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w$3 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          var match$4;
          try {
            match$4 = /* tuple */[
              env$1,
              mk_num_singleton("NORMAL", num$6, neg$6)
            ];
          }
          catch (exn){
            if (Sys.win32) {
              var loc$1 = from_lb(env$1[/* lex_source */0], lexbuf$1);
              var env$8 = lex_error(env$1, loc$1, "WindowsFloatOfString");
              match$4 = /* tuple */[
                env$8,
                /* constructor */{
                  tag: "T_NUMBER_SINGLETON_TYPE",
                  Arg0: "NORMAL",
                  Arg1: 789.0
                }
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
                    mk_num_singleton("NORMAL", num$7, neg$7)
                  ];
          }
          catch (exn$1){
            if (Sys.win32) {
              var loc$2 = from_lb(env$1[/* lex_source */0], lexbuf$1);
              var env$9 = lex_error(env$1, loc$2, "WindowsFloatOfString");
              return /* tuple */[
                      env$9,
                      /* constructor */{
                        tag: "T_NUMBER_SINGLETON_TYPE",
                        Arg0: "NORMAL",
                        Arg1: 789.0
                      }
                    ];
            } else {
              throw exn$1;
            }
          }
      case 15 :
          var neg$8 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$8 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w$4 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$4, mk_num_singleton("NORMAL", num$8, neg$8));
      case 16 :
          var neg$9 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$9 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), lexbuf$1[/* lex_curr_pos */5]);
          return /* tuple */[
                  env$1,
                  mk_num_singleton("NORMAL", num$9, neg$9)
                ];
      case 17 :
          var neg$10 = Lexing.sub_lexeme(lexbuf$1, lexbuf$1[/* lex_start_pos */4], Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$10 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1));
          var w$5 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), lexbuf$1[/* lex_curr_pos */5]);
          return illegal_number(env$1, lexbuf$1, w$5, mk_num_singleton("NORMAL", num$10, neg$10));
      case 18 :
          var neg$11 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 1), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 0));
          var num$11 = Lexing.sub_lexeme(lexbuf$1, Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 3), Caml_array.caml_array_get(lexbuf$1[/* lex_mem */9], 2));
          return /* tuple */[
                  env$1,
                  mk_num_singleton("NORMAL", num$11, neg$11)
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
                      "T_IDENTIFIER"
                    ];
            } else {
              throw exn$2;
            }
          }
      case 22 :
          return /* tuple */[
                  env$1,
                  "T_LCURLY"
                ];
      case 23 :
          return /* tuple */[
                  env$1,
                  "T_RCURLY"
                ];
      case 24 :
          return /* tuple */[
                  env$1,
                  "T_LPAREN"
                ];
      case 25 :
          return /* tuple */[
                  env$1,
                  "T_RPAREN"
                ];
      case 26 :
          return /* tuple */[
                  env$1,
                  "T_ELLIPSIS"
                ];
      case 27 :
          return /* tuple */[
                  env$1,
                  "T_PERIOD"
                ];
      case 28 :
          return /* tuple */[
                  env$1,
                  "T_SEMICOLON"
                ];
      case 29 :
          return /* tuple */[
                  env$1,
                  "T_COMMA"
                ];
      case 20 :
      case 32 :
          return /* tuple */[
                  env$1,
                  "T_LBRACKET"
                ];
      case 21 :
      case 33 :
          return /* tuple */[
                  env$1,
                  "T_RBRACKET"
                ];
      case 34 :
          return /* tuple */[
                  env$1,
                  "T_LESS_THAN"
                ];
      case 35 :
          return /* tuple */[
                  env$1,
                  "T_GREATER_THAN"
                ];
      case 31 :
      case 37 :
          return /* tuple */[
                  env$1,
                  "T_PLING"
                ];
      case 38 :
          return /* tuple */[
                  env$1,
                  "T_MULT"
                ];
      case 30 :
      case 39 :
          return /* tuple */[
                  env$1,
                  "T_COLON"
                ];
      case 40 :
          return /* tuple */[
                  env$1,
                  "T_BIT_OR"
                ];
      case 41 :
          return /* tuple */[
                  env$1,
                  "T_BIT_AND"
                ];
      case 42 :
          return /* tuple */[
                  env$1,
                  "T_TYPEOF"
                ];
      case 43 :
          return /* tuple */[
                  env$1,
                  "T_ARROW"
                ];
      case 36 :
      case 44 :
          return /* tuple */[
                  env$1,
                  "T_ASSIGN"
                ];
      case 45 :
          return /* tuple */[
                  env$1,
                  "T_PLUS"
                ];
      case 46 :
          return /* tuple */[
                  env$1,
                  "T_MINUS"
                ];
      case 47 :
          var env$10;
          if (env$1[/* lex_in_comment_syntax */2]) {
            var loc$3 = from_lb(env$1[/* lex_source */0], lexbuf$1);
            env$10 = lex_error(env$1, loc$3, "UnexpectedEOS");
          } else {
            env$10 = env$1;
          }
          return /* tuple */[
                  env$10,
                  "T_EOF"
                ];
      case 48 :
          return /* tuple */[
                  env$1,
                  "T_ERROR"
                ];
      default:
        Curry._1(lexbuf$1[/* refill_buff */0], lexbuf$1);
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
          return /* tuple */[
                  env,
                  "T_EOF"
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
                  /* constructor */{
                    tag: "T_REGEXP",
                    Arg0: /* tuple */[
                      loc,
                      $$Buffer.contents(buf$2),
                      match$2[1]
                    ]
                  }
                ];
      case 6 :
          var env$4 = lex_error(env, from_lb(env[/* lex_source */0], lexbuf), /* constructor */{
                tag: "UnexpectedToken",
                Arg0: "ILLEGAL"
              });
          return /* tuple */[
                  env$4,
                  "T_ERROR"
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
                  "T_EOF"
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
                  "T_LESS_THAN"
                ];
      case 6 :
          return /* tuple */[
                  env,
                  "T_DIV"
                ];
      case 7 :
          return /* tuple */[
                  env,
                  "T_GREATER_THAN"
                ];
      case 8 :
          return /* tuple */[
                  env,
                  "T_LCURLY"
                ];
      case 9 :
          return /* tuple */[
                  env,
                  "T_COLON"
                ];
      case 10 :
          return /* tuple */[
                  env,
                  "T_PERIOD"
                ];
      case 11 :
          return /* tuple */[
                  env,
                  "T_ASSIGN"
                ];
      case 12 :
          unicode_fix_cols(lexbuf);
          return /* tuple */[
                  env,
                  "T_JSX_IDENTIFIER"
                ];
      case 13 :
          var quote = Caml_bytes.get(lexbuf[/* lex_buffer */1], lexbuf[/* lex_start_pos */4]);
          var start$2 = from_lb(env[/* lex_source */0], lexbuf);
          var buf$2 = $$Buffer.create(127);
          var raw = $$Buffer.create(127);
          $$Buffer.add_char(raw, quote);
          var mode = quote === /* "'" */39 ? "JSX_SINGLE_QUOTED_TEXT" : "JSX_DOUBLE_QUOTED_TEXT";
          var match$2 = jsx_text(env, mode, buf$2, raw, lexbuf);
          $$Buffer.add_char(raw, quote);
          var value = $$Buffer.contents(buf$2);
          var raw$1 = $$Buffer.contents(raw);
          return /* tuple */[
                  match$2[0],
                  /* constructor */{
                    tag: "T_JSX_TEXT",
                    Arg0: /* tuple */[
                      btwn(start$2, match$2[1]),
                      value,
                      raw$1
                    ]
                  }
                ];
      case 14 :
          return /* tuple */[
                  env,
                  "T_ERROR"
                ];
      default:
        Curry._1(lexbuf[/* refill_buff */0], lexbuf);
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
          var match = jsx_text(env$1, "JSX_CHILD_TEXT", buf$1, raw$1, lexbuf$1);
          var value = $$Buffer.contents(buf$1);
          var raw$2 = $$Buffer.contents(raw$1);
          return /* tuple */[
                  match[0],
                  /* constructor */{
                    tag: "T_JSX_TEXT",
                    Arg0: /* tuple */[
                      btwn(start$1, match[1]),
                      value,
                      raw$2
                    ]
                  }
                ];
      case 1 :
          return /* tuple */[
                  env$1,
                  "T_EOF"
                ];
      case 2 :
          return /* tuple */[
                  env$1,
                  "T_LESS_THAN"
                ];
      case 3 :
          return /* tuple */[
                  env$1,
                  "T_LCURLY"
                ];
      case 4 :
          var c = Caml_bytes.get(lexbuf$1[/* lex_buffer */1], lexbuf$1[/* lex_start_pos */4]);
          $$Buffer.add_char(raw$1, c);
          $$Buffer.add_char(buf$1, c);
          var match$1 = jsx_text(env$1, "JSX_CHILD_TEXT", buf$1, raw$1, lexbuf$1);
          var value$1 = $$Buffer.contents(buf$1);
          var raw$3 = $$Buffer.contents(raw$1);
          return /* tuple */[
                  match$1[0],
                  /* constructor */{
                    tag: "T_JSX_TEXT",
                    Arg0: /* tuple */[
                      btwn(start$1, match$1[1]),
                      value$1,
                      raw$3
                    ]
                  }
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
  if (param !== "Empty") {
    return param.Arg3;
  } else {
    return 0;
  }
}

function create(l, v, r) {
  var hl = l !== "Empty" ? l.Arg3 : 0;
  var hr = r !== "Empty" ? r.Arg3 : 0;
  return /* constructor */{
          tag: "Node",
          Arg0: l,
          Arg1: v,
          Arg2: r,
          Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal(l, v, r) {
  var hl = l !== "Empty" ? l.Arg3 : 0;
  var hr = r !== "Empty" ? r.Arg3 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height(ll) >= height(lr)) {
        return create(ll, lv, create(lr, v, r));
      } else if (lr !== "Empty") {
        return create(create(ll, lv, lr.Arg0), lr.Arg1, create(lr.Arg2, v, r));
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
    if (r !== "Empty") {
      var rr = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height(rr) >= height(rl)) {
        return create(create(l, v, rl), rv, rr);
      } else if (rl !== "Empty") {
        return create(create(l, v, rl.Arg0), rl.Arg1, create(rl.Arg2, rv, rr));
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
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: v,
            Arg2: r,
            Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
}

function add(x, t) {
  if (t !== "Empty") {
    var r = t.Arg2;
    var v = t.Arg1;
    var l = t.Arg0;
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return t;
    } else if (c < 0) {
      return bal(add(x, l), v, r);
    } else {
      return bal(l, v, add(x, r));
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: "Empty",
            Arg3: 1
          };
  }
}

function mem(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = Caml_primitive.caml_string_compare(x, param.Arg1);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg2;
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
    case "TYPE" :
        match$1 = type_token$1(lex_env);
        break;
    case "JSX_TAG" :
        match$1 = jsx_tag(lex_env);
        break;
    case "JSX_CHILD" :
        match$1 = jsx_child$1(lex_env);
        break;
    case "TEMPLATE" :
        match$1 = template_tail(lex_env);
        break;
    case "REGEXP" :
        match$1 = regexp(lex_env);
        break;
    case "NORMAL" :
    case "PREDICATE" :
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
    if (typeof match !== "string") {
      var init = lb[/* lex_curr_p */11];
      lb[/* lex_curr_p */11] = /* record */[
        /* pos_fname */match.Arg0,
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
          /* errors : record */[/* contents */"[]"],
          /* comments : record */[/* contents */"[]"],
          /* labels */"Empty",
          /* exports : record */[/* contents */"Empty"],
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
          /* lex_mode_stack : record */[/* contents : constructor */{
              tag: "::",
              Arg0: "NORMAL",
              Arg1: "[]"
            }],
          /* lex_env : record */[/* contents */lex_env],
          /* lookahead : record */[/* contents */create$1(lex_env, "NORMAL")],
          /* token_sink : record */[/* contents */token_sink],
          /* parse_options */parse_options$1,
          /* source */source
        ];
}

function error_at(env, param) {
  var e = param[1];
  env[/* errors */0][0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      param[0],
      e
    ],
    Arg1: env[/* errors */0][0]
  };
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
                    env[/* comments */1][0] = /* constructor */{
                      tag: "::",
                      Arg0: c,
                      Arg1: env[/* comments */1][0]
                    };
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
                /* constructor */{
                  tag: "DuplicateExport",
                  Arg0: export_name
                }
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
  newrecord[/* labels */2] = "Empty";
  newrecord[/* in_loop */7] = false;
  newrecord[/* in_switch */8] = false;
  newrecord[/* in_function */9] = true;
  newrecord[/* allow_yield */13] = generator;
  newrecord[/* allow_await */14] = async;
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
  if (typeof match === "string") {
    switch (match) {
      case "T_SEMICOLON" :
          return false;
      case "T_RCURLY" :
      case "T_EOF" :
          return true;
      default:
        return is_line_terminator(env);
    }
  } else {
    return is_line_terminator(env);
  }
}

function semicolon_loc($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  if (token$2(i, env) === "T_SEMICOLON") {
    return loc(i, env);
  }
  
}

function is_identifier($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  var name = value(i, env);
  var match = token$2(i, env);
  if (is_strict_reserved(name) || is_restricted(name) || is_future_reserved(name)) {
    return true;
  } else if (typeof match === "string") {
    switch (match) {
      case "T_IDENTIFIER" :
      case "T_LET" :
      case "T_DECLARE" :
      case "T_TYPE" :
      case "T_OF" :
      case "T_ASYNC" :
      case "T_AWAIT" :
          return true;
      default:
        return false;
    }
  } else {
    return false;
  }
}

function is_function($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  if (token$2(i, env) === "T_FUNCTION") {
    return true;
  } else if (token$2(i, env) === "T_ASYNC") {
    return token$2(i + 1 | 0, env) === "T_FUNCTION";
  } else {
    return false;
  }
}

function is_class($staropt$star, env) {
  var i = $staropt$star !== undefined ? $staropt$star : 0;
  var match = token$2(i, env);
  if (typeof match === "string") {
    switch (match) {
      case "T_AT" :
      case "T_CLASS" :
          return true;
      default:
        return false;
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
  var tmp = param[0];
  if (typeof tmp === "string") {
    switch (tmp) {
      case "T_IDENTIFIER" :
          return "UnexpectedIdentifier";
      case "T_EOF" :
          return "UnexpectedEOS";
      default:
        
    }
  } else {
    switch (/* XXX */tmp.tag) {
      case "T_NUMBER" :
          return "UnexpectedNumber";
      case "T_STRING" :
      case "T_JSX_TEXT" :
          return "UnexpectedString";
      default:
        
    }
  }
  var word = param[1];
  if (is_future_reserved(word)) {
    return "UnexpectedReserved";
  } else if (is_strict_reserved(word)) {
    return "StrictReservedWord";
  } else {
    return /* constructor */{
            tag: "UnexpectedToken",
            Arg0: word
          };
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
                                "UnsupportedDecorator"
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
  env[/* lex_mode_stack */16][0] = /* constructor */{
    tag: "::",
    Arg0: mode,
    Arg1: env[/* lex_mode_stack */16][0]
  };
  env[/* lookahead */18][0] = create$1(env[/* lex_env */17][0], List.hd(env[/* lex_mode_stack */16][0]));
  return /* () */0;
}

function pop_lex_mode(env) {
  var match = env[/* lex_mode_stack */16][0];
  var new_stack;
  if (match !== "[]") {
    new_stack = match.Arg1;
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
  if (match !== "[]") {
    var match$1 = match.Arg1;
    if (match$1 !== "[]") {
      new_stack = match$1.Arg1;
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
  } else if (token$2(undefined, env) === "T_SEMICOLON") {
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
      /* tail */undefined
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
    return /* constructor */{
            tag: "ParsedSuccessfully",
            Arg0: result
          };
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
      return "FailedToParse";
    } else {
      throw exn;
    }
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
  if (param !== "Empty") {
    return param.Arg3;
  } else {
    return 0;
  }
}

function create$2(l, v, r) {
  var hl = l !== "Empty" ? l.Arg3 : 0;
  var hr = r !== "Empty" ? r.Arg3 : 0;
  return /* constructor */{
          tag: "Node",
          Arg0: l,
          Arg1: v,
          Arg2: r,
          Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal$1(l, v, r) {
  var hl = l !== "Empty" ? l.Arg3 : 0;
  var hr = r !== "Empty" ? r.Arg3 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height$1(ll) >= height$1(lr)) {
        return create$2(ll, lv, create$2(lr, v, r));
      } else if (lr !== "Empty") {
        return create$2(create$2(ll, lv, lr.Arg0), lr.Arg1, create$2(lr.Arg2, v, r));
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
    if (r !== "Empty") {
      var rr = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height$1(rr) >= height$1(rl)) {
        return create$2(create$2(l, v, rl), rv, rr);
      } else if (rl !== "Empty") {
        return create$2(create$2(l, v, rl.Arg0), rl.Arg1, create$2(rl.Arg2, rv, rr));
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
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: v,
            Arg2: r,
            Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
}

function add$1(x, t) {
  if (t !== "Empty") {
    var r = t.Arg2;
    var v = t.Arg1;
    var l = t.Arg0;
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return t;
    } else if (c < 0) {
      return bal$1(add$1(x, l), v, r);
    } else {
      return bal$1(l, v, add$1(x, r));
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: "Empty",
            Arg3: 1
          };
  }
}

function mem$1(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = Caml_primitive.caml_string_compare(x, param.Arg1);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg2;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function height$2(param) {
  if (param !== "Empty") {
    return param.Arg4;
  } else {
    return 0;
  }
}

function create$3(l, x, d, r) {
  var hl = height$2(l);
  var hr = height$2(r);
  return /* constructor */{
          tag: "Node",
          Arg0: l,
          Arg1: x,
          Arg2: d,
          Arg3: r,
          Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal$2(l, x, d, r) {
  var hl = l !== "Empty" ? l.Arg4 : 0;
  var hr = r !== "Empty" ? r.Arg4 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg3;
      var ld = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height$2(ll) >= height$2(lr)) {
        return create$3(ll, lv, ld, create$3(lr, x, d, r));
      } else if (lr !== "Empty") {
        return create$3(create$3(ll, lv, ld, lr.Arg0), lr.Arg1, lr.Arg2, create$3(lr.Arg3, x, d, r));
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
    if (r !== "Empty") {
      var rr = r.Arg3;
      var rd = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height$2(rr) >= height$2(rl)) {
        return create$3(create$3(l, x, d, rl), rv, rd, rr);
      } else if (rl !== "Empty") {
        return create$3(create$3(l, x, d, rl.Arg0), rl.Arg1, rl.Arg2, create$3(rl.Arg3, rv, rd, rr));
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
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: x,
            Arg2: d,
            Arg3: r,
            Arg4: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
}

function add$2(x, data, param) {
  if (param !== "Empty") {
    var r = param.Arg3;
    var d = param.Arg2;
    var v = param.Arg1;
    var l = param.Arg0;
    var c = Caml_primitive.caml_string_compare(x, v);
    if (c === 0) {
      return /* constructor */{
              tag: "Node",
              Arg0: l,
              Arg1: x,
              Arg2: data,
              Arg3: r,
              Arg4: param.Arg4
            };
    } else if (c < 0) {
      return bal$2(add$2(x, data, l), v, d, r);
    } else {
      return bal$2(l, v, d, add$2(x, data, r));
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: data,
            Arg3: "Empty",
            Arg4: 1
          };
  }
}

function find(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = Caml_primitive.caml_string_compare(x, param.Arg1);
      if (c === 0) {
        return param.Arg2;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg3;
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
  if (param !== "Empty") {
    return param.Arg3;
  } else {
    return 0;
  }
}

function create$4(l, v, r) {
  var hl = l !== "Empty" ? l.Arg3 : 0;
  var hr = r !== "Empty" ? r.Arg3 : 0;
  return /* constructor */{
          tag: "Node",
          Arg0: l,
          Arg1: v,
          Arg2: r,
          Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
        };
}

function bal$3(l, v, r) {
  var hl = l !== "Empty" ? l.Arg3 : 0;
  var hr = r !== "Empty" ? r.Arg3 : 0;
  if (hl > (hr + 2 | 0)) {
    if (l !== "Empty") {
      var lr = l.Arg2;
      var lv = l.Arg1;
      var ll = l.Arg0;
      if (height$3(ll) >= height$3(lr)) {
        return create$4(ll, lv, create$4(lr, v, r));
      } else if (lr !== "Empty") {
        return create$4(create$4(ll, lv, lr.Arg0), lr.Arg1, create$4(lr.Arg2, v, r));
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
    if (r !== "Empty") {
      var rr = r.Arg2;
      var rv = r.Arg1;
      var rl = r.Arg0;
      if (height$3(rr) >= height$3(rl)) {
        return create$4(create$4(l, v, rl), rv, rr);
      } else if (rl !== "Empty") {
        return create$4(create$4(l, v, rl.Arg0), rl.Arg1, create$4(rl.Arg2, rv, rr));
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
    return /* constructor */{
            tag: "Node",
            Arg0: l,
            Arg1: v,
            Arg2: r,
            Arg3: hl >= hr ? hl + 1 | 0 : hr + 1 | 0
          };
  }
}

function add$3(x, t) {
  if (t !== "Empty") {
    var r = t.Arg2;
    var v = t.Arg1;
    var l = t.Arg0;
    var c = compare$1(x, v);
    if (c === 0) {
      return t;
    } else if (c < 0) {
      return bal$3(add$3(x, l), v, r);
    } else {
      return bal$3(l, v, add$3(x, r));
    }
  } else {
    return /* constructor */{
            tag: "Node",
            Arg0: "Empty",
            Arg1: x,
            Arg2: "Empty",
            Arg3: 1
          };
  }
}

function mem$2(x, _param) {
  while(true) {
    var param = _param;
    if (param !== "Empty") {
      var c = compare$1(x, param.Arg1);
      if (c === 0) {
        return true;
      } else {
        _param = c < 0 ? param.Arg0 : param.Arg2;
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
                    /* constructor */{
                      tag: "::",
                      Arg0: err,
                      Arg1: deduped
                    }
                  ];
          }
        }), /* tuple */[
        "Empty",
        "[]"
      ], errs$1);
  return List.rev(match[1]);
}

function with_loc(fn, env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var result = Curry._1(fn, env);
  var match = env[/* last_loc */4][0];
  var end_loc = match !== undefined ? match : (error$1(env, /* constructor */{
            tag: "Assertion",
            Arg0: "did not consume any tokens"
          }), Curry._2(Parser_env_Peek.loc, undefined, env));
  return /* tuple */[
          btwn(start_loc, end_loc),
          result
        ];
}

var Parse = Caml_module.init_mod([
      "parser_flow.ml",
      95,
      6
    ], /* constructor */{
      tag: "Module",
      Arg0: [
        [
          "Function",
          "program"
        ],
        [
          "Function",
          "statement"
        ],
        [
          "Function",
          "statement_list_item"
        ],
        [
          "Function",
          "statement_list"
        ],
        [
          "Function",
          "statement_list_with_directives"
        ],
        [
          "Function",
          "module_body"
        ],
        [
          "Function",
          "expression"
        ],
        [
          "Function",
          "assignment"
        ],
        [
          "Function",
          "object_initializer"
        ],
        [
          "Function",
          "array_initializer"
        ],
        [
          "Function",
          "identifier"
        ],
        [
          "Function",
          "identifier_or_reserved_keyword"
        ],
        [
          "Function",
          "identifier_with_type"
        ],
        [
          "Function",
          "block_body"
        ],
        [
          "Function",
          "function_block_body"
        ],
        [
          "Function",
          "jsx_element"
        ],
        [
          "Function",
          "pattern"
        ],
        [
          "Function",
          "pattern_from_expr"
        ],
        [
          "Function",
          "object_key"
        ],
        [
          "Function",
          "class_declaration"
        ],
        [
          "Function",
          "class_expression"
        ],
        [
          "Function",
          "is_assignable_lhs"
        ],
        [
          "Function",
          "predicate"
        ]
      ]
    });

function intersection(env) {
  maybe(env, "T_BIT_AND");
  var left = prefix(env);
  return Curry._2(intersection_with, env, left);
}

function rev_nonempty_acc(acc) {
  var end_loc;
  if (acc !== "[]") {
    end_loc = acc.Arg0[0];
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
  if (acc$1 !== "[]") {
    start_loc = acc$1.Arg0[0];
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

function function_param_list(env) {
  token$4(env, "T_LPAREN");
  var ret = Curry._2(function_param_list_without_parens, env, "[]");
  token$4(env, "T_RPAREN");
  return ret;
}

function union(env) {
  maybe(env, "T_BIT_OR");
  var left = intersection(env);
  return Curry._2(union_with, env, left);
}

function prefix(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string" && match === "T_PLING") {
    var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    token$4(env, "T_PLING");
    var t = prefix(env);
    return /* tuple */[
            btwn(loc, t[0]),
            /* constructor */{
              tag: "Nullable",
              Arg0: t
            }
          ];
  } else {
    var env$1 = env;
    var t$1 = primary(env$1);
    return postfix_with(env$1, t$1);
  }
}

function primitive(param) {
  if (typeof param === "string") {
    switch (param) {
      case "T_NULL" :
          return "Null";
      case "T_ANY_TYPE" :
          return "Any";
      case "T_BOOLEAN_TYPE" :
          return "Boolean";
      case "T_NUMBER_TYPE" :
          return "Number";
      case "T_STRING_TYPE" :
          return "String";
      case "T_VOID_TYPE" :
          return "Void";
      default:
        return ;
    }
  }
  
}

function function_param_or_generic_type(env) {
  var id = Curry._2(Parse.identifier, undefined, env);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var exit = 0;
  if (typeof match === "string") {
    switch (match) {
      case "T_PLING" :
      case "T_COLON" :
          exit = 2;
          break;
      default:
        exit = 1;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 :
        return /* constructor */{
                tag: "Type",
                Arg0: Curry._2(union_with, env, Curry._2(intersection_with, env, postfix_with(env, generic_type_with_identifier(env, id))))
              };
    case 2 :
        var param = function_param_with_id(env, id);
        maybe(env, "T_COMMA");
        return /* constructor */{
                tag: "ParamList",
                Arg0: Curry._2(function_param_list_without_parens, env, /* constructor */{
                      tag: "::",
                      Arg0: param,
                      Arg1: "[]"
                    })
              };
    
  }
}

function generic_type_with_identifier(env, id) {
  var match = Curry._2(raw_generic_with_identifier, env, id);
  return /* tuple */[
          match[0],
          /* constructor */{
            tag: "Generic",
            Arg0: match[1]
          }
        ];
}

function postfix_with(env, _t) {
  while(true) {
    var t = _t;
    if (!Curry._1(Parser_env_Peek.is_line_terminator, env) && maybe(env, "T_LBRACKET")) {
      var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, "T_RBRACKET");
      var loc = btwn(t[0], end_loc);
      var t_001 = /* constructor */{
        tag: "Array",
        Arg0: t
      };
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

function function_param_with_id(env, name) {
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, "UnexpectedTypeAnnotation");
  }
  var optional = maybe(env, "T_PLING");
  token$4(env, "T_COLON");
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

function primary(env) {
  var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var token$5 = Curry._2(Parser_env_Peek.token, undefined, env);
  var exit = 0;
  if (typeof token$5 === "string") {
    switch (token$5) {
      case "T_IDENTIFIER" :
          var match = generic(env);
          return /* tuple */[
                  match[0],
                  /* constructor */{
                    tag: "Generic",
                    Arg0: match[1]
                  }
                ];
      case "T_LCURLY" :
          var match$1 = Curry._2(_object, undefined, env);
          return /* tuple */[
                  match$1[0],
                  /* constructor */{
                    tag: "Object",
                    Arg0: match$1[1]
                  }
                ];
      case "T_LPAREN" :
          var env$1 = env;
          var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
          var match$2 = param_list_or_type(env$1);
          if (/* XXX */match$2.tag === "ParamList") {
            var match$3 = match$2.Arg0;
            token$4(env$1, "T_ARROW");
            var returnType = union(env$1);
            var end_loc = returnType[0];
            return /* tuple */[
                    btwn(start_loc, end_loc),
                    /* constructor */{
                      tag: "Function",
                      Arg0: /* record */[
                        /* params */match$3[1],
                        /* returnType */returnType,
                        /* rest */match$3[0],
                        /* typeParameters */undefined
                      ]
                    }
                  ];
          } else {
            return match$2.Arg0;
          }
      case "T_LBRACKET" :
          var env$2 = env;
          var start_loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env$2);
          token$4(env$2, "T_LBRACKET");
          var tl = types(env$2, "[]");
          var end_loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env$2);
          token$4(env$2, "T_RBRACKET");
          return /* tuple */[
                  btwn(start_loc$1, end_loc$1),
                  /* constructor */{
                    tag: "Tuple",
                    Arg0: tl
                  }
                ];
      case "T_FALSE" :
      case "T_TRUE" :
          exit = 2;
          break;
      case "T_TYPEOF" :
          var start_loc$2 = Curry._2(Parser_env_Peek.loc, undefined, env);
          token$4(env, "T_TYPEOF");
          var t = primary(env);
          return /* tuple */[
                  btwn(start_loc$2, t[0]),
                  /* constructor */{
                    tag: "Typeof",
                    Arg0: t
                  }
                ];
      case "T_LESS_THAN" :
          var env$3 = env;
          var start_loc$3 = Curry._2(Parser_env_Peek.loc, undefined, env$3);
          var typeParameters = Curry._2(type_parameter_declaration, false, env$3);
          var match$4 = function_param_list(env$3);
          token$4(env$3, "T_ARROW");
          var returnType$1 = union(env$3);
          var end_loc$2 = returnType$1[0];
          return /* tuple */[
                  btwn(start_loc$3, end_loc$2),
                  /* constructor */{
                    tag: "Function",
                    Arg0: /* record */[
                      /* params */match$4[1],
                      /* returnType */returnType$1,
                      /* rest */match$4[0],
                      /* typeParameters */typeParameters
                    ]
                  }
                ];
      case "T_MULT" :
          token$4(env, "T_MULT");
          return /* tuple */[
                  loc,
                  "Exists"
                ];
      default:
        exit = 1;
    }
  } else {
    switch (/* XXX */token$5.tag) {
      case "T_STRING" :
          var match$5 = token$5.Arg0;
          var octal = match$5[3];
          var raw = match$5[2];
          var value = match$5[1];
          var loc$1 = match$5[0];
          if (octal) {
            strict_error(env, "StrictOctalLiteral");
          }
          token$4(env, /* constructor */{
                tag: "T_STRING",
                Arg0: /* tuple */[
                  loc$1,
                  value,
                  raw,
                  octal
                ]
              });
          return /* tuple */[
                  loc$1,
                  /* constructor */{
                    tag: "StringLiteral",
                    Arg0: /* record */[
                      /* value */value,
                      /* raw */raw
                    ]
                  }
                ];
      case "T_NUMBER_SINGLETON_TYPE" :
          var value$1 = token$5.Arg1;
          var number_type = token$5.Arg0;
          var raw$1 = Curry._2(Parser_env_Peek.value, undefined, env);
          token$4(env, /* constructor */{
                tag: "T_NUMBER_SINGLETON_TYPE",
                Arg0: number_type,
                Arg1: value$1
              });
          if (number_type === "LEGACY_OCTAL") {
            strict_error(env, "StrictOctalLiteral");
          }
          return /* tuple */[
                  loc,
                  /* constructor */{
                    tag: "NumberLiteral",
                    Arg0: /* record */[
                      /* value */value$1,
                      /* raw */raw$1
                    ]
                  }
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
                  "Any"
                ];
        }
    case 2 :
        var raw$2 = Curry._2(Parser_env_Peek.value, undefined, env);
        token$4(env, token$5);
        var value$2 = token$5 === "T_TRUE";
        return /* tuple */[
                loc,
                /* constructor */{
                  tag: "BooleanLiteral",
                  Arg0: /* record */[
                    /* value */value$2,
                    /* raw */raw$2
                  ]
                }
              ];
    
  }
}

function generic(env) {
  return Curry._2(raw_generic_with_identifier, env, Curry._2(Parse.identifier, undefined, env));
}

function param_list_or_type(env) {
  token$4(env, "T_LPAREN");
  var token$5 = Curry._2(Parser_env_Peek.token, undefined, env);
  var ret;
  var exit = 0;
  if (typeof token$5 === "string") {
    switch (token$5) {
      case "T_IDENTIFIER" :
          ret = function_param_or_generic_type(env);
          break;
      case "T_RPAREN" :
          ret = /* constructor */{
            tag: "ParamList",
            Arg0: /* tuple */[
              undefined,
              "[]"
            ]
          };
          break;
      case "T_ELLIPSIS" :
      case "T_EOF" :
          ret = /* constructor */{
            tag: "ParamList",
            Arg0: Curry._2(function_param_list_without_parens, env, "[]")
          };
          break;
      default:
        exit = 1;
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var match = primitive(token$5);
    if (match !== undefined) {
      var match$1 = Curry._2(Parser_env_Peek.token, 1, env);
      var exit$1 = 0;
      if (typeof match$1 === "string") {
        switch (match$1) {
          case "T_PLING" :
          case "T_COLON" :
              exit$1 = 2;
              break;
          default:
            ret = /* constructor */{
              tag: "Type",
              Arg0: union(env)
            };
        }
      } else {
        ret = /* constructor */{
          tag: "Type",
          Arg0: union(env)
        };
      }
      if (exit$1 === 2) {
        var match$2 = Curry._1(Parse.identifier_or_reserved_keyword, env);
        var name = match$2[0];
        if (!env[/* parse_options */20][/* types */4]) {
          error$1(env, "UnexpectedTypeAnnotation");
        }
        var optional = maybe(env, "T_PLING");
        token$4(env, "T_COLON");
        var typeAnnotation = union(env);
        if (Curry._2(Parser_env_Peek.token, undefined, env) !== "T_RPAREN") {
          token$4(env, "T_COMMA");
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
        ret = /* constructor */{
          tag: "ParamList",
          Arg0: Curry._2(function_param_list_without_parens, env, /* constructor */{
                tag: "::",
                Arg0: param,
                Arg1: "[]"
              })
        };
      }
      
    } else {
      ret = /* constructor */{
        tag: "Type",
        Arg0: union(env)
      };
    }
  }
  token$4(env, "T_RPAREN");
  return ret;
}

function params(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_GREATER_THAN" :
        case "T_EOF" :
            return List.rev(acc);
        default:
          
      }
    }
    var acc$1 = /* constructor */{
      tag: "::",
      Arg0: union(env),
      Arg1: acc
    };
    if (Curry._2(Parser_env_Peek.token, undefined, env) !== "T_GREATER_THAN") {
      token$4(env, "T_COMMA");
    }
    _acc = acc$1;
    continue ;
  };
}

function type_parameter_instantiation(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_LESS_THAN") {
    token$4(env, "T_LESS_THAN");
    var params$1 = params(env, "[]");
    var loc = btwn(start_loc, Curry._2(Parser_env_Peek.loc, undefined, env));
    token$4(env, "T_GREATER_THAN");
    return /* tuple */[
            loc,
            /* record */[/* params */params$1]
          ];
  }
  
}

function param(env) {
  var match = Curry._1(Parse.identifier_or_reserved_keyword, env);
  return function_param_with_id(env, match[0]);
}

function function_param_list_without_parens(env) {
  return (function (param$1) {
      var env$1 = env;
      var _acc = param$1;
      while(true) {
        var acc = _acc;
        var t = Curry._2(Parser_env_Peek.token, undefined, env$1);
        var exit = 0;
        if (typeof t === "string") {
          switch (t) {
            case "T_RPAREN" :
            case "T_ELLIPSIS" :
            case "T_EOF" :
                exit = 2;
                break;
            default:
              exit = 1;
          }
        } else {
          exit = 1;
        }
        switch (exit) {
          case 1 :
              var acc$1 = /* constructor */{
                tag: "::",
                Arg0: param(env$1),
                Arg1: acc
              };
              if (Curry._2(Parser_env_Peek.token, undefined, env$1) !== "T_RPAREN") {
                token$4(env$1, "T_COMMA");
              }
              _acc = acc$1;
              continue ;
          case 2 :
              var rest = t === "T_ELLIPSIS" ? (token$4(env$1, "T_ELLIPSIS"), param(env$1)) : undefined;
              return /* tuple */[
                      rest,
                      List.rev(acc)
                    ];
          
        }
      };
    });
}

function params$1(env, allow_default, _require_default, _acc) {
  while(true) {
    var acc = _acc;
    var require_default = _require_default;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    var variance;
    if (typeof match === "string") {
      switch (match) {
        case "T_PLUS" :
            token$3(env);
            variance = "Plus";
            break;
        case "T_MINUS" :
            token$3(env);
            variance = "Minus";
            break;
        default:
          variance = undefined;
      }
    } else {
      variance = undefined;
    }
    var match$1 = Curry._2(Parse.identifier_with_type, env, "StrictParamName");
    var id = match$1[1];
    var loc = match$1[0];
    var match$2 = Curry._2(Parser_env_Peek.token, undefined, env);
    var match$3;
    if (allow_default) {
      var exit = 0;
      if (typeof match$2 === "string" && match$2 === "T_ASSIGN") {
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
                "MissingTypeParamDefault"
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
    var acc$1 = /* constructor */{
      tag: "::",
      Arg0: param,
      Arg1: acc
    };
    var match$4 = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match$4 === "string") {
      switch (match$4) {
        case "T_GREATER_THAN" :
        case "T_EOF" :
            return List.rev(acc$1);
        default:
          
      }
    }
    token$4(env, "T_COMMA");
    if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_GREATER_THAN") {
      return List.rev(acc$1);
    } else {
      _acc = acc$1;
      _require_default = match$3[1];
      continue ;
    }
  };
}

function type_parameter_declaration(allow_default, env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_LESS_THAN") {
    if (!env[/* parse_options */20][/* types */4]) {
      error$1(env, "UnexpectedTypeAnnotation");
    }
    token$4(env, "T_LESS_THAN");
    var params$2 = params$1(env, allow_default, false, "[]");
    var loc = btwn(start_loc, Curry._2(Parser_env_Peek.loc, undefined, env));
    token$4(env, "T_GREATER_THAN");
    return /* tuple */[
            loc,
            /* record */[/* params */params$2]
          ];
  }
  
}

function intersection_with(env, left) {
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_BIT_AND") {
    var env$1 = env;
    var _acc = /* constructor */{
      tag: "::",
      Arg0: left,
      Arg1: "[]"
    };
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_Peek.token, undefined, env$1);
      if (typeof match === "string" && match === "T_BIT_AND") {
        token$4(env$1, "T_BIT_AND");
        _acc = /* constructor */{
          tag: "::",
          Arg0: prefix(env$1),
          Arg1: acc
        };
        continue ;
      }
      var match$1 = rev_nonempty_acc(acc);
      return /* tuple */[
              match$1[0],
              /* constructor */{
                tag: "Intersection",
                Arg0: match$1[1]
              }
            ];
    };
  } else {
    return left;
  }
}

function union_with(env, left) {
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_BIT_OR") {
    var env$1 = env;
    var _acc = /* constructor */{
      tag: "::",
      Arg0: left,
      Arg1: "[]"
    };
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_Peek.token, undefined, env$1);
      if (typeof match === "string" && match === "T_BIT_OR") {
        token$4(env$1, "T_BIT_OR");
        _acc = /* constructor */{
          tag: "::",
          Arg0: intersection(env$1),
          Arg1: acc
        };
        continue ;
      }
      var match$1 = rev_nonempty_acc(acc);
      return /* tuple */[
              match$1[0],
              /* constructor */{
                tag: "Union",
                Arg0: match$1[1]
              }
            ];
    };
  } else {
    return left;
  }
}

function types(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_RBRACKET" :
        case "T_EOF" :
            return List.rev(acc);
        default:
          
      }
    }
    var acc$1 = /* constructor */{
      tag: "::",
      Arg0: union(env),
      Arg1: acc
    };
    if (Curry._2(Parser_env_Peek.token, undefined, env) !== "T_RBRACKET") {
      token$4(env, "T_COMMA");
    }
    _acc = acc$1;
    continue ;
  };
}

function methodish(env, start_loc) {
  var typeParameters = Curry._2(type_parameter_declaration, false, env);
  var match = function_param_list(env);
  token$4(env, "T_COLON");
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
  var value_001 = /* constructor */{
    tag: "Function",
    Arg0: value[1]
  };
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
  var value = methodish(env, Curry._2(Parser_env_Peek.loc, undefined, env));
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
    error$1(env, "UnexpectedTypeAnnotation");
  }
  var optional = maybe(env, "T_PLING");
  token$4(env, "T_COLON");
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
  token$4(env, "T_LBRACKET");
  var match = Curry._1(Parse.identifier_or_reserved_keyword, env);
  token$4(env, "T_COLON");
  var key = union(env);
  token$4(env, "T_RBRACKET");
  token$4(env, "T_COLON");
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
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string") {
    switch (match) {
      case "T_RCURLY" :
          return /* () */0;
      case "T_SEMICOLON" :
      case "T_COMMA" :
          return token$3(env);
      default:
        return error_unexpected(env);
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
    var $$static = allow_static && maybe(env, "T_STATIC");
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    var exit = 0;
    if (typeof match === "string") {
      switch (match) {
        case "T_LBRACKET" :
            var indexer = indexer_property(env, start_loc, $$static);
            semicolon$1(env);
            _param = /* tuple */[
              acc,
              /* constructor */{
                tag: "::",
                Arg0: indexer,
                Arg1: indexers
              },
              callProperties
            ];
            continue ;
        case "T_LPAREN" :
        case "T_LESS_THAN" :
            exit = 3;
            break;
        case "T_RCURLY" :
        case "T_EOF" :
            exit = 2;
            break;
        default:
          exit = 1;
      }
    } else {
      exit = 1;
    }
    switch (exit) {
      case 1 :
          var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
          var match$2;
          var exit$1 = 0;
          if ($$static && typeof match$1 === "string" && match$1 === "T_COLON") {
            strict_error_at(env, /* tuple */[
                  start_loc,
                  "StrictReservedWord"
                ]);
            var static_key_001 = /* constructor */{
              tag: "Identifier",
              Arg0: /* tuple */[
                start_loc,
                /* record */[
                  /* name */"static",
                  /* typeAnnotation */undefined,
                  /* optional */false
                ]
              ]
            };
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
            push_lex_mode(env, "NORMAL");
            var key = Curry._1(Parse.object_key, env);
            pop_lex_mode(env);
            match$2 = /* tuple */[
              $$static,
              key
            ];
          }
          var key$1 = match$2[1][1];
          var $$static$1 = match$2[0];
          var match$3 = Curry._2(Parser_env_Peek.token, undefined, env);
          var property$1;
          if (typeof match$3 === "string") {
            switch (match$3) {
              case "T_LPAREN" :
              case "T_LESS_THAN" :
                  property$1 = method_property(env, start_loc, $$static$1, key$1);
                  break;
              default:
                property$1 = property(env, start_loc, $$static$1, key$1);
            }
          } else {
            property$1 = property(env, start_loc, $$static$1, key$1);
          }
          semicolon$1(env);
          _param = /* tuple */[
            /* constructor */{
              tag: "::",
              Arg0: property$1,
              Arg1: acc
            },
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
            /* constructor */{
              tag: "::",
              Arg0: call_prop,
              Arg1: callProperties
            }
          ];
          continue ;
      
    }
  };
}

function _object($staropt$star, env) {
  var allow_static = $staropt$star !== undefined ? $staropt$star : false;
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_LCURLY");
  var match = properties(allow_static, env, /* tuple */[
        "[]",
        "[]",
        "[]"
      ]);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_RCURLY");
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[
            /* properties */match[0],
            /* indexers */match[1],
            /* callProperties */match[2]
          ]
        ];
}

function identifier(env, _param) {
  while(true) {
    var param = _param;
    var qualification = param[1];
    var q_loc = param[0];
    if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_PERIOD") {
      token$4(env, "T_PERIOD");
      var id = Curry._2(Parse.identifier, undefined, env);
      var loc = btwn(q_loc, id[0]);
      var qualification$1 = /* constructor */{
        tag: "Qualified",
        Arg0: /* tuple */[
          loc,
          /* record */[
            /* qualification */qualification,
            /* id */id
          ]
        ]
      };
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
  var id_001 = /* constructor */{
    tag: "Unqualified",
    Arg0: id
  };
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

var _type = union;

function annotation(env) {
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, "UnexpectedTypeAnnotation");
  }
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_COLON");
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
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string" && match === "T_COLON") {
    return annotation(env);
  }
  
}

function wrap(f, env) {
  var env$1 = with_strict(true, env);
  push_lex_mode(env$1, "TYPE");
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
    switch (/* XXX */p.tag) {
      case "Object" :
          var check_env$1 = check_env;
          var o = p.Arg0;
          return List.fold_left(object_property, check_env$1, o[/* properties */0]);
      case "Array" :
          var check_env$2 = check_env;
          var arr = p.Arg0;
          return List.fold_left(array_element, check_env$2, arr[/* elements */0]);
      case "Assignment" :
          _param = p.Arg0[/* left */0];
          continue ;
      case "Identifier" :
          var param$1 = check_env;
          var id = p.Arg0;
          var name = id[1][/* name */0];
          var param_names = param$1[1];
          var env = param$1[0];
          if (mem$1(name, param_names)) {
            error_at(env, /* tuple */[
                  id[0],
                  "StrictParamDupe"
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
      case "Expression" :
          error_at(check_env[0], /* tuple */[
                param[0],
                "ExpectedPatternFoundExpression"
              ]);
          return check_env;
      
    }
  };
}

function object_property(check_env, param) {
  if (/* XXX */param.tag === "Property") {
    var property = param.Arg0[1];
    var match = property[/* key */0];
    var check_env$1;
    switch (/* XXX */match.tag) {
      case "Identifier" :
          check_env$1 = identifier_no_dupe_check(check_env, match.Arg0);
          break;
      case "Literal" :
      case "Computed" :
          check_env$1 = check_env;
          break;
      
    }
    return pattern(check_env$1, property[/* pattern */1]);
  } else {
    return pattern(check_env, param.Arg0[1][/* argument */0]);
  }
}

function array_element(check_env, param) {
  if (param !== undefined) {
    var match = param;
    if (/* XXX */match.tag === "Element") {
      return pattern(check_env, match.Arg0);
    } else {
      return pattern(check_env, match.Arg0[1][/* argument */0]);
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
          "StrictParamName"
        ]);
  }
  if (is_future_reserved(name) || is_strict_reserved(name)) {
    strict_error_at(env, /* tuple */[
          loc,
          "StrictReservedWord"
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
              "StrictFunctionName"
            ]);
      }
      if (is_future_reserved(name) || is_strict_reserved(name)) {
        strict_error_at(env$1, /* tuple */[
              loc,
              "StrictReservedWord"
            ]);
      }
      
    }
    List.fold_left(pattern, /* tuple */[
          env$1,
          "Empty"
        ], params);
    return /* () */0;
  } else {
    return 0;
  }
}

function param$1(env) {
  var id = Curry._2(Parse.pattern, env, "StrictParamName");
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_ASSIGN") {
    token$4(env, "T_ASSIGN");
    var $$default = Curry._1(Parse.assignment, env);
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
    var t = Curry._2(Parser_env_Peek.token, undefined, env);
    var exit = 0;
    if (typeof t === "string") {
      switch (t) {
        case "T_RPAREN" :
        case "T_ELLIPSIS" :
        case "T_EOF" :
            exit = 2;
            break;
        default:
          exit = 1;
      }
    } else {
      exit = 1;
    }
    switch (exit) {
      case 1 :
          var match = param$1(env);
          var $$default = match[1];
          var has_default$1 = has_default || $$default !== undefined;
          if (Curry._2(Parser_env_Peek.token, undefined, env) !== "T_RPAREN") {
            token$4(env, "T_COMMA");
          }
          _param = /* tuple */[
            /* constructor */{
              tag: "::",
              Arg0: match[0],
              Arg1: params
            },
            /* constructor */{
              tag: "::",
              Arg0: $$default,
              Arg1: defaults
            },
            has_default$1
          ];
          continue ;
      case 2 :
          var rest = t === "T_ELLIPSIS" ? (token$4(env, "T_ELLIPSIS"), Curry._2(Parse.identifier_with_type, env, "StrictParamName")) : undefined;
          if (Curry._2(Parser_env_Peek.token, undefined, env) !== "T_RPAREN") {
            error$1(env, "ParameterAfterRestParameter");
          }
          return /* tuple */[
                  List.rev(params),
                  has_default ? List.rev(defaults) : "[]",
                  rest
                ];
      
    }
  };
}

function function_params(env) {
  token$4(env, "T_LPAREN");
  var match = param_list(env, /* tuple */[
        "[]",
        "[]",
        false
      ]);
  token$4(env, "T_RPAREN");
  return /* tuple */[
          match[0],
          match[1],
          match[2]
        ];
}

function function_body(env, async, generator) {
  var env$1 = enter_function(env, async, generator);
  var match = Curry._1(Parse.function_block_body, env$1);
  var loc = match[0];
  return /* tuple */[
          loc,
          /* constructor */{
            tag: "BodyBlock",
            Arg0: /* tuple */[
              loc,
              match[1]
            ]
          },
          match[2]
        ];
}

function generator(env, is_async) {
  var match = maybe(env, "T_MULT");
  if (is_async && match) {
    error$1(env, "AsyncGenerator");
    return true;
  } else {
    return match;
  }
}

function is_simple_param(param) {
  if (/* XXX */param[1].tag === "Identifier") {
    return true;
  } else {
    return false;
  }
}

function is_simple_function_params(params, defaults, rest) {
  if (defaults === "[]" && rest === undefined) {
    return List.for_all(is_simple_param, params);
  } else {
    return false;
  }
}

function _function(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var async = maybe(env, "T_ASYNC");
  token$4(env, "T_FUNCTION");
  var generator$1 = generator(env, async);
  var match = env[/* in_export */6];
  var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
  var match$2;
  var exit = 0;
  if (match && typeof match$1 === "string") {
    switch (match$1) {
      case "T_LPAREN" :
          match$2 = /* tuple */[
            undefined,
            undefined
          ];
          break;
      case "T_LESS_THAN" :
          var typeParams = Curry._1(type_parameter_declaration$1, env);
          var id = Curry._2(Parser_env_Peek.token, undefined, env) === "T_LPAREN" ? undefined : Curry._2(Parse.identifier, "StrictFunctionName", env);
          match$2 = /* tuple */[
            typeParams,
            id
          ];
          break;
      default:
        exit = 1;
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var id$1 = Curry._2(Parse.identifier, "StrictFunctionName", env);
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
  var predicate = Curry._1(Parse.predicate, env);
  var match$4 = function_body(env, async, generator$1);
  var body = match$4[1];
  var simple = is_simple_function_params(params, defaults, rest);
  strict_post_check(env, match$4[2], simple, id$2, params);
  var match$5;
  match$5 = /* XXX */body.tag === "BodyBlock" ? /* tuple */[
      body.Arg0[0],
      false
    ] : /* tuple */[
      body.Arg0[0],
      true
    ];
  return /* tuple */[
          btwn(start_loc, match$5[0]),
          /* constructor */{
            tag: "FunctionDeclaration",
            Arg0: /* record */[
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
            ]
          }
        ];
}

function variable_declaration(env) {
  var id = Curry._2(Parse.pattern, env, "StrictVarName");
  var match;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_ASSIGN") {
    token$4(env, "T_ASSIGN");
    match = /* tuple */[
      Curry._1(Parse.assignment, env),
      "[]"
    ];
  } else {
    match = /* XXX */id[1].tag === "Identifier" ? /* tuple */[
        undefined,
        "[]"
      ] : /* tuple */[
        undefined,
        /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            id[0],
            "NoUninitializedDestructuring"
          ],
          Arg1: "[]"
        }
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
    var decls$1 = /* constructor */{
      tag: "::",
      Arg0: decl,
      Arg1: decls
    };
    var errs$1 = Pervasives.$at(match[1], errs);
    if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_COMMA") {
      token$4(env, "T_COMMA");
      _errs = errs$1;
      _decls = decls$1;
      continue ;
    } else {
      var end_loc = decls$1 !== "[]" ? decl[0] : none;
      var declarations = List.rev(decls$1);
      var start_loc = decls$1 !== "[]" ? decl[0] : none;
      return /* tuple */[
              btwn(start_loc, end_loc),
              declarations,
              List.rev(errs$1)
            ];
    }
  };
}

function declarations(token$5, kind, env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, token$5);
  var match = helper(env, "[]", "[]");
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
  var match = declarations("T_CONST", "Const", env$1);
  var match$1 = match[0];
  var variable = match$1[1];
  var errs = List.fold_left((function (errs, decl) {
          if (decl[1][/* init */1] !== undefined) {
            return errs;
          } else {
            return /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      decl[0],
                      "NoUninitializedConst"
                    ],
                    Arg1: errs
                  };
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
  return declarations("T_LET", "Let", env$1);
}

function variable(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var match$1;
  if (typeof match === "string") {
    switch (match) {
      case "T_VAR" :
          match$1 = declarations("T_VAR", "Var", env);
          break;
      case "T_CONST" :
          match$1 = $$const(env);
          break;
      case "T_LET" :
          match$1 = _let(env);
          break;
      default:
        error_unexpected(env);
        match$1 = declarations("T_VAR", "Var", env);
    }
  } else {
    error_unexpected(env);
    match$1 = declarations("T_VAR", "Var", env);
  }
  var match$2 = match$1[0];
  return /* tuple */[
          /* tuple */[
            btwn(start_loc, match$2[0]),
            /* constructor */{
              tag: "VariableDeclaration",
              Arg0: match$2[1]
            }
          ],
          match$1[1]
        ];
}

function is_tighter(a, b) {
  var a_prec;
  a_prec = /* XXX */a.tag === "Left_assoc" ? a.Arg0 : a.Arg0 - 1 | 0;
  return a_prec >= b.Arg0;
}

function is_lhs(param) {
  var tmp = param[1];
  if (typeof tmp === "string") {
    return false;
  } else {
    switch (/* XXX */tmp.tag) {
      case "Member" :
      case "Identifier" :
          return true;
      default:
        return false;
    }
  }
}

function is_assignable_lhs(param) {
  var tmp = param[1];
  if (typeof tmp === "string") {
    return false;
  } else {
    switch (/* XXX */tmp.tag) {
      case "Array" :
      case "Object" :
      case "Member" :
      case "Identifier" :
          return true;
      default:
        return false;
    }
  }
}

function assignment_op(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var op;
  if (typeof match === "string") {
    switch (match) {
      case "T_RSHIFT3_ASSIGN" :
          op = "RShift3Assign";
          break;
      case "T_RSHIFT_ASSIGN" :
          op = "RShiftAssign";
          break;
      case "T_LSHIFT_ASSIGN" :
          op = "LShiftAssign";
          break;
      case "T_BIT_XOR_ASSIGN" :
          op = "BitXorAssign";
          break;
      case "T_BIT_OR_ASSIGN" :
          op = "BitOrAssign";
          break;
      case "T_BIT_AND_ASSIGN" :
          op = "BitAndAssign";
          break;
      case "T_MOD_ASSIGN" :
          op = "ModAssign";
          break;
      case "T_DIV_ASSIGN" :
          op = "DivAssign";
          break;
      case "T_MULT_ASSIGN" :
          op = "MultAssign";
          break;
      case "T_EXP_ASSIGN" :
          op = "ExpAssign";
          break;
      case "T_MINUS_ASSIGN" :
          op = "MinusAssign";
          break;
      case "T_PLUS_ASSIGN" :
          op = "PlusAssign";
          break;
      case "T_ASSIGN" :
          op = "Assign";
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
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_PLING") {
    token$4(env, "T_PLING");
    var env$prime = with_no_in(false, env);
    var consequent = Curry._1(assignment, env$prime);
    token$4(env, "T_COLON");
    var match = with_loc(assignment, env);
    var loc = btwn(start_loc, match[0]);
    return /* tuple */[
            loc,
            /* constructor */{
              tag: "Conditional",
              Arg0: /* record */[
                /* test */expr,
                /* consequent */consequent,
                /* alternate */match[1]
              ]
            }
          ];
  } else {
    return expr;
  }
}

function peek_unary_op(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string") {
    switch (match) {
      case "T_DELETE" :
          return "Delete";
      case "T_TYPEOF" :
          return "Typeof";
      case "T_VOID" :
          return "Void";
      case "T_AWAIT" :
          if (env[/* allow_await */14]) {
            return "Await";
          } else {
            return ;
          }
      case "T_PLUS" :
          return "Plus";
      case "T_MINUS" :
          return "Minus";
      case "T_NOT" :
          return "Not";
      case "T_BIT_NOT" :
          return "BitNot";
      default:
        return ;
    }
  }
  
}

function unary(env) {
  var begin_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var op = peek_unary_op(env);
  if (op !== undefined) {
    var operator = op;
    token$3(env);
    var argument = unary(env);
    var loc = btwn(begin_loc, argument[0]);
    if (operator === "Delete") {
      var tmp = argument[1];
      if (typeof tmp !== "string" && /* XXX */tmp.tag === "Identifier") {
        strict_error_at(env, /* tuple */[
              loc,
              "StrictDelete"
            ]);
      }
      
    }
    return /* tuple */[
            loc,
            /* constructor */{
              tag: "Unary",
              Arg0: /* record */[
                /* operator */operator,
                /* prefix */true,
                /* argument */argument
              ]
            }
          ];
  } else {
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    var op$1;
    if (typeof match === "string") {
      switch (match) {
        case "T_INCR" :
            op$1 = "Increment";
            break;
        case "T_DECR" :
            op$1 = "Decrement";
            break;
        default:
          op$1 = undefined;
      }
    } else {
      op$1 = undefined;
    }
    if (op$1 !== undefined) {
      token$3(env);
      var argument$1 = unary(env);
      if (!is_lhs(argument$1)) {
        error_at(env, /* tuple */[
              argument$1[0],
              "InvalidLHSInAssignment"
            ]);
      }
      var match$1 = argument$1[1];
      if (typeof match$1 !== "string" && /* XXX */match$1.tag === "Identifier" && is_restricted(match$1.Arg0[1][/* name */0])) {
        strict_error(env, "StrictLHSPrefix");
      }
      return /* tuple */[
              btwn(begin_loc, argument$1[0]),
              /* constructor */{
                tag: "Update",
                Arg0: /* record */[
                  /* operator */op$1,
                  /* argument */argument$1,
                  /* prefix */true
                ]
              }
            ];
    } else {
      var env$1 = env;
      var argument$2 = left_hand_side(env$1);
      if (Curry._1(Parser_env_Peek.is_line_terminator, env$1)) {
        return argument$2;
      } else {
        var match$2 = Curry._2(Parser_env_Peek.token, undefined, env$1);
        var op$2;
        if (typeof match$2 === "string") {
          switch (match$2) {
            case "T_INCR" :
                op$2 = "Increment";
                break;
            case "T_DECR" :
                op$2 = "Decrement";
                break;
            default:
              op$2 = undefined;
          }
        } else {
          op$2 = undefined;
        }
        if (op$2 !== undefined) {
          if (!is_lhs(argument$2)) {
            error_at(env$1, /* tuple */[
                  argument$2[0],
                  "InvalidLHSInAssignment"
                ]);
          }
          var match$3 = argument$2[1];
          if (typeof match$3 !== "string" && /* XXX */match$3.tag === "Identifier" && is_restricted(match$3.Arg0[1][/* name */0])) {
            strict_error(env$1, "StrictLHSPostfix");
          }
          var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
          token$3(env$1);
          return /* tuple */[
                  btwn(argument$2[0], end_loc),
                  /* constructor */{
                    tag: "Update",
                    Arg0: /* record */[
                      /* operator */op$2,
                      /* argument */argument$2,
                      /* prefix */false
                    ]
                  }
                ];
        } else {
          return argument$2;
        }
      }
    }
  }
}

function left_hand_side(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var expr;
  var exit = 0;
  if (typeof match === "string" && match === "T_NEW") {
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
  var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match$1 === "string") {
    if (match$1 === "T_LPAREN") {
      return call(env, expr$1);
    } else {
      return expr$1;
    }
  } else if (/* XXX */match$1.tag === "T_TEMPLATE_PART") {
    return member(env, tagged_template(env, expr$1, match$1.Arg0));
  } else {
    return expr$1;
  }
}

function call(env, _left) {
  while(true) {
    var left = _left;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_LPAREN" :
            if (env[/* no_call */11]) {
              return left;
            } else {
              var match$1 = Curry._1($$arguments, env);
              _left = /* tuple */[
                btwn(left[0], match$1[0]),
                /* constructor */{
                  tag: "Call",
                  Arg0: /* record */[
                    /* callee */left,
                    /* arguments */match$1[1]
                  ]
                }
              ];
              continue ;
            }
        case "T_LBRACKET" :
            token$4(env, "T_LBRACKET");
            var expr = Curry._1(Parse.expression, env);
            var last_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
            var loc = btwn(left[0], last_loc);
            token$4(env, "T_RBRACKET");
            _left = /* tuple */[
              loc,
              /* constructor */{
                tag: "Member",
                Arg0: /* record */[
                  /* _object */left,
                  /* property : constructor */{
                    tag: "PropertyExpression",
                    Arg0: expr
                  },
                  /* computed */true
                ]
              }
            ];
            continue ;
        case "T_PERIOD" :
            token$4(env, "T_PERIOD");
            var match$2 = identifier_or_reserved_keyword(env);
            var id = match$2[0];
            _left = /* tuple */[
              btwn(left[0], id[0]),
              /* constructor */{
                tag: "Member",
                Arg0: /* record */[
                  /* _object */left,
                  /* property : constructor */{
                    tag: "PropertyIdentifier",
                    Arg0: id
                  },
                  /* computed */false
                ]
              }
            ];
            continue ;
        default:
          return left;
      }
    } else if (/* XXX */match.tag === "T_TEMPLATE_PART") {
      return tagged_template(env, left, match.Arg0);
    } else {
      return left;
    }
  };
}

function _new(env, _finish_fn) {
  while(true) {
    var finish_fn = _finish_fn;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string" && match === "T_NEW") {
      var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, "T_NEW");
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
            "[]"
          ];
        }
        var callee$prime_000 = btwn(start_loc, match[0]);
        var callee$prime_001 = /* constructor */{
          tag: "New",
          Arg0: /* record */[
            /* callee */callee,
            /* arguments */match[1]
          ]
        };
        var callee$prime = /* tuple */[
          callee$prime_000,
          callee$prime_001
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
    var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
    var callee$1;
    callee$1 = typeof match$1 === "string" || /* XXX */match$1.tag !== "T_TEMPLATE_PART" ? callee : tagged_template(env, callee, match$1.Arg0);
    var match$2 = Curry._2(Parser_env_Peek.token, undefined, env);
    var args;
    args = typeof match$2 === "string" && match$2 === "T_LPAREN" ? Curry._1($$arguments, env) : undefined;
    return Curry._2(finish_fn, callee$1, args);
  };
}

function member(env, left) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string") {
    switch (match) {
      case "T_LBRACKET" :
          token$4(env, "T_LBRACKET");
          var expr = Curry._1(Parse.expression, with_no_call(false, env));
          var last_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
          token$4(env, "T_RBRACKET");
          return call(env, /* tuple */[
                      btwn(left[0], last_loc),
                      /* constructor */{
                        tag: "Member",
                        Arg0: /* record */[
                          /* _object */left,
                          /* property : constructor */{
                            tag: "PropertyExpression",
                            Arg0: expr
                          },
                          /* computed */true
                        ]
                      }
                    ]);
      case "T_PERIOD" :
          token$4(env, "T_PERIOD");
          var match$1 = identifier_or_reserved_keyword(env);
          var id = match$1[0];
          return call(env, /* tuple */[
                      btwn(left[0], id[0]),
                      /* constructor */{
                        tag: "Member",
                        Arg0: /* record */[
                          /* _object */left,
                          /* property : constructor */{
                            tag: "PropertyIdentifier",
                            Arg0: id
                          },
                          /* computed */false
                        ]
                      }
                    ]);
      default:
        return left;
    }
  } else {
    return left;
  }
}

function _function$1(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var async = maybe(env, "T_ASYNC");
  token$4(env, "T_FUNCTION");
  var generator$1 = generator(env, async);
  var match;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_LPAREN") {
    match = /* tuple */[
      undefined,
      undefined
    ];
  } else {
    var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
    var id;
    id = typeof match$1 === "string" && match$1 === "T_LESS_THAN" ? undefined : Curry._2(Parse.identifier, "StrictFunctionName", env);
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
  var predicate = Curry._1(Parse.predicate, env);
  var match$3 = function_body(env, async, generator$1);
  var body = match$3[1];
  var simple = is_simple_function_params(params, defaults, rest);
  strict_post_check(env, match$3[2], simple, id$1, params);
  var expression;
  expression = /* XXX */body.tag === "BodyBlock" ? false : true;
  return /* tuple */[
          btwn(start_loc, match$3[0]),
          /* constructor */{
            tag: "Function",
            Arg0: /* record */[
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
            ]
          }
        ];
}

function number(env, number_type) {
  var value = Curry._2(Parser_env_Peek.value, undefined, env);
  var value$1;
  switch (number_type) {
    case "LEGACY_OCTAL" :
        strict_error(env, "StrictOctalLiteral");
        value$1 = Caml_format.caml_int_of_string("0o" + value);
        break;
    case "BINARY" :
    case "OCTAL" :
        value$1 = Caml_format.caml_int_of_string(value);
        break;
    case "NORMAL" :
        try {
          value$1 = float_of_string(value);
        }
        catch (exn){
          if (Sys.win32) {
            error$1(env, "WindowsFloatOfString");
            value$1 = 789.0;
          } else {
            throw exn;
          }
        }
        break;
    
  }
  token$4(env, /* constructor */{
        tag: "T_NUMBER",
        Arg0: number_type
      });
  return value$1;
}

function primary$1(env) {
  var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var token$5 = Curry._2(Parser_env_Peek.token, undefined, env);
  var exit = 0;
  if (typeof token$5 === "string") {
    switch (token$5) {
      case "T_LCURLY" :
          var env$1 = env;
          var match = Curry._1(Parse.object_initializer, env$1);
          return /* tuple */[
                  match[0],
                  /* constructor */{
                    tag: "Object",
                    Arg0: match[1]
                  }
                ];
      case "T_LPAREN" :
          var env$2 = env;
          token$4(env$2, "T_LPAREN");
          var expression = Curry._1(assignment, env$2);
          var match$1 = Curry._2(Parser_env_Peek.token, undefined, env$2);
          var ret;
          if (typeof match$1 === "string") {
            switch (match$1) {
              case "T_COMMA" :
                  ret = sequence(env$2, /* constructor */{
                        tag: "::",
                        Arg0: expression,
                        Arg1: "[]"
                      });
                  break;
              case "T_COLON" :
                  var typeAnnotation = wrap(annotation, env$2);
                  ret = /* tuple */[
                    btwn(expression[0], typeAnnotation[0]),
                    /* constructor */{
                      tag: "TypeCast",
                      Arg0: /* record */[
                        /* expression */expression,
                        /* typeAnnotation */typeAnnotation
                      ]
                    }
                  ];
                  break;
              default:
                ret = expression;
            }
          } else {
            ret = expression;
          }
          token$4(env$2, "T_RPAREN");
          return ret;
      case "T_LBRACKET" :
          var match$2 = Curry._1(array_initializer, env);
          return /* tuple */[
                  match$2[0],
                  /* constructor */{
                    tag: "Array",
                    Arg0: match$2[1]
                  }
                ];
      case "T_THIS" :
          token$4(env, "T_THIS");
          return /* tuple */[
                  loc,
                  "This"
                ];
      case "T_NULL" :
          var raw = Curry._2(Parser_env_Peek.value, undefined, env);
          token$4(env, "T_NULL");
          return /* tuple */[
                  loc,
                  /* constructor */{
                    tag: "Literal",
                    Arg0: /* record */[
                      /* value */"Null",
                      /* raw */raw
                    ]
                  }
                ];
      case "T_FALSE" :
      case "T_TRUE" :
          exit = 2;
          break;
      case "T_CLASS" :
          return Curry._1(Parse.class_expression, env);
      case "T_SUPER" :
          var loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env);
          token$4(env, "T_SUPER");
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
                  /* constructor */{
                    tag: "Identifier",
                    Arg0: id
                  }
                ];
      case "T_LESS_THAN" :
          var match$3 = Curry._1(Parse.jsx_element, env);
          return /* tuple */[
                  match$3[0],
                  /* constructor */{
                    tag: "JSXElement",
                    Arg0: match$3[1]
                  }
                ];
      case "T_DIV_ASSIGN" :
      case "T_DIV" :
          var env$3 = env;
          push_lex_mode(env$3, "REGEXP");
          var loc$2 = Curry._2(Parser_env_Peek.loc, undefined, env$3);
          var match$4 = Curry._2(Parser_env_Peek.token, undefined, env$3);
          var match$5;
          if (typeof match$4 === "string") {
            throw [
                  Caml_builtin_exceptions.assert_failure,
                  /* tuple */[
                    "parser_flow.ml",
                    1699,
                    15
                  ]
                ];
          } else if (/* XXX */match$4.tag === "T_REGEXP") {
            var match$6 = match$4.Arg0;
            var raw$1 = Curry._2(Parser_env_Peek.value, undefined, env$3);
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
          var f = function (c) {
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
          };
          Bytes.iter(f, Caml_bytes.bytes_of_string(raw_flags));
          var flags = $$Buffer.contents(filtered_flags);
          if (flags !== raw_flags) {
            error$1(env$3, /* constructor */{
                  tag: "InvalidRegExpFlags",
                  Arg0: raw_flags
                });
          }
          var value = /* constructor */{
            tag: "RegExp",
            Arg0: /* record */[
              /* pattern */match$5[1],
              /* flags */flags
            ]
          };
          return /* tuple */[
                  loc$2,
                  /* constructor */{
                    tag: "Literal",
                    Arg0: /* record */[
                      /* value */value,
                      /* raw */match$5[0]
                    ]
                  }
                ];
      default:
        exit = 1;
    }
  } else {
    switch (/* XXX */token$5.tag) {
      case "T_NUMBER" :
          var raw$2 = Curry._2(Parser_env_Peek.value, undefined, env);
          var value$1 = /* constructor */{
            tag: "Number",
            Arg0: number(env, token$5.Arg0)
          };
          return /* tuple */[
                  loc,
                  /* constructor */{
                    tag: "Literal",
                    Arg0: /* record */[
                      /* value */value$1,
                      /* raw */raw$2
                    ]
                  }
                ];
      case "T_STRING" :
          var match$7 = token$5.Arg0;
          var octal = match$7[3];
          var raw$3 = match$7[2];
          var value$2 = match$7[1];
          var loc$3 = match$7[0];
          if (octal) {
            strict_error(env, "StrictOctalLiteral");
          }
          token$4(env, /* constructor */{
                tag: "T_STRING",
                Arg0: /* tuple */[
                  loc$3,
                  value$2,
                  raw$3,
                  octal
                ]
              });
          var value$3 = /* constructor */{
            tag: "String",
            Arg0: value$2
          };
          return /* tuple */[
                  loc$3,
                  /* constructor */{
                    tag: "Literal",
                    Arg0: /* record */[
                      /* value */value$3,
                      /* raw */raw$3
                    ]
                  }
                ];
      case "T_TEMPLATE_PART" :
          var match$8 = Curry._2(template_literal, env, token$5.Arg0);
          return /* tuple */[
                  match$8[0],
                  /* constructor */{
                    tag: "TemplateLiteral",
                    Arg0: match$8[1]
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
          return /* tuple */[
                  id$1[0],
                  /* constructor */{
                    tag: "Identifier",
                    Arg0: id$1
                  }
                ];
        } else {
          error_unexpected(env);
          if (token$5 === "T_ERROR") {
            token$3(env);
          }
          return /* tuple */[
                  loc,
                  /* constructor */{
                    tag: "Literal",
                    Arg0: /* record */[
                      /* value */"Null",
                      /* raw */"null"
                    ]
                  }
                ];
        }
    case 2 :
        var raw$4 = Curry._2(Parser_env_Peek.value, undefined, env);
        token$4(env, token$5);
        var value$4 = /* constructor */{
          tag: "Boolean",
          Arg0: token$5 === "T_TRUE"
        };
        return /* tuple */[
                loc,
                /* constructor */{
                  tag: "Literal",
                  Arg0: /* record */[
                    /* value */value$4,
                    /* raw */raw$4
                  ]
                }
              ];
    
  }
}

function tagged_template(env, tag, part) {
  var quasi = Curry._2(template_literal, env, part);
  return /* tuple */[
          btwn(tag[0], quasi[0]),
          /* constructor */{
            tag: "TaggedTemplate",
            Arg0: /* record */[
              /* tag */tag,
              /* quasi */quasi
            ]
          }
        ];
}

function sequence(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string" && match === "T_COMMA") {
      token$4(env, "T_COMMA");
      var expr = Curry._1(assignment, env);
      _acc = /* constructor */{
        tag: "::",
        Arg0: expr,
        Arg1: acc
      };
      continue ;
    }
    var last_loc = acc !== "[]" ? acc.Arg0[0] : none;
    var expressions = List.rev(acc);
    var first_loc = expressions !== "[]" ? expressions.Arg0[0] : none;
    return /* tuple */[
            btwn(first_loc, last_loc),
            /* constructor */{
              tag: "Sequence",
              Arg0: /* record */[/* expressions */expressions]
            }
          ];
  };
}

function identifier_or_reserved_keyword(env) {
  var lex_token = Curry._2(Parser_env_Peek.token, undefined, env);
  var lex_value = Curry._2(Parser_env_Peek.value, undefined, env);
  var lex_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var exit = 0;
  if (typeof lex_token === "string") {
    switch (lex_token) {
      case "T_IDENTIFIER" :
      case "T_DECLARE" :
      case "T_TYPE" :
      case "T_OF" :
      case "T_ASYNC" :
          exit = 2;
          break;
      default:
        exit = 1;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 :
        var err;
        var exit$1 = 0;
        if (typeof lex_token === "string") {
          switch (lex_token) {
            case "T_FUNCTION" :
            case "T_IF" :
            case "T_IN" :
            case "T_INSTANCEOF" :
            case "T_RETURN" :
            case "T_SWITCH" :
            case "T_THIS" :
            case "T_THROW" :
            case "T_TRY" :
            case "T_VAR" :
            case "T_WHILE" :
            case "T_WITH" :
            case "T_CONST" :
            case "T_LET" :
            case "T_NULL" :
            case "T_FALSE" :
            case "T_TRUE" :
            case "T_BREAK" :
            case "T_CASE" :
            case "T_CATCH" :
            case "T_CONTINUE" :
            case "T_DEFAULT" :
            case "T_DO" :
            case "T_FINALLY" :
            case "T_FOR" :
            case "T_CLASS" :
            case "T_EXTENDS" :
            case "T_STATIC" :
            case "T_ELSE" :
            case "T_NEW" :
            case "T_DELETE" :
            case "T_TYPEOF" :
            case "T_VOID" :
            case "T_ENUM" :
            case "T_EXPORT" :
            case "T_IMPORT" :
            case "T_SUPER" :
            case "T_IMPLEMENTS" :
            case "T_INTERFACE" :
            case "T_PACKAGE" :
            case "T_PRIVATE" :
            case "T_PROTECTED" :
            case "T_PUBLIC" :
            case "T_YIELD" :
            case "T_DEBUGGER" :
            case "T_AWAIT" :
            case "T_ANY_TYPE" :
            case "T_BOOLEAN_TYPE" :
            case "T_NUMBER_TYPE" :
            case "T_STRING_TYPE" :
            case "T_VOID_TYPE" :
                exit$1 = 3;
                break;
            default:
              error_unexpected(env);
              err = undefined;
          }
        } else {
          error_unexpected(env);
          err = undefined;
        }
        if (exit$1 === 3) {
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
    case 2 :
        return /* tuple */[
                Curry._2(Parse.identifier, undefined, env),
                undefined
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
            "InvalidLHSInAssignment"
          ]);
    }
    var match$1 = expr[1];
    if (typeof match$1 !== "string" && /* XXX */match$1.tag === "Identifier" && is_restricted(match$1.Arg0[1][/* name */0])) {
      strict_error_at(env, /* tuple */[
            expr[0],
            "StrictLHSAssignment"
          ]);
    }
    var left = Curry._2(Parse.pattern_from_expr, env, expr);
    var right = Curry._1(assignment, env);
    var loc = btwn(left[0], right[0]);
    return /* tuple */[
            loc,
            /* constructor */{
              tag: "Assignment",
              Arg0: /* record */[
                /* operator */match,
                /* left */left,
                /* right */right
              ]
            }
          ];
  } else {
    return expr;
  }
}

function error_callback(param, param$1) {
  throw Parser_env_Try.Rollback;
}

function try_assignment_but_not_arrow_function(env) {
  var env$1 = with_error_callback(error_callback, env);
  var ret = assignment_but_not_arrow_function(env$1);
  var match = Curry._2(Parser_env_Peek.token, undefined, env$1);
  if (typeof match === "string") {
    switch (match) {
      case "T_ARROW" :
      case "T_COLON" :
          throw Parser_env_Try.Rollback;
      default:
        
    }
  }
  if (Curry._2(Parser_env_Peek.is_identifier, undefined, env$1)) {
    if (Curry._2(Parser_env_Peek.value, undefined, env$1) === "checks") {
      throw Parser_env_Try.Rollback;
    }
    var match$1 = ret[1];
    if (typeof match$1 === "string" || !(/* XXX */match$1.tag === "Identifier" && match$1.Arg0[1][/* name */0] === "async")) {
      return ret;
    } else {
      if (!Curry._1(Parser_env_Peek.is_line_terminator, env$1)) {
        throw Parser_env_Try.Rollback;
      }
      return ret;
    }
  } else {
    return ret;
  }
}

function assignment(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var match$1 = Curry._2(Parser_env_Peek.is_identifier, undefined, env);
  var exit = 0;
  if (typeof match === "string") {
    switch (match) {
      case "T_YIELD" :
          if (env[/* allow_yield */13]) {
            var env$1 = env;
            var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
            token$4(env$1, "T_YIELD");
            if (!env$1[/* allow_yield */13]) {
              error$1(env$1, "IllegalYield");
            }
            var delegate = maybe(env$1, "T_MULT");
            var has_argument = !(Curry._2(Parser_env_Peek.token, undefined, env$1) === "T_SEMICOLON" || Curry._1(Parser_env_Peek.is_implicit_semicolon, env$1));
            var argument = delegate || has_argument ? Curry._1(assignment, env$1) : undefined;
            var end_loc;
            if (argument !== undefined) {
              end_loc = argument[0];
            } else {
              var match$2 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
              var end_loc$1 = match$2 !== undefined ? match$2 : start_loc;
              semicolon(env$1);
              end_loc = end_loc$1;
            }
            return /* tuple */[
                    btwn(start_loc, end_loc),
                    /* constructor */{
                      tag: "Yield",
                      Arg0: /* record */[
                        /* argument */argument,
                        /* delegate */delegate
                      ]
                    }
                  ];
          } else {
            exit = 2;
          }
          break;
      case "T_LPAREN" :
      case "T_LESS_THAN" :
          break;
      default:
        exit = 2;
    }
  } else {
    exit = 2;
  }
  if (exit === 2 && !match$1) {
    return assignment_but_not_arrow_function(env);
  }
  var match$3 = Curry._2(Parser_env_Try.to_parse, env, try_assignment_but_not_arrow_function);
  if (match$3 !== "FailedToParse") {
    return match$3.Arg0;
  } else {
    var match$4 = Curry._2(Parser_env_Try.to_parse, env, try_arrow_function);
    if (match$4 !== "FailedToParse") {
      return match$4.Arg0;
    } else {
      return assignment_but_not_arrow_function(env);
    }
  }
}

function make_logical(left, right, operator, loc) {
  return /* tuple */[
          loc,
          /* constructor */{
            tag: "Logical",
            Arg0: /* record */[
              /* operator */operator,
              /* left */left,
              /* right */right
            ]
          }
        ];
}

function logical_and(env, _left, _lloc) {
  while(true) {
    var lloc = _lloc;
    var left = _left;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string" && match === "T_AND") {
      token$4(env, "T_AND");
      var match$1 = with_loc(binary, env);
      var loc = btwn(lloc, match$1[0]);
      _lloc = loc;
      _left = make_logical(left, match$1[1], "And", loc);
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
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string" && match === "T_OR") {
      token$4(env, "T_OR");
      var match$1 = with_loc(binary, env);
      var match$2 = logical_and(env, match$1[1], match$1[0]);
      var loc = btwn(lloc, match$2[0]);
      _lloc = loc;
      _left = make_logical(left, match$2[1], "Or", loc);
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
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var ret;
  if (typeof match === "string") {
    switch (match) {
      case "T_IN" :
          ret = env[/* no_in */10] ? undefined : /* tuple */[
              "In",
              /* constructor */{
                tag: "Left_assoc",
                Arg0: 6
              }
            ];
          break;
      case "T_INSTANCEOF" :
          ret = /* tuple */[
            "Instanceof",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 6
            }
          ];
          break;
      case "T_BIT_OR" :
          ret = /* tuple */[
            "BitOr",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 2
            }
          ];
          break;
      case "T_BIT_XOR" :
          ret = /* tuple */[
            "Xor",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 3
            }
          ];
          break;
      case "T_BIT_AND" :
          ret = /* tuple */[
            "BitAnd",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 4
            }
          ];
          break;
      case "T_EQUAL" :
          ret = /* tuple */[
            "Equal",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 5
            }
          ];
          break;
      case "T_NOT_EQUAL" :
          ret = /* tuple */[
            "NotEqual",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 5
            }
          ];
          break;
      case "T_STRICT_EQUAL" :
          ret = /* tuple */[
            "StrictEqual",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 5
            }
          ];
          break;
      case "T_STRICT_NOT_EQUAL" :
          ret = /* tuple */[
            "StrictNotEqual",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 5
            }
          ];
          break;
      case "T_LESS_THAN_EQUAL" :
          ret = /* tuple */[
            "LessThanEqual",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 6
            }
          ];
          break;
      case "T_GREATER_THAN_EQUAL" :
          ret = /* tuple */[
            "GreaterThanEqual",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 6
            }
          ];
          break;
      case "T_LESS_THAN" :
          ret = /* tuple */[
            "LessThan",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 6
            }
          ];
          break;
      case "T_GREATER_THAN" :
          ret = /* tuple */[
            "GreaterThan",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 6
            }
          ];
          break;
      case "T_LSHIFT" :
          ret = /* tuple */[
            "LShift",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 7
            }
          ];
          break;
      case "T_RSHIFT" :
          ret = /* tuple */[
            "RShift",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 7
            }
          ];
          break;
      case "T_RSHIFT3" :
          ret = /* tuple */[
            "RShift3",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 7
            }
          ];
          break;
      case "T_PLUS" :
          ret = /* tuple */[
            "Plus",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 8
            }
          ];
          break;
      case "T_MINUS" :
          ret = /* tuple */[
            "Minus",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 8
            }
          ];
          break;
      case "T_DIV" :
          ret = /* tuple */[
            "Div",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 9
            }
          ];
          break;
      case "T_MULT" :
          ret = /* tuple */[
            "Mult",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 9
            }
          ];
          break;
      case "T_EXP" :
          ret = /* tuple */[
            "Exp",
            /* constructor */{
              tag: "Right_assoc",
              Arg0: 10
            }
          ];
          break;
      case "T_MOD" :
          ret = /* tuple */[
            "Mod",
            /* constructor */{
              tag: "Left_assoc",
              Arg0: 9
            }
          ];
          break;
      default:
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
          /* constructor */{
            tag: "Binary",
            Arg0: /* record */[
              /* operator */operator,
              /* left */left,
              /* right */right
            ]
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
    if (stack !== "[]") {
      var match = stack.Arg0;
      var match$1 = match[1];
      if (is_tighter(match$1[1], rpri)) {
        var loc = btwn(match[2], rloc);
        var right$1 = make_binary(match[0], right, match$1[0], loc);
        _stack = stack.Arg1;
        _rloc = loc;
        _param = /* tuple */[
          rop,
          rpri
        ];
        _right = right$1;
        continue ;
      }
      
    }
    return /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              right,
              /* tuple */[
                rop,
                rpri
              ],
              rloc
            ],
            Arg1: stack
          };
  };
}

function binary(env) {
  var env$1 = env;
  var _stack = "[]";
  while(true) {
    var stack = _stack;
    var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
    var is_unary = peek_unary_op(env$1) !== undefined;
    var right = unary(with_no_in(false, env$1));
    var match = env$1[/* last_loc */4][0];
    var end_loc = match !== undefined ? match : right[0];
    var right_loc = btwn(start_loc, end_loc);
    if (Curry._2(Parser_env_Peek.token, undefined, env$1) === "T_LESS_THAN") {
      var tmp = right[1];
      if (typeof tmp !== "string" && /* XXX */tmp.tag === "JSXElement") {
        error$1(env$1, "AdjacentJSXElements");
      }
      
    }
    var match$1 = binary_op(env$1);
    if (match$1 !== undefined) {
      var match$2 = match$1;
      var rop = match$2[0];
      if (is_unary && rop === "Exp") {
        error_at(env$1, /* tuple */[
              right_loc,
              "InvalidLHSInExponentiation"
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
        if (param !== "[]") {
          var match$3 = param.Arg0;
          var loc = btwn(match$3[2], rloc);
          _param = param.Arg1;
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
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string") {
    if (match === "T_ELLIPSIS") {
      var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, "T_ELLIPSIS");
      var argument$1 = Curry._1(assignment, env);
      var loc = btwn(start_loc, argument$1[0]);
      return /* constructor */{
              tag: "Spread",
              Arg0: /* tuple */[
                loc,
                /* record */[/* argument */argument$1]
              ]
            };
    } else {
      return /* constructor */{
              tag: "Expression",
              Arg0: Curry._1(assignment, env)
            };
    }
  } else {
    return /* constructor */{
            tag: "Expression",
            Arg0: Curry._1(assignment, env)
          };
  }
}

function arguments$prime(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_RPAREN" :
        case "T_EOF" :
            return List.rev(acc);
        default:
          
      }
    }
    var acc$1 = /* constructor */{
      tag: "::",
      Arg0: argument(env),
      Arg1: acc
    };
    if (Curry._2(Parser_env_Peek.token, undefined, env) !== "T_RPAREN") {
      token$4(env, "T_COMMA");
    }
    _acc = acc$1;
    continue ;
  };
}

function $$arguments(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_LPAREN");
  var args = arguments$prime(env, "[]");
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_RPAREN");
  return /* tuple */[
          btwn(start_loc, end_loc),
          args
        ];
}

function template_parts(env, _quasis, _expressions) {
  while(true) {
    var expressions = _expressions;
    var quasis = _quasis;
    var expr = Curry._1(Parse.expression, env);
    var expressions$1 = /* constructor */{
      tag: "::",
      Arg0: expr,
      Arg1: expressions
    };
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string" && match === "T_RCURLY") {
      push_lex_mode(env, "TEMPLATE");
      var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
      var match$2;
      if (typeof match$1 === "string") {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "parser_flow.ml",
                1602,
                19
              ]
            ];
      } else if (/* XXX */match$1.tag === "T_TEMPLATE_PART") {
        var match$3 = match$1.Arg0;
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
      var quasis$1 = /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          loc,
          match$2[1]
        ],
        Arg1: quasis
      };
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
    }
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
            List.rev(/* constructor */{
                  tag: "::",
                  Arg0: imaginary_quasi,
                  Arg1: quasis
                }),
            List.rev(expressions$1)
          ];
  };
}

function template_literal(env, part) {
  var is_tail = part[2];
  var match = part[1];
  var start_loc = part[0];
  token$4(env, /* constructor */{
        tag: "T_TEMPLATE_PART",
        Arg0: part
      });
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
      /* constructor */{
        tag: "::",
        Arg0: head,
        Arg1: "[]"
      },
      "[]"
    ] : template_parts(env, /* constructor */{
          tag: "::",
          Arg0: head,
          Arg1: "[]"
        }, "[]");
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
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_COMMA" :
            token$4(env, "T_COMMA");
            _acc = /* constructor */{
              tag: "::",
              Arg0: undefined,
              Arg1: acc
            };
            continue ;
        case "T_ELLIPSIS" :
            var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
            token$4(env, "T_ELLIPSIS");
            var argument = Curry._1(assignment, env);
            var loc = btwn(start_loc, argument[0]);
            var elem = /* constructor */{
              tag: "Spread",
              Arg0: /* tuple */[
                loc,
                /* record */[/* argument */argument]
              ]
            };
            _acc = /* constructor */{
              tag: "::",
              Arg0: elem,
              Arg1: acc
            };
            continue ;
        case "T_RBRACKET" :
        case "T_EOF" :
            return List.rev(acc);
        default:
          
      }
    }
    var elem$1 = /* constructor */{
      tag: "Expression",
      Arg0: Curry._1(assignment, env)
    };
    if (Curry._2(Parser_env_Peek.token, undefined, env) !== "T_RBRACKET") {
      token$4(env, "T_COMMA");
    }
    _acc = /* constructor */{
      tag: "::",
      Arg0: elem$1,
      Arg1: acc
    };
    continue ;
  };
}

function array_initializer(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_LBRACKET");
  var elements$1 = elements(env, "[]");
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_RBRACKET");
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* elements */elements$1]
        ];
}

function error_callback$1(param, param$1) {
  if (typeof param$1 === "string") {
    switch (param$1) {
      case "StrictParamName" :
      case "NewlineBeforeArrow" :
      case "ParameterAfterRestParameter" :
          return /* () */0;
      default:
        throw Parser_env_Try.Rollback;
    }
  } else {
    throw Parser_env_Try.Rollback;
  }
}

function try_arrow_function(env) {
  var env$1 = with_error_callback(error_callback$1, env);
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
  var async = Curry._2(Parser_env_Peek.token, 1, env$1) !== "T_ARROW" && maybe(env$1, "T_ASYNC");
  var typeParameters = Curry._1(type_parameter_declaration$1, env$1);
  var match;
  if (Curry._2(Parser_env_Peek.is_identifier, undefined, env$1) && typeParameters === undefined) {
    var id = Curry._2(Parse.identifier, "StrictParamName", env$1);
    var param_000 = id[0];
    var param_001 = /* constructor */{
      tag: "Identifier",
      Arg0: id
    };
    var param = /* tuple */[
      param_000,
      param_001
    ];
    match = /* tuple */[
      /* constructor */{
        tag: "::",
        Arg0: param,
        Arg1: "[]"
      },
      "[]",
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
  var predicate = Curry._1(Parse.predicate, env$1);
  var env$2 = params === "[]" || rest !== undefined ? without_error_callback(env$1) : env$1;
  if (Curry._1(Parser_env_Peek.is_line_terminator, env$2) && Curry._2(Parser_env_Peek.token, undefined, env$2) === "T_ARROW") {
    error$1(env$2, "NewlineBeforeArrow");
  }
  token$4(env$2, "T_ARROW");
  var env$3 = without_error_callback(env$2);
  var match$2 = with_loc((function (param) {
          var env = param;
          var async$1 = async;
          var generator = false;
          var env$1 = with_in_function(true, env);
          var match = Curry._2(Parser_env_Peek.token, undefined, env$1);
          if (typeof match === "string" && match === "T_LCURLY") {
            var match$1 = function_body(env$1, async$1, generator);
            return /* tuple */[
                    match$1[1],
                    match$1[2]
                  ];
          }
          var env$2 = enter_function(env$1, async$1, generator);
          var expr = Curry._1(Parse.assignment, env$2);
          return /* tuple */[
                  /* constructor */{
                    tag: "BodyExpression",
                    Arg0: expr
                  },
                  env$2[/* in_strict_mode */5]
                ];
        }), env$3);
  var match$3 = match$2[1];
  var body = match$3[0];
  var simple = is_simple_function_params(params, defaults, rest);
  strict_post_check(env$3, match$3[1], simple, undefined, params);
  var expression;
  expression = /* XXX */body.tag === "BodyBlock" ? false : true;
  var loc = btwn(start_loc, match$2[0]);
  return /* tuple */[
          loc,
          /* constructor */{
            tag: "ArrowFunction",
            Arg0: /* record */[
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
            ]
          }
        ];
}

function decorator_list_helper(env, _decorators) {
  while(true) {
    var decorators = _decorators;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string" && match === "T_AT") {
      token$3(env);
      _decorators = /* constructor */{
        tag: "::",
        Arg0: left_hand_side(env),
        Arg1: decorators
      };
      continue ;
    } else {
      return decorators;
    }
  };
}

function decorator_list(env) {
  if (env[/* parse_options */20][/* esproposal_decorators */2]) {
    return List.rev(decorator_list_helper(env, "[]"));
  } else {
    return "[]";
  }
}

function key(env) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string") {
    if (match === "T_LBRACKET") {
      var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, "T_LBRACKET");
      var expr = Curry._1(Parse.assignment, with_no_in(false, env));
      var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, "T_RBRACKET");
      return /* tuple */[
              btwn(start_loc, end_loc),
              /* constructor */{
                tag: "Computed",
                Arg0: expr
              }
            ];
    }
    
  } else {
    switch (/* XXX */match.tag) {
      case "T_NUMBER" :
          var raw = Curry._2(Parser_env_Peek.value, undefined, env);
          var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
          var value = number(env, match.Arg0);
          var value$1 = /* constructor */{
            tag: "Number",
            Arg0: value
          };
          return /* tuple */[
                  loc,
                  /* constructor */{
                    tag: "Literal",
                    Arg0: /* tuple */[
                      loc,
                      /* record */[
                        /* value */value$1,
                        /* raw */raw
                      ]
                    ]
                  }
                ];
      case "T_STRING" :
          var match$1 = match.Arg0;
          var octal = match$1[3];
          var raw$1 = match$1[2];
          var value$2 = match$1[1];
          var loc$1 = match$1[0];
          if (octal) {
            strict_error(env, "StrictOctalLiteral");
          }
          token$4(env, /* constructor */{
                tag: "T_STRING",
                Arg0: /* tuple */[
                  loc$1,
                  value$2,
                  raw$1,
                  octal
                ]
              });
          var value$3 = /* constructor */{
            tag: "String",
            Arg0: value$2
          };
          return /* tuple */[
                  loc$1,
                  /* constructor */{
                    tag: "Literal",
                    Arg0: /* tuple */[
                      loc$1,
                      /* record */[
                        /* value */value$3,
                        /* raw */raw$1
                      ]
                    ]
                  }
                ];
      default:
        
    }
  }
  var match$2 = identifier_or_reserved_keyword(env);
  var id = match$2[0];
  return /* tuple */[
          id[0],
          /* constructor */{
            tag: "Identifier",
            Arg0: id
          }
        ];
}

function _method(env, kind) {
  var generator$1 = generator(env, false);
  var match = key(env);
  var typeParameters;
  switch (kind) {
    case "Init" :
        typeParameters = Curry._1(type_parameter_declaration$1, env);
        break;
    case "Get" :
    case "Set" :
        typeParameters = undefined;
        break;
    
  }
  token$4(env, "T_LPAREN");
  var params;
  switch (kind) {
    case "Init" :
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "parser_flow.ml",
                1954,
                16
              ]
            ];
    case "Get" :
        params = "[]";
        break;
    case "Set" :
        var param = Curry._2(Parse.identifier_with_type, env, "StrictParamName");
        params = /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            param[0],
            /* constructor */{
              tag: "Identifier",
              Arg0: param
            }
          ],
          Arg1: "[]"
        };
        break;
    
  }
  token$4(env, "T_RPAREN");
  var returnType = wrap(annotation_opt, env);
  var match$1 = function_body(env, false, generator$1);
  var body = match$1[1];
  var simple = is_simple_function_params(params, "[]", undefined);
  strict_post_check(env, match$1[2], simple, undefined, params);
  var match$2;
  match$2 = /* XXX */body.tag === "BodyBlock" ? /* tuple */[
      body.Arg0[0],
      false
    ] : /* tuple */[
      body.Arg0[0],
      true
    ];
  var value_000 = match$2[0];
  var value_001 = /* record */[
    /* id */undefined,
    /* params */params,
    /* defaults */"[]",
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
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_ELLIPSIS") {
    token$4(env, "T_ELLIPSIS");
    var argument = Curry._1(Parse.assignment, env);
    return /* constructor */{
            tag: "SpreadProperty",
            Arg0: /* tuple */[
              btwn(start_loc, argument[0]),
              /* record */[/* argument */argument]
            ]
          };
  } else {
    var async = Curry._2(Parser_env_Peek.is_identifier, 1, env) && maybe(env, "T_ASYNC");
    var match = generator(env, async);
    var match$1 = key(env);
    var tmp;
    var exit = 0;
    if (async || match) {
      exit = 1;
    } else {
      var key$1 = match$1[1];
      switch (/* XXX */key$1.tag) {
        case "Identifier" :
            switch (key$1.Arg0[1][/* name */0]) {
              case "get" :
                  var match$2 = Curry._2(Parser_env_Peek.token, undefined, env);
                  var exit$1 = 0;
                  if (typeof match$2 === "string") {
                    switch (match$2) {
                      case "T_LPAREN" :
                      case "T_COLON" :
                      case "T_LESS_THAN" :
                          exit$1 = 2;
                          break;
                      default:
                        tmp = get(env, start_loc);
                    }
                  } else {
                    tmp = get(env, start_loc);
                  }
                  if (exit$1 === 2) {
                    tmp = init(env, start_loc, key$1, false, false);
                  }
                  break;
              case "set" :
                  var match$3 = Curry._2(Parser_env_Peek.token, undefined, env);
                  var exit$2 = 0;
                  if (typeof match$3 === "string") {
                    switch (match$3) {
                      case "T_LPAREN" :
                      case "T_COLON" :
                      case "T_LESS_THAN" :
                          exit$2 = 2;
                          break;
                      default:
                        tmp = set(env, start_loc);
                    }
                  } else {
                    tmp = set(env, start_loc);
                  }
                  if (exit$2 === 2) {
                    tmp = init(env, start_loc, key$1, false, false);
                  }
                  break;
              default:
                exit = 1;
            }
            break;
        case "Literal" :
        case "Computed" :
            exit = 1;
            break;
        
      }
    }
    if (exit === 1) {
      tmp = init(env, start_loc, match$1[1], async, match);
    }
    return /* constructor */{
            tag: "Property",
            Arg0: tmp
          };
  }
}

function get(env, start_loc) {
  var match = _method(env, "Get");
  var match$1 = match[1];
  var end_loc = match$1[0];
  var value_001 = /* constructor */{
    tag: "Function",
    Arg0: match$1[1]
  };
  var value = /* tuple */[
    end_loc,
    value_001
  ];
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[
            /* key */match[0],
            /* value */value,
            /* kind */"Get",
            /* _method */false,
            /* shorthand */false
          ]
        ];
}

function set(env, start_loc) {
  var match = _method(env, "Set");
  var match$1 = match[1];
  var end_loc = match$1[0];
  var value_001 = /* constructor */{
    tag: "Function",
    Arg0: match$1[1]
  };
  var value = /* tuple */[
    end_loc,
    value_001
  ];
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[
            /* key */match[0],
            /* value */value,
            /* kind */"Set",
            /* _method */false,
            /* shorthand */false
          ]
        ];
}

function init(env, start_loc, key, async, generator) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var match$1;
  var exit = 0;
  if (typeof match === "string") {
    switch (match) {
      case "T_RCURLY" :
      case "T_COMMA" :
          exit = 2;
          break;
      case "T_LPAREN" :
      case "T_LESS_THAN" :
          exit = 3;
          break;
      default:
        exit = 1;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 :
        token$4(env, "T_COLON");
        match$1 = /* tuple */[
          Curry._1(Parse.assignment, env),
          false,
          false
        ];
        break;
    case 2 :
        var tmp;
        switch (/* XXX */key.tag) {
          case "Literal" :
              var lit = key.Arg0;
              tmp = /* tuple */[
                lit[0],
                /* constructor */{
                  tag: "Literal",
                  Arg0: lit[1]
                }
              ];
              break;
          case "Identifier" :
              var id = key.Arg0;
              tmp = /* tuple */[
                id[0],
                /* constructor */{
                  tag: "Identifier",
                  Arg0: id
                }
              ];
              break;
          case "Computed" :
              tmp = key.Arg0;
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
        match$4 = /* XXX */body.tag === "BodyBlock" ? /* tuple */[
            body.Arg0[0],
            false
          ] : /* tuple */[
            body.Arg0[0],
            true
          ];
        var value_000 = match$4[0];
        var value_001 = /* constructor */{
          tag: "Function",
          Arg0: /* record */[
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
          ]
        };
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
            /* kind */"Init",
            /* _method */match$1[2],
            /* shorthand */match$1[1]
          ]
        ];
}

function check_property(env, prop_map, prop) {
  if (/* XXX */prop.tag === "Property") {
    var match = prop.Arg0;
    var prop$1 = match[1];
    var prop_loc = match[0];
    switch (/* XXX */prop$1[/* key */0].tag) {
      case "Literal" :
      case "Identifier" :
          break;
      case "Computed" :
          return prop_map;
      
    }
    var match$1 = prop$1[/* key */0];
    var key;
    switch (/* XXX */match$1.tag) {
      case "Literal" :
          var match$2 = match$1.Arg0[1][/* value */0];
          if (typeof match$2 === "string") {
            key = "null";
          } else {
            switch (/* XXX */match$2.tag) {
              case "String" :
                  key = match$2.Arg0;
                  break;
              case "Boolean" :
                  var b = match$2.Arg0;
                  key = b ? "true" : "false";
                  break;
              case "Number" :
                  key = Pervasives.string_of_float(match$2.Arg0);
                  break;
              case "RegExp" :
                  throw [
                        Caml_builtin_exceptions.failure,
                        "RegExp cannot be property key"
                      ];
              
            }
          }
          break;
      case "Identifier" :
          key = match$1.Arg0[1][/* name */0];
          break;
      case "Computed" :
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
        prev_kinds = "Empty";
      } else {
        throw exn;
      }
    }
    var match$3 = prop$1[/* kind */2];
    var kind_string;
    switch (match$3) {
      case "Init" :
          kind_string = "Init";
          break;
      case "Get" :
          kind_string = "Get";
          break;
      case "Set" :
          kind_string = "Set";
          break;
      
    }
    var exit = 0;
    switch (kind_string) {
      case "Init" :
          if (mem$1("Init", prev_kinds)) {
            strict_error_at(env, /* tuple */[
                  prop_loc,
                  "StrictDuplicateProperty"
                ]);
          } else if (mem$1("Set", prev_kinds) || mem$1("Get", prev_kinds)) {
            error_at(env, /* tuple */[
                  prop_loc,
                  "AccessorDataProperty"
                ]);
          }
          break;
      case "Get" :
      case "Set" :
          exit = 1;
          break;
      default:
        
    }
    if (exit === 1) {
      if (mem$1("Init", prev_kinds)) {
        error_at(env, /* tuple */[
              prop_loc,
              "AccessorDataProperty"
            ]);
      } else if (mem$1(kind_string, prev_kinds)) {
        error_at(env, /* tuple */[
              prop_loc,
              "AccessorGetSet"
            ]);
      }
      
    }
    var kinds = add$1(kind_string, prev_kinds);
    return add$2(key, kinds, prop_map);
  } else {
    return prop_map;
  }
}

function properties$1(env, _param) {
  while(true) {
    var param = _param;
    var acc = param[1];
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_RCURLY" :
        case "T_EOF" :
            return List.rev(acc);
        default:
          
      }
    }
    var prop = property$1(env);
    var prop_map = check_property(env, param[0], prop);
    if (Curry._2(Parser_env_Peek.token, undefined, env) !== "T_RCURLY") {
      token$4(env, "T_COMMA");
    }
    _param = /* tuple */[
      prop_map,
      /* constructor */{
        tag: "::",
        Arg0: prop,
        Arg1: acc
      }
    ];
    continue ;
  };
}

function _initializer(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_LCURLY");
  var props = properties$1(env, /* tuple */[
        "Empty",
        "[]"
      ]);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_RCURLY");
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* properties */props]
        ];
}

function class_implements(env, _acc) {
  while(true) {
    var acc = _acc;
    var id = Curry._2(Parse.identifier, undefined, env);
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
    var acc$1 = /* constructor */{
      tag: "::",
      Arg0: implement,
      Arg1: acc
    };
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string" && match === "T_COMMA") {
      token$4(env, "T_COMMA");
      _acc = acc$1;
      continue ;
    } else {
      return List.rev(acc$1);
    }
  };
}

function get$1(env, start_loc, decorators, $$static) {
  var match = _method(env, "Get");
  var value = match[1];
  return /* constructor */{
          tag: "Method",
          Arg0: /* tuple */[
            btwn(start_loc, value[0]),
            /* record */[
              /* kind */"Get",
              /* key */match[0],
              /* value */value,
              /* static */$$static,
              /* decorators */decorators
            ]
          ]
        };
}

function set$1(env, start_loc, decorators, $$static) {
  var match = _method(env, "Set");
  var value = match[1];
  return /* constructor */{
          tag: "Method",
          Arg0: /* tuple */[
            btwn(start_loc, value[0]),
            /* record */[
              /* kind */"Set",
              /* key */match[0],
              /* value */value,
              /* static */$$static,
              /* decorators */decorators
            ]
          ]
        };
}

function init$1(env, start_loc, decorators, key, async, generator, $$static) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var exit = 0;
  if (typeof match === "string") {
    switch (match) {
      case "T_SEMICOLON" :
      case "T_ASSIGN" :
      case "T_COLON" :
          exit = 2;
          break;
      default:
        
    }
  }
  if (exit === 2 && !async && !generator) {
    var typeAnnotation = wrap(annotation_opt, env);
    var options = env[/* parse_options */20];
    var value = Curry._2(Parser_env_Peek.token, undefined, env) === "T_ASSIGN" && ($$static && options[/* esproposal_class_static_fields */1] || !$$static && options[/* esproposal_class_instance_fields */0]) ? (token$4(env, "T_ASSIGN"), Curry._1(Parse.expression, env)) : undefined;
    var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    if (!maybe(env, "T_SEMICOLON")) {
      if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_LBRACKET" || Curry._2(Parser_env_Peek.token, undefined, env) === "T_LPAREN") {
        error_unexpected(env);
      }
      
    }
    var loc = btwn(start_loc, end_loc);
    return /* constructor */{
            tag: "Property",
            Arg0: /* tuple */[
              loc,
              /* record */[
                /* key */key,
                /* value */value,
                /* typeAnnotation */typeAnnotation,
                /* static */$$static
              ]
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
  match$3 = /* XXX */body.tag === "BodyBlock" ? /* tuple */[
      body.Arg0[0],
      false
    ] : /* tuple */[
      body.Arg0[0],
      true
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
  switch (/* XXX */key.tag) {
    case "Literal" :
        var match$4 = key.Arg0[1][/* value */0];
        kind = typeof match$4 === "string" || !(/* XXX */match$4.tag === "String" && match$4.Arg0 === "constructor") ? "Method" : "Constructor";
        break;
    case "Identifier" :
        kind = key.Arg0[1][/* name */0] === "constructor" ? "Constructor" : "Method";
        break;
    case "Computed" :
        kind = "Method";
        break;
    
  }
  return /* constructor */{
          tag: "Method",
          Arg0: /* tuple */[
            btwn(start_loc, end_loc$1),
            /* record */[
              /* kind */kind,
              /* key */key,
              /* value */value$1,
              /* static */$$static,
              /* decorators */decorators
            ]
          ]
        };
}

function class_element(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var decorators = decorator_list(env);
  var $$static = maybe(env, "T_STATIC");
  var async = Curry._2(Parser_env_Peek.token, 1, env) !== "T_LPAREN" && Curry._2(Parser_env_Peek.token, 1, env) !== "T_COLON" && maybe(env, "T_ASYNC");
  var generator$1 = generator(env, async);
  var match = key(env);
  if (!async && !generator$1) {
    var key$1 = match[1];
    switch (/* XXX */key$1.tag) {
      case "Identifier" :
          switch (key$1.Arg0[1][/* name */0]) {
            case "get" :
                var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
                var exit = 0;
                if (typeof match$1 === "string") {
                  switch (match$1) {
                    case "T_LPAREN" :
                    case "T_SEMICOLON" :
                    case "T_ASSIGN" :
                    case "T_COLON" :
                    case "T_LESS_THAN" :
                        exit = 2;
                        break;
                    default:
                      return get$1(env, start_loc, decorators, $$static);
                  }
                } else {
                  return get$1(env, start_loc, decorators, $$static);
                }
                if (exit === 2) {
                  return init$1(env, start_loc, decorators, key$1, async, generator$1, $$static);
                }
                break;
            case "set" :
                var match$2 = Curry._2(Parser_env_Peek.token, undefined, env);
                var exit$1 = 0;
                if (typeof match$2 === "string") {
                  switch (match$2) {
                    case "T_LPAREN" :
                    case "T_SEMICOLON" :
                    case "T_ASSIGN" :
                    case "T_COLON" :
                    case "T_LESS_THAN" :
                        exit$1 = 2;
                        break;
                    default:
                      return set$1(env, start_loc, decorators, $$static);
                  }
                } else {
                  return set$1(env, start_loc, decorators, $$static);
                }
                if (exit$1 === 2) {
                  return init$1(env, start_loc, decorators, key$1, async, generator$1, $$static);
                }
                break;
            default:
              
          }
          break;
      case "Literal" :
      case "Computed" :
          break;
      
    }
  }
  return init$1(env, start_loc, decorators, match[1], async, generator$1, $$static);
}

function elements$1(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_SEMICOLON" :
            token$4(env, "T_SEMICOLON");
            continue ;
        case "T_RCURLY" :
        case "T_EOF" :
            return List.rev(acc);
        default:
          _acc = /* constructor */{
            tag: "::",
            Arg0: Curry._1(class_element, env),
            Arg1: acc
          };
          continue ;
      }
    } else {
      _acc = /* constructor */{
        tag: "::",
        Arg0: Curry._1(class_element, env),
        Arg1: acc
      };
      continue ;
    }
  };
}

function class_body(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_LCURLY");
  var body = elements$1(env, "[]");
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_RCURLY");
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* body */body]
        ];
}

function _class(env) {
  var match;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_EXTENDS") {
    token$4(env, "T_EXTENDS");
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
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_IMPLEMENTS") {
    if (!env[/* parse_options */20][/* types */4]) {
      error$1(env, "UnexpectedTypeInterface");
    }
    token$4(env, "T_IMPLEMENTS");
    $$implements = class_implements(env, "[]");
  } else {
    $$implements = "[]";
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
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
  var decorators$1 = Pervasives.$at(decorators, decorator_list(env$1));
  token$4(env$1, "T_CLASS");
  var tmp_env = with_no_let(true, env$1);
  var match = env$1[/* in_export */6];
  var match$1 = Curry._2(Parser_env_Peek.is_identifier, undefined, tmp_env);
  var id = match && !match$1 ? undefined : Curry._2(Parse.identifier, undefined, tmp_env);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env$1);
  var match$2 = _class(env$1);
  var body = match$2[0];
  var loc = btwn(start_loc, body[0]);
  return /* tuple */[
          loc,
          /* constructor */{
            tag: "ClassDeclaration",
            Arg0: /* record */[
              /* id */id,
              /* body */body,
              /* superClass */match$2[1],
              /* typeParameters */typeParameters,
              /* superTypeParameters */match$2[2],
              /* implements */match$2[3],
              /* classDecorators */decorators$1
            ]
          }
        ];
}

function class_expression(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var decorators = decorator_list(env);
  token$4(env, "T_CLASS");
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  var match$1;
  var exit = 0;
  if (typeof match === "string") {
    switch (match) {
      case "T_LCURLY" :
      case "T_EXTENDS" :
      case "T_LESS_THAN" :
          match$1 = /* tuple */[
            undefined,
            undefined
          ];
          break;
      default:
        exit = 1;
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    var id = Curry._2(Parse.identifier, undefined, env);
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
          /* constructor */{
            tag: "Class",
            Arg0: /* record */[
              /* id */match$1[0],
              /* body */body,
              /* superClass */match$2[1],
              /* typeParameters */match$1[1],
              /* superTypeParameters */match$2[2],
              /* implements */match$2[3],
              /* classDecorators */decorators
            ]
          }
        ];
}

function type_alias_helper(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, "UnexpectedTypeAlias");
  }
  token$4(env, "T_TYPE");
  push_lex_mode(env, "TYPE");
  var id = Curry._2(Parse.identifier, undefined, env);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env);
  token$4(env, "T_ASSIGN");
  var right = wrap(_type, env);
  var match = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
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

function expression(env) {
  var expression$1 = Curry._1(Parse.expression, env);
  var match = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
  var end_loc = match !== undefined ? match : expression$1[0];
  semicolon(env);
  return /* tuple */[
          btwn(expression$1[0], end_loc),
          /* constructor */{
            tag: "Expression",
            Arg0: /* record */[/* expression */expression$1]
          }
        ];
}

function declare_function(env, start_loc) {
  token$4(env, "T_FUNCTION");
  var id = Curry._2(Parse.identifier, undefined, env);
  var start_sig_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var typeParameters = Curry._1(type_parameter_declaration$1, env);
  var match = wrap(function_param_list, env);
  token$4(env, "T_COLON");
  var returnType = wrap(_type, env);
  var end_loc = returnType[0];
  var loc = btwn(start_sig_loc, end_loc);
  var value_001 = /* constructor */{
    tag: "Function",
    Arg0: /* record */[
      /* params */match[1],
      /* returnType */returnType,
      /* rest */match[0],
      /* typeParameters */typeParameters
    ]
  };
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
  var match$1 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
  var end_loc$1 = match$1 !== undefined ? match$1 : end_loc;
  var predicate = Curry._1(Parse.predicate, env);
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
    error$1(env, "UnexpectedTypeDeclaration");
  }
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var match = Curry._2(Parser_env_Peek.token, 1, env);
  if (typeof match === "string") {
    switch (match) {
      case "T_IDENTIFIER" :
          if (Curry._2(Parser_env_Peek.value, 1, env) === "module") {
            token$4(env, "T_DECLARE");
            contextual(env, "module");
            if (in_module || Curry._2(Parser_env_Peek.token, undefined, env) === "T_PERIOD") {
              var env$1 = env;
              var start_loc$1 = start_loc;
              token$4(env$1, "T_PERIOD");
              contextual(env$1, "exports");
              var type_annot = wrap(annotation, env$1);
              var match$1 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
              var end_loc = match$1 !== undefined ? match$1 : type_annot[0];
              semicolon(env$1);
              var loc = btwn(start_loc$1, end_loc);
              return /* tuple */[
                      loc,
                      /* constructor */{
                        tag: "DeclareModuleExports",
                        Arg0: type_annot
                      }
                    ];
            } else {
              var env$2 = env;
              var start_loc$2 = start_loc;
              var match$2 = Curry._2(Parser_env_Peek.token, undefined, env$2);
              var id;
              if (typeof match$2 === "string") {
                id = /* constructor */{
                  tag: "Identifier",
                  Arg0: Curry._2(Parse.identifier, undefined, env$2)
                };
              } else if (/* XXX */match$2.tag === "T_STRING") {
                var match$3 = match$2.Arg0;
                var octal = match$3[3];
                var raw = match$3[2];
                var value = match$3[1];
                var loc$1 = match$3[0];
                if (octal) {
                  strict_error(env$2, "StrictOctalLiteral");
                }
                token$4(env$2, /* constructor */{
                      tag: "T_STRING",
                      Arg0: /* tuple */[
                        loc$1,
                        value,
                        raw,
                        octal
                      ]
                    });
                var value$1 = /* constructor */{
                  tag: "String",
                  Arg0: value
                };
                id = /* constructor */{
                  tag: "Literal",
                  Arg0: /* tuple */[
                    loc$1,
                    /* record */[
                      /* value */value$1,
                      /* raw */raw
                    ]
                  ]
                };
              } else {
                id = /* constructor */{
                  tag: "Identifier",
                  Arg0: Curry._2(Parse.identifier, undefined, env$2)
                };
              }
              var body_start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$2);
              token$4(env$2, "T_LCURLY");
              var match$4 = module_items(env$2, undefined, "[]");
              var module_kind = match$4[0];
              token$4(env$2, "T_RCURLY");
              var body_end_loc = Curry._2(Parser_env_Peek.loc, undefined, env$2);
              var body_loc = btwn(body_start_loc, body_end_loc);
              var body_001 = /* record */[/* body */match$4[1]];
              var body = /* tuple */[
                body_loc,
                body_001
              ];
              var loc$2 = btwn(start_loc$2, body_loc);
              var kind = module_kind !== undefined ? module_kind : /* constructor */({
                    tag: "CommonJS",
                    Arg0: loc$2
                  });
              return /* tuple */[
                      loc$2,
                      /* constructor */{
                        tag: "DeclareModule",
                        Arg0: /* record */[
                          /* id */id,
                          /* body */body,
                          /* kind */kind
                        ]
                      }
                    ];
            }
          }
          break;
      case "T_FUNCTION" :
          token$4(env, "T_DECLARE");
          return declare_function_statement(env, start_loc);
      case "T_VAR" :
          token$4(env, "T_DECLARE");
          return declare_var_statement(env, start_loc);
      case "T_CLASS" :
          token$4(env, "T_DECLARE");
          var env$3 = env;
          var start_loc$3 = start_loc;
          var match$5 = Curry._2(declare_class, env$3, start_loc$3);
          return /* tuple */[
                  match$5[0],
                  /* constructor */{
                    tag: "DeclareClass",
                    Arg0: match$5[1]
                  }
                ];
      case "T_EXPORT" :
          if (in_module) {
            return declare_export_declaration(in_module, env);
          }
          break;
      case "T_INTERFACE" :
          token$4(env, "T_DECLARE");
          return $$interface(env);
      case "T_TYPE" :
          token$4(env, "T_DECLARE");
          return type_alias(env);
      case "T_ASYNC" :
          token$4(env, "T_DECLARE");
          error$1(env, "DeclareAsync");
          token$4(env, "T_ASYNC");
          return declare_function_statement(env, start_loc);
      default:
        
    }
  }
  if (in_module) {
    token$4(env, "T_DECLARE");
    return declare_var_statement(env, start_loc);
  } else {
    return Curry._1(Parse.statement, env);
  }
}

function export_specifiers_and_errs(env, _specifiers, _errs) {
  while(true) {
    var errs = _errs;
    var specifiers = _specifiers;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_RCURLY" :
        case "T_EOF" :
            return /* tuple */[
                    List.rev(specifiers),
                    List.rev(errs)
                  ];
        default:
          
      }
    }
    var match$1 = Curry._1(Parse.identifier_or_reserved_keyword, env);
    var id = match$1[0];
    var match$2;
    if (Curry._2(Parser_env_Peek.value, undefined, env) === "as") {
      contextual(env, "as");
      var match$3 = Curry._1(Parse.identifier_or_reserved_keyword, env);
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
    if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_COMMA") {
      token$4(env, "T_COMMA");
    }
    var errs$1 = err !== undefined ? /* constructor */({
          tag: "::",
          Arg0: err,
          Arg1: errs
        }) : errs;
    _errs = errs$1;
    _specifiers = /* constructor */{
      tag: "::",
      Arg0: specifier,
      Arg1: specifiers
    };
    continue ;
  };
}

function extract_ident_name(param) {
  return param[1][/* name */0];
}

function export_source(env) {
  contextual(env, "from");
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match !== "string" && /* XXX */match.tag === "T_STRING") {
    var match$1 = match.Arg0;
    var octal = match$1[3];
    var raw = match$1[2];
    var value = match$1[1];
    var loc = match$1[0];
    if (octal) {
      strict_error(env, "StrictOctalLiteral");
    }
    token$4(env, /* constructor */{
          tag: "T_STRING",
          Arg0: /* tuple */[
            loc,
            value,
            raw,
            octal
          ]
        });
    var value$1 = /* constructor */{
      tag: "String",
      Arg0: value
    };
    return /* tuple */[
            loc,
            /* record */[
              /* value */value$1,
              /* raw */raw
            ]
          ];
  }
  var raw$1 = Curry._2(Parser_env_Peek.value, undefined, env);
  var value$2 = /* constructor */{
    tag: "String",
    Arg0: raw$1
  };
  var ret_000 = Curry._2(Parser_env_Peek.loc, undefined, env);
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

function declare_var(env, start_loc) {
  token$4(env, "T_VAR");
  var id = Curry._2(Parse.identifier_with_type, env, "StrictVarName");
  var match = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
  var end_loc = match !== undefined ? match : id[0];
  var loc = btwn(start_loc, end_loc);
  semicolon(env);
  return /* tuple */[
          loc,
          /* record */[/* id */id]
        ];
}

function declare_function_statement(env, start_loc) {
  var match = declare_function(env, start_loc);
  return /* tuple */[
          match[0],
          /* constructor */{
            tag: "DeclareFunction",
            Arg0: match[1]
          }
        ];
}

function type_alias(env) {
  if (Curry._2(Parser_env_Peek.is_identifier, 1, env)) {
    var match = type_alias_helper(env);
    return /* tuple */[
            match[0],
            /* constructor */{
              tag: "TypeAlias",
              Arg0: match[1]
            }
          ];
  } else {
    return Curry._1(Parse.statement, env);
  }
}

function declare_var_statement(env, start_loc) {
  var match = declare_var(env, start_loc);
  return /* tuple */[
          match[0],
          /* constructor */{
            tag: "DeclareVariable",
            Arg0: match[1]
          }
        ];
}

function $$interface(env) {
  if (Curry._2(Parser_env_Peek.is_identifier, 1, env)) {
    var match = Curry._1(interface_helper, env);
    return /* tuple */[
            match[0],
            /* constructor */{
              tag: "InterfaceDeclaration",
              Arg0: match[1]
            }
          ];
  } else {
    return expression(env);
  }
}

function declare_export_declaration($staropt$star, env) {
  var allow_export_type = $staropt$star !== undefined ? $staropt$star : false;
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, "UnexpectedTypeDeclaration");
  }
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_DECLARE");
  var env$1 = with_in_export(true, with_strict(true, env));
  token$4(env$1, "T_EXPORT");
  var match = Curry._2(Parser_env_Peek.token, undefined, env$1);
  var exit = 0;
  if (typeof match === "string") {
    switch (match) {
      case "T_DEFAULT" :
          token$4(env$1, "T_DEFAULT");
          var match$1 = Curry._2(Parser_env_Peek.token, undefined, env$1);
          var match$2;
          var exit$1 = 0;
          if (typeof match$1 === "string") {
            switch (match$1) {
              case "T_FUNCTION" :
                  var fn = declare_function(env$1, start_loc);
                  match$2 = /* tuple */[
                    fn[0],
                    /* constructor */{
                      tag: "Function",
                      Arg0: fn
                    }
                  ];
                  break;
              case "T_CLASS" :
                  var _class = Curry._2(declare_class, env$1, start_loc);
                  match$2 = /* tuple */[
                    _class[0],
                    /* constructor */{
                      tag: "Class",
                      Arg0: _class
                    }
                  ];
                  break;
              default:
                exit$1 = 3;
            }
          } else {
            exit$1 = 3;
          }
          if (exit$1 === 3) {
            var _type$1 = wrap(_type, env$1);
            var match$3 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
            var end_loc = match$3 !== undefined ? match$3 : _type$1[0];
            semicolon(env$1);
            match$2 = /* tuple */[
              end_loc,
              /* constructor */{
                tag: "DefaultType",
                Arg0: _type$1
              }
            ];
          }
          return /* tuple */[
                  btwn(start_loc, match$2[0]),
                  /* constructor */{
                    tag: "DeclareExportDeclaration",
                    Arg0: /* record */[
                      /* default */true,
                      /* declaration */match$2[1],
                      /* specifiers */undefined,
                      /* source */undefined
                    ]
                  }
                ];
      case "T_FUNCTION" :
      case "T_VAR" :
      case "T_CONST" :
      case "T_LET" :
      case "T_CLASS" :
          exit = 2;
          break;
      case "T_INTERFACE" :
          if (allow_export_type) {
            var match$4 = Curry._1(interface_helper, env$1);
            var iface_loc = match$4[0];
            var loc = btwn(start_loc, iface_loc);
            return /* tuple */[
                    loc,
                    /* constructor */{
                      tag: "DeclareExportDeclaration",
                      Arg0: /* record */[
                        /* default */false,
                        /* declaration *//* constructor */{
                          tag: "Interface",
                          Arg0: /* tuple */[
                            iface_loc,
                            match$4[1]
                          ]
                        },
                        /* specifiers */undefined,
                        /* source */undefined
                      ]
                    }
                  ];
          } else {
            exit = 1;
          }
          break;
      case "T_TYPE" :
          if (allow_export_type) {
            var match$5 = type_alias_helper(env$1);
            var alias_loc = match$5[0];
            var loc$1 = btwn(start_loc, alias_loc);
            return /* tuple */[
                    loc$1,
                    /* constructor */{
                      tag: "DeclareExportDeclaration",
                      Arg0: /* record */[
                        /* default */false,
                        /* declaration *//* constructor */{
                          tag: "NamedType",
                          Arg0: /* tuple */[
                            alias_loc,
                            match$5[1]
                          ]
                        },
                        /* specifiers */undefined,
                        /* source */undefined
                      ]
                    }
                  ];
          } else {
            exit = 1;
          }
          break;
      case "T_MULT" :
          var loc$2 = Curry._2(Parser_env_Peek.loc, undefined, env$1);
          token$4(env$1, "T_MULT");
          var parse_export_star_as = env$1[/* parse_options */20][/* esproposal_export_star_as */3];
          var local_name = Curry._2(Parser_env_Peek.value, undefined, env$1) === "as" ? (contextual(env$1, "as"), parse_export_star_as ? Curry._2(Parse.identifier, undefined, env$1) : (error$1(env$1, "UnexpectedTypeDeclaration"), undefined)) : undefined;
          var specifiers = /* constructor */{
            tag: "ExportBatchSpecifier",
            Arg0: loc$2,
            Arg1: local_name
          };
          var source = export_source(env$1);
          var match$6 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
          var end_loc$1 = match$6 !== undefined ? match$6 : source[0];
          var source$1 = source;
          semicolon(env$1);
          return /* tuple */[
                  btwn(start_loc, end_loc$1),
                  /* constructor */{
                    tag: "DeclareExportDeclaration",
                    Arg0: /* record */[
                      /* default */false,
                      /* declaration */undefined,
                      /* specifiers */specifiers,
                      /* source */source$1
                    ]
                  }
                ];
      default:
        exit = 1;
    }
  } else {
    exit = 1;
  }
  switch (exit) {
    case 1 :
        var match$7 = Curry._2(Parser_env_Peek.token, undefined, env$1);
        if (typeof match$7 === "string") {
          switch (match$7) {
            case "T_INTERFACE" :
                error$1(env$1, "DeclareExportInterface");
                break;
            case "T_TYPE" :
                error$1(env$1, "DeclareExportType");
                break;
            default:
              
          }
        }
        token$4(env$1, "T_LCURLY");
        var match$8 = export_specifiers_and_errs(env$1, "[]", "[]");
        var specifiers$1 = /* constructor */{
          tag: "ExportSpecifiers",
          Arg0: match$8[0]
        };
        var end_loc$2 = Curry._2(Parser_env_Peek.loc, undefined, env$1);
        token$4(env$1, "T_RCURLY");
        var source$2 = Curry._2(Parser_env_Peek.value, undefined, env$1) === "from" ? export_source(env$1) : (List.iter((function (param) {
                    return error_at(env$1, param);
                  }), match$8[1]), undefined);
        var match$9 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
        var end_loc$3 = match$9 !== undefined ? match$9 : (
            source$2 !== undefined ? source$2[0] : end_loc$2
          );
        semicolon(env$1);
        return /* tuple */[
                btwn(start_loc, end_loc$3),
                /* constructor */{
                  tag: "DeclareExportDeclaration",
                  Arg0: /* record */[
                    /* default */false,
                    /* declaration */undefined,
                    /* specifiers */specifiers$1,
                    /* source */source$2
                  ]
                }
              ];
    case 2 :
        var token$5 = Curry._2(Parser_env_Peek.token, undefined, env$1);
        var match$10;
        var exit$2 = 0;
        if (typeof token$5 === "string") {
          switch (token$5) {
            case "T_FUNCTION" :
                var fn$1 = declare_function(env$1, start_loc);
                match$10 = /* tuple */[
                  fn$1[0],
                  /* constructor */{
                    tag: "Function",
                    Arg0: fn$1
                  }
                ];
                break;
            case "T_VAR" :
            case "T_CONST" :
            case "T_LET" :
                exit$2 = 3;
                break;
            case "T_CLASS" :
                var _class$1 = Curry._2(declare_class, env$1, start_loc);
                match$10 = /* tuple */[
                  _class$1[0],
                  /* constructor */{
                    tag: "Class",
                    Arg0: _class$1
                  }
                ];
                break;
            default:
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    /* tuple */[
                      "parser_flow.ml",
                      3480,
                      17
                    ]
                  ];
          }
        } else {
          throw [
                Caml_builtin_exceptions.assert_failure,
                /* tuple */[
                  "parser_flow.ml",
                  3480,
                  17
                ]
              ];
        }
        if (exit$2 === 3) {
          if (typeof token$5 === "string") {
            switch (token$5) {
              case "T_CONST" :
                  error$1(env$1, "DeclareExportConst");
                  break;
              case "T_LET" :
                  error$1(env$1, "DeclareExportLet");
                  break;
              default:
                
            }
          }
          var $$var = declare_var(env$1, start_loc);
          match$10 = /* tuple */[
            $$var[0],
            /* constructor */{
              tag: "Variable",
              Arg0: $$var
            }
          ];
        }
        return /* tuple */[
                btwn(start_loc, match$10[0]),
                /* constructor */{
                  tag: "DeclareExportDeclaration",
                  Arg0: /* record */[
                    /* default */false,
                    /* declaration */match$10[1],
                    /* specifiers */undefined,
                    /* source */undefined
                  ]
                }
              ];
    
  }
}

function supers(env, _acc) {
  while(true) {
    var acc = _acc;
    var $$super = wrap(generic, env);
    var acc$1 = /* constructor */{
      tag: "::",
      Arg0: $$super,
      Arg1: acc
    };
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string" && match === "T_COMMA") {
      token$4(env, "T_COMMA");
      _acc = acc$1;
      continue ;
    } else {
      return List.rev(acc$1);
    }
  };
}

function interface_helper(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (!env[/* parse_options */20][/* types */4]) {
    error$1(env, "UnexpectedTypeInterface");
  }
  token$4(env, "T_INTERFACE");
  var id = Curry._2(Parse.identifier, undefined, env);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env);
  var $$extends = Curry._2(Parser_env_Peek.token, undefined, env) === "T_EXTENDS" ? (token$4(env, "T_EXTENDS"), supers(env, "[]")) : "[]";
  var body = _object$1(true, env);
  var loc = btwn(start_loc, body[0]);
  return /* tuple */[
          loc,
          /* record */[
            /* id */id,
            /* typeParameters */typeParameters,
            /* body */body,
            /* extends */$$extends,
            /* mixins */"[]"
          ]
        ];
}

function supers$1(env, _acc) {
  while(true) {
    var acc = _acc;
    var $$super = wrap(generic, env);
    var acc$1 = /* constructor */{
      tag: "::",
      Arg0: $$super,
      Arg1: acc
    };
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string" && match === "T_COMMA") {
      token$4(env, "T_COMMA");
      _acc = acc$1;
      continue ;
    } else {
      return List.rev(acc$1);
    }
  };
}

function declare_class(env, start_loc) {
  var env$1 = with_strict(true, env);
  token$4(env$1, "T_CLASS");
  var id = Curry._2(Parse.identifier, undefined, env$1);
  var typeParameters = Curry._1(type_parameter_declaration_with_defaults, env$1);
  var $$extends = Curry._2(Parser_env_Peek.token, undefined, env$1) === "T_EXTENDS" ? (token$4(env$1, "T_EXTENDS"), supers$1(env$1, "[]")) : "[]";
  var mixins = Curry._2(Parser_env_Peek.value, undefined, env$1) === "mixins" ? (contextual(env$1, "mixins"), supers$1(env$1, "[]")) : "[]";
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
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_RCURLY" :
        case "T_EOF" :
            return /* tuple */[
                    module_kind,
                    List.rev(acc)
                  ];
        default:
          
      }
    }
    var stmt = declare(true, env);
    var stmt$1 = stmt[1];
    var loc = stmt[0];
    var module_kind$1;
    if (module_kind !== undefined) {
      if (/* XXX */module_kind.tag === "CommonJS") {
        if (typeof stmt$1 === "string") {
          module_kind$1 = module_kind;
        } else {
          switch (/* XXX */stmt$1.tag) {
            case "DeclareModuleExports" :
                error$1(env, "DuplicateDeclareModuleExports");
                module_kind$1 = module_kind;
                break;
            case "DeclareExportDeclaration" :
                var declaration = stmt$1.Arg0[/* declaration */1];
                if (declaration !== undefined) {
                  switch (/* XXX */declaration.tag) {
                    case "NamedType" :
                    case "Interface" :
                        break;
                    default:
                      error$1(env, "AmbiguousDeclareModuleKind");
                  }
                } else {
                  error$1(env, "AmbiguousDeclareModuleKind");
                }
                module_kind$1 = module_kind;
                break;
            default:
              module_kind$1 = module_kind;
          }
        }
      } else if (typeof stmt$1 === "string" || /* XXX */stmt$1.tag !== "DeclareModuleExports") {
        module_kind$1 = module_kind;
      } else {
        error$1(env, "AmbiguousDeclareModuleKind");
        module_kind$1 = module_kind;
      }
    } else if (typeof stmt$1 === "string") {
      module_kind$1 = module_kind;
    } else {
      switch (/* XXX */stmt$1.tag) {
        case "DeclareModuleExports" :
            module_kind$1 = /* constructor */{
              tag: "CommonJS",
              Arg0: loc
            };
            break;
        case "DeclareExportDeclaration" :
            var declaration$1 = stmt$1.Arg0[/* declaration */1];
            if (declaration$1 !== undefined) {
              switch (/* XXX */declaration$1.tag) {
                case "NamedType" :
                case "Interface" :
                    module_kind$1 = module_kind;
                    break;
                default:
                  module_kind$1 = /* constructor */{
                    tag: "ES",
                    Arg0: loc
                  };
              }
            } else {
              module_kind$1 = /* constructor */{
                tag: "ES",
                Arg0: loc
              };
            }
            break;
        default:
          module_kind$1 = module_kind;
      }
    }
    _acc = /* constructor */{
      tag: "::",
      Arg0: stmt,
      Arg1: acc
    };
    _module_kind = module_kind$1;
    continue ;
  };
}

function fold(acc, _param) {
  while(true) {
    var param = _param;
    var match = param[1];
    switch (/* XXX */match.tag) {
      case "Object" :
          return List.fold_left((function (acc, prop) {
                        if (/* XXX */prop.tag === "Property") {
                          return fold(acc, prop.Arg0[1][/* pattern */1]);
                        } else {
                          return fold(acc, prop.Arg0[1][/* argument */0]);
                        }
                      }), acc, match.Arg0[/* properties */0]);
      case "Array" :
          return List.fold_left((function (acc, elem) {
                        if (elem !== undefined) {
                          var match = elem;
                          if (/* XXX */match.tag === "Element") {
                            return fold(acc, match.Arg0);
                          } else {
                            return fold(acc, match.Arg0[1][/* argument */0]);
                          }
                        } else {
                          return acc;
                        }
                      }), acc, match.Arg0[/* elements */0]);
      case "Assignment" :
          _param = match.Arg0[/* left */0];
          continue ;
      case "Identifier" :
          var match$1 = match.Arg0;
          return /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    match$1[0],
                    match$1[1][/* name */0]
                  ],
                  Arg1: acc
                };
      case "Expression" :
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
    if (/* XXX */match.tag === "InitDeclaration") {
      var match$1 = match.Arg0;
      var declarations = match$1[1][/* declarations */0];
      if (declarations !== "[]" && declarations.Arg0[1][/* init */1] === undefined && declarations.Arg1 === "[]") {
        return /* () */0;
      }
      return error_at(env, /* tuple */[
                  match$1[0],
                  err
                ]);
    } else {
      var match$2 = match.Arg0;
      var loc = match$2[0];
      if (Curry._1(Parse.is_assignable_lhs, /* tuple */[
              loc,
              match$2[1]
            ])) {
        return 0;
      } else {
        return error_at(env, /* tuple */[
                    loc,
                    err
                  ]);
      }
    }
  } else {
    return error$1(env, err);
  }
}

function _if(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_IF");
  token$4(env, "T_LPAREN");
  var test = Curry._1(Parse.expression, env);
  token$4(env, "T_RPAREN");
  Curry._2(Parser_env_Peek.token, undefined, env);
  var consequent = Curry._2(Parser_env_Peek.is_function, undefined, env) ? (strict_error(env, "StrictFunctionStatement"), _function(env)) : Curry._1(Parse.statement, env);
  var alternate = Curry._2(Parser_env_Peek.token, undefined, env) === "T_ELSE" ? (token$4(env, "T_ELSE"), Curry._1(Parse.statement, env)) : undefined;
  var end_loc = alternate !== undefined ? alternate[0] : consequent[0];
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* constructor */{
            tag: "If",
            Arg0: /* record */[
              /* test */test,
              /* consequent */consequent,
              /* alternate */alternate
            ]
          }
        ];
}

function case_list(env, _param) {
  while(true) {
    var param = _param;
    var acc = param[1];
    var seen_default = param[0];
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_RCURLY" :
        case "T_EOF" :
            return List.rev(acc);
        default:
          
      }
    }
    var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
    var test;
    var exit = 0;
    if (typeof match$1 === "string" && match$1 === "T_DEFAULT") {
      if (seen_default) {
        error$1(env, "MultipleDefaultsInSwitch");
      }
      token$4(env, "T_DEFAULT");
      test = undefined;
    } else {
      exit = 1;
    }
    if (exit === 1) {
      token$4(env, "T_CASE");
      test = Curry._1(Parse.expression, env);
    }
    var seen_default$1 = seen_default || test === undefined;
    var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    token$4(env, "T_COLON");
    var term_fn = function (param) {
      if (typeof param === "string") {
        switch (param) {
          case "T_RCURLY" :
          case "T_CASE" :
          case "T_DEFAULT" :
              return true;
          default:
            return false;
        }
      } else {
        return false;
      }
    };
    var consequent = Curry._2(Parse.statement_list, term_fn, with_in_switch(true, env));
    var match$2 = List.rev(consequent);
    var end_loc$1 = match$2 !== "[]" ? match$2.Arg0[0] : end_loc;
    var acc$1 = /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        btwn(start_loc, end_loc$1),
        /* record */[
          /* test */test,
          /* consequent */consequent
        ]
      ],
      Arg1: acc
    };
    _param = /* tuple */[
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
  var match$2 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env);
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
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match !== "string" && /* XXX */match.tag === "T_STRING") {
    var match$1 = match.Arg0;
    var octal = match$1[3];
    var raw = match$1[2];
    var value = match$1[1];
    var loc = match$1[0];
    if (octal) {
      strict_error(env, "StrictOctalLiteral");
    }
    token$4(env, /* constructor */{
          tag: "T_STRING",
          Arg0: /* tuple */[
            loc,
            value,
            raw,
            octal
          ]
        });
    var value$1 = /* constructor */{
      tag: "String",
      Arg0: value
    };
    return /* tuple */[
            loc,
            /* record */[
              /* value */value$1,
              /* raw */raw
            ]
          ];
  }
  var raw$1 = Curry._2(Parser_env_Peek.value, undefined, env);
  var value$2 = /* constructor */{
    tag: "String",
    Arg0: raw$1
  };
  var ret_000 = Curry._2(Parser_env_Peek.loc, undefined, env);
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

function specifier_list(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_RCURLY" :
        case "T_EOF" :
            return List.rev(acc);
        default:
          
      }
    }
    var match$1 = Curry._1(Parse.identifier_or_reserved_keyword, env);
    var err = match$1[1];
    var remote = match$1[0];
    var specifier;
    if (Curry._2(Parser_env_Peek.value, undefined, env) === "as") {
      contextual(env, "as");
      var local = Curry._2(Parse.identifier, undefined, env);
      specifier = /* constructor */{
        tag: "ImportNamedSpecifier",
        Arg0: /* record */[
          /* local */local,
          /* remote */remote
        ]
      };
    } else {
      if (err !== undefined) {
        error_at(env, err);
      }
      specifier = /* constructor */{
        tag: "ImportNamedSpecifier",
        Arg0: /* record */[
          /* local */undefined,
          /* remote */remote
        ]
      };
    }
    if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_COMMA") {
      token$4(env, "T_COMMA");
    }
    _acc = /* constructor */{
      tag: "::",
      Arg0: specifier,
      Arg1: acc
    };
    continue ;
  };
}

function named_or_namespace_specifier(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string" && match === "T_MULT") {
    token$4(env, "T_MULT");
    contextual(env, "as");
    var id = Curry._2(Parse.identifier, undefined, env);
    return /* constructor */{
            tag: "::",
            Arg0: /* constructor */{
              tag: "ImportNamespaceSpecifier",
              Arg0: /* tuple */[
                btwn(start_loc, id[0]),
                id
              ]
            },
            Arg1: "[]"
          };
  }
  token$4(env, "T_LCURLY");
  var specifiers = specifier_list(env, "[]");
  token$4(env, "T_RCURLY");
  return specifiers;
}

function from_expr(env, param) {
  var expr = param[1];
  var loc = param[0];
  if (typeof expr !== "string") {
    switch (/* XXX */expr.tag) {
      case "Array" :
          var env$1 = env;
          var param$1 = /* tuple */[
            loc,
            expr.Arg0
          ];
          var elements = List.map((function (param) {
                  var env$2 = env$1;
                  var param$1 = param;
                  if (param$1 !== undefined) {
                    var match = param$1;
                    if (/* XXX */match.tag === "Expression") {
                      var match$1 = match.Arg0;
                      return /* constructor */{
                              tag: "Element",
                              Arg0: Curry._2(Parse.pattern_from_expr, env$2, /* tuple */[
                                    match$1[0],
                                    match$1[1]
                                  ])
                            };
                    } else {
                      var match$2 = match.Arg0;
                      var argument = Curry._2(Parse.pattern_from_expr, env$2, match$2[1][/* argument */0]);
                      return /* constructor */{
                              tag: "Spread",
                              Arg0: /* tuple */[
                                match$2[0],
                                /* record */[/* argument */argument]
                              ]
                            };
                    }
                  }
                  
                }), param$1[1][/* elements */0]);
          return /* tuple */[
                  param$1[0],
                  /* constructor */{
                    tag: "Array",
                    Arg0: /* record */[
                      /* elements */elements,
                      /* typeAnnotation */undefined
                    ]
                  }
                ];
      case "Object" :
          var env$2 = env;
          var param$2 = /* tuple */[
            loc,
            expr.Arg0
          ];
          var properties = List.map((function (param) {
                  var env$3 = env$2;
                  var prop = param;
                  if (/* XXX */prop.tag === "Property") {
                    var match = prop.Arg0;
                    var match$1 = match[1];
                    var key = match$1[/* key */0];
                    var key$1;
                    switch (/* XXX */key.tag) {
                      case "Literal" :
                          key$1 = /* constructor */{
                            tag: "Literal",
                            Arg0: key.Arg0
                          };
                          break;
                      case "Identifier" :
                          key$1 = /* constructor */{
                            tag: "Identifier",
                            Arg0: key.Arg0
                          };
                          break;
                      case "Computed" :
                          key$1 = /* constructor */{
                            tag: "Computed",
                            Arg0: key.Arg0
                          };
                          break;
                      
                    }
                    var pattern = Curry._2(Parse.pattern_from_expr, env$3, match$1[/* value */1]);
                    return /* constructor */{
                            tag: "Property",
                            Arg0: /* tuple */[
                              match[0],
                              /* record */[
                                /* key */key$1,
                                /* pattern */pattern,
                                /* shorthand */match$1[/* shorthand */4]
                              ]
                            ]
                          };
                  } else {
                    var match$2 = prop.Arg0;
                    var argument = Curry._2(Parse.pattern_from_expr, env$3, match$2[1][/* argument */0]);
                    return /* constructor */{
                            tag: "SpreadProperty",
                            Arg0: /* tuple */[
                              match$2[0],
                              /* record */[/* argument */argument]
                            ]
                          };
                  }
                }), param$2[1][/* properties */0]);
          return /* tuple */[
                  param$2[0],
                  /* constructor */{
                    tag: "Object",
                    Arg0: /* record */[
                      /* properties */properties,
                      /* typeAnnotation */undefined
                    ]
                  }
                ];
      case "Assignment" :
          var match = expr.Arg0;
          if (match[/* operator */0] === "Assign") {
            return /* tuple */[
                    loc,
                    /* constructor */{
                      tag: "Assignment",
                      Arg0: /* record */[
                        /* left */match[/* left */1],
                        /* right */match[/* right */2]
                      ]
                    }
                  ];
          }
          break;
      case "Identifier" :
          return /* tuple */[
                  loc,
                  /* constructor */{
                    tag: "Identifier",
                    Arg0: expr.Arg0
                  }
                ];
      default:
        
    }
  }
  return /* tuple */[
          loc,
          /* constructor */{
            tag: "Expression",
            Arg0: /* tuple */[
              loc,
              expr
            ]
          }
        ];
}

function _object$2(restricted_error) {
  var property = function (env) {
    var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
    if (maybe(env, "T_ELLIPSIS")) {
      var argument = pattern$1(env, restricted_error);
      var loc = btwn(start_loc, argument[0]);
      return /* constructor */{
              tag: "SpreadProperty",
              Arg0: /* tuple */[
                loc,
                /* record */[/* argument */argument]
              ]
            };
    } else {
      var match = Curry._1(Parse.object_key, env);
      var match$1 = match[1];
      var key;
      switch (/* XXX */match$1.tag) {
        case "Literal" :
            key = /* constructor */{
              tag: "Literal",
              Arg0: match$1.Arg0
            };
            break;
        case "Identifier" :
            key = /* constructor */{
              tag: "Identifier",
              Arg0: match$1.Arg0
            };
            break;
        case "Computed" :
            key = /* constructor */{
              tag: "Computed",
              Arg0: match$1.Arg0
            };
            break;
        
      }
      var match$2 = Curry._2(Parser_env_Peek.token, undefined, env);
      var prop;
      var exit = 0;
      if (typeof match$2 === "string" && match$2 === "T_COLON") {
        token$4(env, "T_COLON");
        prop = /* tuple */[
          pattern$1(env, restricted_error),
          false
        ];
      } else {
        exit = 1;
      }
      if (exit === 1) {
        switch (/* XXX */key.tag) {
          case "Identifier" :
              var id = key.Arg0;
              var pattern_000 = id[0];
              var pattern_001 = /* constructor */{
                tag: "Identifier",
                Arg0: id
              };
              var pattern$2 = /* tuple */[
                pattern_000,
                pattern_001
              ];
              prop = /* tuple */[
                pattern$2,
                true
              ];
              break;
          case "Literal" :
          case "Computed" :
              error_unexpected(env);
              prop = undefined;
              break;
          
        }
      }
      if (prop !== undefined) {
        var match$3 = prop;
        var pattern$3 = match$3[0];
        var match$4 = Curry._2(Parser_env_Peek.token, undefined, env);
        var pattern$4;
        if (typeof match$4 === "string" && match$4 === "T_ASSIGN") {
          token$4(env, "T_ASSIGN");
          var $$default = Curry._1(Parse.assignment, env);
          var loc$1 = btwn(pattern$3[0], $$default[0]);
          pattern$4 = /* tuple */[
            loc$1,
            /* constructor */{
              tag: "Assignment",
              Arg0: /* record */[
                /* left */pattern$3,
                /* right */$$default
              ]
            }
          ];
        } else {
          pattern$4 = pattern$3;
        }
        var loc$2 = btwn(start_loc, pattern$4[0]);
        return /* constructor */{
                tag: "Property",
                Arg0: /* tuple */[
                  loc$2,
                  /* record */[
                    /* key */key,
                    /* pattern */pattern$4,
                    /* shorthand */match$3[1]
                  ]
                ]
              };
      } else {
        return ;
      }
    }
  };
  var properties = function (env, _acc) {
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_Peek.token, undefined, env);
      if (typeof match === "string") {
        switch (match) {
          case "T_RCURLY" :
          case "T_EOF" :
              return List.rev(acc);
          default:
            
        }
      }
      var match$1 = property(env);
      if (match$1 !== undefined) {
        if (Curry._2(Parser_env_Peek.token, undefined, env) !== "T_RCURLY") {
          token$4(env, "T_COMMA");
        }
        _acc = /* constructor */{
          tag: "::",
          Arg0: match$1,
          Arg1: acc
        };
        continue ;
      } else {
        continue ;
      }
    };
  };
  return (function (env) {
      var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, "T_LCURLY");
      var properties$1 = properties(env, "[]");
      var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, "T_RCURLY");
      var match;
      if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_COLON") {
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
              /* constructor */{
                tag: "Object",
                Arg0: /* record */[
                  /* properties */properties$1,
                  /* typeAnnotation */match[1]
                ]
              }
            ];
    });
}

function _array(restricted_error) {
  var elements = function (env, _acc) {
    while(true) {
      var acc = _acc;
      var match = Curry._2(Parser_env_Peek.token, undefined, env);
      if (typeof match === "string") {
        switch (match) {
          case "T_COMMA" :
              token$4(env, "T_COMMA");
              _acc = /* constructor */{
                tag: "::",
                Arg0: undefined,
                Arg1: acc
              };
              continue ;
          case "T_ELLIPSIS" :
              var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
              token$4(env, "T_ELLIPSIS");
              var argument = pattern$1(env, restricted_error);
              var loc = btwn(start_loc, argument[0]);
              var element = /* constructor */{
                tag: "Spread",
                Arg0: /* tuple */[
                  loc,
                  /* record */[/* argument */argument]
                ]
              };
              _acc = /* constructor */{
                tag: "::",
                Arg0: element,
                Arg1: acc
              };
              continue ;
          case "T_RBRACKET" :
          case "T_EOF" :
              return List.rev(acc);
          default:
            
        }
      }
      var pattern$2 = pattern$1(env, restricted_error);
      var match$1 = Curry._2(Parser_env_Peek.token, undefined, env);
      var pattern$3;
      if (typeof match$1 === "string" && match$1 === "T_ASSIGN") {
        token$4(env, "T_ASSIGN");
        var $$default = Curry._1(Parse.expression, env);
        var loc$1 = btwn(pattern$2[0], $$default[0]);
        pattern$3 = /* tuple */[
          loc$1,
          /* constructor */{
            tag: "Assignment",
            Arg0: /* record */[
              /* left */pattern$2,
              /* right */$$default
            ]
          }
        ];
      } else {
        pattern$3 = pattern$2;
      }
      var element$1 = /* constructor */{
        tag: "Element",
        Arg0: pattern$3
      };
      if (Curry._2(Parser_env_Peek.token, undefined, env) !== "T_RBRACKET") {
        token$4(env, "T_COMMA");
      }
      _acc = /* constructor */{
        tag: "::",
        Arg0: element$1,
        Arg1: acc
      };
      continue ;
    };
  };
  return (function (env) {
      var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, "T_LBRACKET");
      var elements$1 = elements(env, "[]");
      var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, "T_RBRACKET");
      var match;
      if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_COLON") {
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
              /* constructor */{
                tag: "Array",
                Arg0: /* record */[
                  /* elements */elements$1,
                  /* typeAnnotation */match[1]
                ]
              }
            ];
    });
}

function pattern$1(env, restricted_error) {
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string") {
    switch (match) {
      case "T_LCURLY" :
          return _object$2(restricted_error)(env);
      case "T_LBRACKET" :
          return _array(restricted_error)(env);
      default:
        
    }
  }
  var id = Curry._2(Parse.identifier_with_type, env, restricted_error);
  return /* tuple */[
          id[0],
          /* constructor */{
            tag: "Identifier",
            Arg0: id
          }
        ];
}

function spread_attribute(env) {
  push_lex_mode(env, "NORMAL");
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_LCURLY");
  token$4(env, "T_ELLIPSIS");
  var argument = Curry._1(assignment, env);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_RCURLY");
  pop_lex_mode(env);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* argument */argument]
        ];
}

function expression_container(env) {
  push_lex_mode(env, "NORMAL");
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_LCURLY");
  var expression;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_RCURLY") {
    var empty_loc = btwn_exclusive(start_loc, Curry._2(Parser_env_Peek.loc, undefined, env));
    expression = /* constructor */{
      tag: "EmptyExpression",
      Arg0: empty_loc
    };
  } else {
    expression = /* constructor */{
      tag: "Expression",
      Arg0: Curry._1(Parse.expression, env)
    };
  }
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_RCURLY");
  pop_lex_mode(env);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* expression */expression]
        ];
}

function identifier$1(env) {
  var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var name = Curry._2(Parser_env_Peek.value, undefined, env);
  token$4(env, "T_JSX_IDENTIFIER");
  return /* tuple */[
          loc,
          /* record */[/* name */name]
        ];
}

function member_expression(env, _member) {
  while(true) {
    var member = _member;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string" && match === "T_PERIOD") {
      var _object = /* constructor */{
        tag: "MemberExpression",
        Arg0: member
      };
      token$4(env, "T_PERIOD");
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
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string") {
    switch (match) {
      case "T_PERIOD" :
          var _object = /* constructor */{
            tag: "Identifier",
            Arg0: name$1
          };
          token$4(env, "T_PERIOD");
          var property = identifier$1(env);
          var loc = btwn(name$1[0], property[0]);
          var member_001 = /* record */[
            /* _object */_object,
            /* property */property
          ];
          var member = /* tuple */[
            loc,
            member_001
          ];
          return /* constructor */{
                  tag: "MemberExpression",
                  Arg0: member_expression(env, member)
                };
      case "T_COLON" :
          token$4(env, "T_COLON");
          var name$2 = identifier$1(env);
          var loc$1 = btwn(name$1[0], name$2[0]);
          return /* constructor */{
                  tag: "NamespacedName",
                  Arg0: /* tuple */[
                    loc$1,
                    /* record */[
                      /* namespace */name$1,
                      /* name */name$2
                    ]
                  ]
                };
      default:
        return /* constructor */{
                tag: "Identifier",
                Arg0: name$1
              };
    }
  } else {
    return /* constructor */{
            tag: "Identifier",
            Arg0: name$1
          };
  }
}

function attribute(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var name = identifier$1(env);
  var match;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_COLON") {
    token$4(env, "T_COLON");
    var name$1 = identifier$1(env);
    var loc = btwn(name[0], name$1[0]);
    match = /* tuple */[
      loc,
      /* constructor */{
        tag: "NamespacedName",
        Arg0: /* tuple */[
          loc,
          /* record */[
            /* namespace */name,
            /* name */name$1
          ]
        ]
      }
    ];
  } else {
    match = /* tuple */[
      name[0],
      /* constructor */{
        tag: "Identifier",
        Arg0: name
      }
    ];
  }
  var match$1;
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_ASSIGN") {
    token$4(env, "T_ASSIGN");
    var token$5 = Curry._2(Parser_env_Peek.token, undefined, env);
    var exit = 0;
    if (typeof token$5 === "string") {
      if (token$5 === "T_LCURLY") {
        var match$2 = expression_container(env);
        var expression_container$1 = match$2[1];
        var loc$1 = match$2[0];
        var match$3 = expression_container$1[/* expression */0];
        if (/* XXX */match$3.tag !== "Expression") {
          error$1(env, "JSXAttributeValueEmptyExpression");
        }
        match$1 = /* tuple */[
          loc$1,
          /* constructor */{
            tag: "ExpressionContainer",
            Arg0: loc$1,
            Arg1: expression_container$1
          }
        ];
      } else {
        exit = 1;
      }
    } else if (/* XXX */token$5.tag === "T_JSX_TEXT") {
      var match$4 = token$5.Arg0;
      var loc$2 = match$4[0];
      token$4(env, token$5);
      var value = /* constructor */{
        tag: "String",
        Arg0: match$4[1]
      };
      match$1 = /* tuple */[
        loc$2,
        /* constructor */{
          tag: "Literal",
          Arg0: loc$2,
          Arg1: /* record */[
            /* value */value,
            /* raw */match$4[2]
          ]
        }
      ];
    } else {
      exit = 1;
    }
    if (exit === 1) {
      error$1(env, "InvalidJSXAttributeValue");
      var loc$3 = Curry._2(Parser_env_Peek.loc, undefined, env);
      match$1 = /* tuple */[
        loc$3,
        /* constructor */{
          tag: "Literal",
          Arg0: loc$3,
          Arg1: /* record */[
            /* value : constructor */{
              tag: "String",
              Arg0: ""
            },
            /* raw */""
          ]
        }
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
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_LCURLY" :
            var attribute$1 = /* constructor */{
              tag: "SpreadAttribute",
              Arg0: spread_attribute(env)
            };
            _acc = /* constructor */{
              tag: "::",
              Arg0: attribute$1,
              Arg1: acc
            };
            continue ;
        case "T_GREATER_THAN" :
        case "T_DIV" :
        case "T_EOF" :
            return List.rev(acc);
        default:
          
      }
    }
    var attribute$2 = /* constructor */{
      tag: "Attribute",
      Arg0: attribute(env)
    };
    _acc = /* constructor */{
      tag: "::",
      Arg0: attribute$2,
      Arg1: acc
    };
    continue ;
  };
}

function opening_element_without_lt(env, start_loc) {
  var name$1 = name(env);
  var attributes$1 = attributes(env, "[]");
  var selfClosing = Curry._2(Parser_env_Peek.token, undefined, env) === "T_DIV";
  if (selfClosing) {
    token$4(env, "T_DIV");
  }
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_GREATER_THAN");
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
  token$4(env, "T_DIV");
  var name$1 = name(env);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_GREATER_THAN");
  double_pop_lex_mode(env);
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* name */name$1]
        ];
}

function child(env) {
  var token$5 = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof token$5 === "string") {
    if (token$5 === "T_LCURLY") {
      var expression_container$1 = expression_container(env);
      return /* tuple */[
              expression_container$1[0],
              /* constructor */{
                tag: "ExpressionContainer",
                Arg0: expression_container$1[1]
              }
            ];
    }
    
  } else if (/* XXX */token$5.tag === "T_JSX_TEXT") {
    var match = token$5.Arg0;
    token$4(env, token$5);
    return /* tuple */[
            match[0],
            /* constructor */{
              tag: "Text",
              Arg0: /* record */[
                /* value */match[1],
                /* raw */match[2]
              ]
            }
          ];
  }
  var element$1 = element(env);
  return /* tuple */[
          element$1[0],
          /* constructor */{
            tag: "Element",
            Arg0: element$1[1]
          }
        ];
}

function element(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  push_lex_mode(env, "JSX_TAG");
  token$4(env, "T_LESS_THAN");
  return Curry._2(element_without_lt, env, start_loc);
}

function element_or_closing(env) {
  push_lex_mode(env, "JSX_TAG");
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_LESS_THAN");
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string") {
    switch (match) {
      case "T_DIV" :
      case "T_EOF" :
          return /* constructor */{
                  tag: "Closing",
                  Arg0: closing_element_without_lt(env, start_loc)
                };
      default:
        return /* constructor */{
                tag: "ChildElement",
                Arg0: Curry._2(element_without_lt, env, start_loc)
              };
    }
  } else {
    return /* constructor */{
            tag: "ChildElement",
            Arg0: Curry._2(element_without_lt, env, start_loc)
          };
  }
}

function children_and_closing(env, _acc) {
  while(true) {
    var acc = _acc;
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof match === "string") {
      switch (match) {
        case "T_LESS_THAN" :
            var match$1 = element_or_closing(env);
            if (/* XXX */match$1.tag === "Closing") {
              return /* tuple */[
                      List.rev(acc),
                      match$1.Arg0
                    ];
            } else {
              var element = match$1.Arg0;
              var element_000 = element[0];
              var element_001 = /* constructor */{
                tag: "Element",
                Arg0: element[1]
              };
              var element$1 = /* tuple */[
                element_000,
                element_001
              ];
              _acc = /* constructor */{
                tag: "::",
                Arg0: element$1,
                Arg1: acc
              };
              continue ;
            }
        case "T_EOF" :
            error_unexpected(env);
            return /* tuple */[
                    List.rev(acc),
                    undefined
                  ];
        default:
          _acc = /* constructor */{
            tag: "::",
            Arg0: child(env),
            Arg1: acc
          };
          continue ;
      }
    } else {
      _acc = /* constructor */{
        tag: "::",
        Arg0: child(env),
        Arg1: acc
      };
      continue ;
    }
  };
}

function normalize(name) {
  switch (/* XXX */name.tag) {
    case "Identifier" :
        return name.Arg0[1][/* name */0];
    case "NamespacedName" :
        var match = name.Arg0[1];
        return match[/* namespace */0][1][/* name */0] + (":" + match[/* name */1][1][/* name */0]);
    case "MemberExpression" :
        var match$1 = name.Arg0[1];
        var _object = match$1[/* _object */0];
        var _object$1;
        _object$1 = /* XXX */_object.tag === "Identifier" ? _object.Arg0[1][/* name */0] : normalize(/* constructor */{
                tag: "MemberExpression",
                Arg0: _object.Arg0
              });
        return _object$1 + ("." + match$1[/* property */1][1][/* name */0]);
    
  }
}

function element_without_lt(env, start_loc) {
  var openingElement = opening_element_without_lt(env, start_loc);
  var match = openingElement[1][/* selfClosing */1] ? /* tuple */[
      "[]",
      undefined
    ] : (push_lex_mode(env, "JSX_CHILD"), children_and_closing(env, "[]"));
  var closingElement = match[1];
  var end_loc;
  if (closingElement !== undefined) {
    var match$1 = closingElement;
    var opening_name = normalize(openingElement[1][/* name */0]);
    if (normalize(match$1[1][/* name */0]) !== opening_name) {
      error$1(env, /* constructor */{
            tag: "ExpectedJSXClosingTag",
            Arg0: opening_name
          });
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

function module_item(env) {
  var decorators = decorator_list(env);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string") {
    switch (match) {
      case "T_EXPORT" :
          var env$1 = env;
          var decorators$1 = decorators;
          var env$2 = with_in_export(true, with_strict(true, env$1));
          var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$2);
          token$4(env$2, "T_EXPORT");
          var match$1 = Curry._2(Parser_env_Peek.token, undefined, env$2);
          var exit = 0;
          if (typeof match$1 === "string") {
            switch (match$1) {
              case "T_DEFAULT" :
                  token$4(env$2, "T_DEFAULT");
                  record_export(env$2, /* tuple */[
                        btwn(start_loc, Curry._2(Parser_env_Peek.loc, undefined, env$2)),
                        "default"
                      ]);
                  var match$2 = Curry._2(Parser_env_Peek.token, undefined, env$2);
                  var match$3;
                  var exit$1 = 0;
                  if (typeof match$2 === "string" && match$2 === "T_FUNCTION") {
                    var fn = _function(env$2);
                    match$3 = /* tuple */[
                      fn[0],
                      /* constructor */{
                        tag: "Declaration",
                        Arg0: fn
                      }
                    ];
                  } else {
                    exit$1 = 3;
                  }
                  if (exit$1 === 3) {
                    if (Curry._2(Parser_env_Peek.is_class, undefined, env$2)) {
                      var _class = class_declaration(env$2, decorators$1);
                      match$3 = /* tuple */[
                        _class[0],
                        /* constructor */{
                          tag: "Declaration",
                          Arg0: _class
                        }
                      ];
                    } else {
                      var expr = Curry._1(Parse.assignment, env$2);
                      var match$4 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$2);
                      var end_loc = match$4 !== undefined ? match$4 : expr[0];
                      semicolon(env$2);
                      match$3 = /* tuple */[
                        end_loc,
                        /* constructor */{
                          tag: "Expression",
                          Arg0: expr
                        }
                      ];
                    }
                  }
                  return /* tuple */[
                          btwn(start_loc, match$3[0]),
                          /* constructor */{
                            tag: "ExportDeclaration",
                            Arg0: /* record */[
                              /* default */true,
                              /* declaration */match$3[1],
                              /* specifiers */undefined,
                              /* source */undefined,
                              /* exportKind */"ExportValue"
                            ]
                          }
                        ];
              case "T_INTERFACE" :
                  if (!env$2[/* parse_options */20][/* types */4]) {
                    error$1(env$2, "UnexpectedTypeExport");
                  }
                  var $$interface$1 = $$interface(env$2);
                  var match$5 = $$interface$1[1];
                  if (typeof match$5 === "string") {
                    throw [
                          Caml_builtin_exceptions.failure,
                          "Internal Flow Error! Parsed `export interface` into something other than an interface declaration!"
                        ];
                  } else if (/* XXX */match$5.tag === "InterfaceDeclaration") {
                    record_export(env$2, /* tuple */[
                          $$interface$1[0],
                          extract_ident_name(match$5.Arg0[/* id */0])
                        ]);
                  } else {
                    throw [
                          Caml_builtin_exceptions.failure,
                          "Internal Flow Error! Parsed `export interface` into something other than an interface declaration!"
                        ];
                  }
                  var end_loc$1 = $$interface$1[0];
                  return /* tuple */[
                          btwn(start_loc, end_loc$1),
                          /* constructor */{
                            tag: "ExportDeclaration",
                            Arg0: /* record */[
                              /* default */false,
                              /* declaration *//* constructor */{
                                tag: "Declaration",
                                Arg0: $$interface$1
                              },
                              /* specifiers */undefined,
                              /* source */undefined,
                              /* exportKind */"ExportType"
                            ]
                          }
                        ];
              case "T_TYPE" :
                  if (Curry._2(Parser_env_Peek.token, 1, env$2) !== "T_LCURLY") {
                    if (!env$2[/* parse_options */20][/* types */4]) {
                      error$1(env$2, "UnexpectedTypeExport");
                    }
                    var type_alias$1 = type_alias(env$2);
                    var match$6 = type_alias$1[1];
                    if (typeof match$6 === "string") {
                      throw [
                            Caml_builtin_exceptions.failure,
                            "Internal Flow Error! Parsed `export type` into something other than a type alias!"
                          ];
                    } else if (/* XXX */match$6.tag === "TypeAlias") {
                      record_export(env$2, /* tuple */[
                            type_alias$1[0],
                            extract_ident_name(match$6.Arg0[/* id */0])
                          ]);
                    } else {
                      throw [
                            Caml_builtin_exceptions.failure,
                            "Internal Flow Error! Parsed `export type` into something other than a type alias!"
                          ];
                    }
                    var end_loc$2 = type_alias$1[0];
                    return /* tuple */[
                            btwn(start_loc, end_loc$2),
                            /* constructor */{
                              tag: "ExportDeclaration",
                              Arg0: /* record */[
                                /* default */false,
                                /* declaration *//* constructor */{
                                  tag: "Declaration",
                                  Arg0: type_alias$1
                                },
                                /* specifiers */undefined,
                                /* source */undefined,
                                /* exportKind */"ExportType"
                              ]
                            }
                          ];
                  } else {
                    exit = 1;
                  }
                  break;
              case "T_AT" :
              case "T_FUNCTION" :
              case "T_VAR" :
              case "T_CONST" :
              case "T_LET" :
              case "T_CLASS" :
              case "T_ASYNC" :
                  exit = 2;
                  break;
              case "T_MULT" :
                  var loc = Curry._2(Parser_env_Peek.loc, undefined, env$2);
                  token$4(env$2, "T_MULT");
                  var parse_export_star_as = env$2[/* parse_options */20][/* esproposal_export_star_as */3];
                  var local_name = Curry._2(Parser_env_Peek.value, undefined, env$2) === "as" ? (contextual(env$2, "as"), parse_export_star_as ? Curry._2(Parse.identifier, undefined, env$2) : (error$1(env$2, "UnexpectedTypeDeclaration"), undefined)) : undefined;
                  var specifiers = /* constructor */{
                    tag: "ExportBatchSpecifier",
                    Arg0: loc,
                    Arg1: local_name
                  };
                  var source$1 = export_source(env$2);
                  var match$7 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$2);
                  var end_loc$3 = match$7 !== undefined ? match$7 : source$1[0];
                  var source$2 = source$1;
                  semicolon(env$2);
                  return /* tuple */[
                          btwn(start_loc, end_loc$3),
                          /* constructor */{
                            tag: "ExportDeclaration",
                            Arg0: /* record */[
                              /* default */false,
                              /* declaration */undefined,
                              /* specifiers */specifiers,
                              /* source */source$2,
                              /* exportKind */"ExportValue"
                            ]
                          }
                        ];
              default:
                exit = 1;
            }
          } else {
            exit = 1;
          }
          switch (exit) {
            case 1 :
                var match$8 = Curry._2(Parser_env_Peek.token, undefined, env$2);
                var exportKind;
                if (typeof match$8 === "string" && match$8 === "T_TYPE") {
                  token$3(env$2);
                  exportKind = "ExportType";
                } else {
                  exportKind = "ExportValue";
                }
                token$4(env$2, "T_LCURLY");
                var match$9 = export_specifiers_and_errs(env$2, "[]", "[]");
                var specifiers$1 = /* constructor */{
                  tag: "ExportSpecifiers",
                  Arg0: match$9[0]
                };
                var end_loc$4 = Curry._2(Parser_env_Peek.loc, undefined, env$2);
                token$4(env$2, "T_RCURLY");
                var source$3 = Curry._2(Parser_env_Peek.value, undefined, env$2) === "from" ? export_source(env$2) : (List.iter((function (param) {
                            return error_at(env$2, param);
                          }), match$9[1]), undefined);
                var match$10 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$2);
                var end_loc$5 = match$10 !== undefined ? match$10 : (
                    source$3 !== undefined ? source$3[0] : end_loc$4
                  );
                semicolon(env$2);
                return /* tuple */[
                        btwn(start_loc, end_loc$5),
                        /* constructor */{
                          tag: "ExportDeclaration",
                          Arg0: /* record */[
                            /* default */false,
                            /* declaration */undefined,
                            /* specifiers */specifiers$1,
                            /* source */source$3,
                            /* exportKind */exportKind
                          ]
                        }
                      ];
            case 2 :
                var stmt = Curry._2(Parse.statement_list_item, decorators$1, env$2);
                var match$11 = stmt[1];
                var loc$1 = stmt[0];
                var names;
                if (typeof match$11 === "string") {
                  throw [
                        Caml_builtin_exceptions.failure,
                        "Internal Flow Error! Unexpected export statement declaration!"
                      ];
                } else {
                  switch (/* XXX */match$11.tag) {
                    case "FunctionDeclaration" :
                        var match$12 = match$11.Arg0[/* id */0];
                        if (match$12 !== undefined) {
                          names = /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              loc$1,
                              extract_ident_name(match$12)
                            ],
                            Arg1: "[]"
                          };
                        } else {
                          error_at(env$2, /* tuple */[
                                loc$1,
                                "ExportNamelessFunction"
                              ]);
                          names = "[]";
                        }
                        break;
                    case "VariableDeclaration" :
                        names = List.fold_left((function (names, param) {
                                var id = param[1][/* id */0];
                                var param$1 = names;
                                var param$2 = /* constructor */{
                                  tag: "::",
                                  Arg0: id,
                                  Arg1: "[]"
                                };
                                return List.fold_left(fold, param$1, param$2);
                              }), "[]", match$11.Arg0[/* declarations */0]);
                        break;
                    case "ClassDeclaration" :
                        var match$13 = match$11.Arg0[/* id */0];
                        if (match$13 !== undefined) {
                          names = /* constructor */{
                            tag: "::",
                            Arg0: /* tuple */[
                              loc$1,
                              extract_ident_name(match$13)
                            ],
                            Arg1: "[]"
                          };
                        } else {
                          error_at(env$2, /* tuple */[
                                loc$1,
                                "ExportNamelessClass"
                              ]);
                          names = "[]";
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
                var declaration = /* constructor */{
                  tag: "Declaration",
                  Arg0: stmt
                };
                return /* tuple */[
                        btwn(start_loc, stmt[0]),
                        /* constructor */{
                          tag: "ExportDeclaration",
                          Arg0: /* record */[
                            /* default */false,
                            /* declaration */declaration,
                            /* specifiers */undefined,
                            /* source */undefined,
                            /* exportKind */"ExportValue"
                          ]
                        }
                      ];
            
          }
      case "T_IMPORT" :
          error_on_decorators(env)(decorators);
          var env$3 = env;
          var env$4 = with_strict(true, env$3);
          var start_loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env$4);
          token$4(env$4, "T_IMPORT");
          var match$14 = Curry._2(Parser_env_Peek.token, undefined, env$4);
          var match$15;
          if (typeof match$14 === "string") {
            switch (match$14) {
              case "T_TYPEOF" :
                  if (!env$4[/* parse_options */20][/* types */4]) {
                    error$1(env$4, "UnexpectedTypeImport");
                  }
                  token$4(env$4, "T_TYPEOF");
                  match$15 = /* tuple */[
                    "ImportTypeof",
                    undefined
                  ];
                  break;
              case "T_TYPE" :
                  if (!env$4[/* parse_options */20][/* types */4]) {
                    error$1(env$4, "UnexpectedTypeImport");
                  }
                  match$15 = /* tuple */[
                    "ImportType",
                    Curry._2(Parse.identifier, undefined, env$4)
                  ];
                  break;
              default:
                match$15 = /* tuple */[
                  "ImportValue",
                  undefined
                ];
            }
          } else {
            match$15 = /* tuple */[
              "ImportValue",
              undefined
            ];
          }
          var type_ident = match$15[1];
          var importKind = match$15[0];
          var match$16 = Curry._2(Parser_env_Peek.token, undefined, env$4);
          var match$17 = Curry._2(Parser_env_Peek.is_identifier, undefined, env$4);
          var exit$2 = 0;
          if (typeof match$16 === "string") {
            if (match$16 !== "T_COMMA") {
              exit$2 = 2;
            }
            
          } else if (/* XXX */match$16.tag === "T_STRING" && importKind === "ImportValue") {
            var match$18 = match$16.Arg0;
            var octal = match$18[3];
            var raw = match$18[2];
            var value = match$18[1];
            var str_loc = match$18[0];
            if (octal) {
              strict_error(env$4, "StrictOctalLiteral");
            }
            token$4(env$4, /* constructor */{
                  tag: "T_STRING",
                  Arg0: /* tuple */[
                    str_loc,
                    value,
                    raw,
                    octal
                  ]
                });
            var value$1 = /* constructor */{
              tag: "String",
              Arg0: value
            };
            var source_001 = /* record */[
              /* value */value$1,
              /* raw */raw
            ];
            var source$4 = /* tuple */[
              str_loc,
              source_001
            ];
            var match$19 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$4);
            var end_loc$6 = match$19 !== undefined ? match$19 : str_loc;
            semicolon(env$4);
            return /* tuple */[
                    btwn(start_loc$1, end_loc$6),
                    /* constructor */{
                      tag: "ImportDeclaration",
                      Arg0: /* record */[
                        /* importKind */importKind,
                        /* source */source$4,
                        /* specifiers */"[]"
                      ]
                    }
                  ];
          } else {
            exit$2 = 2;
          }
          if (exit$2 === 2 && !match$17) {
            var specifiers$2 = named_or_namespace_specifier(env$4);
            var source$5 = source(env$4);
            var match$20 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$4);
            var end_loc$7 = match$20 !== undefined ? match$20 : source$5[0];
            semicolon(env$4);
            return /* tuple */[
                    btwn(start_loc$1, end_loc$7),
                    /* constructor */{
                      tag: "ImportDeclaration",
                      Arg0: /* record */[
                        /* importKind */importKind,
                        /* source */source$5,
                        /* specifiers */specifiers$2
                      ]
                    }
                  ];
          }
          var match$21 = Curry._2(Parser_env_Peek.token, undefined, env$4);
          var match$22 = Curry._2(Parser_env_Peek.value, undefined, env$4);
          var match$23;
          var exit$3 = 0;
          if (type_ident !== undefined) {
            var type_ident$1 = type_ident;
            if (typeof match$21 === "string") {
              switch (match$21) {
                case "T_IDENTIFIER" :
                    if (match$22 === "from") {
                      match$23 = /* tuple */[
                        "ImportValue",
                        /* constructor */{
                          tag: "ImportDefaultSpecifier",
                          Arg0: type_ident$1
                        }
                      ];
                    } else {
                      exit$3 = 1;
                    }
                    break;
                case "T_COMMA" :
                    match$23 = /* tuple */[
                      "ImportValue",
                      /* constructor */{
                        tag: "ImportDefaultSpecifier",
                        Arg0: type_ident$1
                      }
                    ];
                    break;
                default:
                  exit$3 = 1;
              }
            } else {
              exit$3 = 1;
            }
          } else {
            exit$3 = 1;
          }
          if (exit$3 === 1) {
            match$23 = /* tuple */[
              importKind,
              /* constructor */{
                tag: "ImportDefaultSpecifier",
                Arg0: Curry._2(Parse.identifier, undefined, env$4)
              }
            ];
          }
          var match$24 = Curry._2(Parser_env_Peek.token, undefined, env$4);
          var additional_specifiers;
          if (typeof match$24 === "string" && match$24 === "T_COMMA") {
            token$4(env$4, "T_COMMA");
            additional_specifiers = named_or_namespace_specifier(env$4);
          } else {
            additional_specifiers = "[]";
          }
          var source$6 = source(env$4);
          var match$25 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$4);
          var end_loc$8 = match$25 !== undefined ? match$25 : source$6[0];
          semicolon(env$4);
          return /* tuple */[
                  btwn(start_loc$1, end_loc$8),
                  /* constructor */{
                    tag: "ImportDeclaration",
                    Arg0: /* record */[
                      /* importKind */match$23[0],
                      /* source */source$6,
                      /* specifiers : constructor */{
                        tag: "::",
                        Arg0: match$23[1],
                        Arg1: additional_specifiers
                      }
                    ]
                  }
                ];
      case "T_DECLARE" :
          if (Curry._2(Parser_env_Peek.token, 1, env) === "T_EXPORT") {
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

function statement(env) {
  while(true) {
    var match = Curry._2(Parser_env_Peek.token, undefined, env);
    var exit = 0;
    if (typeof match === "string") {
      switch (match) {
        case "T_LCURLY" :
            var env$1 = env;
            var match$1 = Curry._1(Parse.block_body, env$1);
            return /* tuple */[
                    match$1[0],
                    /* constructor */{
                      tag: "Block",
                      Arg0: match$1[1]
                    }
                  ];
        case "T_SEMICOLON" :
            var env$2 = env;
            var loc = Curry._2(Parser_env_Peek.loc, undefined, env$2);
            token$4(env$2, "T_SEMICOLON");
            return /* tuple */[
                    loc,
                    "Empty"
                  ];
        case "T_IF" :
            return _if(env);
        case "T_RETURN" :
            var env$3 = env;
            if (!env$3[/* in_function */9]) {
              error$1(env$3, "IllegalReturn");
            }
            var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$3);
            token$4(env$3, "T_RETURN");
            var argument = Curry._2(Parser_env_Peek.token, undefined, env$3) === "T_SEMICOLON" || Curry._1(Parser_env_Peek.is_implicit_semicolon, env$3) ? undefined : Curry._1(Parse.expression, env$3);
            var match$2 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$3);
            var end_loc = match$2 !== undefined ? match$2 : (
                argument !== undefined ? argument[0] : start_loc
              );
            semicolon(env$3);
            return /* tuple */[
                    btwn(start_loc, end_loc),
                    /* constructor */{
                      tag: "Return",
                      Arg0: /* record */[/* argument */argument]
                    }
                  ];
        case "T_SWITCH" :
            var env$4 = env;
            var start_loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env$4);
            token$4(env$4, "T_SWITCH");
            token$4(env$4, "T_LPAREN");
            var discriminant = Curry._1(Parse.expression, env$4);
            token$4(env$4, "T_RPAREN");
            token$4(env$4, "T_LCURLY");
            var cases = case_list(env$4, /* tuple */[
                  false,
                  "[]"
                ]);
            var end_loc$1 = Curry._2(Parser_env_Peek.loc, undefined, env$4);
            token$4(env$4, "T_RCURLY");
            return /* tuple */[
                    btwn(start_loc$1, end_loc$1),
                    /* constructor */{
                      tag: "Switch",
                      Arg0: /* record */[
                        /* discriminant */discriminant,
                        /* cases */cases,
                        /* lexical */false
                      ]
                    }
                  ];
        case "T_THROW" :
            var env$5 = env;
            var start_loc$2 = Curry._2(Parser_env_Peek.loc, undefined, env$5);
            token$4(env$5, "T_THROW");
            if (Curry._1(Parser_env_Peek.is_line_terminator, env$5)) {
              error_at(env$5, /* tuple */[
                    start_loc$2,
                    "NewlineAfterThrow"
                  ]);
            }
            var argument$1 = Curry._1(Parse.expression, env$5);
            var match$3 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$5);
            var end_loc$2 = match$3 !== undefined ? match$3 : argument$1[0];
            semicolon(env$5);
            return /* tuple */[
                    btwn(start_loc$2, end_loc$2),
                    /* constructor */{
                      tag: "Throw",
                      Arg0: /* record */[/* argument */argument$1]
                    }
                  ];
        case "T_TRY" :
            var env$6 = env;
            var start_loc$3 = Curry._2(Parser_env_Peek.loc, undefined, env$6);
            token$4(env$6, "T_TRY");
            var block = Curry._1(Parse.block_body, env$6);
            var match$4 = Curry._2(Parser_env_Peek.token, undefined, env$6);
            var handler;
            if (typeof match$4 === "string" && match$4 === "T_CATCH") {
              var start_loc$4 = Curry._2(Parser_env_Peek.loc, undefined, env$6);
              token$4(env$6, "T_CATCH");
              token$4(env$6, "T_LPAREN");
              var id = Curry._2(Parse.identifier, "StrictCatchVariable", env$6);
              var param_000 = id[0];
              var param_001 = /* constructor */{
                tag: "Identifier",
                Arg0: id
              };
              var param = /* tuple */[
                param_000,
                param_001
              ];
              token$4(env$6, "T_RPAREN");
              var body = Curry._1(Parse.block_body, env$6);
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
            var match$5 = Curry._2(Parser_env_Peek.token, undefined, env$6);
            var finalizer;
            if (typeof match$5 === "string" && match$5 === "T_FINALLY") {
              token$4(env$6, "T_FINALLY");
              finalizer = Curry._1(Parse.block_body, env$6);
            } else {
              finalizer = undefined;
            }
            var end_loc$3 = finalizer !== undefined ? finalizer[0] : (
                handler !== undefined ? handler[0] : (error_at(env$6, /* tuple */[
                          block[0],
                          "NoCatchOrFinally"
                        ]), block[0])
              );
            return /* tuple */[
                    btwn(start_loc$3, end_loc$3),
                    /* constructor */{
                      tag: "Try",
                      Arg0: /* record */[
                        /* block */block,
                        /* handler */handler,
                        /* guardedHandlers */"[]",
                        /* finalizer */finalizer
                      ]
                    }
                  ];
        case "T_VAR" :
            return var_or_const(env);
        case "T_WHILE" :
            var env$7 = env;
            var start_loc$5 = Curry._2(Parser_env_Peek.loc, undefined, env$7);
            token$4(env$7, "T_WHILE");
            token$4(env$7, "T_LPAREN");
            var test = Curry._1(Parse.expression, env$7);
            token$4(env$7, "T_RPAREN");
            var body$1 = Curry._1(Parse.statement, with_in_loop(true, env$7));
            return /* tuple */[
                    btwn(start_loc$5, body$1[0]),
                    /* constructor */{
                      tag: "While",
                      Arg0: /* record */[
                        /* test */test,
                        /* body */body$1
                      ]
                    }
                  ];
        case "T_WITH" :
            var env$8 = env;
            var start_loc$6 = Curry._2(Parser_env_Peek.loc, undefined, env$8);
            token$4(env$8, "T_WITH");
            token$4(env$8, "T_LPAREN");
            var _object = Curry._1(Parse.expression, env$8);
            token$4(env$8, "T_RPAREN");
            var body$2 = Curry._1(Parse.statement, env$8);
            var loc$2 = btwn(start_loc$6, body$2[0]);
            strict_error_at(env$8, /* tuple */[
                  loc$2,
                  "StrictModeWith"
                ]);
            return /* tuple */[
                    loc$2,
                    /* constructor */{
                      tag: "With",
                      Arg0: /* record */[
                        /* _object */_object,
                        /* body */body$2
                      ]
                    }
                  ];
        case "T_BREAK" :
            var env$9 = env;
            var start_loc$7 = Curry._2(Parser_env_Peek.loc, undefined, env$9);
            token$4(env$9, "T_BREAK");
            var label;
            if (Curry._2(Parser_env_Peek.token, undefined, env$9) === "T_SEMICOLON" || Curry._1(Parser_env_Peek.is_implicit_semicolon, env$9)) {
              label = undefined;
            } else {
              var label$1 = Curry._2(Parse.identifier, undefined, env$9);
              var name = label$1[1][/* name */0];
              if (!mem$1(name, env$9[/* labels */2])) {
                error$1(env$9, /* constructor */{
                      tag: "UnknownLabel",
                      Arg0: name
                    });
              }
              label = label$1;
            }
            var match$6 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$9);
            var end_loc$4 = match$6 !== undefined ? match$6 : (
                label !== undefined ? label[0] : start_loc$7
              );
            var loc$3 = btwn(start_loc$7, end_loc$4);
            if (label === undefined && !(env$9[/* in_loop */7] || env$9[/* in_switch */8])) {
              error_at(env$9, /* tuple */[
                    loc$3,
                    "IllegalBreak"
                  ]);
            }
            semicolon(env$9);
            return /* tuple */[
                    loc$3,
                    /* constructor */{
                      tag: "Break",
                      Arg0: /* record */[/* label */label]
                    }
                  ];
        case "T_CONTINUE" :
            var env$10 = env;
            var start_loc$8 = Curry._2(Parser_env_Peek.loc, undefined, env$10);
            token$4(env$10, "T_CONTINUE");
            var label$2;
            if (Curry._2(Parser_env_Peek.token, undefined, env$10) === "T_SEMICOLON" || Curry._1(Parser_env_Peek.is_implicit_semicolon, env$10)) {
              label$2 = undefined;
            } else {
              var label$3 = Curry._2(Parse.identifier, undefined, env$10);
              var name$1 = label$3[1][/* name */0];
              if (!mem$1(name$1, env$10[/* labels */2])) {
                error$1(env$10, /* constructor */{
                      tag: "UnknownLabel",
                      Arg0: name$1
                    });
              }
              label$2 = label$3;
            }
            var match$7 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$10);
            var end_loc$5 = match$7 !== undefined ? match$7 : (
                label$2 !== undefined ? label$2[0] : start_loc$8
              );
            var loc$4 = btwn(start_loc$8, end_loc$5);
            if (!env$10[/* in_loop */7]) {
              error_at(env$10, /* tuple */[
                    loc$4,
                    "IllegalContinue"
                  ]);
            }
            semicolon(env$10);
            return /* tuple */[
                    loc$4,
                    /* constructor */{
                      tag: "Continue",
                      Arg0: /* record */[/* label */label$2]
                    }
                  ];
        case "T_DO" :
            var env$11 = env;
            var start_loc$9 = Curry._2(Parser_env_Peek.loc, undefined, env$11);
            token$4(env$11, "T_DO");
            var body$3 = Curry._1(Parse.statement, with_in_loop(true, env$11));
            token$4(env$11, "T_WHILE");
            token$4(env$11, "T_LPAREN");
            var test$1 = Curry._1(Parse.expression, env$11);
            var end_loc$6 = Curry._2(Parser_env_Peek.loc, undefined, env$11);
            token$4(env$11, "T_RPAREN");
            var match$8 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$11);
            var end_loc$7 = match$8 !== undefined ? match$8 : end_loc$6;
            if (Curry._2(Parser_env_Peek.token, undefined, env$11) === "T_SEMICOLON") {
              semicolon(env$11);
            }
            return /* tuple */[
                    btwn(start_loc$9, end_loc$7),
                    /* constructor */{
                      tag: "DoWhile",
                      Arg0: /* record */[
                        /* body */body$3,
                        /* test */test$1
                      ]
                    }
                  ];
        case "T_FOR" :
            var env$12 = env;
            var start_loc$10 = Curry._2(Parser_env_Peek.loc, undefined, env$12);
            token$4(env$12, "T_FOR");
            token$4(env$12, "T_LPAREN");
            var match$9 = Curry._2(Parser_env_Peek.token, undefined, env$12);
            var match$10;
            var exit$1 = 0;
            if (typeof match$9 === "string") {
              switch (match$9) {
                case "T_SEMICOLON" :
                    match$10 = /* tuple */[
                      undefined,
                      "[]"
                    ];
                    break;
                case "T_VAR" :
                    var match$11 = declarations("T_VAR", "Var", with_no_in(true, env$12));
                    match$10 = /* tuple */[
                      /* constructor */{
                        tag: "InitDeclaration",
                        Arg0: match$11[0]
                      },
                      match$11[1]
                    ];
                    break;
                case "T_CONST" :
                    var match$12 = $$const(with_no_in(true, env$12));
                    match$10 = /* tuple */[
                      /* constructor */{
                        tag: "InitDeclaration",
                        Arg0: match$12[0]
                      },
                      match$12[1]
                    ];
                    break;
                case "T_LET" :
                    var match$13 = _let(with_no_in(true, env$12));
                    match$10 = /* tuple */[
                      /* constructor */{
                        tag: "InitDeclaration",
                        Arg0: match$13[0]
                      },
                      match$13[1]
                    ];
                    break;
                default:
                  exit$1 = 1;
              }
            } else {
              exit$1 = 1;
            }
            if (exit$1 === 1) {
              var expr = Curry._1(Parse.expression, with_no_let(true, with_no_in(true, env$12)));
              match$10 = /* tuple */[
                /* constructor */{
                  tag: "InitExpression",
                  Arg0: expr
                },
                "[]"
              ];
            }
            var init = match$10[0];
            var match$14 = Curry._2(Parser_env_Peek.token, undefined, env$12);
            if (typeof match$14 === "string") {
              switch (match$14) {
                case "T_IN" :
                    assert_can_be_forin_or_forof(env$12, "InvalidLHSInForIn", init);
                    var left;
                    if (init !== undefined) {
                      var match$15 = init;
                      left = /* XXX */match$15.tag === "InitDeclaration" ? /* constructor */({
                            tag: "LeftDeclaration",
                            Arg0: match$15.Arg0
                          }) : /* constructor */({
                            tag: "LeftExpression",
                            Arg0: match$15.Arg0
                          });
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
                    token$4(env$12, "T_IN");
                    var right = Curry._1(Parse.expression, env$12);
                    token$4(env$12, "T_RPAREN");
                    var body$4 = Curry._1(Parse.statement, with_in_loop(true, env$12));
                    return /* tuple */[
                            btwn(start_loc$10, body$4[0]),
                            /* constructor */{
                              tag: "ForIn",
                              Arg0: /* record */[
                                /* left */left,
                                /* right */right,
                                /* body */body$4,
                                /* each */false
                              ]
                            }
                          ];
                case "T_OF" :
                    assert_can_be_forin_or_forof(env$12, "InvalidLHSInForOf", init);
                    var left$1;
                    if (init !== undefined) {
                      var match$16 = init;
                      left$1 = /* XXX */match$16.tag === "InitDeclaration" ? /* constructor */({
                            tag: "LeftDeclaration",
                            Arg0: match$16.Arg0
                          }) : /* constructor */({
                            tag: "LeftExpression",
                            Arg0: match$16.Arg0
                          });
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
                    token$4(env$12, "T_OF");
                    var right$1 = Curry._1(Parse.assignment, env$12);
                    token$4(env$12, "T_RPAREN");
                    var body$5 = Curry._1(Parse.statement, with_in_loop(true, env$12));
                    return /* tuple */[
                            btwn(start_loc$10, body$5[0]),
                            /* constructor */{
                              tag: "ForOf",
                              Arg0: /* record */[
                                /* left */left$1,
                                /* right */right$1,
                                /* body */body$5
                              ]
                            }
                          ];
                default:
                  
              }
            }
            List.iter((function(env$12){
                return function (param) {
                  return error_at(env$12, param);
                }
                }(env$12)), match$10[1]);
            token$4(env$12, "T_SEMICOLON");
            var match$17 = Curry._2(Parser_env_Peek.token, undefined, env$12);
            var test$2;
            test$2 = typeof match$17 === "string" && match$17 === "T_SEMICOLON" ? undefined : Curry._1(Parse.expression, env$12);
            token$4(env$12, "T_SEMICOLON");
            var match$18 = Curry._2(Parser_env_Peek.token, undefined, env$12);
            var update;
            update = typeof match$18 === "string" && match$18 === "T_RPAREN" ? undefined : Curry._1(Parse.expression, env$12);
            token$4(env$12, "T_RPAREN");
            var body$6 = Curry._1(Parse.statement, with_in_loop(true, env$12));
            return /* tuple */[
                    btwn(start_loc$10, body$6[0]),
                    /* constructor */{
                      tag: "For",
                      Arg0: /* record */[
                        /* init */init,
                        /* test */test$2,
                        /* update */update,
                        /* body */body$6
                      ]
                    }
                  ];
        case "T_DEBUGGER" :
            var env$13 = env;
            var start_loc$11 = Curry._2(Parser_env_Peek.loc, undefined, env$13);
            token$4(env$13, "T_DEBUGGER");
            var match$19 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$13);
            var end_loc$8 = match$19 !== undefined ? match$19 : start_loc$11;
            semicolon(env$13);
            return /* tuple */[
                    btwn(start_loc$11, end_loc$8),
                    "Debugger"
                  ];
        case "T_EOF" :
            error_unexpected(env);
            return /* tuple */[
                    Curry._2(Parser_env_Peek.loc, undefined, env),
                    "Empty"
                  ];
        default:
          exit = 2;
      }
    } else {
      exit = 2;
    }
    if (exit === 2) {
      if (Curry._2(Parser_env_Peek.is_identifier, undefined, env)) {
        var env$14 = env;
        var expr$1 = Curry._1(Parse.expression, env$14);
        var match$20 = Curry._2(Parser_env_Peek.token, undefined, env$14);
        var match$21 = expr$1[1];
        var loc$5 = expr$1[0];
        if (typeof match$21 !== "string" && /* XXX */match$21.tag === "Identifier" && typeof match$20 === "string" && match$20 === "T_COLON") {
          var label$4 = match$21.Arg0;
          var match$22 = label$4[1];
          var name$2 = match$22[/* name */0];
          token$4(env$14, "T_COLON");
          if (mem$1(name$2, env$14[/* labels */2])) {
            error_at(env$14, /* tuple */[
                  loc$5,
                  /* constructor */{
                    tag: "Redeclaration",
                    Arg0: "Label",
                    Arg1: name$2
                  }
                ]);
          }
          var env$15 = add_label(env$14, name$2);
          var labeled_stmt = Curry._1(Parse.statement, env$15);
          return /* tuple */[
                  btwn(loc$5, labeled_stmt[0]),
                  /* constructor */{
                    tag: "Labeled",
                    Arg0: /* record */[
                      /* label */label$4,
                      /* body */labeled_stmt
                    ]
                  }
                ];
        }
        var match$23 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$14);
        var end_loc$9 = match$23 !== undefined ? match$23 : expr$1[0];
        semicolon(env$14);
        return /* tuple */[
                btwn(expr$1[0], end_loc$9),
                /* constructor */{
                  tag: "Expression",
                  Arg0: /* record */[/* expression */expr$1]
                }
              ];
      } else if (typeof match === "string") {
        switch (match) {
          case "T_ELSE" :
              return _if(env);
          case "T_RCURLY" :
          case "T_RPAREN" :
          case "T_RBRACKET" :
          case "T_COMMA" :
          case "T_PERIOD" :
          case "T_ARROW" :
          case "T_ELLIPSIS" :
          case "T_IN" :
          case "T_INSTANCEOF" :
          case "T_CASE" :
          case "T_CATCH" :
          case "T_DEFAULT" :
          case "T_FINALLY" :
          case "T_EXTENDS" :
          case "T_STATIC" :
          case "T_EXPORT" :
          case "T_IMPORT" :
          case "T_COLON" :
              break;
          default:
            return expression(env);
        }
      } else {
        return expression(env);
      }
    }
    error_unexpected(env);
    token$3(env);
    continue ;
  };
}

function statement_list_item($staropt$star, env) {
  var decorators = $staropt$star !== undefined ? $staropt$star : "[]";
  if (!Curry._2(Parser_env_Peek.is_class, undefined, env)) {
    error_on_decorators(env)(decorators);
  }
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string") {
    switch (match) {
      case "T_CONST" :
          return var_or_const(env);
      case "T_LET" :
          var env$1 = env;
          var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env$1);
          token$4(env$1, "T_LET");
          if (Curry._2(Parser_env_Peek.token, undefined, env$1) === "T_LPAREN") {
            token$4(env$1, "T_LPAREN");
            var match$1 = helper(with_no_let(true, env$1), "[]", "[]");
            var head = List.map((function (param) {
                    var match = param[1];
                    return /* record */[
                            /* id */match[/* id */0],
                            /* init */match[/* init */1]
                          ];
                  }), match$1[1]);
            token$4(env$1, "T_RPAREN");
            var body = Curry._1(Parse.statement, env$1);
            var match$2 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
            var end_loc = match$2 !== undefined ? match$2 : match$1[0];
            semicolon(env$1);
            List.iter((function (param) {
                    return error_at(env$1, param);
                  }), match$1[2]);
            return /* tuple */[
                    btwn(start_loc, end_loc),
                    /* constructor */{
                      tag: "Let",
                      Arg0: /* record */[
                        /* head */head,
                        /* body */body
                      ]
                    }
                  ];
          } else {
            var match$3 = helper(with_no_let(true, env$1), "[]", "[]");
            var declaration = /* constructor */{
              tag: "VariableDeclaration",
              Arg0: /* record */[
                /* declarations */match$3[1],
                /* kind */"Let"
              ]
            };
            var match$4 = Curry._2(Parser_env_Peek.semicolon_loc, undefined, env$1);
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
      default:
        
    }
  }
  if (Curry._2(Parser_env_Peek.is_function, undefined, env)) {
    return _function(env);
  } else if (Curry._2(Parser_env_Peek.is_class, undefined, env)) {
    return class_declaration$1(env, decorators);
  } else if (typeof match === "string") {
    switch (match) {
      case "T_INTERFACE" :
          return $$interface(env);
      case "T_DECLARE" :
          return declare(undefined, env);
      case "T_TYPE" :
          return type_alias(env);
      default:
        return statement(env);
    }
  } else {
    return statement(env);
  }
}

var class_declaration$1 = class_declaration;

function module_body(term_fn, env) {
  var env$1 = env;
  var term_fn$1 = term_fn;
  var _acc = "[]";
  while(true) {
    var acc = _acc;
    var t = Curry._2(Parser_env_Peek.token, undefined, env$1);
    if (typeof t === "string" && t === "T_EOF") {
      return List.rev(acc);
    }
    if (Curry._1(term_fn$1, t)) {
      return List.rev(acc);
    } else {
      _acc = /* constructor */{
        tag: "::",
        Arg0: module_item(env$1),
        Arg1: acc
      };
      continue ;
    }
  };
}

function statement_list(_env, term_fn, item_fn, _param) {
  while(true) {
    var param = _param;
    var env = _env;
    var stmts = param[1];
    var string_tokens = param[0];
    var t = Curry._2(Parser_env_Peek.token, undefined, env);
    if (typeof t === "string" && t === "T_EOF") {
      return /* tuple */[
              env,
              string_tokens,
              stmts
            ];
    }
    if (Curry._1(term_fn, t)) {
      return /* tuple */[
              env,
              string_tokens,
              stmts
            ];
    } else {
      var string_token_000 = Curry._2(Parser_env_Peek.loc, undefined, env);
      var string_token_001 = Curry._2(Parser_env_Peek.token, undefined, env);
      var string_token = /* tuple */[
        string_token_000,
        string_token_001
      ];
      var possible_directive = Curry._1(item_fn, env);
      var stmts$1 = /* constructor */{
        tag: "::",
        Arg0: possible_directive,
        Arg1: stmts
      };
      var match = possible_directive[1];
      if (typeof match === "string" || /* XXX */match.tag !== "Expression") {
        return /* tuple */[
                env,
                string_tokens,
                stmts$1
              ];
      } else {
        var match$1 = match.Arg0[/* expression */0];
        var match$2 = match$1[1];
        if (typeof match$2 === "string" || /* XXX */match$2.tag !== "Literal") {
          return /* tuple */[
                  env,
                  string_tokens,
                  stmts$1
                ];
        } else {
          var match$3 = match$2.Arg0[/* value */0];
          if (typeof match$3 === "string" || /* XXX */match$3.tag !== "String") {
            return /* tuple */[
                    env,
                    string_tokens,
                    stmts$1
                  ];
          } else {
            var loc = match$1[0];
            var len = loc[/* _end */2][/* column */1] - loc[/* start */1][/* column */1] | 0;
            var strict = env[/* in_strict_mode */5] || match$3.Arg0 === "use strict" && len === 12;
            var string_tokens$1 = /* constructor */{
              tag: "::",
              Arg0: string_token,
              Arg1: string_tokens
            };
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
  };
}

function directives(env, term_fn, item_fn) {
  var match = statement_list(env, term_fn, item_fn, /* tuple */[
        "[]",
        "[]"
      ]);
  var env$1 = match[0];
  List.iter((function (param) {
          var env$2 = env$1;
          var param$1 = param;
          var token = param$1[1];
          if (typeof token !== "string" && /* XXX */token.tag === "T_STRING") {
            if (token.Arg0[3]) {
              return strict_error_at(env$2, /* tuple */[
                          param$1[0],
                          "StrictOctalLiteral"
                        ]);
            } else {
              return 0;
            }
          }
          var s = "Nooo: " + (token_to_string(token) + "\n");
          throw [
                Caml_builtin_exceptions.failure,
                s
              ];
        }), List.rev(match[1]));
  return /* tuple */[
          env$1,
          match[2]
        ];
}

function statement_list$1(term_fn, env) {
  var env$1 = env;
  var term_fn$1 = term_fn;
  var _acc = "[]";
  while(true) {
    var acc = _acc;
    var t = Curry._2(Parser_env_Peek.token, undefined, env$1);
    if (typeof t === "string" && t === "T_EOF") {
      return List.rev(acc);
    }
    if (Curry._1(term_fn$1, t)) {
      return List.rev(acc);
    } else {
      _acc = /* constructor */{
        tag: "::",
        Arg0: statement_list_item(undefined, env$1),
        Arg1: acc
      };
      continue ;
    }
  };
}

function identifier$2(restricted_error, env) {
  var loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  var name = Curry._2(Parser_env_Peek.value, undefined, env);
  var t = Curry._2(Parser_env_Peek.token, undefined, env);
  var exit = 0;
  if (typeof t === "string" && t === "T_LET") {
    if (env[/* in_strict_mode */5]) {
      strict_error(env, "StrictReservedWord");
    } else if (env[/* no_let */12]) {
      error$1(env, /* constructor */{
            tag: "UnexpectedToken",
            Arg0: name
          });
    }
    token$3(env);
  } else {
    exit = 1;
  }
  if (exit === 1) {
    if (is_strict_reserved(name)) {
      strict_error(env, "StrictReservedWord");
      token$3(env);
    } else if (typeof t === "string") {
      switch (t) {
        case "T_DECLARE" :
        case "T_TYPE" :
        case "T_OF" :
        case "T_ASYNC" :
        case "T_AWAIT" :
            token$4(env, t);
            break;
        default:
          token$4(env, "T_IDENTIFIER");
      }
    } else {
      token$4(env, "T_IDENTIFIER");
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

function statement_list_with_directives(term_fn, env) {
  var match = Curry._3(directives, env, term_fn, (function (eta) {
          return statement_list_item(undefined, eta);
        }));
  var env$1 = match[0];
  var stmts = Curry._2(statement_list$1, term_fn, env$1);
  var stmts$1 = List.fold_left((function (acc, stmt) {
          return /* constructor */{
                  tag: "::",
                  Arg0: stmt,
                  Arg1: acc
                };
        }), stmts, match[1]);
  return /* tuple */[
          stmts$1,
          env$1[/* in_strict_mode */5]
        ];
}

function module_body_with_directives(env, term_fn) {
  var match = Curry._3(directives, env, term_fn, module_item);
  var stmts = Curry._2(module_body, term_fn, match[0]);
  return List.fold_left((function (acc, stmt) {
                return /* constructor */{
                        tag: "::",
                        Arg0: stmt,
                        Arg1: acc
                      };
              }), stmts, match[1]);
}

function program(env) {
  var stmts = module_body_with_directives(env, (function (param) {
          return false;
        }));
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_EOF");
  var loc = stmts !== "[]" ? btwn(List.hd(stmts)[0], List.hd(List.rev(stmts))[0]) : end_loc;
  var comments = List.rev(env[/* comments */1][0]);
  return /* tuple */[
          loc,
          stmts,
          comments
        ];
}

function expression$1(env) {
  var expr = Curry._1(assignment, env);
  var match = Curry._2(Parser_env_Peek.token, undefined, env);
  if (typeof match === "string" && match === "T_COMMA") {
    return sequence(env, /* constructor */{
                tag: "::",
                Arg0: expr,
                Arg1: "[]"
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
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_PLING") {
    if (!env[/* parse_options */20][/* types */4]) {
      error$1(env, "UnexpectedTypeAnnotation");
    }
    var loc$1 = btwn(loc, Curry._2(Parser_env_Peek.loc, undefined, env));
    token$4(env, "T_PLING");
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
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_COLON") {
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
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_LCURLY");
  var term_fn = function (t) {
    return t === "T_RCURLY";
  };
  var body = Curry._2(statement_list$1, term_fn, env);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_RCURLY");
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* body */body]
        ];
}

function function_block_body(env) {
  var start_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_LCURLY");
  var term_fn = function (t) {
    return t === "T_RCURLY";
  };
  var match = statement_list_with_directives(term_fn, env);
  var end_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  token$4(env, "T_RCURLY");
  return /* tuple */[
          btwn(start_loc, end_loc),
          /* record */[/* body */match[0]],
          match[1]
        ];
}

function predicate(env) {
  var checks_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
  if (Curry._2(Parser_env_Peek.token, undefined, env) === "T_IDENTIFIER" && Curry._2(Parser_env_Peek.value, undefined, env) === "checks") {
    token$4(env, "T_IDENTIFIER");
    if (maybe(env, "T_LPAREN")) {
      var exp = Curry._1(Parse.expression, env);
      var rparen_loc = Curry._2(Parser_env_Peek.loc, undefined, env);
      token$4(env, "T_RPAREN");
      var loc = btwn(checks_loc, rparen_loc);
      return /* tuple */[
              loc,
              /* constructor */{
                tag: "Declared",
                Arg0: exp
              }
            ];
    } else {
      return /* tuple */[
              checks_loc,
              "Inferred"
            ];
    }
  }
  
}

Caml_module.update_mod(/* constructor */{
      tag: "Module",
      Arg0: [
        [
          "Function",
          "program"
        ],
        [
          "Function",
          "statement"
        ],
        [
          "Function",
          "statement_list_item"
        ],
        [
          "Function",
          "statement_list"
        ],
        [
          "Function",
          "statement_list_with_directives"
        ],
        [
          "Function",
          "module_body"
        ],
        [
          "Function",
          "expression"
        ],
        [
          "Function",
          "assignment"
        ],
        [
          "Function",
          "object_initializer"
        ],
        [
          "Function",
          "array_initializer"
        ],
        [
          "Function",
          "identifier"
        ],
        [
          "Function",
          "identifier_or_reserved_keyword"
        ],
        [
          "Function",
          "identifier_with_type"
        ],
        [
          "Function",
          "block_body"
        ],
        [
          "Function",
          "function_block_body"
        ],
        [
          "Function",
          "jsx_element"
        ],
        [
          "Function",
          "pattern"
        ],
        [
          "Function",
          "pattern_from_expr"
        ],
        [
          "Function",
          "object_key"
        ],
        [
          "Function",
          "class_declaration"
        ],
        [
          "Function",
          "class_expression"
        ],
        [
          "Function",
          "is_assignable_lhs"
        ],
        [
          "Function",
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
  var parser = Parse.program;
  var fail$2 = fail$1;
  var ast = Curry._1(parser, env$1);
  var error_list = filter_duplicate_errors(env$1[/* errors */0][0]);
  if (fail$2 && error_list !== "[]") {
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

var translation_errors = /* record */[/* contents */"[]"];

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
    translation_errors[0] = /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        loc,
        "InvalidRegExp"
      ],
      Arg1: translation_errors[0]
    };
    return new RegExp("", flags);
  }
}

function parse(content, options) {
  try {
    var match = program$1(false, undefined, Caml_option.some(undefined), content);
    translation_errors[0] = "[]";
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
        source = typeof match$1 === "string" ? Curry._1(string, "(global)") : Curry._1(string, match$1.Arg0);
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
    var expression = function (param) {
      var match = param[1];
      var loc = param[0];
      if (typeof match === "string") {
        return node("ThisExpression", loc, /* array */[]);
      } else {
        switch (/* XXX */match.tag) {
          case "Array" :
              return node("ArrayExpression", loc, /* array */[/* tuple */[
                            "elements",
                            array_of_list((function (param) {
                                    return option(expression_or_spread, param);
                                  }), match.Arg0[/* elements */0])
                          ]]);
          case "Object" :
              return node("ObjectExpression", loc, /* array */[/* tuple */[
                            "properties",
                            array_of_list(object_property, match.Arg0[/* properties */0])
                          ]]);
          case "Function" :
              return function_expression(/* tuple */[
                          loc,
                          match.Arg0
                        ]);
          case "ArrowFunction" :
              var arrow = match.Arg0;
              var match$1 = arrow[/* body */4];
              var body;
              body = /* XXX */match$1.tag === "BodyBlock" ? block(match$1.Arg0) : expression(match$1.Arg0);
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
          case "Sequence" :
              return node("SequenceExpression", loc, /* array */[/* tuple */[
                            "expressions",
                            array_of_list(expression, match.Arg0[/* expressions */0])
                          ]]);
          case "Unary" :
              var unary = match.Arg0;
              var match$2 = unary[/* operator */0];
              if (match$2 === "Await") {
                return node("AwaitExpression", loc, /* array */[/* tuple */[
                              "argument",
                              expression(unary[/* argument */2])
                            ]]);
              } else {
                var match$3 = unary[/* operator */0];
                var operator;
                switch (match$3) {
                  case "Minus" :
                      operator = "-";
                      break;
                  case "Plus" :
                      operator = "+";
                      break;
                  case "Not" :
                      operator = "!";
                      break;
                  case "BitNot" :
                      operator = "~";
                      break;
                  case "Typeof" :
                      operator = "typeof";
                      break;
                  case "Void" :
                      operator = "void";
                      break;
                  case "Delete" :
                      operator = "delete";
                      break;
                  case "Await" :
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
          case "Binary" :
              var binary = match.Arg0;
              var match$4 = binary[/* operator */0];
              var operator$1;
              switch (match$4) {
                case "Equal" :
                    operator$1 = "==";
                    break;
                case "NotEqual" :
                    operator$1 = "!=";
                    break;
                case "StrictEqual" :
                    operator$1 = "===";
                    break;
                case "StrictNotEqual" :
                    operator$1 = "!==";
                    break;
                case "LessThan" :
                    operator$1 = "<";
                    break;
                case "LessThanEqual" :
                    operator$1 = "<=";
                    break;
                case "GreaterThan" :
                    operator$1 = ">";
                    break;
                case "GreaterThanEqual" :
                    operator$1 = ">=";
                    break;
                case "LShift" :
                    operator$1 = "<<";
                    break;
                case "RShift" :
                    operator$1 = ">>";
                    break;
                case "RShift3" :
                    operator$1 = ">>>";
                    break;
                case "Plus" :
                    operator$1 = "+";
                    break;
                case "Minus" :
                    operator$1 = "-";
                    break;
                case "Mult" :
                    operator$1 = "*";
                    break;
                case "Exp" :
                    operator$1 = "**";
                    break;
                case "Div" :
                    operator$1 = "/";
                    break;
                case "Mod" :
                    operator$1 = "%";
                    break;
                case "BitOr" :
                    operator$1 = "|";
                    break;
                case "Xor" :
                    operator$1 = "^";
                    break;
                case "BitAnd" :
                    operator$1 = "&";
                    break;
                case "In" :
                    operator$1 = "in";
                    break;
                case "Instanceof" :
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
          case "Assignment" :
              var assignment = match.Arg0;
              var match$5 = assignment[/* operator */0];
              var operator$2;
              switch (match$5) {
                case "Assign" :
                    operator$2 = "=";
                    break;
                case "PlusAssign" :
                    operator$2 = "+=";
                    break;
                case "MinusAssign" :
                    operator$2 = "-=";
                    break;
                case "MultAssign" :
                    operator$2 = "*=";
                    break;
                case "ExpAssign" :
                    operator$2 = "**=";
                    break;
                case "DivAssign" :
                    operator$2 = "/=";
                    break;
                case "ModAssign" :
                    operator$2 = "%=";
                    break;
                case "LShiftAssign" :
                    operator$2 = "<<=";
                    break;
                case "RShiftAssign" :
                    operator$2 = ">>=";
                    break;
                case "RShift3Assign" :
                    operator$2 = ">>>=";
                    break;
                case "BitOrAssign" :
                    operator$2 = "|=";
                    break;
                case "BitXorAssign" :
                    operator$2 = "^=";
                    break;
                case "BitAndAssign" :
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
          case "Update" :
              var update = match.Arg0;
              var match$6 = update[/* operator */0];
              var operator$3 = match$6 !== "Increment" ? "--" : "++";
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
          case "Logical" :
              var logical = match.Arg0;
              var match$7 = logical[/* operator */0];
              var operator$4 = match$7 !== "Or" ? "&&" : "||";
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
          case "Conditional" :
              var conditional = match.Arg0;
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
          case "New" :
              var _new = match.Arg0;
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
          case "Call" :
              var call = match.Arg0;
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
          case "Member" :
              var member = match.Arg0;
              var match$8 = member[/* property */1];
              var property;
              property = /* XXX */match$8.tag === "PropertyIdentifier" ? identifier(match$8.Arg0) : expression(match$8.Arg0);
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
          case "Yield" :
              var $$yield = match.Arg0;
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
          case "Comprehension" :
              var comp = match.Arg0;
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
          case "Generator" :
              var gen = match.Arg0;
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
          case "Let" :
              var _let = match.Arg0;
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
          case "Identifier" :
              return identifier(match.Arg0);
          case "Literal" :
              return literal(/* tuple */[
                          loc,
                          match.Arg0
                        ]);
          case "TemplateLiteral" :
              return template_literal(/* tuple */[
                          loc,
                          match.Arg0
                        ]);
          case "TaggedTemplate" :
              var param$1 = /* tuple */[
                loc,
                match.Arg0
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
          case "JSXElement" :
              return jsx_element(/* tuple */[
                          loc,
                          match.Arg0
                        ]);
          case "Class" :
              var param$2 = /* tuple */[
                loc,
                match.Arg0
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
          case "TypeCast" :
              var typecast = match.Arg0;
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
    var interface_extends = function (param) {
      var g = param[1];
      var match = g[/* id */0];
      var id;
      id = /* XXX */match.tag === "Unqualified" ? identifier(match.Arg0) : generic_type_qualified_identifier(match.Arg0);
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
    var type_param = function (param) {
      var tp = param[1];
      var variance = function (param) {
        if (param !== "Plus") {
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
    var _type = function (param) {
      var t = param[1];
      var loc = param[0];
      if (typeof t === "string") {
        switch (t) {
          case "Any" :
              return node("AnyTypeAnnotation", loc, /* array */[]);
          case "Void" :
              return node("VoidTypeAnnotation", loc, /* array */[]);
          case "Null" :
              return node("NullTypeAnnotation", loc, /* array */[]);
          case "Number" :
              return node("NumberTypeAnnotation", loc, /* array */[]);
          case "String" :
              return node("StringTypeAnnotation", loc, /* array */[]);
          case "Boolean" :
              return node("BooleanTypeAnnotation", loc, /* array */[]);
          case "Exists" :
              return node("ExistsTypeAnnotation", loc, /* array */[]);
          
        }
      } else {
        switch (/* XXX */t.tag) {
          case "Nullable" :
              var loc$1 = loc;
              var t$1 = t.Arg0;
              return node("NullableTypeAnnotation", loc$1, /* array */[/* tuple */[
                            "typeAnnotation",
                            _type(t$1)
                          ]]);
          case "Function" :
              return function_type(/* tuple */[
                          loc,
                          t.Arg0
                        ]);
          case "Object" :
              return object_type(/* tuple */[
                          loc,
                          t.Arg0
                        ]);
          case "Array" :
              var loc$2 = loc;
              var t$2 = t.Arg0;
              return node("ArrayTypeAnnotation", loc$2, /* array */[/* tuple */[
                            "elementType",
                            _type(t$2)
                          ]]);
          case "Generic" :
              var param$1 = /* tuple */[
                loc,
                t.Arg0
              ];
              var g = param$1[1];
              var match = g[/* id */0];
              var id;
              id = /* XXX */match.tag === "Unqualified" ? identifier(match.Arg0) : generic_type_qualified_identifier(match.Arg0);
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
          case "Union" :
              var param$2 = /* tuple */[
                loc,
                t.Arg0
              ];
              return node("UnionTypeAnnotation", param$2[0], /* array */[/* tuple */[
                            "types",
                            array_of_list(_type, param$2[1])
                          ]]);
          case "Intersection" :
              var param$3 = /* tuple */[
                loc,
                t.Arg0
              ];
              return node("IntersectionTypeAnnotation", param$3[0], /* array */[/* tuple */[
                            "types",
                            array_of_list(_type, param$3[1])
                          ]]);
          case "Typeof" :
              var param$4 = /* tuple */[
                loc,
                t.Arg0
              ];
              return node("TypeofTypeAnnotation", param$4[0], /* array */[/* tuple */[
                            "argument",
                            _type(param$4[1])
                          ]]);
          case "Tuple" :
              var param$5 = /* tuple */[
                loc,
                t.Arg0
              ];
              return node("TupleTypeAnnotation", param$5[0], /* array */[/* tuple */[
                            "types",
                            array_of_list(_type, param$5[1])
                          ]]);
          case "StringLiteral" :
              var param$6 = /* tuple */[
                loc,
                t.Arg0
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
          case "NumberLiteral" :
              var param$7 = /* tuple */[
                loc,
                t.Arg0
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
          case "BooleanLiteral" :
              var param$8 = /* tuple */[
                loc,
                t.Arg0
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
      if (typeof value === "string") {
        value_ = $$null;
      } else {
        switch (/* XXX */value.tag) {
          case "String" :
              value_ = Curry._1(string, value.Arg0);
              break;
          case "Boolean" :
              value_ = Curry._1(bool, value.Arg0);
              break;
          case "Number" :
              value_ = Curry._1(number$1, value.Arg0);
              break;
          case "RegExp" :
              var match = value.Arg0;
              value_ = regexp$1(loc, match[/* pattern */0], match[/* flags */1]);
              break;
          
        }
      }
      var props;
      var exit = 0;
      if (typeof value === "string" || /* XXX */value.tag !== "RegExp") {
        exit = 1;
      } else {
        var match$1 = value.Arg0;
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
    var function_expression = function (param) {
      var _function = param[1];
      var match = _function[/* body */4];
      var body;
      body = /* XXX */match.tag === "BodyBlock" ? block(match.Arg0) : expression(match.Arg0);
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
    var jsx_identifier = function (param) {
      return node("JSXIdentifier", param[0], /* array */[/* tuple */[
                    "name",
                    Curry._1(string, param[1][/* name */0])
                  ]]);
    };
    var class_element = function (param) {
      if (/* XXX */param.tag === "Method") {
        var param$1 = param.Arg0;
        var method_ = param$1[1];
        var key = method_[/* key */1];
        var match;
        switch (/* XXX */key.tag) {
          case "Literal" :
              match = /* tuple */[
                literal(key.Arg0),
                false
              ];
              break;
          case "Identifier" :
              match = /* tuple */[
                identifier(key.Arg0),
                false
              ];
              break;
          case "Computed" :
              match = /* tuple */[
                expression(key.Arg0),
                true
              ];
              break;
          
        }
        var kind;
        switch (method_[/* kind */0]) {
          case "Constructor" :
              kind = "constructor";
              break;
          case "Method" :
              kind = "method";
              break;
          case "Get" :
              kind = "get";
              break;
          case "Set" :
              kind = "set";
              break;
          
        }
        return node("MethodDefinition", param$1[0], /* array */[
                    /* tuple */[
                      "key",
                      match[0]
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
                      Curry._1(bool, match[1])
                    ],
                    /* tuple */[
                      "decorators",
                      array_of_list(expression, method_[/* decorators */4])
                    ]
                  ]);
      } else {
        var param$2 = param.Arg0;
        var prop = param$2[1];
        var match$1 = prop[/* key */0];
        var match$2;
        switch (/* XXX */match$1.tag) {
          case "Literal" :
              match$2 = /* tuple */[
                literal(match$1.Arg0),
                false
              ];
              break;
          case "Identifier" :
              match$2 = /* tuple */[
                identifier(match$1.Arg0),
                false
              ];
              break;
          case "Computed" :
              match$2 = /* tuple */[
                expression(match$1.Arg0),
                true
              ];
              break;
          
        }
        return node("ClassProperty", param$2[0], /* array */[
                    /* tuple */[
                      "key",
                      match$2[0]
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
                      Curry._1(bool, match$2[1])
                    ],
                    /* tuple */[
                      "static",
                      Curry._1(bool, prop[/* static */3])
                    ]
                  ]);
      }
    };
    var type_parameter_instantiation = function (param) {
      return node("TypeParameterInstantiation", param[0], /* array */[/* tuple */[
                    "params",
                    array_of_list(_type, param[1][/* params */0])
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
    var class_body = function (param) {
      return node("ClassBody", param[0], /* array */[/* tuple */[
                    "body",
                    array_of_list(class_element, param[1][/* body */0])
                  ]]);
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
    var pattern = function (param) {
      var match = param[1];
      var loc = param[0];
      switch (/* XXX */match.tag) {
        case "Object" :
            var obj = match.Arg0;
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
        case "Array" :
            var arr = match.Arg0;
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
        case "Assignment" :
            var match$1 = match.Arg0;
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
        case "Identifier" :
            return identifier(match.Arg0);
        case "Expression" :
            return expression(match.Arg0);
        
      }
    };
    var block = function (param) {
      return node("BlockStatement", param[0], /* array */[/* tuple */[
                    "body",
                    array_of_list(statement, param[1][/* body */0])
                  ]]);
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
    var jsx_member_expression = function (param) {
      var member_expression = param[1];
      var match = member_expression[/* _object */0];
      var _object;
      _object = /* XXX */match.tag === "Identifier" ? jsx_identifier(match.Arg0) : jsx_member_expression(match.Arg0);
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
    var generic_type_qualified_identifier = function (param) {
      var q = param[1];
      var match = q[/* qualification */0];
      var qualification;
      qualification = /* XXX */match.tag === "Unqualified" ? identifier(match.Arg0) : generic_type_qualified_identifier(match.Arg0);
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
    var statement = function (param) {
      var match = param[1];
      var loc = param[0];
      if (typeof match === "string") {
        if (match === "Empty") {
          return node("EmptyStatement", loc, /* array */[]);
        } else {
          return node("DebuggerStatement", loc, /* array */[]);
        }
      } else {
        switch (/* XXX */match.tag) {
          case "Block" :
              return block(/* tuple */[
                          loc,
                          match.Arg0
                        ]);
          case "Expression" :
              return node("ExpressionStatement", loc, /* array */[/* tuple */[
                            "expression",
                            expression(match.Arg0[/* expression */0])
                          ]]);
          case "If" :
              var _if = match.Arg0;
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
          case "Labeled" :
              var labeled = match.Arg0;
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
          case "Break" :
              return node("BreakStatement", loc, /* array */[/* tuple */[
                            "label",
                            option(identifier, match.Arg0[/* label */0])
                          ]]);
          case "Continue" :
              return node("ContinueStatement", loc, /* array */[/* tuple */[
                            "label",
                            option(identifier, match.Arg0[/* label */0])
                          ]]);
          case "With" :
              var _with = match.Arg0;
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
          case "TypeAlias" :
              return type_alias(/* tuple */[
                          loc,
                          match.Arg0
                        ]);
          case "Switch" :
              var $$switch = match.Arg0;
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
          case "Return" :
              return node("ReturnStatement", loc, /* array */[/* tuple */[
                            "argument",
                            option(expression, match.Arg0[/* argument */0])
                          ]]);
          case "Throw" :
              return node("ThrowStatement", loc, /* array */[/* tuple */[
                            "argument",
                            expression(match.Arg0[/* argument */0])
                          ]]);
          case "Try" :
              var _try = match.Arg0;
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
          case "While" :
              var _while = match.Arg0;
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
          case "DoWhile" :
              var dowhile = match.Arg0;
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
          case "For" :
              var _for = match.Arg0;
              var init = function (param) {
                if (/* XXX */param.tag === "InitDeclaration") {
                  return variable_declaration(param.Arg0);
                } else {
                  return expression(param.Arg0);
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
          case "ForIn" :
              var forin = match.Arg0;
              var match$1 = forin[/* left */0];
              var left;
              left = /* XXX */match$1.tag === "LeftDeclaration" ? variable_declaration(match$1.Arg0) : expression(match$1.Arg0);
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
          case "ForOf" :
              var forof = match.Arg0;
              var match$2 = forof[/* left */0];
              var left$1;
              left$1 = /* XXX */match$2.tag === "LeftDeclaration" ? variable_declaration(match$2.Arg0) : expression(match$2.Arg0);
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
          case "Let" :
              var _let = match.Arg0;
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
          case "FunctionDeclaration" :
              var fn = match.Arg0;
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
              body = /* XXX */match$5.tag === "BodyBlock" ? block(match$5.Arg0) : expression(match$5.Arg0);
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
          case "VariableDeclaration" :
              return variable_declaration(/* tuple */[
                          loc,
                          match.Arg0
                        ]);
          case "ClassDeclaration" :
              var param$1 = /* tuple */[
                loc,
                match.Arg0
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
          case "InterfaceDeclaration" :
              return interface_declaration(/* tuple */[
                          loc,
                          match.Arg0
                        ]);
          case "DeclareVariable" :
              return declare_variable(/* tuple */[
                          loc,
                          match.Arg0
                        ]);
          case "DeclareFunction" :
              return declare_function(/* tuple */[
                          loc,
                          match.Arg0
                        ]);
          case "DeclareClass" :
              return declare_class(/* tuple */[
                          loc,
                          match.Arg0
                        ]);
          case "DeclareModule" :
              var m = match.Arg0;
              var match$8 = m[/* id */0];
              var id;
              id = /* XXX */match$8.tag === "Identifier" ? identifier(match$8.Arg0) : literal(match$8.Arg0);
              var match$9 = m[/* kind */2];
              var tmp;
              tmp = /* XXX */match$9.tag === "CommonJS" ? Curry._1(string, "CommonJS") : Curry._1(string, "ES");
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
          case "DeclareModuleExports" :
              return node("DeclareModuleExports", loc, /* array */[/* tuple */[
                            "typeAnnotation",
                            type_annotation(match.Arg0)
                          ]]);
          case "DeclareExportDeclaration" :
              var $$export = match.Arg0;
              var match$10 = $$export[/* declaration */1];
              var declaration;
              if (match$10 !== undefined) {
                var match$11 = match$10;
                switch (/* XXX */match$11.tag) {
                  case "Variable" :
                      declaration = declare_variable(match$11.Arg0);
                      break;
                  case "Function" :
                      declaration = declare_function(match$11.Arg0);
                      break;
                  case "Class" :
                      declaration = declare_class(match$11.Arg0);
                      break;
                  case "DefaultType" :
                      declaration = _type(match$11.Arg0);
                      break;
                  case "NamedType" :
                      declaration = type_alias(match$11.Arg0);
                      break;
                  case "Interface" :
                      declaration = interface_declaration(match$11.Arg0);
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
          case "ExportDeclaration" :
              var $$export$1 = match.Arg0;
              var match$12 = $$export$1[/* declaration */1];
              var declaration$1;
              if (match$12 !== undefined) {
                var match$13 = match$12;
                declaration$1 = /* XXX */match$13.tag === "Declaration" ? statement(match$13.Arg0) : expression(match$13.Arg0);
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
                            Curry._1(string, export_kind($$export$1[/* exportKind */4]))
                          ]
                        ]);
          case "ImportDeclaration" :
              var $$import = match.Arg0;
              var specifiers = List.map((function (param) {
                      switch (/* XXX */param.tag) {
                        case "ImportNamedSpecifier" :
                            var match = param.Arg0;
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
                        case "ImportDefaultSpecifier" :
                            var id = param.Arg0;
                            return node("ImportDefaultSpecifier", id[0], /* array */[/* tuple */[
                                          "id",
                                          identifier(id)
                                        ]]);
                        case "ImportNamespaceSpecifier" :
                            var param$1 = param.Arg0;
                            return node("ImportNamespaceSpecifier", param$1[0], /* array */[/* tuple */[
                                          "id",
                                          identifier(param$1[1])
                                        ]]);
                        
                      }
                    }), $$import[/* specifiers */2]);
              var match$14 = $$import[/* importKind */0];
              var import_kind;
              switch (match$14) {
                case "ImportType" :
                    import_kind = "type";
                    break;
                case "ImportTypeof" :
                    import_kind = "typeof";
                    break;
                case "ImportValue" :
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
    var type_annotation = function (param) {
      return node("TypeAnnotation", param[0], /* array */[/* tuple */[
                    "typeAnnotation",
                    _type(param[1])
                  ]]);
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
    var declare_variable = function (param) {
      return node("DeclareVariable", param[0], /* array */[/* tuple */[
                    "id",
                    identifier(param[1][/* id */0])
                  ]]);
    };
    var export_kind = function (param) {
      if (param !== "ExportType") {
        return "value";
      } else {
        return "type";
      }
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
    var variable_declaration = function (param) {
      var $$var = param[1];
      var match = $$var[/* kind */1];
      var kind;
      switch (match) {
        case "Var" :
            kind = "var";
            break;
        case "Let" :
            kind = "let";
            break;
        case "Const" :
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
    var export_specifiers = function (param) {
      if (param !== undefined) {
        var match = param;
        if (/* XXX */match.tag === "ExportSpecifiers") {
          return array_of_list(export_specifier, match.Arg0);
        } else {
          return Curry._1(array, /* array */[node("ExportBatchSpecifier", match.Arg0, /* array */[/* tuple */[
                              "name",
                              option(identifier, match.Arg1)
                            ]])]);
        }
      } else {
        return Curry._1(array, /* array */[]);
      }
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
    var declare_function = function (param) {
      return node("DeclareFunction", param[0], /* array */[/* tuple */[
                    "id",
                    identifier(param[1][/* id */0])
                  ]]);
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
    var jsx_opening_attribute = function (param) {
      if (/* XXX */param.tag === "Attribute") {
        var param$1 = param.Arg0;
        var attribute = param$1[1];
        var match = attribute[/* name */0];
        var name;
        name = /* XXX */match.tag === "Identifier" ? jsx_identifier(match.Arg0) : jsx_namespaced_name(match.Arg0);
        return node("JSXAttribute", param$1[0], /* array */[
                    /* tuple */[
                      "name",
                      name
                    ],
                    /* tuple */[
                      "value",
                      option(jsx_attribute_value, attribute[/* value */1])
                    ]
                  ]);
      } else {
        var param$2 = param.Arg0;
        return node("JSXSpreadAttribute", param$2[0], /* array */[/* tuple */[
                      "argument",
                      expression(param$2[1][/* argument */0])
                    ]]);
      }
    };
    var jsx_name = function (param) {
      switch (/* XXX */param.tag) {
        case "Identifier" :
            return jsx_identifier(param.Arg0);
        case "NamespacedName" :
            return jsx_namespaced_name(param.Arg0);
        case "MemberExpression" :
            return jsx_member_expression(param.Arg0);
        
      }
    };
    var jsx_expression_container = function (param) {
      var match = param[1][/* expression */0];
      var expression$1;
      expression$1 = /* XXX */match.tag === "Expression" ? expression(match.Arg0) : node("JSXEmptyExpression", match.Arg0, /* array */[]);
      return node("JSXExpressionContainer", param[0], /* array */[/* tuple */[
                    "expression",
                    expression$1
                  ]]);
    };
    var comment = function (param) {
      var c = param[1];
      var match;
      match = /* XXX */c.tag === "Block" ? /* tuple */[
          "Block",
          c.Arg0
        ] : /* tuple */[
          "Line",
          c.Arg0
        ];
      return node(match[0], param[0], /* array */[/* tuple */[
                    "value",
                    Curry._1(string, match[1])
                  ]]);
    };
    var jsx_closing = function (param) {
      return node("JSXClosingElement", param[0], /* array */[/* tuple */[
                    "name",
                    jsx_name(param[1][/* name */0])
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
    var jsx_child = function (param) {
      var match = param[1];
      var loc = param[0];
      switch (/* XXX */match.tag) {
        case "Element" :
            return jsx_element(/* tuple */[
                        loc,
                        match.Arg0
                      ]);
        case "ExpressionContainer" :
            return jsx_expression_container(/* tuple */[
                        loc,
                        match.Arg0
                      ]);
        case "Text" :
            var param$1 = /* tuple */[
              loc,
              match.Arg0
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
    var jsx_attribute_value = function (param) {
      if (/* XXX */param.tag === "Literal") {
        return literal(/* tuple */[
                    param.Arg0,
                    param.Arg1
                  ]);
      } else {
        return jsx_expression_container(/* tuple */[
                    param.Arg0,
                    param.Arg1
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
    var object_property = function (param) {
      if (/* XXX */param.tag === "Property") {
        var match = param.Arg0;
        var prop = match[1];
        var match$1 = prop[/* key */0];
        var match$2;
        switch (/* XXX */match$1.tag) {
          case "Literal" :
              match$2 = /* tuple */[
                literal(match$1.Arg0),
                false
              ];
              break;
          case "Identifier" :
              match$2 = /* tuple */[
                identifier(match$1.Arg0),
                false
              ];
              break;
          case "Computed" :
              match$2 = /* tuple */[
                expression(match$1.Arg0),
                true
              ];
              break;
          
        }
        var match$3 = prop[/* kind */2];
        var kind;
        switch (match$3) {
          case "Init" :
              kind = "init";
              break;
          case "Get" :
              kind = "get";
              break;
          case "Set" :
              kind = "set";
              break;
          
        }
        return node("Property", match[0], /* array */[
                    /* tuple */[
                      "key",
                      match$2[0]
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
                      Curry._1(bool, match$2[1])
                    ]
                  ]);
      } else {
        var match$4 = param.Arg0;
        return node("SpreadProperty", match$4[0], /* array */[/* tuple */[
                      "argument",
                      expression(match$4[1][/* argument */0])
                    ]]);
      }
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
      if (/* XXX */param.tag === "Expression") {
        return expression(param.Arg0);
      } else {
        var match = param.Arg0;
        return node("SpreadElement", match[0], /* array */[/* tuple */[
                      "argument",
                      expression(match[1][/* argument */0])
                    ]]);
      }
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
      switch (/* XXX */match.tag) {
        case "Literal" :
            key = literal(match.Arg0);
            break;
        case "Identifier" :
            key = identifier(match.Arg0);
            break;
        case "Computed" :
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
    var array_pattern_element = function (param) {
      if (/* XXX */param.tag === "Element") {
        return pattern(param.Arg0);
      } else {
        var match = param.Arg0;
        return node("SpreadElementPattern", match[0], /* array */[/* tuple */[
                      "argument",
                      pattern(match[1][/* argument */0])
                    ]]);
      }
    };
    var object_pattern_property = function (param) {
      if (/* XXX */param.tag === "Property") {
        var match = param.Arg0;
        var prop = match[1];
        var match$1 = prop[/* key */0];
        var match$2;
        switch (/* XXX */match$1.tag) {
          case "Literal" :
              match$2 = /* tuple */[
                literal(match$1.Arg0),
                false
              ];
              break;
          case "Identifier" :
              match$2 = /* tuple */[
                identifier(match$1.Arg0),
                false
              ];
              break;
          case "Computed" :
              match$2 = /* tuple */[
                expression(match$1.Arg0),
                true
              ];
              break;
          
        }
        return node("PropertyPattern", match[0], /* array */[
                    /* tuple */[
                      "key",
                      match$2[0]
                    ],
                    /* tuple */[
                      "pattern",
                      pattern(prop[/* pattern */1])
                    ],
                    /* tuple */[
                      "computed",
                      Curry._1(bool, match$2[1])
                    ],
                    /* tuple */[
                      "shorthand",
                      Curry._1(bool, prop[/* shorthand */2])
                    ]
                  ]);
      } else {
        var match$3 = param.Arg0;
        return node("SpreadPropertyPattern", match$3[0], /* array */[/* tuple */[
                      "argument",
                      pattern(match$3[1][/* argument */0])
                    ]]);
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

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: x,
                  Arg1: y
                };
        })
    ],
    Arg1: suites[0]
  };
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
