'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Queue = require("../../lib/js/queue.js");
var Genlex = require("../../lib/js/genlex.js");
var Stream = require("../../lib/js/stream.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Parse_error = Caml_exceptions.create("Stream_parser_test.Parse_error");

function parse(token) {
  var look_ahead = /* record */[
    /* length */0,
    /* first : Nil */0,
    /* last : Nil */0
  ];
  var token$1 = function (param) {
    if (look_ahead[/* length */0] === 0) {
      try {
        return Curry._1(token, /* () */0);
      }
      catch (exn){
        return /* Kwd */Block.__(0, ["=="]);
      }
    } else {
      return Queue.pop(look_ahead);
    }
  };
  var parse_atom = function (param) {
    var e = token$1(/* () */0);
    switch (e.tag | 0) {
      case 0 : 
          if (e[0] === "(") {
            var v = parse_expr_aux(parse_term_aux(parse_atom(/* () */0)));
            var match = token$1(/* () */0);
            if (match.tag) {
              throw [
                    Parse_error,
                    "Unbalanced parens"
                  ];
            } else if (match[0] === ")") {
              return v;
            } else {
              throw [
                    Parse_error,
                    "Unbalanced parens"
                  ];
            }
          } else {
            Queue.push(e, look_ahead);
            throw [
                  Parse_error,
                  "unexpected token"
                ];
          }
      case 2 : 
          return e[0];
      default:
        Queue.push(e, look_ahead);
        throw [
              Parse_error,
              "unexpected token"
            ];
    }
  };
  var parse_term_aux = function (e1) {
    var e = token$1(/* () */0);
    if (e.tag) {
      Queue.push(e, look_ahead);
      return e1;
    } else {
      switch (e[0]) {
        case "*" : 
            return Caml_int32.imul(e1, parse_term_aux(parse_atom(/* () */0)));
        case "/" : 
            return Caml_int32.div(e1, parse_term_aux(parse_atom(/* () */0)));
        default:
          Queue.push(e, look_ahead);
          return e1;
      }
    }
  };
  var parse_expr_aux = function (e1) {
    var e = token$1(/* () */0);
    if (e.tag) {
      Queue.push(e, look_ahead);
      return e1;
    } else {
      switch (e[0]) {
        case "+" : 
            return e1 + parse_expr_aux(parse_term_aux(parse_atom(/* () */0))) | 0;
        case "-" : 
            return e1 - parse_expr_aux(parse_term_aux(parse_atom(/* () */0))) | 0;
        default:
          Queue.push(e, look_ahead);
          return e1;
      }
    }
  };
  var r = parse_expr_aux(parse_term_aux(parse_atom(/* () */0)));
  return /* tuple */[
          r,
          Queue.fold((function (acc, x) {
                  return /* :: */[
                          x,
                          acc
                        ];
                }), /* [] */0, look_ahead)
        ];
}

var lexer = Genlex.make_lexer(/* :: */[
      "(",
      /* :: */[
        "*",
        /* :: */[
          "/",
          /* :: */[
            "+",
            /* :: */[
              "-",
              /* :: */[
                ")",
                /* [] */0
              ]
            ]
          ]
        ]
      ]
    ]);

function token(chars) {
  var strm = lexer(chars);
  return (function (param) {
      return Stream.next(strm);
    });
}

function l_parse(token) {
  var look_ahead = /* record */[
    /* length */0,
    /* first : Nil */0,
    /* last : Nil */0
  ];
  var token$1 = function (param) {
    if (look_ahead[/* length */0] === 0) {
      try {
        return Curry._1(token, /* () */0);
      }
      catch (exn){
        return /* Kwd */Block.__(0, ["=="]);
      }
    } else {
      return Queue.pop(look_ahead);
    }
  };
  var parse_f_aux = function (_a) {
    while(true) {
      var a = _a;
      var t = token$1(/* () */0);
      if (t.tag) {
        Queue.push(t, look_ahead);
        return a;
      } else {
        switch (t[0]) {
          case "*" : 
              _a = Caml_int32.imul(a, parse_f(/* () */0));
              continue ;
          case "/" : 
              _a = Caml_int32.div(a, parse_f(/* () */0));
              continue ;
          default:
            Queue.push(t, look_ahead);
            return a;
        }
      }
    };
  };
  var parse_f = function (param) {
    var t = token$1(/* () */0);
    switch (t.tag | 0) {
      case 0 : 
          if (t[0] === "(") {
            var v = parse_t_aux(parse_f_aux(parse_f(/* () */0)));
            var t$1 = token$1(/* () */0);
            if (t$1.tag) {
              throw [
                    Parse_error,
                    "Unbalanced )"
                  ];
            } else if (t$1[0] === ")") {
              return v;
            } else {
              throw [
                    Parse_error,
                    "Unbalanced )"
                  ];
            }
          } else {
            throw [
                  Parse_error,
                  "Unexpected token"
                ];
          }
      case 2 : 
          return t[0];
      default:
        throw [
              Parse_error,
              "Unexpected token"
            ];
    }
  };
  var parse_t_aux = function (_a) {
    while(true) {
      var a = _a;
      var t = token$1(/* () */0);
      if (t.tag) {
        Queue.push(t, look_ahead);
        return a;
      } else {
        switch (t[0]) {
          case "+" : 
              _a = a + parse_f_aux(parse_f(/* () */0)) | 0;
              continue ;
          case "-" : 
              _a = a - parse_f_aux(parse_f(/* () */0)) | 0;
              continue ;
          default:
            Queue.push(t, look_ahead);
            return a;
        }
      }
    };
  };
  var r = parse_t_aux(parse_f_aux(parse_f(/* () */0)));
  return /* tuple */[
          r,
          Queue.fold((function (acc, x) {
                  return /* :: */[
                          x,
                          acc
                        ];
                }), /* [] */0, look_ahead)
        ];
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

var match = parse(token(Stream.of_string("1 + 2 + (3  - 2) * 3 * 3  - 2 a")));

eq("File \"stream_parser_test.ml\", line 132, characters 5-12", /* tuple */[
      match[0],
      match[1]
    ], /* tuple */[
      10,
      /* :: */[
        /* Ident */Block.__(1, ["a"]),
        /* [] */0
      ]
    ]);

eq("File \"stream_parser_test.ml\", line 133, characters 5-12", /* tuple */[
      2,
      /* :: */[
        /* Kwd */Block.__(0, ["=="]),
        /* [] */0
      ]
    ], parse(token(Stream.of_string("3 - 2  - 1"))));

eq("File \"stream_parser_test.ml\", line 134, characters 5-12", /* tuple */[
      0,
      /* :: */[
        /* Kwd */Block.__(0, ["=="]),
        /* [] */0
      ]
    ], l_parse(token(Stream.of_string("3 - 2  - 1"))));

Mt.from_pair_suites("Stream_parser_test", suites[0]);

exports.Parse_error = Parse_error;
exports.parse = parse;
exports.lexer = lexer;
exports.token = token;
exports.l_parse = l_parse;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/* lexer Not a pure module */
