'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Queue = require("../../lib/js/queue.js");
var Genlex = require("../../lib/js/genlex.js");
var Stream = require("../../lib/js/stream.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var Parse_error = /* @__PURE__ */Caml_exceptions.create("Stream_parser_test.Parse_error");

function parse(token) {
  var look_ahead = {
    length: 0,
    first: /* Nil */0,
    last: /* Nil */0
  };
  var token$1 = function (param) {
    if (look_ahead.length !== 0) {
      return Queue.pop(look_ahead);
    }
    try {
      return Curry._1(token, undefined);
    }
    catch (exn){
      return {
              TAG: /* Kwd */0,
              _0: "=="
            };
    }
  };
  var parse_atom = function (param) {
    var n = token$1(undefined);
    switch (n.TAG | 0) {
      case /* Kwd */0 :
          if (n._0 === "(") {
            var v = parse_expr_aux(parse_term_aux(parse_atom(undefined)));
            var match = token$1(undefined);
            if (match.TAG === /* Kwd */0) {
              if (match._0 === ")") {
                return v;
              }
              throw {
                    RE_EXN_ID: Parse_error,
                    _1: "Unbalanced parens",
                    Error: new Error()
                  };
            }
            throw {
                  RE_EXN_ID: Parse_error,
                  _1: "Unbalanced parens",
                  Error: new Error()
                };
          }
          Queue.push(n, look_ahead);
          throw {
                RE_EXN_ID: Parse_error,
                _1: "unexpected token",
                Error: new Error()
              };
      case /* Int */2 :
          return n._0;
      default:
        Queue.push(n, look_ahead);
        throw {
              RE_EXN_ID: Parse_error,
              _1: "unexpected token",
              Error: new Error()
            };
    }
  };
  var parse_term_aux = function (e1) {
    var e = token$1(undefined);
    if (e.TAG === /* Kwd */0) {
      switch (e._0) {
        case "*" :
            return Math.imul(e1, parse_term_aux(parse_atom(undefined)));
        case "/" :
            return Caml_int32.div(e1, parse_term_aux(parse_atom(undefined)));
        default:
          Queue.push(e, look_ahead);
          return e1;
      }
    } else {
      Queue.push(e, look_ahead);
      return e1;
    }
  };
  var parse_expr_aux = function (e1) {
    var e = token$1(undefined);
    if (e.TAG === /* Kwd */0) {
      switch (e._0) {
        case "+" :
            return e1 + parse_expr_aux(parse_term_aux(parse_atom(undefined))) | 0;
        case "-" :
            return e1 - parse_expr_aux(parse_term_aux(parse_atom(undefined))) | 0;
        default:
          Queue.push(e, look_ahead);
          return e1;
      }
    } else {
      Queue.push(e, look_ahead);
      return e1;
    }
  };
  var r = parse_expr_aux(parse_term_aux(parse_atom(undefined)));
  return [
          r,
          Queue.fold((function (acc, x) {
                  return {
                          hd: x,
                          tl: acc
                        };
                }), /* [] */0, look_ahead)
        ];
}

var lexer = Genlex.make_lexer({
      hd: "(",
      tl: {
        hd: "*",
        tl: {
          hd: "/",
          tl: {
            hd: "+",
            tl: {
              hd: "-",
              tl: {
                hd: ")",
                tl: /* [] */0
              }
            }
          }
        }
      }
    });

function token(chars) {
  var strm = lexer(chars);
  return function (param) {
    return Stream.next(strm);
  };
}

function l_parse(token) {
  var look_ahead = {
    length: 0,
    first: /* Nil */0,
    last: /* Nil */0
  };
  var token$1 = function (param) {
    if (look_ahead.length !== 0) {
      return Queue.pop(look_ahead);
    }
    try {
      return Curry._1(token, undefined);
    }
    catch (exn){
      return {
              TAG: /* Kwd */0,
              _0: "=="
            };
    }
  };
  var parse_f_aux = function (_a) {
    while(true) {
      var a = _a;
      var t = token$1(undefined);
      if (t.TAG === /* Kwd */0) {
        switch (t._0) {
          case "*" :
              _a = Math.imul(a, parse_f(undefined));
              continue ;
          case "/" :
              _a = Caml_int32.div(a, parse_f(undefined));
              continue ;
          default:
            Queue.push(t, look_ahead);
            return a;
        }
      } else {
        Queue.push(t, look_ahead);
        return a;
      }
    };
  };
  var parse_f = function (param) {
    var i = token$1(undefined);
    switch (i.TAG | 0) {
      case /* Kwd */0 :
          if (i._0 === "(") {
            var v = parse_t_aux(parse_f_aux(parse_f(undefined)));
            var t = token$1(undefined);
            if (t.TAG === /* Kwd */0) {
              if (t._0 === ")") {
                return v;
              }
              throw {
                    RE_EXN_ID: Parse_error,
                    _1: "Unbalanced )",
                    Error: new Error()
                  };
            }
            throw {
                  RE_EXN_ID: Parse_error,
                  _1: "Unbalanced )",
                  Error: new Error()
                };
          }
          throw {
                RE_EXN_ID: Parse_error,
                _1: "Unexpected token",
                Error: new Error()
              };
      case /* Int */2 :
          return i._0;
      default:
        throw {
              RE_EXN_ID: Parse_error,
              _1: "Unexpected token",
              Error: new Error()
            };
    }
  };
  var parse_t_aux = function (_a) {
    while(true) {
      var a = _a;
      var t = token$1(undefined);
      if (t.TAG === /* Kwd */0) {
        switch (t._0) {
          case "+" :
              _a = a + parse_f_aux(parse_f(undefined)) | 0;
              continue ;
          case "-" :
              _a = a - parse_f_aux(parse_f(undefined)) | 0;
              continue ;
          default:
            Queue.push(t, look_ahead);
            return a;
        }
      } else {
        Queue.push(t, look_ahead);
        return a;
      }
    };
  };
  var r = parse_t_aux(parse_f_aux(parse_f(undefined)));
  return [
          r,
          Queue.fold((function (acc, x) {
                  return {
                          hd: x,
                          tl: acc
                        };
                }), /* [] */0, look_ahead)
        ];
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

var match = parse(token(Stream.of_string("1 + 2 + (3  - 2) * 3 * 3  - 2 a")));

eq("File \"stream_parser_test.ml\", line 132, characters 5-12", [
      match[0],
      match[1]
    ], [
      10,
      {
        hd: {
          TAG: /* Ident */1,
          _0: "a"
        },
        tl: /* [] */0
      }
    ]);

eq("File \"stream_parser_test.ml\", line 133, characters 5-12", [
      2,
      {
        hd: {
          TAG: /* Kwd */0,
          _0: "=="
        },
        tl: /* [] */0
      }
    ], parse(token(Stream.of_string("3 - 2  - 1"))));

eq("File \"stream_parser_test.ml\", line 134, characters 5-12", [
      0,
      {
        hd: {
          TAG: /* Kwd */0,
          _0: "=="
        },
        tl: /* [] */0
      }
    ], l_parse(token(Stream.of_string("3 - 2  - 1"))));

Mt.from_pair_suites("Stream_parser_test", suites.contents);

exports.Parse_error = Parse_error;
exports.parse = parse;
exports.lexer = lexer;
exports.token = token;
exports.l_parse = l_parse;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/* lexer Not a pure module */
