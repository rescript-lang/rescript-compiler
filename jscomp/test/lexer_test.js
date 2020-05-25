'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");
var Lexing = require("../../lib/js/lexing.js");
var Arith_lexer = require("./arith_lexer.js");
var Arith_parser = require("./arith_parser.js");
var Arith_syntax = require("./arith_syntax.js");
var Number_lexer = require("./number_lexer.js");

function get_tokens(lex, str) {
  var buf = Lexing.from_string(str);
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var v = Curry._1(lex, buf);
    if (v === /* EOF */7) {
      return List.rev(acc);
    }
    _acc = /* :: */{
      _0: v,
      _1: acc
    };
    continue ;
  };
}

function f(param) {
  return get_tokens(Arith_lexer.lexeme, param);
}

function from_tokens(lst) {
  var l = {
    contents: lst
  };
  return function (param) {
    var match = l.contents;
    if (match) {
      l.contents = match._1;
      return match._0;
    }
    throw {
          RE_EXN_ID: "End_of_file",
          Error: new Error()
        };
  };
}

var lexer_suites_0 = [
  "arith_token",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: get_tokens(Arith_lexer.lexeme, "x + 3 + 4 + y"),
              _1: /* :: */{
                _0: {
                  tag: /* IDENT */1,
                  _0: "x"
                },
                _1: /* :: */{
                  _0: /* PLUS */0,
                  _1: /* :: */{
                    _0: {
                      tag: /* NUMERAL */0,
                      _0: 3
                    },
                    _1: /* :: */{
                      _0: /* PLUS */0,
                      _1: /* :: */{
                        _0: {
                          tag: /* NUMERAL */0,
                          _0: 4
                        },
                        _1: /* :: */{
                          _0: /* PLUS */0,
                          _1: /* :: */{
                            _0: {
                              tag: /* IDENT */1,
                              _0: "y"
                            },
                            _1: /* [] */0
                          }
                        }
                      }
                    }
                  }
                }
              }
            };
    })
];

var lexer_suites_1 = /* :: */{
  _0: [
    "simple token",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: Arith_lexer.lexeme(Lexing.from_string("10")),
                _1: {
                  tag: /* NUMERAL */0,
                  _0: 10
                }
              };
      })
  ],
  _1: /* :: */{
    _0: [
      "number_lexer",
      (function (param) {
          var v = {
            contents: /* [] */0
          };
          var add = function (t) {
            v.contents = /* :: */{
              _0: t,
              _1: v.contents
            };
            
          };
          Number_lexer.token(add, Lexing.from_string("32 + 32 ( ) * / "));
          return {
                  tag: /* Eq */0,
                  _0: List.rev(v.contents),
                  _1: /* :: */{
                    _0: "number",
                    _1: /* :: */{
                      _0: "32",
                      _1: /* :: */{
                        _0: "new line",
                        _1: /* :: */{
                          _0: "+",
                          _1: /* :: */{
                            _0: "new line",
                            _1: /* :: */{
                              _0: "number",
                              _1: /* :: */{
                                _0: "32",
                                _1: /* :: */{
                                  _0: "new line",
                                  _1: /* :: */{
                                    _0: "(",
                                    _1: /* :: */{
                                      _0: "new line",
                                      _1: /* :: */{
                                        _0: ")",
                                        _1: /* :: */{
                                          _0: "new line",
                                          _1: /* :: */{
                                            _0: "*",
                                            _1: /* :: */{
                                              _0: "new line",
                                              _1: /* :: */{
                                                _0: "/",
                                                _1: /* :: */{
                                                  _0: "new line",
                                                  _1: /* :: */{
                                                    _0: "eof",
                                                    _1: /* [] */0
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
                };
        })
    ],
    _1: /* :: */{
      _0: [
        "simple number",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: Arith_syntax.str(Arith_parser.toplevel(Arith_lexer.lexeme, Lexing.from_string("10"))),
                    _1: "10."
                  };
          })
      ],
      _1: /* :: */{
        _0: [
          "arith",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: Arith_syntax.str(Arith_parser.toplevel(Arith_lexer.lexeme, Lexing.from_string("x + 3 + 4 + y"))),
                      _1: "x+3.+4.+y"
                    };
            })
        ],
        _1: /* [] */0
      }
    }
  }
};

var lexer_suites = /* :: */{
  _0: lexer_suites_0,
  _1: lexer_suites_1
};

Mt.from_pair_suites("Lexer_test", lexer_suites);

exports.get_tokens = get_tokens;
exports.f = f;
exports.from_tokens = from_tokens;
exports.lexer_suites = lexer_suites;
/*  Not a pure module */
