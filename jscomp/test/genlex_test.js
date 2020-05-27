'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Genlex = require("../../lib/js/genlex.js");
var Stream = require("../../lib/js/stream.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var lexer = Genlex.make_lexer({
      hd: "+",
      tl: {
        hd: "-",
        tl: {
          hd: "*",
          tl: {
            hd: "/",
            tl: {
              hd: "let",
              tl: {
                hd: "=",
                tl: {
                  hd: "(",
                  tl: {
                    hd: ")",
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        }
      }
    });

function to_list(s) {
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var v;
    try {
      v = Stream.next(s);
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === Stream.Failure) {
        return List.rev(acc);
      }
      throw exn;
    }
    _acc = {
      hd: v,
      tl: acc
    };
    continue ;
  };
}

var suites_0 = [
  "lexer_stream_genlex",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: {
                hd: {
                  TAG: /* Int */2,
                  _0: 3
                },
                tl: {
                  hd: {
                    TAG: /* Kwd */0,
                    _0: "("
                  },
                  tl: {
                    hd: {
                      TAG: /* Int */2,
                      _0: 3
                    },
                    tl: {
                      hd: {
                        TAG: /* Kwd */0,
                        _0: "+"
                      },
                      tl: {
                        hd: {
                          TAG: /* Int */2,
                          _0: 2
                        },
                        tl: {
                          hd: {
                            TAG: /* Int */2,
                            _0: -1
                          },
                          tl: {
                            hd: {
                              TAG: /* Kwd */0,
                              _0: ")"
                            },
                            tl: /* [] */0
                          }
                        }
                      }
                    }
                  }
                }
              },
              _1: to_list(lexer(Stream.of_string("3(3 + 2 -1)")))
            };
    })
];

var suites = {
  hd: suites_0,
  tl: /* [] */0
};

Mt.from_pair_suites("Genlex_test", suites);

exports.lexer = lexer;
exports.to_list = to_list;
exports.suites = suites;
/* lexer Not a pure module */
