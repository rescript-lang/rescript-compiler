'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Bytes = require("../../lib/js/bytes.js");
var Caml_char = require("../../lib/js/caml_char.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");

var suites_0 = /* tuple */[
  "caml_is_printable",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: Caml_char.caml_is_printable(/* "a" */97),
              _1: true
            };
    })
];

var suites_1 = /* :: */{
  _0: /* tuple */[
    "caml_string_of_bytes",
    (function (param) {
        var match = List.split(List.map((function (x) {
                    var b = Caml_bytes.caml_create_bytes(1000);
                    Caml_bytes.caml_fill_bytes(b, 0, x, /* "c" */99);
                    return /* tuple */[
                            Caml_bytes.bytes_to_string(b),
                            Caml_bytes.bytes_to_string(Bytes.init(x, (function (param) {
                                        return /* "c" */99;
                                      })))
                          ];
                  }), /* :: */{
                  _0: 1000,
                  _1: /* :: */{
                    _0: 1024,
                    _1: /* :: */{
                      _0: 1025,
                      _1: /* :: */{
                        _0: 4095,
                        _1: /* :: */{
                          _0: 4096,
                          _1: /* :: */{
                            _0: 5000,
                            _1: /* :: */{
                              _0: 10000,
                              _1: /* [] */0
                            }
                          }
                        }
                      }
                    }
                  }
                }));
        return {
                tag: /* Eq */0,
                _0: match[0],
                _1: match[1]
              };
      })
  ],
  _1: /* [] */0
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("String_runtime_test", suites);

var S;

var B;

exports.S = S;
exports.B = B;
exports.suites = suites;
/*  Not a pure module */
