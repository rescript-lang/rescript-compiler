'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Bytes = require("../../lib/js/bytes.js");
var Caml_char = require("../../lib/js/caml_char.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "caml_is_printable",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: Caml_char.caml_is_printable(/* "a" */97),
                Arg1: true
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "caml_string_of_bytes",
      (function (param) {
          var match = List.split(List.map((function (x) {
                      var len = x;
                      var b = Caml_bytes.caml_create_bytes(1000);
                      Caml_bytes.caml_fill_bytes(b, 0, len, /* "c" */99);
                      return /* tuple */[
                              Caml_bytes.bytes_to_string(b),
                              Caml_bytes.bytes_to_string(Bytes.init(len, (function (param) {
                                          return /* "c" */99;
                                        })))
                            ];
                    }), /* constructor */{
                    tag: "::",
                    Arg0: 1000,
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: 1024,
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: 1025,
                        Arg1: /* constructor */{
                          tag: "::",
                          Arg0: 4095,
                          Arg1: /* constructor */{
                            tag: "::",
                            Arg0: 4096,
                            Arg1: /* constructor */{
                              tag: "::",
                              Arg0: 5000,
                              Arg1: /* constructor */{
                                tag: "::",
                                Arg0: 10000,
                                Arg1: "[]"
                              }
                            }
                          }
                        }
                      }
                    }
                  }));
          return /* constructor */{
                  tag: "Eq",
                  Arg0: match[0],
                  Arg1: match[1]
                };
        })
    ],
    Arg1: "[]"
  }
};

Mt.from_pair_suites("String_runtime_test", suites);

var S = 0;

var B = 0;

exports.S = S;
exports.B = B;
exports.suites = suites;
/*  Not a pure module */
