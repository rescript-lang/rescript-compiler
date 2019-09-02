'use strict';

var Mt = require("./mt.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Caml_string = require("../../lib/js/caml_string.js");

Mt.from_pair_suites("String_get_set_test", /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "File \"string_get_set_test.ml\", line 8, characters 4-11",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: Caml_string.caml_string_get16("2\0", 0),
                    Arg1: 50
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "File \"string_get_set_test.ml\", line 9, characters 4-11",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: Caml_string.caml_string_get16("20", 0),
                      Arg1: 12338
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "File \"string_get_set_test.ml\", line 10, characters 4-11",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: Caml_string.caml_string_get32("0123", 0),
                        Arg1: 858927408
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "File \"string_get_set_test.ml\", line 11, characters 4-11",
              (function (param) {
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: Caml_string.caml_string_get32("0123", 0),
                          Arg1: 858927408
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "File \"string_get_set_test.ml\", line 12, characters 4-11",
                (function (param) {
                    return /* constructor */{
                            tag: "Eq",
                            Arg0: Caml_string.caml_string_get32("3210", 0),
                            Arg1: 808530483
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "File \"string_get_set_test.ml\", line 13, characters 4-11",
                  (function (param) {
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: Caml_int64.get64("12345678", 0),
                              Arg1: /* int64 */[
                                /* hi */943142453,
                                /* lo */875770417
                              ]
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "File \"string_get_set_test.ml\", line 14, characters 4-11",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: Caml_int64.get64("87654321", 0),
                                Arg1: /* int64 */[
                                  /* hi */825373492,
                                  /* lo */892745528
                                ]
                              };
                      })
                  ],
                  Arg1: "[]"
                }
              }
            }
          }
        }
      }
    });

/*  Not a pure module */
