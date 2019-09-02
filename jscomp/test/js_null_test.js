'use strict';

var Mt = require("./mt.js");
var Js_null = require("../../lib/js/js_null.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "toOption - empty",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: undefined,
                Arg1: undefined
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "toOption - 'a",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: /* () */0,
                  Arg1: Caml_option.null_to_opt(/* () */0)
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "return",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: "something",
                    Arg1: Caml_option.null_to_opt("something")
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "test - empty",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: true,
                      Arg1: true
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "test - 'a",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: false,
                        Arg1: false
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "bind - empty",
              (function (param) {
                  return /* constructor */{
                          tag: "StrictEq",
                          Arg0: null,
                          Arg1: Js_null.bind(null, (function (v) {
                                  return v;
                                }))
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "bind - 'a",
                (function (param) {
                    return /* constructor */{
                            tag: "StrictEq",
                            Arg0: 4,
                            Arg1: Js_null.bind(2, (function (n) {
                                    return (n << 1);
                                  }))
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "iter - empty",
                  (function (param) {
                      var hit = /* record */[/* contents */false];
                      Js_null.iter(null, (function (param) {
                              hit[0] = true;
                              return /* () */0;
                            }));
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: false,
                              Arg1: hit[0]
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "iter - 'a",
                    (function (param) {
                        var hit = /* record */[/* contents */0];
                        Js_null.iter(2, (function (v) {
                                hit[0] = v;
                                return /* () */0;
                              }));
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: 2,
                                Arg1: hit[0]
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "fromOption - None",
                      (function (param) {
                          return /* constructor */{
                                  tag: "Eq",
                                  Arg0: null,
                                  Arg1: Js_null.fromOption(undefined)
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "fromOption - Some",
                        (function (param) {
                            return /* constructor */{
                                    tag: "Eq",
                                    Arg0: 2,
                                    Arg1: Js_null.fromOption(2)
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
        }
      }
    }
  }
};

Mt.from_pair_suites("Js_null_test", suites);

exports.suites = suites;
/*  Not a pure module */
