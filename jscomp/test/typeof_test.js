'use strict';

var Mt = require("./mt.js");
var Js_types = require("../../lib/js/js_types.js");

function string_or_number(x) {
  var ty = Js_types.classify(x);
  if (typeof ty === "string") {
    switch (ty) {
      case "JSFalse" :
      case "JSTrue" :
          return false;
      default:
        return false;
    }
  } else {
    switch (/* XXX */ty.tag) {
      case "JSNumber" :
          console.log(ty.Arg0 + 3);
          return true;
      case "JSString" :
          console.log(ty.Arg0 + "hei");
          return true;
      case "JSFunction" :
          console.log("Function");
          return false;
      default:
        return false;
    }
  }
}

var suites = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "int_type",
    (function (param) {
        return /* constructor */{
                tag: "Eq",
                Arg0: "number",
                Arg1: "number"
              };
      })
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "string_type",
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: "string",
                  Arg1: "string"
                };
        })
    ],
    Arg1: /* constructor */{
      tag: "::",
      Arg0: /* tuple */[
        "number_gadt_test",
        (function (param) {
            return /* constructor */{
                    tag: "Eq",
                    Arg0: Js_types.test(3, "Number"),
                    Arg1: true
                  };
          })
      ],
      Arg1: /* constructor */{
        tag: "::",
        Arg0: /* tuple */[
          "boolean_gadt_test",
          (function (param) {
              return /* constructor */{
                      tag: "Eq",
                      Arg0: Js_types.test(true, "Boolean"),
                      Arg1: true
                    };
            })
        ],
        Arg1: /* constructor */{
          tag: "::",
          Arg0: /* tuple */[
            "undefined_gadt_test",
            (function (param) {
                return /* constructor */{
                        tag: "Eq",
                        Arg0: Js_types.test(undefined, "Undefined"),
                        Arg1: true
                      };
              })
          ],
          Arg1: /* constructor */{
            tag: "::",
            Arg0: /* tuple */[
              "string_on_number1",
              (function (param) {
                  return /* constructor */{
                          tag: "Eq",
                          Arg0: string_or_number("xx"),
                          Arg1: true
                        };
                })
            ],
            Arg1: /* constructor */{
              tag: "::",
              Arg0: /* tuple */[
                "string_on_number2",
                (function (param) {
                    return /* constructor */{
                            tag: "Eq",
                            Arg0: string_or_number(3.02),
                            Arg1: true
                          };
                  })
              ],
              Arg1: /* constructor */{
                tag: "::",
                Arg0: /* tuple */[
                  "string_on_number3",
                  (function (param) {
                      return /* constructor */{
                              tag: "Eq",
                              Arg0: string_or_number((function (x) {
                                      return x;
                                    })),
                              Arg1: false
                            };
                    })
                ],
                Arg1: /* constructor */{
                  tag: "::",
                  Arg0: /* tuple */[
                    "string_gadt_test",
                    (function (param) {
                        return /* constructor */{
                                tag: "Eq",
                                Arg0: Js_types.test("3", "String"),
                                Arg1: true
                              };
                      })
                  ],
                  Arg1: /* constructor */{
                    tag: "::",
                    Arg0: /* tuple */[
                      "string_gadt_test_neg",
                      (function (param) {
                          return /* constructor */{
                                  tag: "Eq",
                                  Arg0: Js_types.test(3, "String"),
                                  Arg1: false
                                };
                        })
                    ],
                    Arg1: /* constructor */{
                      tag: "::",
                      Arg0: /* tuple */[
                        "function_gadt_test",
                        (function (param) {
                            return /* constructor */{
                                    tag: "Eq",
                                    Arg0: Js_types.test((function (x) {
                                            return x;
                                          }), "Function"),
                                    Arg1: true
                                  };
                          })
                      ],
                      Arg1: /* constructor */{
                        tag: "::",
                        Arg0: /* tuple */[
                          "object_gadt_test",
                          (function (param) {
                              return /* constructor */{
                                      tag: "Eq",
                                      Arg0: Js_types.test({
                                            x: 3
                                          }, "Object"),
                                      Arg1: true
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
  }
};

Mt.from_pair_suites("Typeof_test", suites);

exports.string_or_number = string_or_number;
exports.suites = suites;
/*  Not a pure module */
