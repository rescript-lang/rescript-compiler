'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_types = require("../../lib/js/js_types.js");

function string_or_number(x) {
  var ty = Js_types.classify(x);
  if (typeof ty === "number") {
    switch (ty) {
      case /* JSFalse */0 :
      case /* JSTrue */1 :
          return false;
      default:
        return false;
    }
  } else {
    switch (ty.tag | 0) {
      case /* JSNumber */0 :
          console.log(ty._0 + 3);
          return true;
      case /* JSString */1 :
          console.log(ty._0 + "hei");
          return true;
      case /* JSFunction */2 :
          console.log("Function");
          return false;
      default:
        return false;
    }
  }
}

var suites_000 = /* tuple */[
  "int_type",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: "number",
              _1: "number"
            };
    })
];

var suites_001 = /* :: */{
  _0: /* tuple */[
    "string_type",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: "string",
                _1: "string"
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "number_gadt_test",
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: Js_types.test(3, /* Number */3),
                  _1: true
                };
        })
    ],
    _1: /* :: */{
      _0: /* tuple */[
        "boolean_gadt_test",
        (function (param) {
            return {
                    tag: /* Eq */0,
                    _0: Js_types.test(true, /* Boolean */2),
                    _1: true
                  };
          })
      ],
      _1: /* :: */{
        _0: /* tuple */[
          "undefined_gadt_test",
          (function (param) {
              return {
                      tag: /* Eq */0,
                      _0: Js_types.test(undefined, /* Undefined */0),
                      _1: true
                    };
            })
        ],
        _1: /* :: */{
          _0: /* tuple */[
            "string_on_number1",
            (function (param) {
                return {
                        tag: /* Eq */0,
                        _0: string_or_number("xx"),
                        _1: true
                      };
              })
          ],
          _1: /* :: */{
            _0: /* tuple */[
              "string_on_number2",
              (function (param) {
                  return {
                          tag: /* Eq */0,
                          _0: string_or_number(3.02),
                          _1: true
                        };
                })
            ],
            _1: /* :: */{
              _0: /* tuple */[
                "string_on_number3",
                (function (param) {
                    return {
                            tag: /* Eq */0,
                            _0: string_or_number(function (x) {
                                  return x;
                                }),
                            _1: false
                          };
                  })
              ],
              _1: /* :: */{
                _0: /* tuple */[
                  "string_gadt_test",
                  (function (param) {
                      return {
                              tag: /* Eq */0,
                              _0: Js_types.test("3", /* String */4),
                              _1: true
                            };
                    })
                ],
                _1: /* :: */{
                  _0: /* tuple */[
                    "string_gadt_test_neg",
                    (function (param) {
                        return {
                                tag: /* Eq */0,
                                _0: Js_types.test(3, /* String */4),
                                _1: false
                              };
                      })
                  ],
                  _1: /* :: */{
                    _0: /* tuple */[
                      "function_gadt_test",
                      (function (param) {
                          return {
                                  tag: /* Eq */0,
                                  _0: Js_types.test((function (x) {
                                          return x;
                                        }), /* Function */5),
                                  _1: true
                                };
                        })
                    ],
                    _1: /* :: */{
                      _0: /* tuple */[
                        "object_gadt_test",
                        (function (param) {
                            return {
                                    tag: /* Eq */0,
                                    _0: Js_types.test({
                                          x: 3
                                        }, /* Object */6),
                                    _1: true
                                  };
                          })
                      ],
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
};

var suites = /* :: */{
  _0: suites_000,
  _1: suites_001
};

Mt.from_pair_suites("Typeof_test", suites);

exports.string_or_number = string_or_number;
exports.suites = suites;
/*  Not a pure module */
