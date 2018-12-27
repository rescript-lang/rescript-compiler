'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_types = require("../../lib/js/js_types.js");

function string_or_number(x) {
  var ty = Js_types.classify(x);
  if (typeof ty === "number") {
    switch (ty) {
      case 0 : 
      case 1 : 
          return false;
      default:
        return false;
    }
  } else {
    switch (ty.tag | 0) {
      case 0 : 
          console.log(ty[0] + 3);
          return true;
      case 1 : 
          console.log(ty[0] + "hei");
          return true;
      case 2 : 
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
      return /* Eq */Block.__(0, [
                "number",
                "number"
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "string_type",
    (function (param) {
        return /* Eq */Block.__(0, [
                  "string",
                  "string"
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "number_gadt_test",
      (function (param) {
          return /* Eq */Block.__(0, [
                    Js_types.test(3, /* Number */3),
                    true
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "boolean_gadt_test",
        (function (param) {
            return /* Eq */Block.__(0, [
                      Js_types.test(true, /* Boolean */2),
                      true
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "undefined_gadt_test",
          (function (param) {
              return /* Eq */Block.__(0, [
                        Js_types.test(undefined, /* Undefined */0),
                        true
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "string_on_number1",
            (function (param) {
                return /* Eq */Block.__(0, [
                          string_or_number("xx"),
                          true
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "string_on_number2",
              (function (param) {
                  return /* Eq */Block.__(0, [
                            string_or_number(3.02),
                            true
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "string_on_number3",
                (function (param) {
                    return /* Eq */Block.__(0, [
                              string_or_number((function (x) {
                                      return x;
                                    })),
                              false
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "string_gadt_test",
                  (function (param) {
                      return /* Eq */Block.__(0, [
                                Js_types.test("3", /* String */4),
                                true
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "string_gadt_test_neg",
                    (function (param) {
                        return /* Eq */Block.__(0, [
                                  Js_types.test(3, /* String */4),
                                  false
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "function_gadt_test",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    Js_types.test((function (x) {
                                            return x;
                                          }), /* Function */5),
                                    true
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "object_gadt_test",
                        (function (param) {
                            return /* Eq */Block.__(0, [
                                      Js_types.test({
                                            x: 3
                                          }, /* Object */6),
                                      true
                                    ]);
                          })
                      ],
                      /* [] */0
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("Typeof_test", suites);

exports.string_or_number = string_or_number;
exports.suites = suites;
/*  Not a pure module */
