'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_types = require("../../lib/js/js_types.js");

function string_or_number(x) {
  var match = Js_types.reify_type(x);
  var v = match[1];
  switch (match[0]) {
    case 3 : 
        console.log(v + 3);
        return /* true */1;
    case 4 : 
        console.log(v + "hei");
        return /* true */1;
    case 5 : 
        console.log("Function");
        return /* false */0;
    case 0 : 
    case 1 : 
    case 2 : 
    case 6 : 
    case 7 : 
        return /* false */0;
    
  }
}

var suites_000 = /* tuple */[
  "int_type",
  (function () {
      return /* Eq */Block.__(0, [
                "number",
                "number"
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "string_type",
    (function () {
        return /* Eq */Block.__(0, [
                  "string",
                  "string"
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "number_gadt_test",
      (function () {
          return /* Eq */Block.__(0, [
                    Js_types.test(3, /* Number */3),
                    /* true */1
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "boolean_gadt_test",
        (function () {
            return /* Eq */Block.__(0, [
                      Js_types.test(true, /* Boolean */2),
                      /* true */1
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "undefined_gadt_test",
          (function () {
              return /* Eq */Block.__(0, [
                        Js_types.test(undefined, /* Undefined */0),
                        /* true */1
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "string_on_number1",
            (function () {
                return /* Eq */Block.__(0, [
                          string_or_number("xx"),
                          /* true */1
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "string_on_number2",
              (function () {
                  return /* Eq */Block.__(0, [
                            string_or_number(3.02),
                            /* true */1
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "string_on_number3",
                (function () {
                    return /* Eq */Block.__(0, [
                              string_or_number((function (x) {
                                      return x;
                                    })),
                              /* false */0
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "string_gadt_test",
                  (function () {
                      return /* Eq */Block.__(0, [
                                Js_types.test("3", /* String */4),
                                /* true */1
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "string_gadt_test_neg",
                    (function () {
                        return /* Eq */Block.__(0, [
                                  Js_types.test(3, /* String */4),
                                  /* false */0
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "function_gadt_test",
                      (function () {
                          return /* Eq */Block.__(0, [
                                    Js_types.test((function (x) {
                                            return x;
                                          }), /* Function */5),
                                    /* true */1
                                  ]);
                        })
                    ],
                    /* :: */[
                      /* tuple */[
                        "object_gadt_test",
                        (function () {
                            return /* Eq */Block.__(0, [
                                      Js_types.test({
                                            x: 3
                                          }, /* Object */6),
                                      /* true */1
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

Mt.from_pair_suites("typeof_test.ml", suites);

exports.string_or_number = string_or_number;
exports.suites = suites;
/*  Not a pure module */
