'use strict';

var Mt       = require("./mt");
var Block    = require("../../lib/js/block");
var Js_types = require("../../lib/js/js_types");

var suites_000 = /* tuple */[
  "int_type",
  function () {
    return /* Eq */Block.__(0, [
              "number",
              "number"
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "string_type",
    function () {
      return /* Eq */Block.__(0, [
                "string",
                "string"
              ]);
    }
  ],
  /* :: */[
    /* tuple */[
      "number_gadt_test",
      function () {
        return /* Eq */Block.__(0, [
                  Js_types.test(3, /* Number */3),
                  /* true */1
                ]);
      }
    ],
    /* :: */[
      /* tuple */[
        "boolean_gadt_test",
        function () {
          return /* Eq */Block.__(0, [
                    Js_types.test(true, /* Boolean */2),
                    /* true */1
                  ]);
        }
      ],
      /* :: */[
        /* tuple */[
          "undefined_gadt_test",
          function () {
            return /* Eq */Block.__(0, [
                      Js_types.test(undefined, /* Undefined */0),
                      /* true */1
                    ]);
          }
        ],
        /* :: */[
          /* tuple */[
            "string_gadt_test",
            function () {
              return /* Eq */Block.__(0, [
                        Js_types.test("3", /* String */4),
                        /* true */1
                      ]);
            }
          ],
          /* :: */[
            /* tuple */[
              "string_gadt_test_neg",
              function () {
                return /* Eq */Block.__(0, [
                          Js_types.test(3, /* String */4),
                          /* false */0
                        ]);
              }
            ],
            /* :: */[
              /* tuple */[
                "function_gadt_test",
                function () {
                  return /* Eq */Block.__(0, [
                            Js_types.test(function (x) {
                                  return x;
                                }, /* Function */5),
                            /* true */1
                          ]);
                }
              ],
              /* :: */[
                /* tuple */[
                  "object_gadt_test",
                  function () {
                    return /* Eq */Block.__(0, [
                              Js_types.test({
                                    x: 3
                                  }, /* Object */6),
                              /* true */1
                            ]);
                  }
                ],
                /* [] */0
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

exports.suites = suites;
/*  Not a pure module */
