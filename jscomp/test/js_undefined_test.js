'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_primitive = require("../../lib/js/js_primitive.js");
var Js_undefined = require("../../lib/js/js_undefined.js");

var suites_000 = /* tuple */[
  "to_opt - empty",
  (function () {
      return /* Eq */Block.__(0, [
                /* None */0,
                undefined === undefined ? /* None */0 : [undefined]
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "to_opt - 'a",
    (function () {
        return /* Eq */Block.__(0, [
                  /* Some */[/* () */0],
                  Js_primitive.undefined_to_opt(/* () */0)
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "return",
      (function () {
          return /* Eq */Block.__(0, [
                    /* Some */["something"],
                    Js_primitive.undefined_to_opt("something")
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "test - empty",
        (function () {
            return /* Eq */Block.__(0, [
                      true,
                      true
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "test - 'a",
          (function () {
              return /* Eq */Block.__(0, [
                        false,
                        false
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "bind - empty",
            (function () {
                return /* Eq */Block.__(0, [
                          undefined,
                          Js_undefined.bind(undefined, (function (v) {
                                  return v;
                                }))
                        ]);
              })
          ],
          /* :: */[
            /* tuple */[
              "bind - 'a",
              (function () {
                  return /* Eq */Block.__(0, [
                            4,
                            Js_undefined.bind(2, (function (n) {
                                    return (n << 1);
                                  }))
                          ]);
                })
            ],
            /* :: */[
              /* tuple */[
                "iter - empty",
                (function () {
                    var hit = [false];
                    Js_undefined.iter(undefined, (function () {
                            hit[0] = true;
                            return /* () */0;
                          }));
                    return /* Eq */Block.__(0, [
                              false,
                              hit[0]
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "iter - 'a",
                  (function () {
                      var hit = [0];
                      Js_undefined.iter(2, (function (v) {
                              hit[0] = v;
                              return /* () */0;
                            }));
                      return /* Eq */Block.__(0, [
                                2,
                                hit[0]
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "from_opt - None",
                    (function () {
                        return /* Eq */Block.__(0, [
                                  undefined,
                                  Js_undefined.from_opt(/* None */0)
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "from_opt - Some",
                      (function () {
                          return /* Eq */Block.__(0, [
                                    2,
                                    Js_undefined.from_opt(/* Some */[2])
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
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_undefined_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
