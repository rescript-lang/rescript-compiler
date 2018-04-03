'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_null = require("../../lib/js/js_null.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

var suites_000 = /* tuple */[
  "to_opt - empty",
  (function () {
      return /* Eq */Block.__(0, [
                /* None */0,
                /* None */0
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "to_opt - 'a",
    (function () {
        return /* Eq */Block.__(0, [
                  /* Some */[/* () */0],
                  Js_primitive.null_to_opt(/* () */0)
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "toOption - empty",
      (function () {
          return /* Eq */Block.__(0, [
                    /* None */0,
                    /* None */0
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "toOption - 'a",
        (function () {
            return /* Eq */Block.__(0, [
                      /* Some */[/* () */0],
                      Js_primitive.null_to_opt(/* () */0)
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "return",
          (function () {
              return /* Eq */Block.__(0, [
                        /* Some */["something"],
                        Js_primitive.null_to_opt("something")
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
                    return /* StrictEq */Block.__(2, [
                              null,
                              Js_null.bind(null, (function (v) {
                                      return v;
                                    }))
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "bind - 'a",
                  (function () {
                      return /* StrictEq */Block.__(2, [
                                4,
                                Js_null.bind(2, (function (n) {
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
                        Js_null.iter(null, (function () {
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
                          Js_null.iter(2, (function (v) {
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
                        "fromOption - None",
                        (function () {
                            return /* Eq */Block.__(0, [
                                      null,
                                      Js_null.fromOption(/* None */0)
                                    ]);
                          })
                      ],
                      /* :: */[
                        /* tuple */[
                          "fromOption - Some",
                          (function () {
                              return /* Eq */Block.__(0, [
                                        2,
                                        Js_null.fromOption(/* Some */[2])
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
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_null_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
