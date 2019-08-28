'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_null = require("../../lib/js/js_null.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites_000 = /* tuple */[
  "toOption - empty",
  (function (param) {
      return /* Eq */Block.__(0, [
                undefined,
                undefined
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "toOption - 'a",
    (function (param) {
        return /* Eq */Block.__(0, [
                  /* () */0,
                  Caml_option.null_to_opt(/* () */0)
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "return",
      (function (param) {
          return /* Eq */Block.__(0, [
                    "something",
                    Caml_option.null_to_opt("something")
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "test - empty",
        (function (param) {
            return /* Eq */Block.__(0, [
                      true,
                      true
                    ]);
          })
      ],
      /* :: */[
        /* tuple */[
          "test - 'a",
          (function (param) {
              return /* Eq */Block.__(0, [
                        false,
                        false
                      ]);
            })
        ],
        /* :: */[
          /* tuple */[
            "bind - empty",
            (function (param) {
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
              (function (param) {
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
                (function (param) {
                    var hit = /* record */{
                      contents: false
                    };
                    Js_null.iter(null, (function (param) {
                            hit.contents = true;
                            return /* () */0;
                          }));
                    return /* Eq */Block.__(0, [
                              false,
                              hit.contents
                            ]);
                  })
              ],
              /* :: */[
                /* tuple */[
                  "iter - 'a",
                  (function (param) {
                      var hit = /* record */{
                        contents: 0
                      };
                      Js_null.iter(2, (function (v) {
                              hit.contents = v;
                              return /* () */0;
                            }));
                      return /* Eq */Block.__(0, [
                                2,
                                hit.contents
                              ]);
                    })
                ],
                /* :: */[
                  /* tuple */[
                    "fromOption - None",
                    (function (param) {
                        return /* Eq */Block.__(0, [
                                  null,
                                  Js_null.fromOption(undefined)
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "fromOption - Some",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    2,
                                    Js_null.fromOption(2)
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

Mt.from_pair_suites("Js_null_test", suites);

exports.suites = suites;
/*  Not a pure module */
