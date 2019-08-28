'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_option = require("../../lib/js/caml_option.js");
var Js_undefined = require("../../lib/js/js_undefined.js");

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
                  Caml_option.undefined_to_opt(/* () */0)
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "return",
      (function (param) {
          return /* Eq */Block.__(0, [
                    "something",
                    Caml_option.undefined_to_opt("something")
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
              (function (param) {
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
                (function (param) {
                    var hit = /* record */{
                      contents: false
                    };
                    Js_undefined.iter(undefined, (function (param) {
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
                      Js_undefined.iter(2, (function (v) {
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
                                  undefined,
                                  Js_undefined.fromOption(undefined)
                                ]);
                      })
                  ],
                  /* :: */[
                    /* tuple */[
                      "fromOption - Some",
                      (function (param) {
                          return /* Eq */Block.__(0, [
                                    2,
                                    Js_undefined.fromOption(2)
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

Mt.from_pair_suites("Js_undefined_test", suites);

exports.suites = suites;
/*  Not a pure module */
