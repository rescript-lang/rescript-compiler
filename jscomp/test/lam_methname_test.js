// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Lam_methname = require("./lam_methname");
var Mt           = require("./mt");
var Block        = require("../runtime/block");

var suites_000 = /* tuple */[
  "normal",
  function () {
    return /* Eq */Block.__(0, [
              Lam_methname.process("xx"),
              /* tuple */[
                /* Unknown */Block.__(2, [/* None */0]),
                "xx"
              ]
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "js",
    function () {
      return /* Eq */Block.__(0, [
                Lam_methname.process("xx__js"),
                /* tuple */[
                  /* Js */Block.__(0, [/* None */0]),
                  "xx"
                ]
              ]);
    }
  ],
  /* :: */[
    /* tuple */[
      "js_set",
      function () {
        return /* Eq */Block.__(0, [
                  Lam_methname.process("xx__w"),
                  /* tuple */[
                    /* Js_write */2,
                    "xx"
                  ]
                ]);
      }
    ],
    /* :: */[
      /* tuple */[
        "js_none",
        function () {
          return /* Eq */Block.__(0, [
                    Lam_methname.process("xx__"),
                    /* tuple */[
                      /* Js */Block.__(0, [/* None */0]),
                      "xx"
                    ]
                  ]);
        }
      ],
      /* :: */[
        /* tuple */[
          "js1",
          function () {
            return /* Eq */Block.__(0, [
                      Lam_methname.process("xx__js_1"),
                      /* tuple */[
                        /* Js */Block.__(0, [/* Some */[1]]),
                        "xx"
                      ]
                    ]);
          }
        ],
        /* :: */[
          /* tuple */[
            "js2",
            function () {
              return /* Eq */Block.__(0, [
                        Lam_methname.process("xx__w"),
                        /* tuple */[
                          /* Js_write */2,
                          "xx"
                        ]
                      ]);
            }
          ],
          /* :: */[
            /* tuple */[
              "js3",
              function () {
                return /* Eq */Block.__(0, [
                          Lam_methname.process("xx__2"),
                          /* tuple */[
                            /* Js */Block.__(0, [/* Some */[2]]),
                            "xx"
                          ]
                        ]);
              }
            ],
            /* :: */[
              /* tuple */[
                "ml1",
                function () {
                  return /* Eq */Block.__(0, [
                            Lam_methname.process("xx__2_ml"),
                            /* tuple */[
                              /* Ml */Block.__(1, [/* Some */[2]]),
                              "xx"
                            ]
                          ]);
                }
              ],
              /* :: */[
                /* tuple */[
                  "index",
                  function () {
                    return /* Eq */Block.__(0, [
                              Lam_methname.process("index__"),
                              /* tuple */[
                                /* Js_read_index */0,
                                "index"
                              ]
                            ]);
                  }
                ],
                /* :: */[
                  /* tuple */[
                    "set_index",
                    function () {
                      return /* Eq */Block.__(0, [
                                Lam_methname.process("index__w"),
                                /* tuple */[
                                  /* Js_write_index */1,
                                  "index"
                                ]
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
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("lam_methname_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
