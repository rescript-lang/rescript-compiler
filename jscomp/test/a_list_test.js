'use strict';

var Mt       = require("./mt");
var Block    = require("../../lib/js/block");
var Ext_list = require("./ext_list");

var suites_000 = /* tuple */[
  "drop",function () {
    return /* Eq */Block.__(0, [
              Ext_list.drop(3, /* Nested :: */[
                    0,[
                      1,[
                        2,/* [] */0
                      ]
                    ]
                  ]),/* [] */0
            ]);
  }
];

var suites_001 = /* Nested :: */[
  /* tuple */[
    "drop1",function () {
      return /* Eq */Block.__(0, [
                Ext_list.drop(2, /* Nested :: */[
                      0,[
                        1,[
                          2,/* [] */0
                        ]
                      ]
                    ]),/* Nested :: */[
                  2,/* [] */0
                ]
              ]);
    }
  ],[
    /* tuple */[
      "flat_map",function () {
        return /* Eq */Block.__(0, [
                  /* Nested :: */[
                    0,[
                      0,[
                        1,[
                          1,[
                            0,/* [] */0
                          ]
                        ]
                      ]
                    ]
                  ],Ext_list.flat_map(function (x) {
                        if (x % 2) {
                          return /* Nested :: */[
                                  1,[
                                    1,/* [] */0
                                  ]
                                ];
                        }
                        else {
                          return /* Nested :: */[
                                  0,/* [] */0
                                ];
                        }
                      }, /* Nested :: */[
                        0,[
                          0,[
                            3,[
                              0,/* [] */0
                            ]
                          ]
                        ]
                      ])
                ]);
      }
    ],/* [] */0
  ]
];

var suites = /* Nested :: */[
  suites_000,suites_001
];

Mt.from_pair_suites("a_list_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
