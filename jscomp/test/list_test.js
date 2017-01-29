'use strict';

var Mt       = require("./mt");
var List     = require("../../lib/js/list");
var $$Array  = require("../../lib/js/array");
var Block    = require("../../lib/js/block");
var Caml_obj = require("../../lib/js/caml_obj");

var a = /* Nested :: */[
  1,[
    2,/* [] */0
  ]
];

var b_001 = /* Nested :: */[
  [
    3,[
      4,/* [] */0
    ]
  ],/* [] */0
];

var b = /* Nested :: */[
  a,b_001
];

var list_suites_000 = /* tuple */[
  "length",function () {
    return /* Eq */Block.__(0, [
              1,List.length(/* Nested :: */[
                    /* tuple */[
                      0,1,2,3,4
                    ],/* [] */0
                  ])
            ]);
  }
];

var list_suites_001 = /* Nested :: */[
  /* tuple */[
    "length2",function () {
      return /* Eq */Block.__(0, [
                5,List.length(/* Nested :: */[
                      0,[
                        1,[
                          2,[
                            3,[
                              4,/* [] */0
                            ]
                          ]
                        ]
                      ]
                    ])
              ]);
    }
  ],[
    /* tuple */[
      "long_length",function () {
        return /* Eq */Block.__(0, [
                  30000,List.length($$Array.to_list($$Array.init(30000, function () {
                                return 0;
                              })))
                ]);
      }
    ],[
      /* tuple */[
        "sort",function () {
          return /* Eq */Block.__(0, [
                    List.sort(Caml_obj.caml_int_compare, /* Nested :: */[
                          4,[
                            1,[
                              2,[
                                3,/* [] */0
                              ]
                            ]
                          ]
                        ]),/* Nested :: */[
                      1,[
                        2,[
                          3,[
                            4,/* [] */0
                          ]
                        ]
                      ]
                    ]
                  ]);
        }
      ],/* [] */0
    ]
  ]
];

var list_suites = /* Nested :: */[
  list_suites_000,list_suites_001
];

Mt.from_pair_suites("list_test.ml", list_suites);

exports.a           = a;
exports.b           = b;
exports.list_suites = list_suites;
/*  Not a pure module */
