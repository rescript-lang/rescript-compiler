'use strict';

var Mt       = require("./mt");
var Block    = require("../../lib/js/block");
var Caml_obj = require("../../lib/js/caml_obj");

var suites_000 = /* tuple */[
  "option",function () {
    return /* Eq */Block.__(0, [
              /* true */1,Caml_obj.caml_lessthan(/* None */0, /* Some */[1])
            ]);
  }
];

var suites_001 = /* Nested :: */[
  /* tuple */[
    "option2",function () {
      return /* Eq */Block.__(0, [
                /* true */1,Caml_obj.caml_lessthan(/* Some */[1], /* Some */[2])
              ]);
    }
  ],[
    /* tuple */[
      "list0",function () {
        return /* Eq */Block.__(0, [
                  /* true */1,Caml_obj.caml_greaterthan(/* Nested :: */[
                        1,/* [] */0
                      ], /* [] */0)
                ]);
      }
    ],[
      /* tuple */[
        "listeq",function () {
          return /* Eq */Block.__(0, [
                    /* true */1,Caml_obj.caml_equal(/* Nested :: */[
                          1,[
                            2,[
                              3,/* [] */0
                            ]
                          ]
                        ], /* Nested :: */[
                          1,[
                            2,[
                              3,/* [] */0
                            ]
                          ]
                        ])
                  ]);
        }
      ],[
        /* tuple */[
          "listneq",function () {
            return /* Eq */Block.__(0, [
                      /* true */1,Caml_obj.caml_greaterthan(/* Nested :: */[
                            1,[
                              2,[
                                3,/* [] */0
                              ]
                            ]
                          ], /* Nested :: */[
                            1,[
                              2,[
                                2,/* [] */0
                              ]
                            ]
                          ])
                    ]);
          }
        ],[
          /* tuple */[
            "custom_u",function () {
              return /* Eq */Block.__(0, [
                        /* true */1,Caml_obj.caml_greaterthan(/* tuple */[
                              /* A */Block.__(0, [3]),/* B */Block.__(1, [
                                  2,/* false */0
                                ]),/* C */Block.__(2, [1])
                            ], /* tuple */[
                              /* A */Block.__(0, [3]),/* B */Block.__(1, [
                                  2,/* false */0
                                ]),/* C */Block.__(2, [0])
                            ])
                      ]);
            }
          ],[
            /* tuple */[
              "custom_u2",function () {
                return /* Eq */Block.__(0, [
                          /* true */1,Caml_obj.caml_equal(/* tuple */[
                                /* A */Block.__(0, [3]),/* B */Block.__(1, [
                                    2,/* false */0
                                  ]),/* C */Block.__(2, [1])
                              ], /* tuple */[
                                /* A */Block.__(0, [3]),/* B */Block.__(1, [
                                    2,/* false */0
                                  ]),/* C */Block.__(2, [1])
                              ])
                        ]);
              }
            ],/* [] */0
          ]
        ]
      ]
    ]
  ]
];

var suites = /* Nested :: */[
  suites_000,suites_001
];

Mt.from_pair_suites("caml_compare_test.ml", suites);

exports.suites = suites;
/*  Not a pure module */
