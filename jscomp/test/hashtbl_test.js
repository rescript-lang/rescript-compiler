'use strict';

var Mt             = require("./mt.js");
var List           = require("../../lib/js/list.js");
var $$Array        = require("../../lib/js/array.js");
var Block          = require("../../lib/js/block.js");
var Curry          = require("../../lib/js/curry.js");
var Hashtbl        = require("../../lib/js/hashtbl.js");
var MoreLabels     = require("../../lib/js/moreLabels.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

function to_list(tbl) {
  return Hashtbl.fold((function (k, v, acc) {
                return /* :: */[
                        /* tuple */[
                          k,
                          v
                        ],
                        acc
                      ];
              }), tbl, /* [] */0);
}

function f() {
  var tbl = Hashtbl.create(/* None */0, 17);
  Hashtbl.add(tbl, 1, /* "1" */49);
  Hashtbl.add(tbl, 2, /* "2" */50);
  return List.sort((function (param, param$1) {
                return Caml_primitive.caml_int_compare(param[0], param$1[0]);
              }), to_list(tbl));
}

function g(count) {
  var tbl = Hashtbl.create(/* None */0, 17);
  for(var i = 0; i <= count; ++i){
    Hashtbl.replace(tbl, (i << 1), "" + i);
  }
  for(var i$1 = 0; i$1 <= count; ++i$1){
    Hashtbl.replace(tbl, (i$1 << 1), "" + i$1);
  }
  var v = to_list(tbl);
  return $$Array.of_list(List.sort((function (param, param$1) {
                    return Caml_primitive.caml_int_compare(param[0], param$1[0]);
                  }), v));
}

var suites_000 = /* tuple */[
  "simple",
  (function () {
      return /* Eq */Block.__(0, [
                /* :: */[
                  /* tuple */[
                    1,
                    /* "1" */49
                  ],
                  /* :: */[
                    /* tuple */[
                      2,
                      /* "2" */50
                    ],
                    /* [] */0
                  ]
                ],
                f(/* () */0)
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "more_iterations",
    (function () {
        return /* Eq */Block.__(0, [
                  $$Array.init(1001, (function (i) {
                          return /* tuple */[
                                  (i << 1),
                                  "" + i
                                ];
                        })),
                  g(1000)
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "More_labels_regressionfix_374",
      (function () {
          var tbl = Curry._2(MoreLabels.Hashtbl[/* create */0], /* None */0, 30);
          Hashtbl.add(tbl, 3, 3);
          return /* Eq */Block.__(0, [
                    tbl[/* size */0],
                    1
                  ]);
        })
    ],
    /* [] */0
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("hashtbl_test.ml", suites);

exports.to_list = to_list;
exports.f       = f;
exports.g       = g;
exports.suites  = suites;
/*  Not a pure module */
