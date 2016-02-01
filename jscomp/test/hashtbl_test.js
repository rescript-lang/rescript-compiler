// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Hashtbl        = require("../stdlib/hashtbl");
var Mt             = require("./mt");
var Caml_primitive = require("../runtime/caml_primitive");
var $$Array        = require("../stdlib/array");
var List           = require("../stdlib/list");

function to_list(tbl) {
  return Hashtbl.fold(function (k, v, acc) {
              return [
                      /* :: */0,
                      [
                        /* tuple */0,
                        k,
                        v
                      ],
                      acc
                    ];
            }, tbl, /* [] */0);
}

function f() {
  var tbl = Hashtbl.create(/* None */0, 17);
  Hashtbl.add(tbl, 1, /* "1" */49);
  Hashtbl.add(tbl, 2, /* "2" */50);
  return List.sort(function (param, param$1) {
              return Caml_primitive.caml_int_compare(param[1], param$1[1]);
            }, to_list(tbl));
}

function g(count) {
  var tbl = Hashtbl.create(/* None */0, 17);
  for(var i = 0; i<= count; ++i){
    Hashtbl.replace(tbl, i * 2, "" + i);
  }
  for(var i$1 = 0; i$1<= count; ++i$1){
    Hashtbl.replace(tbl, i$1 * 2, "" + i$1);
  }
  var v = to_list(tbl);
  return $$Array.of_list(List.sort(function (param, param$1) {
                  return Caml_primitive.caml_int_compare(param[1], param$1[1]);
                }, v));
}

var suites_001 = [
  /* tuple */0,
  "simple",
  function () {
    return [
            /* Eq */0,
            [
              /* :: */0,
              [
                /* tuple */0,
                1,
                /* "1" */49
              ],
              [
                /* :: */0,
                [
                  /* tuple */0,
                  2,
                  /* "2" */50
                ],
                /* [] */0
              ]
            ],
            f(/* () */0)
          ];
  }
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "more_iterations",
    function () {
      var count = 1000;
      return [
              /* Eq */0,
              $$Array.init(count + 1, function (i) {
                    return [
                            /* tuple */0,
                            2 * i,
                            "" + i
                          ];
                  }),
              g(count)
            ];
    }
  ],
  /* [] */0
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_pair_suites("hashtbl_test.ml", suites);

exports.to_list = to_list;
exports.f       = f;
exports.g       = g;
exports.suites  = suites;
/*  Not a pure module */
