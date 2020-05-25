'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var $$Array = require("../../lib/js/array.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Hashtbl = require("../../lib/js/hashtbl.js");
var MoreLabels = require("../../lib/js/moreLabels.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");

function to_list(tbl) {
  return Hashtbl.fold((function (k, v, acc) {
                return /* :: */{
                        _0: /* tuple */[
                          k,
                          v
                        ],
                        _1: acc
                      };
              }), tbl, /* [] */0);
}

function f(param) {
  var tbl = Hashtbl.create(undefined, 17);
  Hashtbl.add(tbl, 1, /* "1" */49);
  Hashtbl.add(tbl, 2, /* "2" */50);
  return List.sort((function (param, param$1) {
                return Caml_primitive.caml_int_compare(param[0], param$1[0]);
              }), to_list(tbl));
}

function g(count) {
  var tbl = Hashtbl.create(undefined, 17);
  for(var i = 0; i <= count; ++i){
    Hashtbl.replace(tbl, (i << 1), String(i));
  }
  for(var i$1 = 0; i$1 <= count; ++i$1){
    Hashtbl.replace(tbl, (i$1 << 1), String(i$1));
  }
  var v = to_list(tbl);
  return $$Array.of_list(List.sort((function (param, param$1) {
                    return Caml_primitive.caml_int_compare(param[0], param$1[0]);
                  }), v));
}

var suites_000 = /* tuple */[
  "simple",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: /* :: */{
                _0: /* tuple */[
                  1,
                  /* "1" */49
                ],
                _1: /* :: */{
                  _0: /* tuple */[
                    2,
                    /* "2" */50
                  ],
                  _1: /* [] */0
                }
              },
              _1: f(undefined)
            };
    })
];

var suites_001 = /* :: */{
  _0: /* tuple */[
    "more_iterations",
    (function (param) {
        return {
                tag: /* Eq */0,
                _0: $$Array.init(1001, (function (i) {
                        return /* tuple */[
                                (i << 1),
                                String(i)
                              ];
                      })),
                _1: g(1000)
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "More_labels_regressionfix_374",
      (function (param) {
          var tbl = Curry._2(MoreLabels.Hashtbl.create, undefined, 30);
          Hashtbl.add(tbl, 3, 3);
          return {
                  tag: /* Eq */0,
                  _0: tbl.size,
                  _1: 1
                };
        })
    ],
    _1: /* [] */0
  }
};

var suites = /* :: */{
  _0: suites_000,
  _1: suites_001
};

Mt.from_pair_suites("Hashtbl_test", suites);

exports.to_list = to_list;
exports.f = f;
exports.g = g;
exports.suites = suites;
/*  Not a pure module */
