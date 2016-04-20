// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj   = require("../runtime/caml_obj");
var Hashtbl    = require("../stdlib/hashtbl");
var Mt         = require("./mt");
var $$Array    = require("../stdlib/array");
var Caml_curry = require("../runtime/caml_curry");
var List       = require("../stdlib/list");

function f(H) {
  var tbl = Caml_curry.app1(H[0], 17);
  Caml_curry.app3(H[4], tbl, 1, /* "1" */49);
  Caml_curry.app3(H[4], tbl, 2, /* "2" */50);
  return List.sort(function (param, param$1) {
              return Caml_obj.caml_int_compare(param[0], param$1[0]);
            }, Caml_curry.app3(H[11], function (k, v, acc) {
                  return /* :: */[
                          /* tuple */[
                            k,
                            v
                          ],
                          acc
                        ];
                }, tbl, /* [] */0));
}

function g(H) {
  return function (count) {
    var tbl = Caml_curry.app1(H[0], 17);
    for(var i = 0; i<= count; ++i){
      Caml_curry.app3(H[8], tbl, (i << 1), "" + i);
    }
    for(var i$1 = 0; i$1<= count; ++i$1){
      Caml_curry.app3(H[8], tbl, (i$1 << 1), "" + i$1);
    }
    var v = Caml_curry.app3(H[11], function (k, v, acc) {
          return /* :: */[
                  /* tuple */[
                    k,
                    v
                  ],
                  acc
                ];
        }, tbl, /* [] */0);
    return $$Array.of_list(List.sort(function (param, param$1) {
                    return Caml_obj.caml_int_compare(param[0], param$1[0]);
                  }, v));
  };
}

var hash = Hashtbl.hash

function equal(x, y) {
  return +(x === y);
}

var Int_hash = Hashtbl.Make(/* module */[
      equal,
      hash
    ]);

var suites_000 = /* tuple */[
  "simple",
  function () {
    return /* Eq */{
            0: /* :: */[
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
            1: f(Int_hash),
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "more_iterations",
    function () {
      return /* Eq */{
              0: $$Array.init(1001, function (i) {
                    return /* tuple */[
                            (i << 1),
                            "" + i
                          ];
                  }),
              1: g(Int_hash)(1000),
              length: 2,
              tag: 0
            };
    }
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("int_hashtbl_test.ml", suites);

exports.f        = f;
exports.g        = g;
exports.Int_hash = Int_hash;
exports.suites   = suites;
/* Int_hash Not a pure module */
