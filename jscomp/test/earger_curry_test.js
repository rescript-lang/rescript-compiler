'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function map(f, a) {
  var f$1 = Curry.__1(f);
  var a$1 = a;
  var l = a$1.length;
  if (l === 0) {
    return /* array */[];
  } else {
    var r = Caml_array.caml_make_vect(l, f$1(a$1[0]));
    for(var i = 1 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
      r[i] = f$1(a$1[i]);
    }
    return r;
  }
}

function init(l, f) {
  var l$1 = l;
  var f$1 = Curry.__1(f);
  if (l$1 === 0) {
    return /* array */[];
  } else if (l$1 < 0) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Array.init"
        ];
  } else {
    var res = Caml_array.caml_make_vect(l$1, f$1(0));
    for(var i = 1 ,i_finish = l$1 - 1 | 0; i <= i_finish; ++i){
      res[i] = f$1(i);
    }
    return res;
  }
}

function fold_left(f, x, a) {
  var f$1 = Curry.__2(f);
  var x$1 = x;
  var a$1 = a;
  var r = x$1;
  for(var i = 0 ,i_finish = a$1.length - 1 | 0; i <= i_finish; ++i){
    r = f$1(r, a$1[i]);
  }
  return r;
}

function f2() {
  var arr = init(30000000, (function (i) {
          return i;
        }));
  var b = map((function (i) {
          return i + i - 1;
        }), arr);
  var v = fold_left((function (prim, prim$1) {
          return prim + prim$1;
        }), 0, b);
  console.log(Pervasives.string_of_float(v));
  return /* () */0;
}

f2(/* () */0);

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

var v = [0];

var all_v = [/* [] */0];

function add5(a0, a1, a2, a3, a4) {
  console.log(/* tuple */[
        a0,
        a1,
        a2,
        a3,
        a4
      ]);
  all_v[0] = /* :: */[
    v[0],
    all_v[0]
  ];
  return (((a0 + a1 | 0) + a2 | 0) + a3 | 0) + a4 | 0;
}

function f(x) {
  v[0] = v[0] + 1 | 0;
  var partial_arg = 2;
  v[0] = v[0] + 1 | 0;
  var partial_arg$1 = 1;
  return (function (param, param$1) {
      return add5(x, partial_arg$1, partial_arg, param, param$1);
    });
}

function g(x) {
  v[0] = v[0] + 1 | 0;
  var partial_arg = 2;
  v[0] = v[0] + 1 | 0;
  var partial_arg$1 = 1;
  var u = function (param, param$1) {
    return add5(x, partial_arg$1, partial_arg, param, param$1);
  };
  all_v[0] = /* :: */[
    v[0],
    all_v[0]
  ];
  return u;
}

var a = f(0)(3, 4);

var b = f(0)(3, 5);

var c = Curry._2(g(0), 3, 4);

var d = Curry._2(g(0), 3, 5);

eq("File \"earger_curry_test.ml\", line 118, characters 7-14", a, 10);

eq("File \"earger_curry_test.ml\", line 119, characters 7-14", b, 11);

eq("File \"earger_curry_test.ml\", line 120, characters 7-14", c, 10);

eq("File \"earger_curry_test.ml\", line 121, characters 7-14", d, 11);

eq("File \"earger_curry_test.ml\", line 122, characters 7-14", all_v[0], /* :: */[
      8,
      /* :: */[
        8,
        /* :: */[
          6,
          /* :: */[
            6,
            /* :: */[
              4,
              /* :: */[
                2,
                /* [] */0
              ]
            ]
          ]
        ]
      ]
    ]);

Mt.from_pair_suites("earger_curry_test.ml", suites[0]);

exports.map = map;
exports.init = init;
exports.fold_left = fold_left;
exports.f2 = f2;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.all_v = all_v;
exports.add5 = add5;
exports.f = f;
exports.g = g;
exports.a = a;
exports.b = b;
exports.c = c;
exports.d = d;
/*  Not a pure module */
