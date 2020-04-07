'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function map(f, a) {
  var f$1 = Curry.__1(f);
  var l = a.length;
  if (l === 0) {
    return [];
  }
  var r = Caml_array.caml_make_vect(l, f$1(a[0]));
  for(var i = 1; i < l; ++i){
    r[i] = f$1(a[i]);
  }
  return r;
}

function init(l, f) {
  var f$1 = Curry.__1(f);
  if (l === 0) {
    return [];
  }
  if (l < 0) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Array.init"
        ];
  }
  var res = Caml_array.caml_make_vect(l, f$1(0));
  for(var i = 1 ,i_finish = l - 1 | 0; i <= i_finish; ++i){
    res[i] = f$1(i);
  }
  return res;
}

function fold_left(f, x, a) {
  var f$1 = Curry.__2(f);
  var r = x;
  for(var i = 0 ,i_finish = a.length; i < i_finish; ++i){
    r = f$1(r, a[i]);
  }
  return r;
}

function f2(param) {
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
  
}

f2(undefined);

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  
}

var v = {
  contents: 0
};

var all_v = {
  contents: /* [] */0
};

function add5(a0, a1, a2, a3, a4) {
  console.log(/* tuple */[
        a0,
        a1,
        a2,
        a3,
        a4
      ]);
  all_v.contents = /* :: */[
    v.contents,
    all_v.contents
  ];
  return (((a0 + a1 | 0) + a2 | 0) + a3 | 0) + a4 | 0;
}

function f(x) {
  v.contents = v.contents + 1 | 0;
  var partial_arg = 2;
  v.contents = v.contents + 1 | 0;
  var partial_arg$1 = 1;
  return (function (param, param$1) {
      return add5(x, partial_arg$1, partial_arg, param, param$1);
    });
}

function g(x) {
  v.contents = v.contents + 1 | 0;
  var partial_arg = 2;
  v.contents = v.contents + 1 | 0;
  var partial_arg$1 = 1;
  var u = function (param, param$1) {
    return add5(x, partial_arg$1, partial_arg, param, param$1);
  };
  all_v.contents = /* :: */[
    v.contents,
    all_v.contents
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

eq("File \"earger_curry_test.ml\", line 122, characters 7-14", all_v.contents, /* :: */[
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

Mt.from_pair_suites("Earger_curry_test", suites.contents);

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
