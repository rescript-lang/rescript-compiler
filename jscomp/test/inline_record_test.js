'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var v = /* A0 */Block.__(0, [
    /* lbl */3,
    /* more : [] */0
  ]);

var v1 = /* A1 */Block.__(1, [/* more : :: */[
      1,
      /* :: */[
        2,
        /* [] */0
      ]
    ]]);

function f(x) {
  if (x.tag) {
    return List.fold_left((function (prim, prim$1) {
                  return prim + prim$1 | 0;
                }), 0, x[/* more */0]);
  } else {
    return List.fold_left((function (prim, prim$1) {
                  return prim + prim$1 | 0;
                }), x[/* lbl */0], x[/* more */1]);
  }
}

eq("File \"inline_record_test.ml\", line 25, characters 6-13", f(v), 3);

eq("File \"inline_record_test.ml\", line 26, characters 6-13", f(v1), 3);

console.log(f(v));

console.log(f(v1));

var A0 = Caml_exceptions.create("Inline_record_test.A0");

var v3 = [
  A0,
  /* lbl */3,
  /* more : [] */0
];

var tmp;

if (A0 === A0) {
  tmp = 3;
} else {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "inline_record_test.ml",
          51,
          52
        ]
      ];
}

eq("File \"inline_record_test.ml\", line 51, characters 6-13", tmp, 3);

function ff(x) {
  if (x.tag) {
    x[/* z */0] = x[/* z */0] + 2 | 0;
    return /* () */0;
  } else {
    x[/* x */0] = x[/* x */0] + 1 | 0;
    return /* () */0;
  }
}

var v4 = /* A0 */Block.__(0, [
    /* x */0,
    /* y */0,
    /* z */0
  ]);

var v5 = /* A1 */Block.__(1, [/* z */0]);

for(var i = 0; i <= 10; ++i){
  ff(v4);
  ff(v5);
}

var tmp$1;

if (v4.tag) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "inline_record_test.ml",
          69,
          48
        ]
      ];
} else {
  tmp$1 = v4[/* x */0];
}

eq("File \"inline_record_test.ml\", line 69, characters 6-13", tmp$1, 11);

var tmp$2;

if (v5.tag) {
  tmp$2 = v5[/* z */0];
} else {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "inline_record_test.ml",
          71,
          48
        ]
      ];
}

eq("File \"inline_record_test.ml\", line 71, characters 6-13", tmp$2, 22);

var A4 = Caml_exceptions.create("Inline_record_test.A4");

var v6 = [
  A4,
  /* x */0,
  /* y */0,
  /* z */0
];

function ff0(x) {
  if (x[0] === A4) {
    x[/* x */1] = x[/* x */1] + 1 | 0;
    x[/* z */3] = x[/* z */3] + 1 | 0;
    return /* () */0;
  } else {
    return /* () */0;
  }
}

for(var i$1 = 0; i$1 <= 10; ++i$1){
  ff0(v6);
}

var tmp$3;

if (v6[0] === A4) {
  tmp$3 = v6[/* x */1];
} else {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "inline_record_test.ml",
          87,
          49
        ]
      ];
}

eq("File \"inline_record_test.ml\", line 87, characters 6-13", tmp$3, 11);

function ff1(x) {
  if (x) {
    return /* A0 */[
            /* lbl */x[/* lbl */0] + 1 | 0,
            /* more */x[/* more */1]
          ];
  } else {
    return /* A1 */0;
  }
}

Mt.from_pair_suites("Inline_record_test", suites[0]);

var v2 = /* A0 */[
  /* lbl */3,
  /* more : [] */0
];

var vvv = /* A0 */[
  /* lbl */3,
  /* more : [] */0
];

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.v1 = v1;
exports.f = f;
exports.v2 = v2;
exports.A0 = A0;
exports.v3 = v3;
exports.vvv = vvv;
exports.ff = ff;
exports.v4 = v4;
exports.v5 = v5;
exports.A4 = A4;
exports.v6 = v6;
exports.ff0 = ff0;
exports.ff1 = ff1;
/*  Not a pure module */
