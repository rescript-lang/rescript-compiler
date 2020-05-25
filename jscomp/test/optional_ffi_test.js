'use strict';

var Mt = require("./mt.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

function hey(x, y) {
    if (x === void 0) { x = 3; }
    return x + y;
  }
;

var u = hey(undefined, 3);

var z = hey(5, 3);

eq("File \"optional_ffi_test.ml\", line 23, characters 5-12", [
      [
        u,
        z
      ],
      [
        6,
        8
      ]
    ]);

var counter = {
  contents: 0
};

function side_effect(x) {
  x.contents = x.contents + 1 | 0;
  return x.contents;
}

function bug_to_fix(f, x) {
  return hey(f(x), 3);
}

function bug_to_fix2(f, x) {
  return hey(Caml_option.option_get(f(x)), 3);
}

var counter2 = {
  contents: 0
};

function side_effect2(x) {
  x.contents = x.contents + 1 | 0;
  return x.contents;
}

var v = bug_to_fix(side_effect, counter);

var pair_0 = [
  v,
  counter.contents
];

var pair_1 = [
  4,
  1
];

var pair = [
  pair_0,
  pair_1
];

var v2 = bug_to_fix2(side_effect2, counter2);

var pair2_0 = [
  v2,
  counter.contents
];

var pair2_1 = [
  4,
  1
];

var pair2 = [
  pair2_0,
  pair2_1
];

eq("File \"optional_ffi_test.ml\", line 43, characters 5-12", pair);

eq("File \"optional_ffi_test.ml\", line 44, characters 5-12", pair2);

function heystr(x, y) {
    if (x === void 0) { x = "3"; }
    return x + y;
  }
;

var pair_1$1 = heystr("name", "4");

var pair$1 = [
  "name4",
  pair_1$1
];

eq("File \"optional_ffi_test.ml\", line 58, characters 5-12", pair$1);

Mt.from_pair_suites("Optional_ffi_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.u = u;
exports.z = z;
exports.counter = counter;
exports.side_effect = side_effect;
exports.bug_to_fix = bug_to_fix;
exports.bug_to_fix2 = bug_to_fix2;
exports.counter2 = counter2;
exports.side_effect2 = side_effect2;
/*  Not a pure module */
