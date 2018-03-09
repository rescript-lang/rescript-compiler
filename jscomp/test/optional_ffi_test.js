'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Js_primitive = require("../../lib/js/js_primitive.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
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


function hey(x, y) {
    if (x === void 0) { x = 3; }
    return x + y;
  }

;

var u = hey(undefined, 3);

var z = hey(5, 3);

eq("File \"optional_ffi_test.ml\", line 23, characters 5-12", /* tuple */[
      /* tuple */[
        u,
        z
      ],
      /* tuple */[
        6,
        8
      ]
    ]);

var counter = [0];

function side_effect(x) {
  x[0] = x[0] + 1 | 0;
  return x[0];
}

function bug_to_fix(f, x) {
  return hey(f(x), 3);
}

function bug_to_fix2(f, x) {
  return hey(Js_primitive.option_get(f(x)), 3);
}

var counter2 = [0];

function side_effect2(x) {
  x[0] = x[0] + 1 | 0;
  return /* Some */[x[0]];
}

var v = bug_to_fix(side_effect, counter);

var pair_000 = /* tuple */[
  v,
  counter[0]
];

var pair_001 = /* tuple */[
  4,
  1
];

var pair = /* tuple */[
  pair_000,
  pair_001
];

var v2 = bug_to_fix2(side_effect2, counter2);

var pair2_000 = /* tuple */[
  v2,
  counter[0]
];

var pair2_001 = /* tuple */[
  4,
  1
];

var pair2 = /* tuple */[
  pair2_000,
  pair2_001
];

eq("File \"optional_ffi_test.ml\", line 43, characters 5-12", pair);

eq("File \"optional_ffi_test.ml\", line 44, characters 5-12", pair2);


function heystr(x, y) {
    if (x === void 0) { x = "3"; }
    return x + y;
  }
  
;

var pair_001$1 = heystr("name", "4");

var pair$1 = /* tuple */[
  "name4",
  pair_001$1
];

eq("File \"optional_ffi_test.ml\", line 58, characters 5-12", pair$1);

Mt.from_pair_suites("optional_ffi_test.ml", suites[0]);

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
