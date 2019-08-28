'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_option = require("../../lib/js/caml_option.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  Pervasives.incr(test_id);
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

var counter = /* record */{
  contents: 0
};

function side_effect(x) {
  Pervasives.incr(x);
  return x.contents;
}

function bug_to_fix(f, x) {
  return hey(f(x), 3);
}

function bug_to_fix2(f, x) {
  return hey(Caml_option.option_get(f(x)), 3);
}

var counter2 = /* record */{
  contents: 0
};

function side_effect2(x) {
  Pervasives.incr(x);
  return x.contents;
}

var v = bug_to_fix(side_effect, counter);

var pair_000 = /* tuple */[
  v,
  counter.contents
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
  counter.contents
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
