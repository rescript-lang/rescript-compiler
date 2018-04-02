'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_array = require("../../lib/js/caml_array.js");
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

var v = /* array */[
  1,
  2,
  3,
  3
];

eq("File \"array_subtle_test.ml\", line 12, characters 5-12", /* tuple */[
      4,
      v.length
    ]);

eq("File \"array_subtle_test.ml\", line 15, characters 5-12", /* tuple */[
      5,
      v.push(3)
    ]);

eq("File \"array_subtle_test.ml\", line 16, characters 5-12", /* tuple */[
      5,
      v.length
    ]);

eq("File \"array_subtle_test.ml\", line 17, characters 5-12", /* tuple */[
      5,
      v.length
    ]);

eq("File \"array_subtle_test.ml\", line 21, characters 5-12", /* tuple */[
      3,
      Caml_array.caml_array_get(v, 2)
    ]);

Caml_array.caml_array_set(v, 2, 4);

eq("File \"array_subtle_test.ml\", line 23, characters 5-12", /* tuple */[
      4,
      Caml_array.caml_array_get(v, 2)
    ]);

while(v.length > 0) {
  Js_primitive.undefined_to_opt(v.pop());
};

eq("File \"array_subtle_test.ml\", line 29, characters 5-12", /* tuple */[
      0,
      v.length
    ]);

Mt.from_pair_suites("array_subtle_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
/*  Not a pure module */
