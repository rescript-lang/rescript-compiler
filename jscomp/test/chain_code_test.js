'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

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

function f(h) {
  return h.x.y.z;
}

function f2(h) {
  return h.x.y.z;
}

function f3(h, x, y) {
  return h.paint(x, y).draw(x, y);
}

function f4(h, x, y) {
  h.paint = /* tuple */[
    x,
    y
  ];
  h.paint.draw = /* tuple */[
    x,
    y
  ];
  return /* () */0;
}

eq("File \"chain_code_test.ml\", line 28, characters 5-12", 32, ({
        x: {
          y: {
            z: 32
          }
        }
      }).x.y.z);

Mt.from_pair_suites("chain_code_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
/*  Not a pure module */
