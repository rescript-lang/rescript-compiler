'use strict';

var Mt = require("./mt.js");

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: x,
                  Arg1: y
                };
        })
    ],
    Arg1: suites[0]
  };
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

Mt.from_pair_suites("Chain_code_test", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
/*  Not a pure module */
