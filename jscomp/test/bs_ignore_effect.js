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


function add(x,y){
  return x + y
}

;

var v = /* record */[/* contents */0];

var h = (v[0] = v[0] + 1 | 0, {
    hi: 2,
    lo: 0
  });

var z = (v[0] = v[0] + 1 | 0, "Float", add(3.0, 2.0));

eq("File \"bs_ignore_effect.ml\", line 26, characters 5-12", v[0], 2);

eq("File \"bs_ignore_effect.ml\", line 27, characters 5-12", z, 5.0);

Mt.from_pair_suites("Bs_ignore_effect", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.h = h;
exports.z = z;
/*  Not a pure module */
