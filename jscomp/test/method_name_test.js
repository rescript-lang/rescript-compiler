'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
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

function f(x, i, file, v) {
  x.case(i);
  x.case(i, v);
  x.open(file);
  x.open(file);
  return x.MAX_LENGTH;
}

function ff(x, i, v) {
  x.make;
  x.make_config;
  x.make = v;
  x.make_config = v;
  x.case(i);
  return x._open(3);
}

var u = {
  "Content'type": "x"
};

var h = {
  open: 3,
  end: 32
};

function hg(x) {
  return x.open + x.end | 0;
}

eq("File \"method_name_test.ml\", line 39, characters 12-19", 35, hg(h));

Mt.from_pair_suites("method_name_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.ff = ff;
exports.u = u;
exports.h = h;
exports.hg = hg;
/*  Not a pure module */
