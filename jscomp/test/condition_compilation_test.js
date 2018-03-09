'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

var v = [1];

v[0] = v[0] + 1 | 0;

var a = v[0];

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

eq("File \"condition_compilation_test.ml\", line 96, characters 5-12", 3, 3);

eq("File \"condition_compilation_test.ml\", line 97, characters 5-12", v[0], 2);

Mt.from_pair_suites("condition_compilation_test.ml", suites[0]);

var b = "u";

var buffer_size = 1;

var vv = 3;

var version_gt_3 = /* true */1;

var version = -1;

var ocaml_veriosn = "4.02.3";

exports.b = b;
exports.buffer_size = buffer_size;
exports.vv = vv;
exports.v = v;
exports.a = a;
exports.version_gt_3 = version_gt_3;
exports.version = version;
exports.ocaml_veriosn = ocaml_veriosn;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
