'use strict';

var Mt = require("./mt.js");

var v = {
  contents: 1
};

v.contents = v.contents + 1 | 0;

var a = v.contents;

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

eq("File \"condition_compilation_test.ml\", line 98, characters 5-12", 3, 3);

eq("File \"condition_compilation_test.ml\", line 99, characters 5-12", v.contents, 2);

Mt.from_pair_suites("Condition_compilation_test", suites.contents);

var b = "u";

var buffer_size = 1;

var vv = 3;

var version_gt_3 = true;

var version = -1;

var ocaml_veriosn = "unknown";

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
