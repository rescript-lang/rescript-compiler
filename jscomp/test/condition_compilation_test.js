'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");

var v = /* record */{
  contents: 1
};

Pervasives.incr(v);

var a = v.contents;

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
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

eq("File \"condition_compilation_test.ml\", line 98, characters 5-12", 3, 3);

eq("File \"condition_compilation_test.ml\", line 99, characters 5-12", v.contents, 2);

Mt.from_pair_suites("Condition_compilation_test", suites.contents);

var b = "u";

var buffer_size = 1;

var vv = 3;

var version_gt_3 = true;

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
