'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Int64 = require("../../lib/js/int64.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");

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

function id(x) {
  return Caml_format.caml_int64_of_string(Caml_format.caml_int64_format("%d", x));
}

var i = /* int64 */{
  hi: 2074848171,
  lo: 2880154539
};

var s = Caml_format.caml_int64_format("%d", i);

var i$prime = Caml_format.caml_int64_of_string(s);

eq("File \"gpr_1503_test.ml\", line 18, characters 5-12", i, i$prime);

eq("File \"gpr_1503_test.ml\", line 21, characters 7-14", Int64.max_int, Caml_format.caml_int64_of_string(Caml_format.caml_int64_format("%d", Int64.max_int)));

eq("File \"gpr_1503_test.ml\", line 22, characters 7-14", Int64.min_int, Caml_format.caml_int64_of_string(Caml_format.caml_int64_format("%d", Int64.min_int)));

Mt.from_pair_suites("Gpr_1503_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.id = id;
/* s Not a pure module */
