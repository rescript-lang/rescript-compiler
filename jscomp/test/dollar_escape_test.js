'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Pervasives = require("../../lib/js/pervasives.js");

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

function $$(x, y) {
  return x + y | 0;
}

var $$$plus = Caml_int32.imul;

eq("File \"dollar_escape_test.ml\", line 20, characters 6-13", 3, 3);

eq("File \"dollar_escape_test.ml\", line 21, characters 6-13", 3, 3);

Mt.from_pair_suites("Dollar_escape_test", suites.contents);

var v = 3;

var u = 3;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.$$ = $$;
exports.v = v;
exports.$$$plus = $$$plus;
exports.u = u;
/*  Not a pure module */
