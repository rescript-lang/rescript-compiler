'use strict';

var Mt = require("./mt.js");
var Caml_array = require("../../lib/js/caml_array.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var v = Caml_array.caml_make_float_vect(5);

for(var i = 0; i <= 4; ++i){
  v[i] = 0;
}

Caml_array.caml_array_set(v, 2, 15.5);

eq("File \"floatarray_test.ml\", line 17, characters 5-12", /* tuple */[
      v.length,
      v[2],
      Caml_array.caml_array_get(v, 1)
    ], /* tuple */[
      5,
      15.5,
      0
    ]);

Mt.from_pair_suites("Floatarray_test", suites[0]);

var K = 0;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.K = K;
/* v Not a pure module */
