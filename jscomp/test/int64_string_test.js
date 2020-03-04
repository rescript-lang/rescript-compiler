'use strict';

var Mt = require("./mt.js");
var Int64 = require("../../lib/js/int64.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Caml_format = require("../../lib/js/caml_format.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var v = Caml_format.caml_int64_format("%d", /* int64 */[
      /* hi */0,
      /* lo */333
    ]);

function f(a, b) {
  return eq("File \"int64_string_test.ml\", line 10, characters 5-12", Caml_format.caml_int64_format("%d", a), b);
}

f(/* int64 */[
      /* hi */-1,
      /* lo */4294967263
    ], "-33");

f(/* int64 */[
      /* hi */0,
      /* lo */33
    ], "33");

f(Int64.min_int, "-9223372036854775808");

f(Caml_int64.add(Int64.min_int, /* int64 */[
          /* hi */0,
          /* lo */100
        ]), "-9223372036854775708");

for(var i = 0; i <= 8; ++i){
  eq("File \"int64_string_test.ml\", line 20, characters 5-12", Caml_format.caml_int64_format("%d", Caml_int64.add(Int64.min_int, Caml_int64.of_int32(i))), "-922337203685477580" + String(8 - i | 0));
}

for(var i$1 = 0; i$1 <= 8; ++i$1){
  eq("File \"int64_string_test.ml\", line 24, characters 5-12", Caml_format.caml_int64_format("%d", Caml_int64.add(Int64.min_int, Caml_int64.of_int32(100 + i$1 | 0))), "-922337203685477570" + String(8 - i$1 | 0));
}

Mt.from_pair_suites("File \"int64_string_test.ml\", line 28, characters 23-30", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.f = f;
/* v Not a pure module */
