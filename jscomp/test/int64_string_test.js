'use strict';

var Mt = require("./mt.js");
var Int64 = require("../../lib/js/int64.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var v = Caml_int64.to_string(Int64.max_int);

eq("File \"int64_string_test.ml\", line 9, characters 6-13", v, "9223372036854775807");

function f(a, b) {
  return eq("File \"int64_string_test.ml\", line 11, characters 5-12", Caml_int64.to_string(a), b);
}

var hh = Caml_int64.add(Int64.min_int, /* int64 */[
      /* hi */0,
      /* lo */100
    ]);

eq("File \"int64_string_test.ml\", line 15, characters 6-13", hh, /* int64 */[
      /* hi */-2147483648,
      /* lo */100
    ]);

f(/* int64 */[
      /* hi */-1,
      /* lo */4294967263
    ], "-33");

f(/* int64 */[
      /* hi */0,
      /* lo */33
    ], "33");

f(Int64.min_int, "-9223372036854775808");

f(hh, "-9223372036854775708");

f(/* int64 */[
      /* hi */232830,
      /* lo */2764472320
    ], "1000000000000000");

for(var i = 0; i <= 8; ++i){
  eq("File \"int64_string_test.ml\", line 25, characters 5-12", Caml_int64.to_string(Caml_int64.add(Int64.min_int, Caml_int64.of_int32(i))), "-922337203685477580" + String(8 - i | 0));
}

for(var i$1 = 0; i$1 <= 8; ++i$1){
  eq("File \"int64_string_test.ml\", line 29, characters 5-12", Caml_int64.to_string(Caml_int64.add(Int64.min_int, Caml_int64.of_int32(100 + i$1 | 0))), "-922337203685477570" + String(8 - i$1 | 0));
}

for(var i$2 = 0; i$2 <= 8; ++i$2){
  eq("File \"int64_string_test.ml\", line 33, characters 5-12", Caml_int64.to_string(Caml_int64.add(Int64.min_int, Caml_int64.of_int32(1000000 + i$2 | 0))), "-922337203685377580" + String(8 - i$2 | 0));
}

var u = /* int64 */[
  /* hi */2097151,
  /* lo */4294957295
];

for(var i$3 = 0; i$3 <= 6; ++i$3){
  eq("File \"int64_string_test.ml\", line 42, characters 5-12", Caml_int64.to_string(Caml_int64.add(u, Caml_int64.of_int32(Caml_int32.imul(i$3, 10000)))), "90071992547" + (String(3 + i$3 | 0) + "0991"));
}

var v$1 = /* int64 */[
  /* hi */-2097153,
  /* lo */4294917297
];

for(var i$4 = 0; i$4 <= 9; ++i$4){
  eq("File \"int64_string_test.ml\", line 49, characters 5-12", Caml_int64.to_string(Caml_int64.add(v$1, Caml_int64.of_int32(Caml_int32.imul(i$4, 10000)))), "-90071992547" + (String(9 - i$4 | 0) + "0991"));
}

eq("File \"int64_string_test.ml\", line 52, characters 6-13", Caml_int64.to_string(/* int64 */[
          /* hi */2097151,
          /* lo */4294967295
        ]), "9007199254740991");

eq("File \"int64_string_test.ml\", line 53, characters 6-13", Caml_int64.to_string(/* int64 */[
          /* hi */-2097152,
          /* lo */1
        ]), "-9007199254740991");

eq("File \"int64_string_test.ml\", line 54, characters 6-13", Caml_int64.to_string(/* int64 */[
          /* hi */-1,
          /* lo */4294967063
        ]), "-233");

Mt.from_pair_suites("File \"int64_string_test.ml\", line 57, characters 23-30", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.hh = hh;
exports.u = u;
exports.v = v$1;
/* v Not a pure module */
