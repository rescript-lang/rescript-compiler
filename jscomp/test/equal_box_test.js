'use strict';

var Mt = require("./mt.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");

var aa = Caml_obj.equal_null;

var bb = Caml_obj.equal_undefined;

var cc = Caml_obj.equal_nullable;

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  Mt.eq_suites(test_id, suites, loc, x, y);
}

function b(loc, x) {
  Mt.bool_suites(test_id, suites, loc, x);
}

function f(param) {
  
}

function shouldBeNull(param) {
  return null;
}

b("File \"equal_box_test.res\", line 18, characters 4-11", 3 !== null);

b("File \"equal_box_test.res\", line 19, characters 4-11", undefined !== null);

b("File \"equal_box_test.res\", line 20, characters 4-11", "3" !== null);

b("File \"equal_box_test.res\", line 21, characters 4-11", /* '3' */51 !== null);

b("File \"equal_box_test.res\", line 22, characters 4-11", !Caml_int64.equal_null(Caml_int64.zero, null));

b("File \"equal_box_test.res\", line 23, characters 4-11", 0 !== null);

b("File \"equal_box_test.res\", line 24, characters 4-11", true);

b("File \"equal_box_test.res\", line 25, characters 4-11", !Caml_obj.equal_null(undefined, null));

b("File \"equal_box_test.res\", line 26, characters 4-11", Caml_obj.equal_null(null, null));

b("File \"equal_box_test.res\", line 27, characters 4-11", true);

b("File \"equal_box_test.res\", line 28, characters 4-11", true);

b("File \"equal_box_test.res\", line 29, characters 4-11", !Caml_obj.equal_null(3, undefined));

var v = null;

b("File \"equal_box_test.res\", line 34, characters 4-11", 3 !== v);

b("File \"equal_box_test.res\", line 35, characters 4-11", undefined !== v);

b("File \"equal_box_test.res\", line 36, characters 4-11", "3" !== v);

b("File \"equal_box_test.res\", line 37, characters 4-11", /* '3' */51 !== v);

b("File \"equal_box_test.res\", line 38, characters 4-11", !Caml_int64.equal_nullable(Caml_int64.zero, v));

b("File \"equal_box_test.res\", line 39, characters 4-11", 0 !== v);

b("File \"equal_box_test.res\", line 40, characters 4-11", 0 !== v);

b("File \"equal_box_test.res\", line 41, characters 4-11", !Caml_obj.equal_nullable(undefined, v));

b("File \"equal_box_test.res\", line 42, characters 4-11", Caml_obj.equal_nullable(null, v));

b("File \"equal_box_test.res\", line 43, characters 4-11", true);

b("File \"equal_box_test.res\", line 44, characters 4-11", true);

b("File \"equal_box_test.res\", line 45, characters 4-11", !Caml_obj.equal_nullable(3, undefined));

b("File \"equal_box_test.res\", line 51, characters 4-11", 3 !== undefined);

b("File \"equal_box_test.res\", line 52, characters 4-11", true);

b("File \"equal_box_test.res\", line 53, characters 4-11", "3" !== undefined);

b("File \"equal_box_test.res\", line 54, characters 4-11", /* '3' */51 !== undefined);

b("File \"equal_box_test.res\", line 55, characters 4-11", !Caml_int64.equal_undefined(Caml_int64.zero, undefined));

b("File \"equal_box_test.res\", line 56, characters 4-11", 0 !== undefined);

b("File \"equal_box_test.res\", line 57, characters 4-11", true);

b("File \"equal_box_test.res\", line 58, characters 4-11", Caml_obj.equal_undefined(undefined, undefined));

b("File \"equal_box_test.res\", line 62, characters 4-11", !Caml_obj.equal_undefined(null, undefined));

b("File \"equal_box_test.res\", line 63, characters 4-11", true);

b("File \"equal_box_test.res\", line 64, characters 4-11", true);

b("File \"equal_box_test.res\", line 65, characters 4-11", !Caml_obj.equal_undefined(3, undefined));

Mt.from_pair_suites("File \"equal_box_test.res\", line 68, characters 20-27", suites.contents);

exports.aa = aa;
exports.bb = bb;
exports.cc = cc;
exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.b = b;
exports.f = f;
exports.shouldBeNull = shouldBeNull;
/*  Not a pure module */
