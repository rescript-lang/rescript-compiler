'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function f0(x) {
  var newrecord = Caml_array.caml_array_dup(x);
  newrecord[/* x0 */0] = 1;
  return newrecord;
}

var v1 = /* A0 */[
  /* x0 */9,
  /* x1 */9,
  /* x2 */9,
  /* x3 */9,
  /* x4 */9,
  /* x5 */9,
  /* x6 */9,
  /* x7 */9,
  /* x8 */9,
  /* x9 */9,
  /* x10 */9,
  /* x11 */9,
  /* x12 */9,
  /* x13 */9,
  /* x14 */9,
  /* x15 */9,
  /* x16 */9,
  /* x17 */9,
  /* x18 */9,
  /* x19 */9,
  /* x20 */9,
  /* x21 */9,
  /* x22 */9
];

function get_x0(x) {
  if (x) {
    return x[/* x0 */0];
  }
  
}

function f1(x) {
  if (x) {
    var newrecord = Caml_array.caml_array_dup(x);
    newrecord[/* x0 */0] = 1;
    return newrecord;
  } else {
    return /* A1 */0;
  }
}

eq("File \"large_record_duplication_test.ml\", line 129, characters 6-13", get_x0(f1(v1)), 1);

var v2 = /* A0 */Block.__(0, [
    /* x0 */9,
    /* x1 */9,
    /* x2 */9,
    /* x3 */9,
    /* x4 */9,
    /* x5 */9,
    /* x6 */9,
    /* x7 */9,
    /* x8 */9,
    /* x9 */9,
    /* x10 */9,
    /* x11 */9,
    /* x12 */9,
    /* x13 */9,
    /* x14 */9,
    /* x15 */9,
    /* x16 */9,
    /* x17 */9,
    /* x18 */9,
    /* x19 */9,
    /* x20 */9,
    /* x21 */9,
    /* x22 */9
  ]);

function get_x0$1(x) {
  if (x.tag) {
    return undefined;
  } else {
    return x[/* x0 */0];
  }
}

function f2(x) {
  if (x.tag) {
    return x;
  } else {
    var newrecord = Caml_obj.caml_obj_dup(x);
    newrecord[/* x0 */0] = 1;
    return newrecord;
  }
}

eq("File \"large_record_duplication_test.ml\", line 194, characters 6-13", get_x0$1(f2(v2)), 1);

var A0 = Caml_exceptions.create("Large_record_duplication_test.A0");

function f3(x) {
  if (x[0] === A0) {
    var newrecord = Caml_array.caml_array_dup(x);
    newrecord[/* x0 */1] = 1;
    return newrecord;
  } else {
    return x;
  }
}

function get_x0$2(x) {
  if (x[0] === A0) {
    return x[/* x0 */1];
  }
  
}

var v3 = [
  A0,
  /* x0 */9,
  /* x1 */9,
  /* x2 */9,
  /* x3 */9,
  /* x4 */9,
  /* x5 */9,
  /* x6 */9,
  /* x7 */9,
  /* x8 */9,
  /* x9 */9,
  /* x10 */9,
  /* x11 */9,
  /* x12 */9,
  /* x13 */9,
  /* x14 */9,
  /* x15 */9,
  /* x16 */9,
  /* x17 */9,
  /* x18 */9,
  /* x19 */9,
  /* x20 */9,
  /* x21 */9,
  /* x22 */9
];

eq("File \"large_record_duplication_test.ml\", line 260, characters 6-13", get_x0$2(f3(v3)), 1);

eq("File \"large_record_duplication_test.ml\", line 261, characters 6-13", get_x0$2(v3), 9);

eq("File \"large_record_duplication_test.ml\", line 262, characters 6-13", get_x0$2(Caml_builtin_exceptions.not_found), undefined);

Mt.from_pair_suites("Large_record_duplication_test", suites[0]);

var v0 = /* record */[
  /* x0 */9,
  /* x1 */9,
  /* x2 */9,
  /* x3 */9,
  /* x4 */9,
  /* x5 */9,
  /* x6 */9,
  /* x7 */9,
  /* x8 */9,
  /* x9 */9,
  /* x10 */9,
  /* x11 */9,
  /* x12 */9,
  /* x13 */9,
  /* x14 */9,
  /* x15 */9,
  /* x16 */9,
  /* x17 */9,
  /* x18 */9,
  /* x19 */9,
  /* x20 */9,
  /* x21 */9,
  /* x22 */9
];

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v0 = v0;
exports.f0 = f0;
exports.v1 = v1;
exports.f1 = f1;
exports.v2 = v2;
exports.f2 = f2;
exports.A0 = A0;
exports.f3 = f3;
exports.get_x0 = get_x0$2;
exports.v3 = v3;
/*  Not a pure module */
