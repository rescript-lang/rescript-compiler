'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = [/* contents : [] */0];

var test_id = [/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var rec_cell = [];

rec_cell[0] = 3;

rec_cell[1] = rec_cell;

function a0(x) {
  return (x[/* content */0] + x[/* next */1][/* content */0] | 0) + x[/* next */1][/* next */1][/* content */0] | 0;
}

eq("File \"recursive_records_test.ml\", line 22, characters 5-12", a0(rec_cell), 9);

var rec_cell2 = [];

rec_cell2[0] = 3;

rec_cell2[1] = rec_cell2;

function hd(x) {
  if (x) {
    return x[/* content */0];
  } else {
    return 0;
  }
}

function tl_exn(x) {
  if (x) {
    return x[/* next */1];
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "recursive_records_test.ml",
            39,
            11
          ]
        ];
  }
}

eq("File \"recursive_records_test.ml\", line 43, characters 6-13", (hd(rec_cell2) + hd(tl_exn(rec_cell2)) | 0) + hd(tl_exn(tl_exn(rec_cell2))) | 0, 9);

var rec_cell3 = [];

rec_cell3[0] = 3;

rec_cell3[1] = rec_cell3;

eq("File \"recursive_records_test.ml\", line 51, characters 5-12", (List.hd(rec_cell3) + List.hd(List.tl(rec_cell3)) | 0) + List.hd(List.tl(List.tl(rec_cell3))) | 0, 9);

Mt.from_pair_suites("recursive_records_test.ml", suites[/* contents */0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.rec_cell = rec_cell;
exports.a0 = a0;
exports.rec_cell2 = rec_cell2;
exports.hd = hd;
exports.tl_exn = tl_exn;
exports.rec_cell3 = rec_cell3;
/*  Not a pure module */
