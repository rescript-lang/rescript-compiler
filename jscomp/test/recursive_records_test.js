'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var rec_cell = {};

rec_cell.content = 3;

rec_cell.next = rec_cell;

function f0(x) {
  var rec_cell = {};
  Caml_obj.update_dummy(rec_cell, {
        content: Math.imul(x, x) - 6 | 0,
        next: rec_cell
      });
  return rec_cell;
}

function a0(x) {
  return (x.content + x.next.content | 0) + x.next.next.content | 0;
}

eq("File \"recursive_records_test.ml\", line 29, characters 5-12", a0(rec_cell), 9);

eq("File \"recursive_records_test.ml\", line 30, characters 5-12", a0(f0(3)), 9);

var rec_cell2 = {};

rec_cell2.content = 3;

rec_cell2.next = rec_cell2;

function f2(x) {
  var rec_cell2 = {};
  Caml_obj.update_dummy(rec_cell2, /* Cons */{
        content: Math.imul(x, x) - 6 | 0,
        next: rec_cell2
      });
  return rec_cell2;
}

function hd(x) {
  if (x) {
    return x.content;
  } else {
    return 0;
  }
}

function tl_exn(x) {
  if (x) {
    return x.next;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: /* tuple */[
          "recursive_records_test.ml",
          52,
          11
        ],
        Error: new Error()
      };
}

eq("File \"recursive_records_test.ml\", line 56, characters 6-13", (hd(rec_cell2) + hd(tl_exn(rec_cell2)) | 0) + hd(tl_exn(tl_exn(rec_cell2))) | 0, 9);

var rec_cell2$1 = f2(3);

eq("File \"recursive_records_test.ml\", line 60, characters 5-12", (hd(rec_cell2$1) + hd(tl_exn(rec_cell2$1)) | 0) + hd(tl_exn(tl_exn(rec_cell2$1))) | 0, 9);

var rec_cell3 = {};

rec_cell3._0 = 3;

rec_cell3._1 = rec_cell3;

function f3(x) {
  var rec_cell3 = {};
  Caml_obj.update_dummy(rec_cell3, /* :: */{
        _0: Math.imul(x, x) - 6 | 0,
        _1: rec_cell3
      });
  return rec_cell3;
}

eq("File \"recursive_records_test.ml\", line 74, characters 5-12", (List.hd(rec_cell3) + List.hd(List.tl(rec_cell3)) | 0) + List.hd(List.tl(List.tl(rec_cell3))) | 0, 9);

var rec_cell3$1 = f3(3);

eq("File \"recursive_records_test.ml\", line 77, characters 5-12", (List.hd(rec_cell3$1) + List.hd(List.tl(rec_cell3$1)) | 0) + List.hd(List.tl(List.tl(rec_cell3$1))) | 0, 9);

Mt.from_pair_suites("recursive_records_test.ml", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.rec_cell = rec_cell;
exports.f0 = f0;
exports.a0 = a0;
exports.rec_cell2 = rec_cell2;
exports.f2 = f2;
exports.hd = hd;
exports.tl_exn = tl_exn;
exports.rec_cell3 = rec_cell3;
exports.f3 = f3;
/*  Not a pure module */
