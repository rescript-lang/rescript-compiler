// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (() => {
        return {
          TAG: "Eq",
          _0: x,
          _1: y
        };
      })
    ],
    tl: suites.contents
  };
}

function f(x, i, file, v) {
  x.case(i);
  x.case__set(i, v);
  x._open(file);
  x.open__(file);
  return x._MAX_LENGTH;
}

function ff(x, i, v) {
  x.make__config = v;
  x.make_config = v;
  x.case__unsafe(i);
  return x._open__(3);
}

let u = {
  "_Content'type": "x"
};

let h = {
  _open: 3,
  _end: 32
};

function hg(x) {
  return x._open + x._end | 0;
}

eq("File \"method_name_test.res\", line 39, characters 12-19", 35, hg(h));

Mt.from_pair_suites("Method_name_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.ff = ff;
exports.u = u;
exports.h = h;
exports.hg = hg;
/*  Not a pure module */
