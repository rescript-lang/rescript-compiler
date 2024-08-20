// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Caml_obj = require("../../lib/js/caml_obj.js");

let suites = {
  contents: /* [] */0
};

let test_id = {
  contents: 0
};

function eq(loc, x, y) {
  console.log([
    x,
    y
  ]);
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (() => ({
        TAG: "Eq",
        _0: x,
        _1: y
      }))
    ],
    tl: suites.contents
  };
}

function f(x) {
  let y = Caml_obj.obj_dup(x);
  return {
    a0: 1,
    a1: y.a1,
    a2: y.a2,
    a3: y.a3,
    a4: y.a4,
    a5: y.a5
  };
}

eq("File \"update_record_test.res\", line 28, characters 5-12", 1, f({
  a0: 0,
  a1: 0,
  a2: 0,
  a3: 0,
  a4: 0,
  a5: 0
}).a0);

let val0 = {
  "invalid_js_id'": 3,
  x: 2
};

function fff(x) {
  return {
    "invalid_js_id'": x["invalid_js_id'"] + 2 | 0,
    x: x.x
  };
}

let val1 = fff(val0);

eq("File \"update_record_test.res\", line 41, characters 5-12", 3, 3);

eq("File \"update_record_test.res\", line 42, characters 5-12", val1["invalid_js_id'"], 5);

Mt.from_pair_suites("Update_record_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.val0 = val0;
exports.fff = fff;
exports.val1 = val1;
/*  Not a pure module */
