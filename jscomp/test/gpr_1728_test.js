// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Caml_format = require("../../lib/js/caml_format.js");

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
      (() => ({
        TAG: "Eq",
        _0: x,
        _1: y
      }))
    ],
    tl: suites.contents
  };
}

function foo(x) {
  return Caml_format.int_of_string(x) !== 3;
}

function badInlining(obj) {
  let x = obj.field;
  Caml_format.int_of_string(x) !== 3;
}

eq("File \"gpr_1728_test.res\", line 16, characters 3-10", badInlining({
  field: "3"
}), undefined);

eq("File \"gpr_1728_test.res\", line 18, characters 3-10", Caml_format.int_of_string("-13"), -13);

eq("File \"gpr_1728_test.res\", line 19, characters 3-10", Caml_format.int_of_string("+13"), 13);

eq("File \"gpr_1728_test.res\", line 20, characters 3-10", Caml_format.int_of_string("13"), 13);

eq("File \"gpr_1728_test.res\", line 21, characters 3-10", Caml_format.int_of_string("0u32"), 32);

eq("File \"gpr_1728_test.res\", line 22, characters 3-10", Caml_format.int_of_string("-0u32"), -32);

eq("File \"gpr_1728_test.res\", line 23, characters 3-10", Caml_format.int_of_string("+0u32"), 32);

eq("File \"gpr_1728_test.res\", line 24, characters 3-10", Caml_format.int_of_string("+0x32"), 50);

eq("File \"gpr_1728_test.res\", line 25, characters 3-10", Caml_format.int_of_string("-0x32"), -50);

eq("File \"gpr_1728_test.res\", line 26, characters 3-10", Caml_format.int_of_string("0x32"), 50);

Mt.from_pair_suites("Gpr_1728_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.foo = foo;
exports.badInlining = badInlining;
/*  Not a pure module */
