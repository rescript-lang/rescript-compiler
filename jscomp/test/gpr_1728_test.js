'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_format = require("../../lib/js/caml_format.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  Pervasives.incr(test_id);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

function foo(x) {
  return Caml_format.caml_int_of_string(x) !== 3;
}

function badInlining(obj) {
  var x = obj.field;
  Caml_format.caml_int_of_string(x) !== 3;
  return /* () */0;
}

eq("File \"gpr_1728_test.ml\", line 17, characters 6-13", badInlining({
          field: "3"
        }), /* () */0);

eq("File \"gpr_1728_test.ml\", line 19, characters 6-13", Caml_format.caml_int_of_string("-13"), -13);

eq("File \"gpr_1728_test.ml\", line 20, characters 6-13", Caml_format.caml_int_of_string("+13"), 13);

eq("File \"gpr_1728_test.ml\", line 21, characters 6-13", Caml_format.caml_int_of_string("13"), 13);

eq("File \"gpr_1728_test.ml\", line 22, characters 6-13", Caml_format.caml_int_of_string("0u32"), 32);

eq("File \"gpr_1728_test.ml\", line 23, characters 6-13", Caml_format.caml_int_of_string("-0u32"), -32);

eq("File \"gpr_1728_test.ml\", line 24, characters 6-13", Caml_format.caml_int_of_string("+0u32"), 32);

eq("File \"gpr_1728_test.ml\", line 25, characters 6-13", Caml_format.caml_int_of_string("+0x32"), 50);

eq("File \"gpr_1728_test.ml\", line 26, characters 6-13", Caml_format.caml_int_of_string("-0x32"), -50);

eq("File \"gpr_1728_test.ml\", line 27, characters 6-13", Caml_format.caml_int_of_string("0x32"), 50);

Mt.from_pair_suites("Gpr_1728_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.foo = foo;
exports.badInlining = badInlining;
/*  Not a pure module */
