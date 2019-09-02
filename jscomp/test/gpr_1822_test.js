'use strict';

var Mt = require("./mt.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");

var suites = /* record */[/* contents */"[]"];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      loc + (" id " + String(test_id[0])),
      (function (param) {
          return /* constructor */{
                  tag: "Eq",
                  Arg0: x,
                  Arg1: y
                };
        })
    ],
    Arg1: suites[0]
  };
  return /* () */0;
}

var myShape = /* constructor */{
  tag: "Circle",
  Arg0: 10
};

var area;

area = /* XXX */myShape.tag === "Circle" ? 100 * 3.14 : Caml_int32.imul(10, myShape.Arg1);

eq("File \"gpr_1822_test.ml\", line 21, characters 6-13", area, 314);

Mt.from_pair_suites("Gpr_1822_test", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.myShape = myShape;
exports.area = area;
/* area Not a pure module */
