'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

var class_tables = [
  0,
  0,
  0
];

var class_tables$1 = [
  0,
  0,
  0
];

function step1() {
  if (!class_tables[0]) {
    var $$class = CamlinternalOO.create_table(["step2"]);
    var step2 = CamlinternalOO.get_method_label($$class, "step2");
    CamlinternalOO.set_method($$class, step2, (function () {
            if (!class_tables$1[0]) {
              var $$class = CamlinternalOO.create_table(["step3"]);
              var step3 = CamlinternalOO.get_method_label($$class, "step3");
              CamlinternalOO.set_method($$class, step3, (function () {
                      return 33;
                    }));
              var env_init = function () {
                return CamlinternalOO.create_object_opt(0, $$class);
              };
              CamlinternalOO.init_class($$class);
              class_tables$1[0] = env_init;
            }
            return Curry._1(class_tables$1[0], 0);
          }));
    var env_init = function () {
      return CamlinternalOO.create_object_opt(0, $$class);
    };
    CamlinternalOO.init_class($$class);
    class_tables[0] = env_init;
  }
  return Curry._1(class_tables[0], 0);
}

var tmp = step1(/* () */0);

var tmp$1 = Caml_oo_curry.js1(68057958, 1, tmp);

var x = Caml_oo_curry.js1(68057959, 2, tmp$1);

eq("File \"gpr_1285_test.ml\", line 20, characters 5-12", x, 33);

Mt.from_pair_suites("gpr_1285_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.step1 = step1;
exports.x = x;
/* x Not a pure module */
