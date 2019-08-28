'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");

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

function step1(param) {
  if (!class_tables[0]) {
    var $$class = CamlinternalOO.create_table(["step2"]);
    var env = CamlinternalOO.new_variable($$class, "");
    var step2 = CamlinternalOO.get_method_label($$class, "step2");
    CamlinternalOO.set_method($$class, step2, (function (self$1) {
            if (!class_tables$1[0]) {
              var $$class = CamlinternalOO.create_table(["step3"]);
              var env = CamlinternalOO.new_variable($$class, "");
              var step3 = CamlinternalOO.get_method_label($$class, "step3");
              CamlinternalOO.set_method($$class, step3, (function (self$2) {
                      return 33;
                    }));
              var env_init = function (env$1) {
                var self = CamlinternalOO.create_object_opt(0, $$class);
                self[env] = env$1;
                return self;
              };
              CamlinternalOO.init_class($$class);
              class_tables$1[0] = env_init;
            }
            return Curry._1(class_tables$1[0], 0);
          }));
    var env_init = function (env$1) {
      var self = CamlinternalOO.create_object_opt(0, $$class);
      self[env] = env$1;
      return self;
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

Mt.from_pair_suites("Gpr_1285_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.step1 = step1;
exports.x = x;
/* x Not a pure module */
