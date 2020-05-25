'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

var class_tables = /* Cons */{
  key: undefined,
  data: undefined,
  next: undefined
};

var class_tables$1 = /* Cons */{
  key: undefined,
  data: undefined,
  next: undefined
};

function step1(param) {
  if (!class_tables.key) {
    var $$class = CamlinternalOO.create_table(["step2"]);
    var env = CamlinternalOO.new_variable($$class, "");
    var step2 = CamlinternalOO.get_method_label($$class, "step2");
    CamlinternalOO.set_method($$class, step2, (function (self$1) {
            if (!class_tables$1.key) {
              var $$class = CamlinternalOO.create_table(["step3"]);
              var env = CamlinternalOO.new_variable($$class, "");
              var step3 = CamlinternalOO.get_method_label($$class, "step3");
              CamlinternalOO.set_method($$class, step3, (function (self$2) {
                      return 33;
                    }));
              var env_init = function (env$1) {
                var self = CamlinternalOO.create_object_opt(undefined, $$class);
                self[env] = env$1;
                return self;
              };
              CamlinternalOO.init_class($$class);
              class_tables$1.key = env_init;
            }
            return Curry._1(class_tables$1.key, undefined);
          }));
    var env_init = function (env$1) {
      var self = CamlinternalOO.create_object_opt(undefined, $$class);
      self[env] = env$1;
      return self;
    };
    CamlinternalOO.init_class($$class);
    class_tables.key = env_init;
  }
  return Curry._1(class_tables.key, undefined);
}

var tmp = step1(undefined);

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
