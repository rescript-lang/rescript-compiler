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
    _0: /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  tag: /* Eq */0,
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

function create(param) {
  if (!class_tables.key) {
    var $$class = CamlinternalOO.create_table([
          "add",
          "get"
        ]);
    var env = CamlinternalOO.new_variable($$class, "");
    var ids = CamlinternalOO.new_methods_variables($$class, [
          "get",
          "add"
        ], ["data"]);
    var get = ids[0];
    var add = ids[1];
    var data = ids[2];
    CamlinternalOO.set_methods($$class, [
          add,
          (function (self$1, param) {
              self$1[data] = self$1[data] + 1 | 0;
              return self$1;
            }),
          get,
          (function (self$1, param) {
              return self$1[data];
            })
        ]);
    var env_init = function (env$1) {
      var self = CamlinternalOO.create_object_opt(undefined, $$class);
      self[data] = 0;
      self[env] = env$1;
      return self;
    };
    CamlinternalOO.init_class($$class);
    class_tables.key = env_init;
  }
  return Curry._1(class_tables.key, undefined);
}

var cxt1 = create(undefined);

var tmp = Caml_oo_curry.js2(4846113, 1, cxt1, undefined);

var result = Caml_oo_curry.js2(5144726, 2, tmp, undefined);

eq("File \"gpr_2250_test.ml\", line 26, characters 5-12", result, 1);

var cxt2 = create(undefined);

var tmp$1 = Caml_oo_curry.js2(4846113, 3, cxt2, undefined);

var tmp$2 = Caml_oo_curry.js2(4846113, 4, tmp$1, undefined);

var result2 = Caml_oo_curry.js2(5144726, 5, tmp$2, undefined);

eq("File \"gpr_2250_test.ml\", line 37, characters 5-12", result2, 2);

Mt.from_pair_suites("Gpr_2250_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.create = create;
exports.cxt1 = cxt1;
exports.result = result;
exports.cxt2 = cxt2;
exports.result2 = result2;
/* cxt1 Not a pure module */
