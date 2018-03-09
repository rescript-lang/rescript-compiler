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
      loc + (" id " + String(test_id[0])),
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

function create() {
  if (!class_tables[0]) {
    var $$class = CamlinternalOO.create_table([
          "add",
          "get"
        ]);
    var ids = CamlinternalOO.new_methods_variables($$class, [
          "get",
          "add"
        ], ["data"]);
    var get = ids[0];
    var add = ids[1];
    var data = ids[2];
    CamlinternalOO.set_methods($$class, /* array */[
          add,
          (function (self$1, _) {
              self$1[data] = self$1[data] + 1 | 0;
              return self$1;
            }),
          get,
          (function (self$1, _) {
              return self$1[data];
            })
        ]);
    var env_init = function () {
      var self = CamlinternalOO.create_object_opt(0, $$class);
      self[data] = 0;
      return self;
    };
    CamlinternalOO.init_class($$class);
    class_tables[0] = env_init;
  }
  return Curry._1(class_tables[0], 0);
}

var cxt1 = create(/* () */0);

var tmp = Caml_oo_curry.js2(4846113, 1, cxt1, /* () */0);

var result = Caml_oo_curry.js2(5144726, 2, tmp, /* () */0);

eq("File \"gpr_2250_test.ml\", line 26, characters 5-12", result, 1);

var cxt2 = create(/* () */0);

var tmp$1 = Caml_oo_curry.js2(4846113, 3, cxt2, /* () */0);

var tmp$2 = Caml_oo_curry.js2(4846113, 4, tmp$1, /* () */0);

var result2 = Caml_oo_curry.js2(5144726, 5, tmp$2, /* () */0);

eq("File \"gpr_2250_test.ml\", line 37, characters 5-12", result2, 2);

Mt.from_pair_suites("gpr_2250_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.create = create;
exports.cxt1 = cxt1;
exports.result = result;
exports.cxt2 = cxt2;
exports.result2 = result2;
/* cxt1 Not a pure module */
