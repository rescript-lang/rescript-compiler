'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Hashtbl = require("../../lib/js/hashtbl.js");
var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var shared = ["calc"];

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

function fib_init($$class) {
  var calc = CamlinternalOO.get_method_label($$class, "calc");
  CamlinternalOO.set_method($$class, calc, (function (self$1, x) {
          if (x === 0 || x === 1) {
            return 1;
          } else {
            return Curry._2(self$1[0][calc], self$1, x - 1 | 0) + Curry._2(self$1[0][calc], self$1, x - 2 | 0) | 0;
          }
        }));
  return (function (_, self) {
      return CamlinternalOO.create_object_opt(self, $$class);
    });
}

var fib = CamlinternalOO.make_class(shared, fib_init);

function memo_fib_init($$class) {
  var ids = CamlinternalOO.new_methods_variables($$class, shared, ["cache"]);
  var calc = ids[0];
  var cache = ids[1];
  var inh = CamlinternalOO.inherits($$class, 0, 0, shared, fib, 1);
  var obj_init = inh[0];
  var calc$1 = inh[1];
  CamlinternalOO.set_method($$class, calc, (function (self$2, x) {
          try {
            return Hashtbl.find(self$2[cache], x);
          }
          catch (exn){
            if (exn === Caml_builtin_exceptions.not_found) {
              var v = Curry._2(calc$1, self$2, x);
              Hashtbl.add(self$2[cache], x, v);
              return v;
            } else {
              throw exn;
            }
          }
        }));
  return (function (_, self) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      self$1[cache] = Hashtbl.create(/* None */0, 31);
      Curry._1(obj_init, self$1);
      return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
    });
}

var memo_fib = CamlinternalOO.make_class(shared, memo_fib_init);

var tmp = Curry._1(memo_fib[0], 0);

eq("File \"class_fib_open_recursion_test.ml\", line 33, characters 5-12", Caml_oo_curry.js2(-1044768619, 1, tmp, 40), 165580141);

Mt.from_pair_suites("class_fib_open_recursion_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.fib = fib;
exports.memo_fib = memo_fib;
/* fib Not a pure module */
