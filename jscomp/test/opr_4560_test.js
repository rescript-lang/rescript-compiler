'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");

var shared = ["b"];

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var class_tables = /* Cons */[
  /* key */undefined,
  /* data */undefined,
  /* next */undefined
];

var class_tables$1 = /* Cons */[
  /* key */undefined,
  /* data */undefined,
  /* next */undefined
];

function c1_init($$class) {
  var b = CamlinternalOO.get_method_label($$class, "b");
  CamlinternalOO.set_method($$class, b, (function (self$1) {
          if (!class_tables[/* key */0]) {
            var $$class = CamlinternalOO.create_table([
                  "c",
                  "h"
                ]);
            var env = CamlinternalOO.new_variable($$class, "");
            var ids = CamlinternalOO.get_method_labels($$class, [
                  "h",
                  "c"
                ]);
            var h = ids[0];
            var c = ids[1];
            CamlinternalOO.set_methods($$class, [
                  c,
                  (function (self$2) {
                      var env$1 = self$2[env];
                      if (!class_tables$1[/* key */0]) {
                        var $$class = CamlinternalOO.create_table(["d"]);
                        var env$2 = CamlinternalOO.new_variable($$class, "");
                        var d = CamlinternalOO.get_method_label($$class, "d");
                        CamlinternalOO.set_method($$class, d, (function (self$3) {
                                var env$3 = self$3[env$2];
                                var tmp = env$3[1];
                                return Curry._1(tmp[0][env$3[0]], tmp);
                              }));
                        var env_init = function (env$3) {
                          var self = CamlinternalOO.create_object_opt(undefined, $$class);
                          self[env$2] = env$3;
                          return self;
                        };
                        CamlinternalOO.init_class($$class);
                        class_tables$1[/* key */0] = env_init;
                      }
                      return Curry._1(class_tables$1[/* key */0], [
                                  env$1[0],
                                  env$1[1]
                                ]);
                    }),
                  h,
                  (function (self$2) {
                      return 33;
                    })
                ]);
            var env_init = function (env$1) {
              var self = CamlinternalOO.create_object_opt(undefined, $$class);
              self[env] = env$1;
              return self;
            };
            CamlinternalOO.init_class($$class);
            class_tables[/* key */0] = env_init;
          }
          return Curry._1(class_tables[/* key */0], [
                      b,
                      self$1
                    ]);
        }));
  return function (env, self) {
    return CamlinternalOO.create_object_opt(self, $$class);
  };
}

var c1 = CamlinternalOO.make_class(shared, c1_init);

function c2_init($$class) {
  var a = CamlinternalOO.get_method_label($$class, "a");
  var inh = CamlinternalOO.inherits($$class, 0, 0, shared, c1, true);
  var obj_init = inh[0];
  CamlinternalOO.set_method($$class, a, (function (self$4) {
          
        }));
  return function (env, self) {
    var self$1 = CamlinternalOO.create_object_opt(self, $$class);
    Curry._1(obj_init, self$1);
    return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
  };
}

var c2 = CamlinternalOO.make_class([
      "a",
      "b"
    ], c2_init);

var tmp = Curry._1(c2[0], undefined);

Caml_oo_curry.js1(98, 1, tmp);

var tmp$1 = Curry._1(c1[0], undefined);

var tmp$2 = Caml_oo_curry.js1(98, 2, tmp$1);

var tmp$3 = Caml_oo_curry.js1(99, 3, tmp$2);

var tmp$4 = Caml_oo_curry.js1(100, 4, tmp$3);

var e = Caml_oo_curry.js1(104, 5, tmp$4);

eq("File \"opr_4560_test.ml\", line 26, characters 3-10", e, 33);

Mt.from_pair_suites("opr_4560_test.ml", suites.contents);

var magic = 33;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.magic = magic;
exports.c1 = c1;
exports.c2 = c2;
exports.e = e;
/* c1 Not a pure module */
