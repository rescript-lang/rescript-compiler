'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");

var shared = [
  "m1",
  "m2"
];

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

function a_init($$class) {
  var ids = CamlinternalOO.get_method_labels($$class, [
        "m2",
        "m1"
      ]);
  var m2 = ids[0];
  var m1 = ids[1];
  CamlinternalOO.set_methods($$class, [
        m1,
        (function (self$1) {
            if (!class_tables$1[/* key */0]) {
              var $$class = CamlinternalOO.create_table(["m3"]);
              var env = CamlinternalOO.new_variable($$class, "");
              var m3 = CamlinternalOO.get_method_label($$class, "m3");
              CamlinternalOO.set_method($$class, m3, (function (self$2) {
                      return 3;
                    }));
              var env_init = function (env$1) {
                var self = CamlinternalOO.create_object_opt(undefined, $$class);
                self[env] = env$1;
                return self;
              };
              CamlinternalOO.init_class($$class);
              class_tables$1[/* key */0] = env_init;
            }
            return Curry._1(class_tables$1[/* key */0], undefined);
          }),
        m2,
        (function (self$1) {
            if (!class_tables[/* key */0]) {
              var $$class = CamlinternalOO.create_table(["m4"]);
              var env = CamlinternalOO.new_variable($$class, "");
              var m4 = CamlinternalOO.get_method_label($$class, "m4");
              CamlinternalOO.set_method($$class, m4, (function (self$3) {
                      var env$1 = self$3[env];
                      var tmp = env$1[1];
                      return Curry._1(tmp[0][env$1[0]], tmp);
                    }));
              var env_init = function (env$1) {
                var self = CamlinternalOO.create_object_opt(undefined, $$class);
                self[env] = env$1;
                return self;
              };
              CamlinternalOO.init_class($$class);
              class_tables[/* key */0] = env_init;
            }
            return Curry._1(class_tables[/* key */0], [
                        m1,
                        self$1
                      ]);
          })
      ]);
  return function (env, self) {
    return CamlinternalOO.create_object_opt(self, $$class);
  };
}

var a = CamlinternalOO.make_class(shared, a_init);

function b_init($$class) {
  var a_text = CamlinternalOO.get_method_label($$class, "a_text");
  var inh = CamlinternalOO.inherits($$class, 0, 0, shared, a, true);
  var obj_init = inh[0];
  CamlinternalOO.set_method($$class, a_text, (function (self$4, param) {
          
        }));
  return function (env, self) {
    var self$1 = CamlinternalOO.create_object_opt(self, $$class);
    Curry._1(obj_init, self$1);
    return CamlinternalOO.run_initializers_opt(self, self$1, $$class);
  };
}

var b = CamlinternalOO.make_class([
      "a_text",
      "m1",
      "m2"
    ], b_init);

var tmp = Curry._1(a[0], undefined);

Caml_oo_curry.js1(24357, 1, tmp);

var tmp$1 = Curry._1(b[0], undefined);

var tmp$2 = Caml_oo_curry.js1(24357, 2, tmp$1);

var tmp$3 = Caml_oo_curry.js1(24359, 3, tmp$2);

eq("File \"opr_3576_test.ml\", line 22, characters 6-13", Caml_oo_curry.js1(24358, 4, tmp$3), 3);

Mt.from_pair_suites("opr_3576_test.ml", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.a = a;
exports.b = b;
/* a Not a pure module */
