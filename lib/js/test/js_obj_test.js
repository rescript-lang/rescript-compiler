// GENERATED CODE BY BUCKLESCRIPT VERSION 0.3 , PLEASE EDIT WITH CARE
'use strict';

var Mt             = require("./mt");
var Block          = require("../block");
var Curry          = require("../curry");
var CamlinternalOO = require("../camlinternalOO");

function f(u) {
  return Curry.js2(5740587, 1, u, 32);
}

function f_js(u) {
  return u.say(32);
}

var class_tables = [
  0,
  0,
  0
];

var suites_000 = /* tuple */[
  "caml_obj",
  function () {
    if (!class_tables[0]) {
      var $$class = CamlinternalOO.create_table(["say"]);
      var say = CamlinternalOO.get_method_label($$class, "say");
      CamlinternalOO.set_method($$class, say, function (_, x) {
            return 1 + x | 0;
          });
      var env_init = function () {
        return CamlinternalOO.create_object_opt(0, $$class);
      };
      CamlinternalOO.init_class($$class);
      class_tables[0] = env_init;
    }
    return /* Eq */Block.__(0, [
              33,
              f(Curry._1(class_tables[0], 0))
            ]);
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "js_obj",
    function () {
      return /* Eq */Block.__(0, [
                34,
                f_js({
                      "say": function (x) {
                        return x + 2 | 0;
                      }
                    })
              ]);
    }
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_obj_test.ml", suites);

exports.f      = f;
exports.f_js   = f_js;
exports.suites = suites;
/*  Not a pure module */
