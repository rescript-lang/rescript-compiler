'use strict';

var Caml_module = require("../../lib/js/caml_module.js");

function Make(X) {
  var f = function (param) {
    return /* () */0;
  };
  var M = {
    f: f
  };
  return {
          M: M
        };
}

var B = Caml_module.init_mod([
      "recursive_unbound_module_test.ml",
      18,
      0
    ], [[[
          [[[
                0,
                "f"
              ]]],
          "M"
        ]]]);

function f(param) {
  return /* () */0;
}

var M = {
  f: f
};

Caml_module.update_mod([[[
          [[[
                0,
                "f"
              ]]],
          "M"
        ]]], B, {
      M: M
    });

var A = /* () */0;

exports.Make = Make;
exports.A = A;
exports.B = B;
/* B Not a pure module */
