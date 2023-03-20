'use strict';

var Caml_module = require("../../lib/js/caml_module.js");

function Make(X) {
  var f = function (param) {
    
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
    ], {
      TAG: "Module",
      _0: [[
          {
            TAG: "Module",
            _0: [[
                "Function",
                "f"
              ]]
          },
          "M"
        ]]
    });

function f(param) {
  
}

var M = {
  f: f
};

Caml_module.update_mod({
      TAG: "Module",
      _0: [[
          {
            TAG: "Module",
            _0: [[
                "Function",
                "f"
              ]]
          },
          "M"
        ]]
    }, B, {
      M: M
    });

var A;

exports.Make = Make;
exports.A = A;
exports.B = B;
/* B Not a pure module */
