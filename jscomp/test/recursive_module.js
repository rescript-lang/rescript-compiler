'use strict';

var Caml_module = require("../../lib/js/caml_module.js");

var Int32 = Caml_module.init_mod([
      "recursive_module.ml",
      9,
      6
    ], [[
        0,
        0,
        0,
        0,
        0
      ]]);

Caml_module.update_mod([[
        0,
        0,
        0,
        0,
        0
      ]], Int32, Int32);

var Int3 = Caml_module.init_mod([
      "recursive_module.ml",
      14,
      6
    ], [[0]]);

Caml_module.update_mod([[0]], Int3, Int3);

exports.Int32 = Int32;
exports.Int3 = Int3;
/* Int32 Not a pure module */
