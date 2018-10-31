'use strict';

var Caml_module = require("../../lib/js/caml_module.js");

var Int3 = Caml_module.init_mod([
      "recursive_module.ml",
      14,
      6
    ], [[0]]);

Caml_module.update_mod([[0]], Int3, Int3);

var Int32 = /* () */0;

exports.Int32 = Int32;
exports.Int3 = Int3;
/* Int3 Not a pure module */
