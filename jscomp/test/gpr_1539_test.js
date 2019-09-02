'use strict';

var Caml_module = require("../../lib/js/caml_module.js");

var Point = Caml_module.init_mod([
      "gpr_1539_test.ml",
      10,
      6
    ], /* constructor */{
      tag: "Module",
      Arg0: [[
          "Function",
          "add"
        ]]
    });

Caml_module.update_mod(/* constructor */{
      tag: "Module",
      Arg0: [[
          "Function",
          "add"
        ]]
    }, Point, {
      add: (function (prim, prim$1) {
          return prim.add(prim$1);
        })
    });

var CRS = /* () */0;

var Layer = /* () */0;

exports.CRS = CRS;
exports.Layer = Layer;
exports.Point = Point;
/* Point Not a pure module */
