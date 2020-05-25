'use strict';

var Caml_module = require("../../lib/js/caml_module.js");

var Point = Caml_module.init_mod([
      "gpr_1539_test.ml",
      10,
      6
    ], {
      tag: /* Module */0,
      _0: [[
          /* Function */0,
          "add"
        ]]
    });

Caml_module.update_mod({
      tag: /* Module */0,
      _0: [[
          /* Function */0,
          "add"
        ]]
    }, Point, {
      add: (function (prim, prim$1) {
          return prim.add(prim$1);
        })
    });

var CRS;

var Layer;

exports.CRS = CRS;
exports.Layer = Layer;
exports.Point = Point;
/* Point Not a pure module */
