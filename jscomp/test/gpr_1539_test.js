'use strict';

var Caml_module = require("../../lib/js/caml_module.js");

var Point = Caml_module.init_mod([
      "gpr_1539_test.ml",
      10,
      6
    ], [[0]]);

Caml_module.update_mod([[0]], Point, /* module */[(function (prim, prim$1) {
          return prim.add(prim$1);
        })]);

var CRS = /* () */0;

var Layer = /* () */0;

exports.CRS = CRS;
exports.Layer = Layer;
exports.Point = Point;
/* Point Not a pure module */
