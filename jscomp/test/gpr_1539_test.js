'use strict';

var Block = require("../../lib/js/block.js");
var Caml_module = require("../../lib/js/caml_module.js");

var Point = Caml_module.init_mod(/* tuple */[
      "gpr_1539_test.ml",
      10,
      6
    ], /* Module */Block.__(0, [/* array */[/* tuple */[
            /* Function */0,
            "add"
          ]]]));

Caml_module.update_mod(/* Module */Block.__(0, [/* array */[/* tuple */[
            /* Function */0,
            "add"
          ]]]), Point, {
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
