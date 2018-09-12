'use strict';

var Oo = require("../../lib/js/oo.js");
var Curry = require("../../lib/js/curry.js");
var CamlinternalOO = require("../../lib/js/camlinternalOO.js");

function x_init($$class) {
  var v = CamlinternalOO.new_variable($$class, "");
  var x = CamlinternalOO.new_variable($$class, "x");
  return (function (_, self, v$1) {
      var self$1 = CamlinternalOO.create_object_opt(self, $$class);
      self$1[v] = v$1;
      self$1[x] = v$1;
      return self$1;
    });
}

var x = CamlinternalOO.make_class(0, x_init);

var v = Curry._2(x[0], 0, 3);

var u = Oo.copy(v);

exports.x = x;
exports.v = v;
exports.u = u;
/* x Not a pure module */
