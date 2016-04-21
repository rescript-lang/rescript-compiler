// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Curry          = require("../runtime/curry");
var CamlinternalOO = require("../stdlib/camlinternalOO");
var Oo             = require("../stdlib/oo");

function x_init($$class) {
  var x = CamlinternalOO.new_variable($$class, "x");
  return function (_, self, v) {
    var self$1 = CamlinternalOO.create_object_opt(self, $$class);
    self$1[x] = v;
    return self$1;
  };
}

var x = CamlinternalOO.make_class(0, x_init);

var v = Curry._2(x[0], 0, 3);

var u = Oo.copy(v);

exports.x = x;
exports.v = v;
exports.u = u;
/* x Not a pure module */
