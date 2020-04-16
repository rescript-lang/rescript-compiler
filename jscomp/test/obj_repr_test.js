'use strict';

var CamlinternalOO = require("../../lib/js/camlinternalOO.js");

var $$class = CamlinternalOO.create_table(0);

var ids = CamlinternalOO.new_methods_variables($$class, 0, [
      "x0",
      "x1",
      "x2",
      "x3",
      "x4",
      "x5"
    ]);

var x0 = ids[0];

var x1 = ids[1];

var x2 = ids[2];

var x3 = ids[3];

var x4 = ids[4];

var x5 = ids[5];

function obj_init(env) {
  var self = CamlinternalOO.create_object_opt(undefined, $$class);
  self[x0] = 1;
  self[x1] = 2;
  self[x2] = 3;
  self[x3] = 4;
  self[x4] = 5;
  self[x5] = 6;
  return self;
}

CamlinternalOO.init_class($$class);

var u = obj_init(undefined);

exports.u = u;
/* class Not a pure module */
