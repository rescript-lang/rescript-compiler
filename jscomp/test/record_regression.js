'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_option = require("../../lib/js/caml_option.js");

var f3_y = 3;

var f3 = {
  x: 3,
  y: f3_y,
  z: 2
};

var f4_y = 3;

var f4_yy = Caml_option.some(undefined);

var f4 = {
  x: 3,
  y: f4_y,
  yy: f4_yy,
  z: 2
};

var theseTwoShouldBeIdentical = [
  Caml_option.some(undefined),
  Caml_option.some(undefined)
];

var v = {
  x: 2,
  z: 3
};

var newrecord = Caml_obj.obj_dup(v);

newrecord.y1 = 22;

var h10_y1 = 22;

var h10 = {
  x: 2,
  y1: h10_y1,
  z: 3
};

function h11(v1) {
  return {
          x: v1.x,
          y0: v1.y0,
          y1: 22,
          z: v1.z
        };
}

var f1 = {
  x: 3,
  z: 2
};

var f2 = {
  x: 3,
  y: 3,
  z: 3
};

var v2 = {
  x: 3,
  y: undefined,
  z: 2
};

var h = newrecord;

var v1 = {
  x: 2,
  z: 3
};

var po = {
  aa: 3,
  bb: 4
};

exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.theseTwoShouldBeIdentical = theseTwoShouldBeIdentical;
exports.v2 = v2;
exports.v = v;
exports.h = h;
exports.v1 = v1;
exports.h10 = h10;
exports.h11 = h11;
exports.po = po;
/*  Not a pure module */
