'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");

var f1 = {
  x: 3,
  z: 2
};

var newrecord = Caml_obj.obj_dup(f1);

newrecord.y = 3;

var v = {
  x: 2,
  z: 3
};

var newrecord$1 = Caml_obj.obj_dup(v);

newrecord$1.y1 = 22;

var v1 = {
  x: 2,
  z: 3
};

var newrecord$2 = Caml_obj.obj_dup(v1);

newrecord$2.y1 = 22;

function h11(v1) {
  var newrecord = Caml_obj.obj_dup(v1);
  newrecord.y1 = 22;
  return newrecord;
}

var f2 = {
  x: 3,
  y: 3,
  z: 3
};

var f3 = newrecord;

var v2 = {
  x: 3,
  y: undefined,
  z: 2
};

var h = newrecord$1;

var h10 = newrecord$2;

exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.v2 = v2;
exports.v = v;
exports.h = h;
exports.v1 = v1;
exports.h10 = h10;
exports.h11 = h11;
/*  Not a pure module */
