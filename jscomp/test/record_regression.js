'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_option = require("../../lib/js/caml_option.js");

var f1 = {
  x: 3,
  z: 2
};

var newrecord = Caml_obj.obj_dup(f1);

newrecord.y = 3;

var newrecord$1 = Caml_obj.obj_dup(newrecord);

newrecord$1.yy = Caml_option.some(undefined);

var theseTwoShouldBeIdentical = [
  newrecord$1.yy,
  Caml_option.some(undefined)
];

var v = {
  x: 2,
  z: 3
};

var newrecord$2 = Caml_obj.obj_dup(v);

newrecord$2.y1 = 22;

var v1 = {
  x: 2,
  z: 3
};

var newrecord$3 = Caml_obj.obj_dup(v1);

newrecord$3.y1 = 22;

function h11(v1) {
  var newrecord = Caml_obj.obj_dup(v1);
  newrecord.y1 = 22;
  return newrecord;
}

var po = {
  aa: 3,
  bb: 4
};

var newrecord$4 = Caml_obj.obj_dup(po);

newrecord$4.aa = undefined;

function setAA(ao) {
  return {
          aa: ao
        };
}

var ir0 = {
  TAG: /* V0 */0,
  x0: "v0",
  x3: 3
};

var ir1 = {
  TAG: /* V0 */0,
  x0: "v0",
  x1: "v1",
  x3: 3
};

var ir2 = {
  TAG: /* V0 */0,
  x0: "v0",
  x1: "v1",
  x2: 2,
  x3: 3
};

var pm0;

pm0 = ir0.TAG === /* V0 */0 ? [
    "v0",
    3
  ] : [
    "v0",
    undefined
  ];

var pm1;

pm1 = ir1.TAG === /* V0 */0 ? [
    "v0",
    "v1",
    3
  ] : [
    "v0",
    undefined,
    "v1"
  ];

var pm2;

pm2 = ir2.TAG === /* V0 */0 ? [
    "v0",
    "v1",
    2,
    3
  ] : [
    "v0",
    undefined,
    undefined,
    "v1"
  ];

var pm3;

if (ir2.TAG === /* V0 */0) {
  var x2 = 2;
  var x1 = "v1";
  var x0 = "v0";
  pm3 = Caml_obj.equal(x1, "x1") ? [
      x0,
      "x1!",
      x2,
      3
    ] : (
      x1 !== undefined ? [
          x0,
          x1,
          x2,
          3
        ] : [
          x0,
          "not existed",
          x2,
          3
        ]
    );
} else {
  pm3 = [
    "v0",
    "not existed",
    undefined,
    "v1"
  ];
}

var f2 = {
  x: 3,
  y: 3,
  z: 3
};

var f3 = newrecord;

var f4 = newrecord$1;

var v2 = {
  x: 3,
  y: undefined,
  z: 2
};

var h = newrecord$2;

var h10 = newrecord$3;

var foo1 = /* Foo */{
  name: "foo"
};

var foo2 = /* Foo */{
  name: "foo",
  age: 3
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
exports.setAA = setAA;
exports.foo1 = foo1;
exports.foo2 = foo2;
exports.ir0 = ir0;
exports.ir1 = ir1;
exports.ir2 = ir2;
exports.pm0 = pm0;
exports.pm1 = pm1;
exports.pm2 = pm2;
exports.pm3 = pm3;
/*  Not a pure module */
