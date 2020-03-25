'use strict';

var List = require("../../lib/js/list.js");
var Curry = require("../../lib/js/curry.js");

function h0(x) {
  return x();
}

function h00(x) {
  return x();
}

function h1(x, y) {
  return x(y);
}

function h10(x) {
  return x(3);
}

function h30(x) {
  return (function (a) {
      return x(3, 3, a);
    });
}

function h33(x) {
  return x(1, 2, 3);
}

function h34(x) {
  return Curry._1(x(1, 2, 3), 4);
}

function ocaml_run(b, c) {
  return (function (x, y, z) {
              return (x + y | 0) + z | 0;
            })(1, b, c);
}

function a0() {
  console.log("hi");
  
}

function a1(param) {
  return (function (x) {
      return x;
    });
}

function a2(x, y) {
  return x + y | 0;
}

function a3(x, y, z) {
  return (x + y | 0) + z | 0;
}

function xx(param) {
  return (function (param) {
      console.log(3);
      
    });
}

var test_as = List.map;

exports.h0 = h0;
exports.h00 = h00;
exports.h1 = h1;
exports.h10 = h10;
exports.h30 = h30;
exports.h33 = h33;
exports.h34 = h34;
exports.ocaml_run = ocaml_run;
exports.a0 = a0;
exports.a1 = a1;
exports.a2 = a2;
exports.a3 = a3;
exports.test_as = test_as;
exports.xx = xx;
/* No side effect */
