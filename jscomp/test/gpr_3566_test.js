'use strict';

var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_option = require("../../lib/js/caml_option.js");

function eq_A(x, y) {
  if (x.tag || y.tag) {
    return false;
  } else {
    return x[0] === y[0];
  }
}

function Test($star) {
  console.log("no inline");
  var u = /* A */Block.__(0, [3]);
  var Block = /* module */[];
  var b = eq_A(/* A */Block.__(0, [3]), u);
  return /* module */[
          /* u */u,
          /* Block */Block,
          /* y */32,
          /* b */b
        ];
}

function Test2($star) {
  console.log("no inline");
  var Block = /* module */[];
  var b = eq_A(/* A */Block.__(0, [3]), /* A */Block.__(0, [3]));
  return /* module */[
          /* Block */Block,
          /* y */32,
          /* b */b
        ];
}

function f(i, y) {
  var x = /* A */Block.__(0, [i]);
  return eq_A(x, y);
}

function Test3($star) {
  var f = Caml_obj.caml_equal;
  var Caml_obj$1 = /* module */[];
  return /* module */[
          /* f */f,
          /* Caml_obj */Caml_obj$1
        ];
}

function Test4($star) {
  var Caml_obj$1 = /* module */[];
  var f = Caml_obj.caml_equal;
  return /* module */[
          /* Caml_obj */Caml_obj$1,
          /* f */f
        ];
}

function Test5($star) {
  var f = function (x) {
    return Caml_option.some(x);
  };
  var Caml_option = /* module */[];
  return /* module */[
          /* f */f,
          /* Caml_option */Caml_option
        ];
}

function Test6($star) {
  var Caml_option = /* module */[];
  var f = function (x) {
    return Caml_option.some(x);
  };
  return /* module */[
          /* Caml_option */Caml_option,
          /* f */f
        ];
}

function Test7($star) {
  var Caml_option = /* module */[];
  return /* module */[/* Caml_option */Caml_option];
}

function Test8($star) {
  var Curry = /* module */[];
  var f = function (x) {
    return Curry._1(x, 1);
  };
  return /* module */[
          /* Curry */Curry,
          /* f */f
        ];
}

function Test9($star) {
  var f = function (x) {
    return Curry._1(x, 1);
  };
  var Curry = /* module */[];
  return /* module */[
          /* f */f,
          /* Curry */Curry
        ];
}

var x = 3;

exports.eq_A = eq_A;
exports.Test = Test;
exports.Test2 = Test2;
exports.x = x;
exports.f = f;
exports.Test3 = Test3;
exports.Test4 = Test4;
exports.Test5 = Test5;
exports.Test6 = Test6;
exports.Test7 = Test7;
exports.Test8 = Test8;
exports.Test9 = Test9;
/* No side effect */
