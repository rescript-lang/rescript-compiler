'use strict';

var Curry = require("../../lib/js/curry.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_option = require("../../lib/js/caml_option.js");

function eq_A(x, y) {
  if (x.TAG || y.TAG) {
    return false;
  } else {
    return x._0 === y._0;
  }
}

function Test($star) {
  console.log("no inline");
  var u = {
    TAG: /* A */0,
    _0: 3
  };
  var Block = {};
  var b = eq_A({
        TAG: /* A */0,
        _0: 3
      }, u);
  return {
          u,
          Block,
          y: 32,
          b
        };
}

function Test2($star) {
  console.log("no inline");
  var Block = {};
  var b = eq_A({
        TAG: /* A */0,
        _0: 3
      }, {
        TAG: /* A */0,
        _0: 3
      });
  return {
          Block,
          y: 32,
          b
        };
}

function f(i, y) {
  var x = {
    TAG: /* A */0,
    _0: i
  };
  return eq_A(x, y);
}

function Test3($star) {
  var f = Caml_obj.caml_equal;
  var Caml_obj$1 = {};
  return {
          f,
          Caml_obj: Caml_obj$1
        };
}

function Test4($star) {
  var Caml_obj$1 = {};
  var f = Caml_obj.caml_equal;
  return {
          Caml_obj: Caml_obj$1,
          f
        };
}

function Test5($star) {
  var f = function (x) {
    return Caml_option.some(x);
  };
  var Caml_option$1 = {};
  return {
          f,
          Caml_option: Caml_option$1
        };
}

function Test6($star) {
  var Caml_option$1 = {};
  var f = function (x) {
    return Caml_option.some(x);
  };
  return {
          Caml_option: Caml_option$1,
          f
        };
}

function Test7($star) {
  var Caml_option = {};
  return {
          Caml_option
        };
}

function Test8($star) {
  var Curry$1 = {};
  var f = function (x) {
    return Curry._1(x, 1);
  };
  return {
          Curry: Curry$1,
          f
        };
}

function Test9($star) {
  var f = function (x) {
    return Curry._1(x, 1);
  };
  var Curry$1 = {};
  return {
          f,
          Curry: Curry$1
        };
}

function Test10($star) {
  var Curry = {};
  return {
          Curry
        };
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
exports.Test10 = Test10;
/* No side effect */
