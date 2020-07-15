'use strict';

var Curry = require("../../lib/js/curry.js");

function O(X) {
  var cow = function (x) {
    return Curry._1(X.foo, x);
  };
  var sheep = function (x) {
    return 1 + Curry._1(X.foo, x) | 0;
  };
  return {
          cow,
          sheep
        };
}

function F(X, Y) {
  var cow = function (x) {
    return Curry._1(Y.foo, Curry._1(X.foo, x));
  };
  var sheep = function (x) {
    return 1 + Curry._1(Y.foo, Curry._1(X.foo, x)) | 0;
  };
  return {
          cow,
          sheep
        };
}

function F1(X, Y) {
  var sheep = function (x) {
    return 1 + Curry._1(Y.foo, Curry._1(X.foo, x)) | 0;
  };
  return {
          sheep
        };
}

function F2(X, Y) {
  var sheep = function (x) {
    return 1 + Curry._1(Y.foo, Curry._1(X.foo, x)) | 0;
  };
  return {
          sheep
        };
}

var M = {
  F: (function (funarg, funarg$1) {
      var sheep = function (x) {
        return 1 + Curry._1(funarg$1.foo, Curry._1(funarg.foo, x)) | 0;
      };
      return {
              sheep
            };
    })
};

exports.O = O;
exports.F = F;
exports.F1 = F1;
exports.F2 = F2;
exports.M = M;
/* No side effect */
