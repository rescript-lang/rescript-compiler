// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var $$Array    = require("../stdlib/array");
var Caml_curry = require("../runtime/caml_curry");

function for_(x) {
  for(var i = 0 ,i_finish = (console.log("hi"), x.length); i<= i_finish; ++i){
    console.log(x[i]);
  }
  return /* () */0;
}

function for_2(x) {
  for(var i = 0 ,i_finish = x.length; i<= i_finish; ++i){
    console.log(x[i]);
  }
  return /* () */0;
}

function for_3(x) {
  var v = [
    0,
    0
  ];
  var arr = $$Array.map(function (_, _$1) {
        return /* () */0;
      }, x);
  for(var i = 0 ,i_finish = x.length; i<= i_finish; ++i){
    var j = i * 2;
    arr[i] = (function(j){
    return function () {
      v[1] += j;
      return /* () */0;
    }
    }(j));
  }
  $$Array.iter(function (x) {
        return Caml_curry.app1(x, /* () */0);
      }, arr);
  return v[1];
}

function for_4(x) {
  var v = [
    0,
    0
  ];
  var arr = $$Array.map(function (_, _$1) {
        return /* () */0;
      }, x);
  for(var i = 0 ,i_finish = x.length; i<= i_finish; ++i){
    var j = i * 2;
    var k = 2 * j;
    arr[i] = (function(k){
    return function () {
      v[1] += k;
      return /* () */0;
    }
    }(k));
  }
  $$Array.iter(function (x) {
        return Caml_curry.app1(x, /* () */0);
      }, arr);
  return v[1];
}

function for_5(x, u) {
  var v = [
    0,
    0
  ];
  var arr = $$Array.map(function (_, _$1) {
        return /* () */0;
      }, x);
  for(var i = 0 ,i_finish = x.length; i<= i_finish; ++i){
    var k = 2 * u * u;
    arr[i] = (function(k){
    return function () {
      v[1] += k;
      return /* () */0;
    }
    }(k));
  }
  $$Array.iter(function (x) {
        return Caml_curry.app1(x, /* () */0);
      }, arr);
  return v[1];
}

function for_6(x, u) {
  var v = [
    0,
    0
  ];
  var arr = $$Array.map(function (_, _$1) {
        return /* () */0;
      }, x);
  var v4 = [
    0,
    0
  ];
  var v5 = [
    0,
    0
  ];
  ++ v4[1];
  for(var j = 0; j<= 1; ++j){
    ++ v5[1];
    var v2 = [
      0,
      0
    ];
    (function(v2){
    for(var i = 0 ,i_finish = x.length; i<= i_finish; ++i){
      var k = 2 * u * u;
      var h = 2 * v5[1];
      ++ v2[1];
      arr[i] = (function(k,h){
      return function () {
        v[1] = v[1] + k + v2[1] + u + v4[1] + v5[1] + h;
        return /* () */0;
      }
      }(k,h));
    }
    }(v2));
  }
  $$Array.iter(function (x) {
        return Caml_curry.app1(x, /* () */0);
      }, arr);
  return v[1];
}

exports.for_  = for_;
exports.for_2 = for_2;
exports.for_3 = for_3;
exports.for_4 = for_4;
exports.for_5 = for_5;
exports.for_6 = for_6;
/* No side effect */
