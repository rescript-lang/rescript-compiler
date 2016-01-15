// Generated CODE, PLEASE EDIT WITH CARE
"use strict";
var Pervasives = require("../stdlib/pervasives");
var $$Array = require("../stdlib/array");

function f(a, b, _) {
  return a + b;
}

function f2(a) {
  return function (param) {
    return f(a, 1, param);
  };
}

var f3 = f2(100);

var arr = $$Array.init(2, function () {
      return 0;
    });

for(var i = 0; i<= 2; ++i){
  var f3$1 = f2(i);
  arr[i] = f3$1(2);
}

var match_001 = Pervasives.string_of_int(f(1, 2, 3));

var match_002 = f3(2);

var a = match_001;

var b = match_002;

var c = arr;

exports.f = f;
exports.f2 = f2;
exports.a = a;
exports.b = b;
exports.c = c;
/* match Not a pure module */
