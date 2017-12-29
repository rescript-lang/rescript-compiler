'use strict';

var Js_primitive = require("../../lib/js/js_primitive.js");

function Make() {
  var opt_get = function (f, i) {
    return Js_primitive.undefined_to_opt(f[i]);
  };
  return /* module */[/* opt_get */opt_get];
}

function opt_get(f, i) {
  return Js_primitive.undefined_to_opt(f[i]);
}

var Int_arr = /* module */[/* opt_get */opt_get];

function f(v) {
  return /* tuple */[
          v[0],
          Js_primitive.undefined_to_opt(v[1])
        ];
}

exports.Make = Make;
exports.Int_arr = Int_arr;
exports.f = f;
/* No side effect */
