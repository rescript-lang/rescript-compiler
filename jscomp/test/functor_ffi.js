// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Primitive_option = require("../../lib/js/primitive_option.js");

function Make(S) {
  let opt_get = (f, i) => Primitive_option.fromUndefined(f[i]);
  return {
    opt_get: opt_get
  };
}

function opt_get(f, i) {
  return Primitive_option.fromUndefined(f[i]);
}

let Int_arr = {
  opt_get: opt_get
};

function f(v) {
  return [
    v[0],
    Primitive_option.fromUndefined(v[1])
  ];
}

exports.Make = Make;
exports.Int_arr = Int_arr;
exports.f = f;
/* No side effect */
