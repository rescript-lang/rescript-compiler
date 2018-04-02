'use strict';

var Bigarray = require("../../lib/js/bigarray.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_missing_polyfill = require("../../lib/js/caml_missing_polyfill.js");

function sum() {
  var result = 0;
  for(var i = 0 ,i_finish = Caml_missing_polyfill.not_implemented("caml_ba_dim_1 not implemented by bucklescript yet\n") - 1 | 0; i <= i_finish; ++i){
    result = result + Caml_missing_polyfill.not_implemented("caml_ba_get_1 not implemented by bucklescript yet\n") | 0;
  }
  return /* () */0;
}

function init(v) {
  for(var i = 0 ,i_finish = Caml_missing_polyfill.not_implemented("caml_ba_dim_1 not implemented by bucklescript yet\n") - 1 | 0; i <= i_finish; ++i){
    v[i] = /* array */[
      Caml_int32.imul(i, i),
      Caml_int32.imul(Caml_int32.imul(i, i), i)
    ];
  }
  return /* () */0;
}

function init2(v) {
  for(var i = 0 ,i_finish = Caml_missing_polyfill.not_implemented("caml_ba_dim_1 not implemented by bucklescript yet\n") - 1 | 0; i <= i_finish; ++i){
    v[i] = i;
  }
  return /* () */0;
}

function init3() {
  for(var i = 0 ,i_finish = Caml_missing_polyfill.not_implemented("caml_ba_dim_1 not implemented by bucklescript yet\n") - 1 | 0; i <= i_finish; ++i){
    Caml_missing_polyfill.not_implemented("caml_ba_set_1 not implemented by bucklescript yet\n");
  }
  return /* () */0;
}

var BA1 = 0;

exports.BA1 = BA1;
exports.sum = sum;
exports.init = init;
exports.init2 = init2;
exports.init3 = init3;
/* Bigarray Not a pure module */
