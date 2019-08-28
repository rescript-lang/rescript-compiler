'use strict';

var Caml_int32 = require("../../lib/js/caml_int32.js");
var Caml_external_polyfill = require("../../lib/js/caml_external_polyfill.js");

function sum(v) {
  var result = 0;
  for(var i = 0 ,i_finish = Caml_external_polyfill.resolve("caml_ba_dim_1")(v) - 1 | 0; i <= i_finish; ++i){
    result = result + Caml_external_polyfill.resolve("caml_ba_get_1")(v, i) | 0;
  }
  return /* () */0;
}

function init(v) {
  for(var i = 0 ,i_finish = Caml_external_polyfill.resolve("caml_ba_dim_1")(v) - 1 | 0; i <= i_finish; ++i){
    v[i] = /* record */{
      re: Caml_int32.imul(i, i),
      im: Caml_int32.imul(Caml_int32.imul(i, i), i)
    };
  }
  return /* () */0;
}

function init2(v) {
  for(var i = 0 ,i_finish = Caml_external_polyfill.resolve("caml_ba_dim_1")(v) - 1 | 0; i <= i_finish; ++i){
    v[i] = i;
  }
  return /* () */0;
}

function init3(v) {
  for(var i = 0 ,i_finish = Caml_external_polyfill.resolve("caml_ba_dim_1")(v) - 1 | 0; i <= i_finish; ++i){
    Caml_external_polyfill.resolve("caml_ba_set_1")(v, i, i);
  }
  return /* () */0;
}

var BA1 = 0;

exports.BA1 = BA1;
exports.sum = sum;
exports.init = init;
exports.init2 = init2;
exports.init3 = init3;
/* No side effect */
