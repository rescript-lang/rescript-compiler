// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_bigarray = require("../runtime/caml_bigarray");
var Bigarray      = require("../stdlib/bigarray");
var Caml_curry    = require("../runtime/caml_curry");

var v = Caml_curry.app3(Bigarray.Array1[0], /* Int32 */6, /* C_layout */0, 20);

function sum(v) {
  var result = 0;
  for(var i = 0 ,i_finish = Caml_bigarray.caml_ba_dim_1(v) - 1; i<= i_finish; ++i){
    result = result + Caml_bigarray.caml_ba_get_1(v, i) | 0;
  }
  return /* () */0;
}

var vv = Caml_curry.app3(Bigarray.Array1[0], /* Int32 */6, /* Fortran_layout */1, 30);

function init(v) {
  for(var i = 0 ,i_finish = Caml_bigarray.caml_ba_dim_1(v) - 1; i<= i_finish; ++i){
    v[i] = /* float array */[
      i * i,
      i * i * i
    ];
  }
  return /* () */0;
}

function init2(v) {
  for(var i = 0 ,i_finish = Caml_bigarray.caml_ba_dim_1(v) - 1; i<= i_finish; ++i){
    v[i] = i;
  }
  return /* () */0;
}

function init3(v) {
  for(var i = 0 ,i_finish = Caml_bigarray.caml_ba_dim_1(v) - 1; i<= i_finish; ++i){
    Caml_bigarray.caml_ba_set_1(v, i, i);
  }
  return /* () */0;
}

var BA1 = 0;

exports.BA1   = BA1;
exports.v     = v;
exports.sum   = sum;
exports.vv    = vv;
exports.init  = init;
exports.init2 = init2;
exports.init3 = init3;
/* v Not a pure module */
