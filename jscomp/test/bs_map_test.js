'use strict';

var Bs_Map   = require("../../lib/js/bs_Map.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

var cmp = Caml_obj.caml_int_compare;

var I = /* module */[/* cmp */cmp];

function cmp$1(x, y) {
  return Caml_obj.caml_int_compare(y, x);
}

var I2 = /* module */[/* cmp */cmp$1];

var m = /* record */[
  /* cmp */I,
  /* data : Empty */0
];

var m2 = /* record */[
  /* cmp */I2,
  /* data : Empty */0
];

var data = /* Empty */0;

for(var i = 0; i <= 1000000; ++i){
  data = Bs_Map.add0(cmp, i, i, data);
}

var newm = /* record */[
  /* cmp */I,
  /* data */data
];

console.log(newm);

exports.I  = I;
exports.I2 = I2;
exports.m  = m;
exports.m2 = m2;
/*  Not a pure module */
