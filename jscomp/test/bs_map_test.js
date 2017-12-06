'use strict';

var Bs_Map   = require("../../lib/js/bs_Map.js");
var Bs_Set   = require("../../lib/js/bs_Set.js");
var Caml_obj = require("../../lib/js/caml_obj.js");

var N = /* module */[/* cmp */Caml_obj.caml_int_compare];

var m0 = /* record */[
  /* cmp */N,
  /* data : Empty */0
];

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

for(var i = 0; i <= 100000; ++i){
  data = Bs_Map.add0(cmp, i, i, data);
}

var newm = /* record */[
  /* cmp */I,
  /* data */data
];

console.log(newm);

var m1 = Bs_Map.add0(cmp, 1, 1, Bs_Map.empty0);

console.log(/* record */[
      /* cmp */I,
      /* data */m1
    ]);

var data$1 = /* Empty */0;

for(var i$1 = 0; i$1 <= 100000; ++i$1){
  data$1 = Bs_Set.add0(cmp, i$1, data$1);
}

console.log(/* record */[
      /* cmp */I,
      /* data */data$1
    ]);

exports.N  = N;
exports.m0 = m0;
exports.I  = I;
exports.I2 = I2;
exports.m  = m;
exports.m2 = m2;
/*  Not a pure module */
