'use strict';

var Bs_Map             = require("../../lib/js/bs_Map.js");
var Bs_Set             = require("../../lib/js/bs_Set.js");
var Caml_obj           = require("../../lib/js/caml_obj.js");
var Bs_internalAVLset  = require("../../lib/js/bs_internalAVLset.js");
var Bs_internalAVLtree = require("../../lib/js/bs_internalAVLtree.js");

var N = /* module */[/* cmp */Caml_obj.caml_int_compare];

var m0 = /* record */[
  /* dict */N,
  /* data */Bs_internalAVLtree.empty0
];

var cmp = Caml_obj.caml_int_compare;

var I = /* module */[/* cmp */cmp];

function cmp$1(x, y) {
  return Caml_obj.caml_int_compare(y, x);
}

var I2 = /* module */[/* cmp */cmp$1];

var m = /* record */[
  /* dict */I,
  /* data */Bs_internalAVLtree.empty0
];

var m2 = /* record */[
  /* dict */I2,
  /* data */Bs_internalAVLtree.empty0
];

var data = Bs_internalAVLtree.empty0;

for(var i = 0; i <= 100000; ++i){
  data = Bs_Map.add0(cmp, i, i, data);
}

var newm = /* record */[
  /* dict */I,
  /* data */data
];

console.log(newm);

var m1 = Bs_Map.add0(cmp, 1, 1, Bs_Map.empty0);

console.log(/* record */[
      /* dict */I,
      /* data */m1
    ]);

var data$1 = Bs_internalAVLset.empty0;

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
