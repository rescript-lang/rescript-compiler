'use strict';

var Bs_internalMutableAVLSet = require("../../lib/js/bs_internalMutableAVLSet.js");

var v = Bs_internalMutableAVLSet.empty;

console.time("bs_mutable_set_test.ml 11");

for(var i = 0; i <= 1000000; ++i){
  v = Bs_internalMutableAVLSet.add(i, v);
}

console.timeEnd("bs_mutable_set_test.ml 11");

console.log(Bs_internalMutableAVLSet.checkInvariant(v));

console.log(Bs_internalMutableAVLSet.cardinal(v));

var N = 0;

exports.N = N;
/*  Not a pure module */
