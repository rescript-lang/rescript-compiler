'use strict';

var Bs_Map = require("../../lib/js/bs_Map.js");
var Bs_Set = require("../../lib/js/bs_Set.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Bs_internalAVLset = require("../../lib/js/bs_internalAVLset.js");
var Bs_internalAVLtree = require("../../lib/js/bs_internalAVLtree.js");

var N = /* module */[/* cmp */Caml_primitive.caml_int_compare];

var m0 = {
  dict: N,
  data: Bs_internalAVLtree.empty0
};

var cmp = Caml_primitive.caml_int_compare;

var I = /* module */[/* cmp */cmp];

function cmp$1(x, y) {
  return Caml_primitive.caml_int_compare(y, x);
}

var I2 = /* module */[/* cmp */cmp$1];

var m = {
  dict: I,
  data: Bs_internalAVLtree.empty0
};

var m2 = {
  dict: I2,
  data: Bs_internalAVLtree.empty0
};

var data = m.data;

m2.dict;

var m_dict = m.dict;

for(var i = 0; i <= 100000; ++i){
  data = Bs_Map.add0(m_dict[/* cmp */0], i, i, data);
}

var newm = {
  dict: m_dict,
  data: data
};

console.log(newm);

var m11 = Bs_Map.add0(cmp, 1, 1, Bs_Map.empty0);

console.log(m11);

var v = {
  dict: I,
  data: Bs_internalAVLset.empty0
};

var m_dict$1 = m.dict;

var cmp$2 = m_dict$1[/* cmp */0];

var data$1 = v.data;

for(var i$1 = 0; i$1 <= 100000; ++i$1){
  data$1 = Bs_Set.add0(cmp$2, data$1, i$1);
}

console.log(data$1);

var B = 0;

var ISet = 0;

exports.N = N;
exports.m0 = m0;
exports.I = I;
exports.I2 = I2;
exports.m = m;
exports.m2 = m2;
exports.B = B;
exports.ISet = ISet;
/* data Not a pure module */
