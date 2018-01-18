'use strict';

var Bs_Array = require("../../lib/js/bs_Array.js");
var Bs_MapInt = require("../../lib/js/bs_MapInt.js");
var Immutable = require("immutable");

var empty = new Immutable.OrderedMap();

function ofArray(kvs) {
  var v = empty;
  for(var i = 0 ,i_finish = kvs.length - 1 | 0; i <= i_finish; ++i){
    var match = kvs[i];
    v = v.set(match[0], match[1]);
  }
  return v;
}

function should(b) {
  if (b) {
    return 0;
  } else {
    throw new Error("impossible");
  }
}

var shuffledDataAdd = Bs_Array.shuffle(Bs_Array.init(1000001, (function (i) {
            return /* tuple */[
                    i,
                    i
                  ];
          })));

var shuffleRemoved = Bs_Array.shuffle(Bs_Array.init(2000001, (function (i) {
            return i;
          })));

function test() {
  var v = ofArray(shuffledDataAdd);
  for(var j = 0; j <= 1000000; ++j){
    should(+v.has(j));
  }
  return /* () */0;
}

function test2() {
  var v = Bs_MapInt.ofArray(shuffledDataAdd);
  for(var j = 0; j <= 1000000; ++j){
    should(Bs_MapInt.mem(v, j));
  }
  return /* () */0;
}

console.time("imm_map_bench.ml 42");

test(/* () */0);

console.timeEnd("imm_map_bench.ml 42");

console.time("imm_map_bench.ml 43");

test2(/* () */0);

console.timeEnd("imm_map_bench.ml 43");

var A = 0;

var count = 1000000;

var M = 0;

exports.A = A;
exports.empty = empty;
exports.ofArray = ofArray;
exports.should = should;
exports.count = count;
exports.shuffledDataAdd = shuffledDataAdd;
exports.shuffleRemoved = shuffleRemoved;
exports.test = test;
exports.M = M;
exports.test2 = test2;
/* empty Not a pure module */
