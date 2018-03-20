'use strict';

var Immutable = require("immutable");
var Belt_Array = require("../../lib/js/belt_Array.js");
var Belt_MapInt = require("../../lib/js/belt_MapInt.js");

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

var shuffledDataAdd = Belt_Array.makeByAndShuffle(1000001, (function (i) {
        return /* tuple */[
                i,
                i
              ];
      }));

function test() {
  var v = ofArray(shuffledDataAdd);
  for(var j = 0; j <= 1000000; ++j){
    should(v.has(j));
  }
  return /* () */0;
}

function test2() {
  var v = Belt_MapInt.ofArray(shuffledDataAdd);
  for(var j = 0; j <= 1000000; ++j){
    should(Belt_MapInt.has(v, j));
  }
  return /* () */0;
}

console.time("imm_map_bench.ml 44");

test(/* () */0);

console.timeEnd("imm_map_bench.ml 44");

console.time("imm_map_bench.ml 45");

test2(/* () */0);

console.timeEnd("imm_map_bench.ml 45");

var A = 0;

var count = 1000000;

var M = 0;

exports.A = A;
exports.empty = empty;
exports.ofArray = ofArray;
exports.should = should;
exports.count = count;
exports.shuffledDataAdd = shuffledDataAdd;
exports.test = test;
exports.M = M;
exports.test2 = test2;
/* empty Not a pure module */
