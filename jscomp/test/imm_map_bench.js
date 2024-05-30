// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Immutable = require("immutable");
let Belt_Array = require("../../lib/js/belt_Array.js");
let Belt_MapInt = require("../../lib/js/belt_MapInt.js");

let empty = new Immutable.OrderedMap();

function fromArray(kvs) {
  let v = empty;
  for(let i = 0 ,i_finish = kvs.length; i < i_finish; ++i){
    let match = kvs[i];
    v = v.set(match[0], match[1]);
  }
  return v;
}

function should(b) {
  if (b) {
    return;
  }
  throw new Error(new Error("impossible").RE_EXN_ID, {
        cause: new Error("impossible")
      });
}

let shuffledDataAdd = Belt_Array.makeByAndShuffle(1000001, (function (i) {
  return [
    i,
    i
  ];
}));

function test(param) {
  let v = fromArray(shuffledDataAdd);
  for(let j = 0; j <= 1000000; ++j){
    should(v.has(j));
  }
}

function test2(param) {
  let v = Belt_MapInt.fromArray(shuffledDataAdd);
  for(let j = 0; j <= 1000000; ++j){
    should(Belt_MapInt.has(v, j));
  }
}

console.time("imm_map_bench.res 43");

test();

console.timeEnd("imm_map_bench.res 43");

console.time("imm_map_bench.res 44");

test2();

console.timeEnd("imm_map_bench.res 44");

let A;

let count = 1000000;

let M;

exports.A = A;
exports.empty = empty;
exports.fromArray = fromArray;
exports.should = should;
exports.count = count;
exports.shuffledDataAdd = shuffledDataAdd;
exports.test = test;
exports.M = M;
exports.test2 = test2;
/* empty Not a pure module */
