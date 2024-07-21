// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";

function fromArray(a) {
  return a.slice(0);
}

function toArray(a) {
  return a.slice(0);
}

function length(a) {
  return a.length;
}

function size(a) {
  return a.length;
}

let get = Belt_Array.get;

let getExn = Belt_Array.getExn;

function getUnsafe(a, x) {
  return a[x];
}

function getUndefined(a, x) {
  return a[x];
}

let shuffle = Belt_Array.shuffle;

let reverse = Belt_Array.reverse;

function makeUninitialized(x) {
  return new Array(x);
}

function makeUninitializedUnsafe(x) {
  return new Array(x);
}

let make = Belt_Array.make;

let range = Belt_Array.range;

let rangeBy = Belt_Array.rangeBy;

let makeByU = Belt_Array.makeByU;

function makeBy(c, f) {
  return Belt_Array.makeBy(c, Curry.__1(f));
}

let makeByAndShuffleU = Belt_Array.makeByAndShuffleU;

function makeByAndShuffle(c, f) {
  return Belt_Array.makeByAndShuffle(c, Curry.__1(f));
}

let zip = Belt_Array.zip;

let zipByU = Belt_Array.zipByU;

function zipBy(a1, a2, f) {
  return Belt_Array.zipBy(a1, a2, Curry.__2(f));
}

let unzip = Belt_Array.unzip;

let concat = Belt_Array.concat;

let concatMany = Belt_Array.concatMany;

let slice = Belt_Array.slice;

let sliceToEnd = Belt_Array.sliceToEnd;

function copy(a) {
  return a.slice(0);
}

let forEachU = Belt_Array.forEachU;

function forEach(a, f) {
  Belt_Array.forEach(a, Curry.__1(f));
}

let mapU = Belt_Array.mapU;

function map(a, f) {
  return Belt_Array.map(a, Curry.__1(f));
}

let keepWithIndexU = Belt_Array.keepWithIndexU;

function keepWithIndex(a, f) {
  return Belt_Array.keepWithIndex(a, Curry.__2(f));
}

let keepMapU = Belt_Array.keepMapU;

function keepMap(a, f) {
  return Belt_Array.keepMap(a, Curry.__1(f));
}

let forEachWithIndexU = Belt_Array.forEachWithIndexU;

function forEachWithIndex(a, f) {
  Belt_Array.forEachWithIndex(a, Curry.__2(f));
}

let mapWithIndexU = Belt_Array.mapWithIndexU;

function mapWithIndex(a, f) {
  return Belt_Array.mapWithIndex(a, Curry.__2(f));
}

let partitionU = Belt_Array.partitionU;

function partition(a, f) {
  return Belt_Array.partition(a, Curry.__1(f));
}

let reduceU = Belt_Array.reduceU;

function reduce(a, b, f) {
  return Belt_Array.reduce(a, b, Curry.__2(f));
}

let reduceReverseU = Belt_Array.reduceReverseU;

function reduceReverse(a, b, f) {
  return Belt_Array.reduceReverse(a, b, Curry.__2(f));
}

let reduceReverse2U = Belt_Array.reduceReverse2U;

function reduceReverse2(a1, a2, c, f) {
  return Belt_Array.reduceReverse2(a1, a2, c, Curry.__3(f));
}

let someU = Belt_Array.someU;

function some(a, f) {
  return Belt_Array.some(a, Curry.__1(f));
}

let everyU = Belt_Array.everyU;

function every(a, f) {
  return Belt_Array.every(a, Curry.__1(f));
}

let every2U = Belt_Array.every2U;

function every2(a1, a2, f) {
  return Belt_Array.every2(a1, a2, Curry.__2(f));
}

let some2U = Belt_Array.some2U;

function some2(a1, a2, f) {
  return Belt_Array.some2(a1, a2, Curry.__2(f));
}

let cmpU = Belt_Array.cmpU;

function cmp(a1, a2, f) {
  return Belt_Array.cmp(a1, a2, Curry.__2(f));
}

let eqU = Belt_Array.eqU;

function eq(a1, a2, f) {
  return Belt_Array.eq(a1, a2, Curry.__2(f));
}

let $$Array$1 = {
  get: get
};

export {
  $$Array$1 as $$Array,
  fromArray,
  toArray,
  length,
  size,
  get,
  getExn,
  getUnsafe,
  getUndefined,
  shuffle,
  reverse,
  makeUninitialized,
  makeUninitializedUnsafe,
  make,
  range,
  rangeBy,
  makeByU,
  makeBy,
  makeByAndShuffleU,
  makeByAndShuffle,
  zip,
  zipByU,
  zipBy,
  unzip,
  concat,
  concatMany,
  slice,
  sliceToEnd,
  copy,
  forEachU,
  forEach,
  mapU,
  map,
  keepWithIndexU,
  keepWithIndex,
  keepMapU,
  keepMap,
  forEachWithIndexU,
  forEachWithIndex,
  mapWithIndexU,
  mapWithIndex,
  partitionU,
  partition,
  reduceU,
  reduce,
  reduceReverseU,
  reduceReverse,
  reduceReverse2U,
  reduceReverse2,
  someU,
  some,
  everyU,
  every,
  every2U,
  every2,
  some2U,
  some2,
  cmpU,
  cmp,
  eqU,
  eq,
}
/* No side effect */
