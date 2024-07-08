// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Caml = require("../../lib/js/caml.js");
let Belt_Id = require("../../lib/js/belt_Id.js");
let Hashtbl = require("../../lib/js/hashtbl.js");
let Belt_HashMap = require("../../lib/js/belt_HashMap.js");
let Belt_MapDict = require("../../lib/js/belt_MapDict.js");
let Belt_HashMapInt = require("../../lib/js/belt_HashMapInt.js");
let Belt_HashSetInt = require("../../lib/js/belt_HashSetInt.js");
let Belt_HashMapString = require("../../lib/js/belt_HashMapString.js");
let Caml_hash_primitive = require("../../lib/js/caml_hash_primitive.js");

function hash_string(s) {
  return Caml_hash_primitive.hash_final_mix(Caml_hash_primitive.hash_mix_string(0, s));
}

let hashString = (function(str){ 
                                              var hash = 5381,
                                              i    = str.length | 0;

                                              while(i !== 0) {
                                              hash = (hash * 33) ^ str.charCodeAt(--i);
                                              }
                                              return hash});

let $$String = Belt_Id.hashable(Hashtbl.hash, (function (x, y) {
  return x === y;
}));

let String1 = Belt_Id.hashable(hashString, (function (x, y) {
  return x === y;
}));

let String2 = Belt_Id.hashable((function (x) {
  return Caml_hash_primitive.hash_final_mix(Caml_hash_primitive.hash_mix_string(0, x));
}), (function (x, y) {
  return x === y;
}));

let Int = Belt_Id.hashable(Hashtbl.hash, (function (x, y) {
  return x === y;
}));

let empty = Belt_HashMap.make(500000, Int);

function bench() {
  for(let i = 0; i <= 1000000; ++i){
    Belt_HashMap.set(empty, i, i);
  }
  for(let i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Belt_HashMap.has(empty, i$1)) {
      throw new Error("Assert_failure", {
            cause: {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "bs_hashtbl_string_test.res",
                37,
                4
              ]
            }
          });
    }
    
  }
  Belt_HashMap.logStats(empty);
}

function bench2(m) {
  let empty = Belt_HashMap.make(1000000, m);
  for(let i = 0; i <= 1000000; ++i){
    Belt_HashMap.set(empty, String(i), i);
  }
  for(let i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Belt_HashMap.has(empty, String(i$1))) {
      throw new Error("Assert_failure", {
            cause: {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "bs_hashtbl_string_test.res",
                61,
                4
              ]
            }
          });
    }
    
  }
  for(let i$2 = 0; i$2 <= 1000000; ++i$2){
    Belt_HashMap.remove(empty, String(i$2));
  }
  if (empty.size === 0) {
    return;
  }
  throw new Error("Assert_failure", {
        cause: {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "bs_hashtbl_string_test.res",
            66,
            2
          ]
        }
      });
}

function bench3(m) {
  let empty_cmp = m.cmp;
  let cmp = m.cmp;
  let table = undefined;
  for(let i = 0; i <= 1000000; ++i){
    table = Belt_MapDict.set(table, String(i), i, cmp);
  }
  for(let i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Belt_MapDict.has(table, String(i$1), cmp)) {
      throw new Error("Assert_failure", {
            cause: {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "bs_hashtbl_string_test.res",
                81,
                4
              ]
            }
          });
    }
    
  }
  for(let i$2 = 0; i$2 <= 1000000; ++i$2){
    table = Belt_MapDict.remove(table, String(i$2), cmp);
  }
  if (Belt_MapDict.size(table) === 0) {
    return;
  }
  throw new Error("Assert_failure", {
        cause: {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "bs_hashtbl_string_test.res",
            86,
            2
          ]
        }
      });
}

let Sx = Belt_Id.comparable(Caml.string_compare);

function bench4() {
  let table = Belt_HashMapString.make(1000000);
  for(let i = 0; i <= 1000000; ++i){
    Belt_HashMapString.set(table, String(i), i);
  }
  for(let i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Belt_HashMapString.has(table, String(i$1))) {
      throw new Error("Assert_failure", {
            cause: {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "bs_hashtbl_string_test.res",
                98,
                4
              ]
            }
          });
    }
    
  }
  for(let i$2 = 0; i$2 <= 1000000; ++i$2){
    Belt_HashMapString.remove(table, String(i$2));
  }
  if (Belt_HashMapString.isEmpty(table)) {
    return;
  }
  throw new Error("Assert_failure", {
        cause: {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "bs_hashtbl_string_test.res",
            103,
            2
          ]
        }
      });
}

function bench5() {
  let table = Belt_HashMap.make(1000000, Int);
  console.time("bs_hashtbl_string_test.res 112");
  for(let i = 0; i <= 1000000; ++i){
    Belt_HashMap.set(table, i, i);
  }
  console.timeEnd("bs_hashtbl_string_test.res 112");
  console.time("bs_hashtbl_string_test.res 117");
  for(let i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Belt_HashMap.has(table, i$1)) {
      throw new Error("Assert_failure", {
            cause: {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "bs_hashtbl_string_test.res",
                119,
                6
              ]
            }
          });
    }
    
  }
  console.timeEnd("bs_hashtbl_string_test.res 117");
  console.time("bs_hashtbl_string_test.res 122");
  for(let i$2 = 0; i$2 <= 1000000; ++i$2){
    Belt_HashMap.remove(table, i$2);
  }
  console.timeEnd("bs_hashtbl_string_test.res 122");
  if (Belt_HashMap.isEmpty(table)) {
    return;
  }
  throw new Error("Assert_failure", {
        cause: {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "bs_hashtbl_string_test.res",
            127,
            2
          ]
        }
      });
}

function bench6() {
  let table = Belt_HashMapInt.make(1000000);
  for(let i = 0; i <= 1000000; ++i){
    Belt_HashMapInt.set(table, i, i);
  }
  for(let i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Belt_HashMapInt.has(table, i$1)) {
      throw new Error("Assert_failure", {
            cause: {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "bs_hashtbl_string_test.res",
                138,
                4
              ]
            }
          });
    }
    
  }
  for(let i$2 = 0; i$2 <= 1000000; ++i$2){
    Belt_HashMapInt.remove(table, i$2);
  }
  if (table.size === 0) {
    return;
  }
  throw new Error("Assert_failure", {
        cause: {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "bs_hashtbl_string_test.res",
            143,
            2
          ]
        }
      });
}

function bench7() {
  let table = Belt_HashSetInt.make(2000000);
  for(let i = 0; i <= 1000000; ++i){
    Belt_HashSetInt.add(table, i);
  }
  for(let i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Belt_HashSetInt.has(table, i$1)) {
      throw new Error("Assert_failure", {
            cause: {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "bs_hashtbl_string_test.res",
                160,
                4
              ]
            }
          });
    }
    
  }
  for(let i$2 = 0; i$2 <= 1000000; ++i$2){
    Belt_HashSetInt.remove(table, i$2);
  }
  if (table.size === 0) {
    return;
  }
  throw new Error("Assert_failure", {
        cause: {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "bs_hashtbl_string_test.res",
            170,
            2
          ]
        }
      });
}

console.time("bs_hashtbl_string_test.res 181");

bench7();

console.timeEnd("bs_hashtbl_string_test.res 181");

let N;

let count = 1000000;

let initial_size = 1000000;

let M;

let Md;

let Md0;

let H;

let H0;

let HI;

let S;

exports.hash_string = hash_string;
exports.hashString = hashString;
exports.$$String = $$String;
exports.String1 = String1;
exports.String2 = String2;
exports.Int = Int;
exports.N = N;
exports.empty = empty;
exports.bench = bench;
exports.count = count;
exports.initial_size = initial_size;
exports.M = M;
exports.bench2 = bench2;
exports.Md = Md;
exports.Md0 = Md0;
exports.bench3 = bench3;
exports.Sx = Sx;
exports.H = H;
exports.bench4 = bench4;
exports.H0 = H0;
exports.bench5 = bench5;
exports.HI = HI;
exports.bench6 = bench6;
exports.S = S;
exports.bench7 = bench7;
/* String Not a pure module */
