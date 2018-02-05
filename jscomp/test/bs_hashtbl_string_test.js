'use strict';

var Bs_Dict = require("../../lib/js/bs_Dict.js");
var Hashtbl = require("../../lib/js/hashtbl.js");
var Caml_hash = require("../../lib/js/caml_hash.js");
var Bs_HashMap = require("../../lib/js/bs_HashMap.js");
var Bs_MapDict = require("../../lib/js/bs_MapDict.js");
var Bs_HashMapInt = require("../../lib/js/bs_HashMapInt.js");
var Bs_HashSetInt = require("../../lib/js/bs_HashSetInt.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Bs_HashMapString = require("../../lib/js/bs_HashMapString.js");
var Bs_internalBucketsType = require("../../lib/js/bs_internalBucketsType.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function hash_string(s) {
  return Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, s));
}

var hashString = (function (str) {
                                              var hash = 5381,
                                              i    = str.length | 0;

                                              while(i !== 0) {
                                              hash = (hash * 33) ^ str.charCodeAt(--i);
                                              }
                                              return hash  
                                              }
                                            );

var $$String = Bs_Dict.hashable(Hashtbl.hash, (function (x, y) {
        return +(x === y);
      }));

var String1 = Bs_Dict.hashable(hashString, (function (x, y) {
        return +(x === y);
      }));

var String2 = Bs_Dict.hashable((function (x) {
        return Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, x));
      }), (function (x, y) {
        return +(x === y);
      }));

var Int = Bs_Dict.hashable(Hashtbl.hash, (function (x, y) {
        return +(x === y);
      }));

var empty = Bs_HashMap.make(500000, Int);

function bench() {
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMap.set(empty, i, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMap.has(empty, i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              50,
              4
            ]
          ];
    }
    
  }
  return Bs_HashMap.logStats(empty);
}

function bench2(m) {
  var empty = Bs_HashMap.make(1000000, m);
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMap.set(empty, "" + i, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMap.has(empty, "" + i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              76,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashMap.remove(empty, "" + i$2);
  }
  if (empty.size) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            82,
            2
          ]
        ];
  } else {
    return 0;
  }
}

function bench3(m) {
  var empty = {
    cmp: m[/* cmp */0],
    data: Bs_MapDict.empty
  };
  var cmp = m[/* cmp */0];
  var table = empty.data;
  for(var i = 0; i <= 1000000; ++i){
    table = Bs_MapDict.set(table, "" + i, i, cmp);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_MapDict.has(table, "" + i$1, cmp)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              98,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    table = Bs_MapDict.remove(table, "" + i$2, cmp);
  }
  if (Bs_MapDict.size(table)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            105,
            2
          ]
        ];
  } else {
    return 0;
  }
}

var Sx = Bs_Dict.comparable(Caml_primitive.caml_string_compare);

function bench4() {
  var table = Bs_internalBucketsType.make(/* () */0, /* () */0, 1000000);
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMapString.set(table, "" + i, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMapString.has(table, "" + i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              118,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashMapString.remove(table, "" + i$2);
  }
  if (Bs_HashMapString.isEmpty(table)) {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            124,
            2
          ]
        ];
  }
}

function bench5() {
  var table = Bs_HashMap.make(1000000, Int);
  console.time("bs_hashtbl_string_test.ml 133");
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMap.set(table, i, i);
  }
  console.timeEnd("bs_hashtbl_string_test.ml 133");
  console.time("bs_hashtbl_string_test.ml 137");
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMap.has(table, i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              138,
              6
            ]
          ];
    }
    
  }
  console.timeEnd("bs_hashtbl_string_test.ml 137");
  console.time("bs_hashtbl_string_test.ml 141");
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashMap.remove(table, i$2);
  }
  console.timeEnd("bs_hashtbl_string_test.ml 141");
  if (Bs_HashMap.isEmpty(table)) {
    return 0;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            144,
            2
          ]
        ];
  }
}

function bench6() {
  var table = Bs_internalBucketsType.make(/* () */0, /* () */0, 1000000);
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMapInt.set(table, i, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMapInt.has(table, i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              156,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashMapInt.remove(table, i$2);
  }
  if (table.size) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            162,
            2
          ]
        ];
  } else {
    return 0;
  }
}

function bench7() {
  var size = 2000000;
  var table = Bs_internalBucketsType.make(/* () */0, /* () */0, size);
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashSetInt.add(table, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashSetInt.has(table, i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              181,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashSetInt.remove(table, i$2);
  }
  if (table.size) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            192,
            2
          ]
        ];
  } else {
    return 0;
  }
}

console.time("bs_hashtbl_string_test.ml 203");

bench7(/* () */0);

console.timeEnd("bs_hashtbl_string_test.ml 203");

var N = 0;

var count = 1000000;

var initial_size = 1000000;

var M = 0;

var Md = 0;

var Md0 = 0;

var H = 0;

var H0 = 0;

var HI = 0;

var S = 0;

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
/* hashString Not a pure module */
