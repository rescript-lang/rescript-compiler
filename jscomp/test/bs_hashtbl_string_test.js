'use strict';

var Bs_Map                  = require("../../lib/js/bs_Map.js");
var Hashtbl                 = require("../../lib/js/hashtbl.js");
var Caml_hash               = require("../../lib/js/caml_hash.js");
var Bs_HashMap              = require("../../lib/js/bs_HashMap.js");
var Caml_string             = require("../../lib/js/caml_string.js");
var Bs_HashMapInt           = require("../../lib/js/bs_HashMapInt.js");
var Bs_HashMapString        = require("../../lib/js/bs_HashMapString.js");
var Bs_internalBuckets      = require("../../lib/js/bs_internalBuckets.js");
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

var String_000 = Hashtbl.hash;

function String_001(x, y) {
  return +(x === y);
}

var $$String = /* module */[
  String_000,
  String_001
];

function String1_001(x, y) {
  return +(x === y);
}

var String1 = /* module */[
  /* hash */hashString,
  String1_001
];

function String2_000(x) {
  return Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, x));
}

function String2_001(x, y) {
  return +(x === y);
}

var String2 = /* module */[
  String2_000,
  String2_001
];

var Int_000 = Hashtbl.hash;

function Int_001(x, y) {
  return +(x === y);
}

var Int = /* module */[
  Int_000,
  Int_001
];

var empty_001 = /* data */Bs_internalBuckets.create0(500000);

var empty = /* record */[
  /* dict */Int,
  empty_001
];

function bench() {
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMap.add(empty, i, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMap.mem(empty, i$1)) {
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
  return Bs_internalBuckets.logStats0(empty_001);
}

function bench2(m) {
  var empty_001 = /* data */Bs_internalBuckets.create0(1000000);
  var hash = m[/* hash */0];
  var eq = m[/* eq */1];
  var table = empty_001;
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMap.add0(hash, table, "" + i, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMap.mem0(hash, eq, table, "" + i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              74,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashMap.remove0(hash, eq, table, "" + i$2);
  }
  if (Bs_HashMap.length0(table)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            81,
            2
          ]
        ];
  } else {
    return 0;
  }
}

function bench3(m) {
  var cmp = m[/* cmp */0];
  var table = /* data : Empty */0;
  for(var i = 0; i <= 1000000; ++i){
    table = Bs_Map.add0(cmp, "" + i, i, table);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_Map.mem0(cmp, "" + i$1, table)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              95,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    table = Bs_Map.remove0(cmp, "" + i$2, table);
  }
  if (Bs_Map.cardinal0(table)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            102,
            2
          ]
        ];
  } else {
    return 0;
  }
}

var S = /* module */[/* cmp */Caml_string.caml_string_compare];

function bench4() {
  var table = Bs_HashMapString.create(1000000);
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMapString.add(table, "" + i, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMapString.mem(table, "" + i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              115,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashMapString.remove(table, "" + i$2);
  }
  if (Bs_HashMapString.length(table)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            121,
            2
          ]
        ];
  } else {
    return 0;
  }
}

function bench5() {
  var table_001 = /* data */Bs_internalBuckets.create0(1000000);
  var table_data = table_001;
  var hash = Int_000;
  var eq = Int_001;
  console.time("bs_hashtbl_string_test.ml 129");
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMap.add0(hash, table_data, i, i);
  }
  console.timeEnd("bs_hashtbl_string_test.ml 129");
  console.time("bs_hashtbl_string_test.ml 133");
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMap.mem0(hash, eq, table_data, i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              134,
              4
            ]
          ];
    }
    
  }
  console.timeEnd("bs_hashtbl_string_test.ml 133");
  console.time("bs_hashtbl_string_test.ml 137");
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashMap.remove0(hash, eq, table_data, i$2);
  }
  console.timeEnd("bs_hashtbl_string_test.ml 137");
  if (table_001[/* size */0]) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            140,
            2
          ]
        ];
  } else {
    return 0;
  }
}

function bench6() {
  var table = Bs_HashMapInt.create(1000000);
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMapInt.add(table, i, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMapInt.mem(table, i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              151,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashMapInt.remove(table, i$2);
  }
  if (Bs_HashMapString.length(table)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            157,
            2
          ]
        ];
  } else {
    return 0;
  }
}

console.time("bs_hashtbl_string_test.ml 168");

bench6(/* () */0);

console.timeEnd("bs_hashtbl_string_test.ml 168");

console.time("bs_hashtbl_string_test.ml 169");

bench6(/* () */0);

console.timeEnd("bs_hashtbl_string_test.ml 169");

console.time("bs_hashtbl_string_test.ml 170");

bench6(/* () */0);

console.timeEnd("bs_hashtbl_string_test.ml 170");

var count = 1000000;

var initial_size = 1000000;

exports.hash_string  = hash_string;
exports.hashString   = hashString;
exports.$$String     = $$String;
exports.String1      = String1;
exports.String2      = String2;
exports.Int          = Int;
exports.empty        = empty;
exports.bench        = bench;
exports.count        = count;
exports.initial_size = initial_size;
exports.bench2       = bench2;
exports.bench3       = bench3;
exports.S            = S;
exports.bench4       = bench4;
exports.bench5       = bench5;
exports.bench6       = bench6;
/* hashString Not a pure module */
