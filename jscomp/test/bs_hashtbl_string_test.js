'use strict';

var Bs_Map = require("../../lib/js/bs_Map.js");
var Hashtbl = require("../../lib/js/hashtbl.js");
var Caml_hash = require("../../lib/js/caml_hash.js");
var Bs_HashMap = require("../../lib/js/bs_HashMap.js");
var Bs_HashMapInt = require("../../lib/js/bs_HashMapInt.js");
var Bs_HashSetInt = require("../../lib/js/bs_HashSetInt.js");
var Caml_primitive = require("../../lib/js/caml_primitive.js");
var Bs_HashMapString = require("../../lib/js/bs_HashMapString.js");
var Bs_internalAVLtree = require("../../lib/js/bs_internalAVLtree.js");
var Bs_internalBuckets = require("../../lib/js/bs_internalBuckets.js");
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

var empty = {
  dict: Int,
  data: Bs_internalBucketsType.create0(500000)
};

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
  return Bs_internalBuckets.logStats0(empty.data);
}

function bench2(m) {
  var empty = {
    dict: m,
    data: Bs_internalBucketsType.create0(1000000)
  };
  var hash = m[/* hash */0];
  var eq = m[/* eq */1];
  var table = empty.data;
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMap.add0(hash, eq, table, "" + i, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMap.mem0(hash, eq, table, "" + i$1)) {
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
    Bs_HashMap.remove0(hash, eq, table, "" + i$2);
  }
  if (Bs_HashMap.length0(table)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            83,
            2
          ]
        ];
  } else {
    return 0;
  }
}

function bench3(m) {
  var empty = {
    dict: m,
    data: Bs_internalAVLtree.empty0
  };
  var cmp = m[/* cmp */0];
  var table = empty.data;
  for(var i = 0; i <= 1000000; ++i){
    table = Bs_Map.set0(table, "" + i, i, cmp);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_Map.has0(table, "" + i$1, cmp)) {
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
    table = Bs_Map.remove0(table, "" + i$2, cmp);
  }
  if (Bs_Map.size0(table)) {
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

var Sx = /* module */[/* cmp */Caml_primitive.caml_string_compare];

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
              118,
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
            124,
            2
          ]
        ];
  } else {
    return 0;
  }
}

function bench5() {
  var table = {
    dict: Int,
    data: Bs_internalBucketsType.create0(1000000)
  };
  var table_data = table.data;
  var hash = Int_000;
  var eq = Int_001;
  console.time("bs_hashtbl_string_test.ml 132");
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMap.add0(hash, eq, table_data, i, i);
  }
  console.timeEnd("bs_hashtbl_string_test.ml 132");
  console.time("bs_hashtbl_string_test.ml 136");
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashMap.mem0(hash, eq, table_data, i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              137,
              6
            ]
          ];
    }
    
  }
  console.timeEnd("bs_hashtbl_string_test.ml 136");
  console.time("bs_hashtbl_string_test.ml 140");
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashMap.remove0(hash, eq, table_data, i$2);
  }
  console.timeEnd("bs_hashtbl_string_test.ml 140");
  if (table.data.size) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            143,
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
              154,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashMapInt.remove(table, i$2);
  }
  if (Bs_HashMapInt.length(table)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            160,
            2
          ]
        ];
  } else {
    return 0;
  }
}

function bench7() {
  var table = Bs_HashSetInt.create(2000000);
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashSetInt.add(table, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_HashSetInt.has(table, i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_hashtbl_string_test.ml",
              179,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Bs_HashSetInt.remove(table, i$2);
  }
  if (Bs_HashSetInt.size(table)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_hashtbl_string_test.ml",
            190,
            2
          ]
        ];
  } else {
    return 0;
  }
}

console.time("bs_hashtbl_string_test.ml 201");

bench7(/* () */0);

console.timeEnd("bs_hashtbl_string_test.ml 201");

var count = 1000000;

var initial_size = 1000000;

var M = 0;

var S = 0;

exports.hash_string = hash_string;
exports.hashString = hashString;
exports.$$String = $$String;
exports.String1 = String1;
exports.String2 = String2;
exports.Int = Int;
exports.empty = empty;
exports.bench = bench;
exports.count = count;
exports.initial_size = initial_size;
exports.M = M;
exports.bench2 = bench2;
exports.bench3 = bench3;
exports.Sx = Sx;
exports.bench4 = bench4;
exports.bench5 = bench5;
exports.bench6 = bench6;
exports.S = S;
exports.bench7 = bench7;
/* hashString Not a pure module */
