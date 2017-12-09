'use strict';

var Bs_Map                  = require("../../lib/js/bs_Map.js");
var Bs_Hash                 = require("../../lib/js/bs_Hash.js");
var Hashtbl                 = require("../../lib/js/hashtbl.js");
var Caml_hash               = require("../../lib/js/caml_hash.js");
var Bs_HashMap              = require("../../lib/js/bs_HashMap.js");
var Caml_string             = require("../../lib/js/caml_string.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function hash_string(seed, s) {
  var hash = seed;
  hash = Caml_hash.caml_hash_mix_string(hash, s);
  return Caml_hash.caml_hash_final_mix(hash);
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
  return hash_string(0, x);
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

var empty = Bs_HashMap.create(Int, 500000);

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
              52,
              4
            ]
          ];
    }
    
  }
  return Bs_HashMap.logStats(empty);
}

function bench2(m) {
  var empty = Bs_HashMap.create(m, 500000);
  var hash = m[/* hash */0];
  var eq = m[/* eq */1];
  var table = empty[/* data */1];
  for(var i = 0; i <= 1000000; ++i){
    Bs_HashMap.add0(hash, table, "" + i, i);
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
  return /* () */0;
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
              93,
              4
            ]
          ];
    }
    
  }
  return /* () */0;
}

var S = /* module */[/* cmp */Caml_string.caml_string_compare];

console.time("bs_hashtbl_string_test.ml 100");

bench2(String1);

console.timeEnd("bs_hashtbl_string_test.ml 100");

console.time("bs_hashtbl_string_test.ml 101");

bench2($$String);

console.timeEnd("bs_hashtbl_string_test.ml 101");

console.time("bs_hashtbl_string_test.ml 102");

bench2(String2);

console.timeEnd("bs_hashtbl_string_test.ml 102");

console.time("bs_hashtbl_string_test.ml 103");

bench3(S);

console.timeEnd("bs_hashtbl_string_test.ml 103");

var count = 1000000;

exports.hash_string = hash_string;
exports.hashString  = hashString;
exports.$$String    = $$String;
exports.String1     = String1;
exports.String2     = String2;
exports.Int         = Int;
exports.empty       = empty;
exports.bench       = bench;
exports.count       = count;
exports.bench2      = bench2;
exports.bench3      = bench3;
exports.S           = S;
/* hashString Not a pure module */
