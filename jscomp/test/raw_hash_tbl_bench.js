'use strict';

var Hashtbl = require("../../lib/js/hashtbl.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function bench(param) {
  var table = Hashtbl.create(undefined, 1000000);
  for(var i = 0; i <= 1000000; ++i){
    Hashtbl.add(table, i, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Hashtbl.mem(table, i$1)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            /* tuple */[
              "raw_hash_tbl_bench.ml",
              9,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Hashtbl.remove(table, i$2);
  }
  return /* () */0;
}

bench(/* () */0);

var count = 1000000;

exports.count = count;
exports.bench = bench;
/*  Not a pure module */
