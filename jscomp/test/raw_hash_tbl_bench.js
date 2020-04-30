'use strict';

var Hashtbl = require("../../lib/js/hashtbl.js");

function bench(param) {
  var table = Hashtbl.create(undefined, 1000000);
  for(var i = 0; i <= 1000000; ++i){
    Hashtbl.add(table, i, i);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Hashtbl.mem(table, i$1)) {
      throw {
            ExceptionID: -9,
            _1: /* tuple */[
              "raw_hash_tbl_bench.ml",
              9,
              4
            ],
            Debug: "Assert_failure"
          };
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    Hashtbl.remove(table, i$2);
  }
  
}

bench(undefined);

var count = 1000000;

exports.count = count;
exports.bench = bench;
/*  Not a pure module */
