'use strict';

var Bs_SetInt               = require("../../lib/js/bs_SetInt.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function bench() {
  var data = Bs_SetInt.empty;
  for(var i = 0; i <= 1000000; ++i){
    data = Bs_SetInt.add(i, data);
  }
  for(var i$1 = 0; i$1 <= 1000000; ++i$1){
    if (!Bs_SetInt.mem(i$1, data)) {
      throw [
            Caml_builtin_exceptions.assert_failure,
            [
              "bs_set_bench.ml",
              12,
              4
            ]
          ];
    }
    
  }
  for(var i$2 = 0; i$2 <= 1000000; ++i$2){
    data = Bs_SetInt.remove(i$2, data);
  }
  if (Bs_SetInt.cardinal(data)) {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "bs_set_bench.ml",
            17,
            2
          ]
        ];
  } else {
    return 0;
  }
}

console.time("bs_set_bench.ml 21");

bench(/* () */0);

console.timeEnd("bs_set_bench.ml 21");

var count = 1000000;

exports.count = count;
exports.bench = bench;
/*  Not a pure module */
