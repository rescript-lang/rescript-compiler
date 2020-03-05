'use strict';

var Int64 = require("../../lib/js/int64.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");

console.time("Int64.to_string");

var u = Caml_int64.sub(Int64.max_int, /* int64 */[
      /* hi */0,
      /* lo */200000
    ]);

for(var i = 0; i <= 100000; ++i){
  Caml_int64.to_string(u);
}

console.timeEnd("Int64.to_string");

console.time("Int64.to_string");

for(var i$1 = 0; i$1 <= 100000; ++i$1){
  Caml_int64.to_string(/* int64 */[
        /* hi */0,
        /* lo */30000
      ]);
}

console.timeEnd("Int64.to_string");

/*  Not a pure module */
