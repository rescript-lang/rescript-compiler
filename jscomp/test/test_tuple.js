'use strict';

var Caml_int32 = require("../../lib/js/caml_int32.js");

var r = 0;

for(var k = 1; k <= 10; ++k){
  for(var i = 1; i <= 10; ++i){
    var match = i % 2 === 0 ? /* tuple */[
        1,
        (i << 1)
      ] : /* tuple */[
        2,
        Caml_int32.imul(i, 3)
      ];
    r = Caml_int32.imul(r, match[0]) + match[1] | 0;
  }
}

/*  Not a pure module */
