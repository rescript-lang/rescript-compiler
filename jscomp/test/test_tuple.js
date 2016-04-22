// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_primitive = require("../runtime/caml_primitive");

var r = 0;

for(var k = 1; k <= 10; ++k){
  for(var i = 1; i <= 10; ++i){
    var match = i % 2 ? /* tuple */[
        2,
        Caml_primitive.imul(i, 3)
      ] : /* tuple */[
        1,
        (i << 1)
      ];
    r = Caml_primitive.imul(r, match[0]) + match[1] | 0;
  }
}

/*  Not a pure module */
