// GENERATED CODE BY BUCKLESCRIPT VERSION 0.8.1 , PLEASE EDIT WITH CARE
'use strict';

var Caml_int32 = require("../caml_int32");

var r = 0;

for(var k = 1; k <= 10; ++k){
  for(var i = 1; i <= 10; ++i){
    var match = i % 2 ? /* tuple */[
        2,
        Caml_int32.imul(i, 3)
      ] : /* tuple */[
        1,
        (i << 1)
      ];
    r = Caml_int32.imul(r, match[0]) + match[1] | 0;
  }
}

/*  Not a pure module */
