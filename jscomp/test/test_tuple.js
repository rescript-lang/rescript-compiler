// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var r = 0;

for(var k = 1; k<= 10; ++k){
  for(var i = 1; i<= 10; ++i){
    var match = i % 2 ? [
        /* tuple */0,
        2,
        i * 3
      ] : [
        /* tuple */0,
        1,
        i * 2
      ];
    r = r * match[1] + match[2];
  }
}

/*  Not a pure module */
