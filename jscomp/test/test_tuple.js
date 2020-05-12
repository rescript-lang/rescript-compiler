'use strict';


var r = 0;

for(var k = 1; k <= 10; ++k){
  for(var i = 1; i <= 10; ++i){
    var match = i % 2 === 0 ? /* tuple */[
        1,
        (i << 1)
      ] : /* tuple */[
        2,
        Math.imul(i, 3)
      ];
    r = Math.imul(r, match[0]) + match[1] | 0;
  }
}

/*  Not a pure module */
