'use strict';

var Caml_int32 = require("../../lib/js/caml_int32.js");

var r = 0;

for(var k = 1; k <= 10; ++k){
  for(var i = 1; i <= 10; ++i){
    if (i % 2 === 0) {
      var y = (i << 1);
      var y$1 = y;
      var x = 1;
      r = Caml_int32.imul(r, x) + y$1 | 0;
    } else {
      var y$2 = Caml_int32.imul(i, 3);
      var y$3 = y$2;
      var x$1 = 2;
      r = Caml_int32.imul(r, x$1) + y$3 | 0;
    }
  }
}

/*  Not a pure module */
