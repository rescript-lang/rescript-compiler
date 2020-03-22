'use strict';


var o = {
  hi: (function (x, y) {
      return x + y | 0;
    })
};

exports.o = o;
/* o Not a pure module */
