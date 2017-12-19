'use strict';


var v = {
  hd: 3,
  tl: null
};

v.tl = v;

var f = {
  k: (function (x, y) {
      return +(x === y);
    }),
  y: "x"
};

exports.v = v;
exports.f = f;
/* v Not a pure module */
