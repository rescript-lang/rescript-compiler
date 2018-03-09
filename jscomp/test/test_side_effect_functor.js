'use strict';


var v = 0;

v = v + 1 | 0;

console.log(String(v));

function unuse_v() {
  return 35;
}

var h = unuse_v;

exports.h = h;
/*  Not a pure module */
