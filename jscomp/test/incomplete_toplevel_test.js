'use strict';


function f(param) {
  console.log("no inline");
  return [
          1,
          2,
          3
        ];
}

var match = f(undefined);

var a = match[0];

var b = match[1];

var c = match[2];

exports.f = f;
exports.a = a;
exports.b = b;
exports.c = c;
/* match Not a pure module */
