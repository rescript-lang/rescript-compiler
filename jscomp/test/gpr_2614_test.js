'use strict';


var v = {
  "Content-Type": 3,
  l: 2,
  open: 2
};

var a = v["Content-Type"];

var b = v.l;

var c = v.open;

function ff() {
  v["Content-Type"] = 3;
  v.l = 2;
  return /* () */0;
}

exports.v = v;
exports.a = a;
exports.b = b;
exports.c = c;
exports.ff = ff;
/* a Not a pure module */
