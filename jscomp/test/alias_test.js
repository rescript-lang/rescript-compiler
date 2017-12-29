'use strict';


var a10 = "hello world";

var v = 1;

var a21 = "hello worldnothello worldnot";

function ff() {
  return "cool test hello worldnothello worldnothello";
}

var a23 = ff(/* () */0);

var a15 = a10;

var b15 = 111;

exports.a15 = a15;
exports.b15 = b15;
exports.a21 = a21;
exports.v = v;
exports.a23 = a23;
exports.ff = ff;
/* a23 Not a pure module */
