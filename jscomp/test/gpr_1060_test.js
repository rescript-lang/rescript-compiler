'use strict';

var Foo = require("foo.react");
var A   = require("react");

var v = A;

var h = A.bool();

var c = Foo;

var d = Foo.bar();

exports.v = v;
exports.h = h;
exports.c = c;
exports.d = d;
/* v Not a pure module */
