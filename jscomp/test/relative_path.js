'use strict';

var FileJs = require("./File.js");

var foo = FileJs.foo;

function foo2(prim) {
  return FileJs.foo2(prim);
}

var bar = foo;

exports.foo = foo;
exports.foo2 = foo2;
exports.bar = bar;
/* foo Not a pure module */
