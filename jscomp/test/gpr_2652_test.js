'use strict';

var $$Buffer = require("../../lib/js/buffer.js");

var foo = $$Buffer.contents;

function bar(str) {
  return Buffer.from(str);
}

exports.foo = foo;
exports.bar = bar;
/* No side effect */
