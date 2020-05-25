'use strict';

var $$Node = require("../../lib/js/node.js");

function f(str) {
  var match = $$Node.test(str);
  if (match[0]) {
    console.log([
          "buffer",
          Buffer.isBuffer(match[1])
        ]);
  } else {
    console.log([
          "string",
          match[1]
        ]);
  }
  
}

f("xx");

f((Buffer.from ('xx')));

exports.f = f;
/*  Not a pure module */
