'use strict';

var $$Node = require("../../lib/js/node.js");

function f(str) {
  var match = $$Node.test(str);
  if (match[0]) {
    console.log(/* tuple */[
          "buffer",
          Buffer.isBuffer(match[1])
        ]);
    return ;
  } else {
    console.log(/* tuple */[
          "string",
          match[1]
        ]);
    return ;
  }
}

f("xx");

f((Buffer.from ('xx')));

exports.f = f;
/*  Not a pure module */
