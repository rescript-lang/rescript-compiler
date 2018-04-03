'use strict';


function test(x) {
  x.nodeValue = null;
  return /* () */0;
}

exports.test = test;
/* No side effect */
