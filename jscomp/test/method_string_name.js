'use strict';


function test(f) {
  f["Content-Type"] = "hehi";
  f.open(3);
  return f.open = 2;
}

exports.test = test;
/* No side effect */
