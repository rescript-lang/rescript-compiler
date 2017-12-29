'use strict';


function test_v(x) {
  return x.hey(1, 2);
}

function test_vv(h) {
  var hey = h.hey;
  return hey(1, 2);
}

exports.test_v = test_v;
exports.test_vv = test_vv;
/* No side effect */
