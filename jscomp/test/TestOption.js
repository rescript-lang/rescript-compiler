'use strict';


function test1(r) {
  return r.f.f;
}

function getF2(r2) {
  return r2.f2;
}

function test2(r2) {
  return r2.f2.f2;
}

exports.test1 = test1;
exports.getF2 = getF2;
exports.test2 = test2;
/* No side effect */
