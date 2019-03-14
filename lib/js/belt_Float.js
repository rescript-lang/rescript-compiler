'use strict';


function $plus(prim, prim$1) {
  return prim + prim$1;
}

function $neg(prim, prim$1) {
  return prim - prim$1;
}

function $slash(prim, prim$1) {
  return prim / prim$1;
}

function $star(prim, prim$1) {
  return prim * prim$1;
}

exports.$plus = $plus;
exports.$neg = $neg;
exports.$slash = $slash;
exports.$star = $star;
/* No side effect */
