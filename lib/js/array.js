'use strict';


function length(prim) {
  return prim.length;
}

function unsafe_get(prim0, prim1) {
  return prim0[prim1];
}

exports.length = length;
exports.unsafe_get = unsafe_get;
/* No side effect */
