// Generated CODE, PLEASE EDIT WITH CARE
'use strict';


function has(t, mask) {
  return +((t & mask) === mask);
}

function add(x, y) {
  return x | y;
}

var DocumentPosition = [
  1,
  2,
  4,
  8,
  16,
  32,
  has,
  add,
  add
];

exports.DocumentPosition = DocumentPosition;
/* No side effect */
