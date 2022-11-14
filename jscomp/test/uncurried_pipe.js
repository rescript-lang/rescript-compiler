'use strict';


function add(x, y) {
  return x + y | 0;
}

function addC(x, y) {
  return x + y | 0;
}

var v7 = add(3, 4);

var v17 = add(10, add(3, 4));

var v27 = add(20, 7);

var v37 = 30 + add(3, 4) | 0;

function unary(x) {
  return x + 1 | 0;
}

var StandardNotation = {
  add: add,
  addC: addC,
  v7: v7,
  v17: v17,
  v27: v27,
  v37: v37,
  unary: unary
};

var v7$1 = add(3, 4);

var v17$1 = add(10, add(3, 4));

var v27$1 = add(20, 7);

var v37$1 = 30 + add(3, 4) | 0;

var v100 = unary(99);

exports.StandardNotation = StandardNotation;
exports.v7 = v7$1;
exports.v17 = v17$1;
exports.v27 = v27$1;
exports.v37 = v37$1;
exports.v100 = v100;
/* v7 Not a pure module */
