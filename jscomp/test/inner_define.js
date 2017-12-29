'use strict';


function add(x, y) {
  return x + y | 0;
}

var N = /* module */[/* add */add];

function f1() {
  return /* () */0;
}

function f2(_, _$1) {
  return /* () */0;
}

function f3(_, _$1, _$2) {
  return /* () */0;
}

var N0 = /* module */[
  /* f1 */f1,
  /* f2 */f2,
  /* f3 */f3
];

function f2$1(_, _$1) {
  return /* () */0;
}

function f3$1(_, _$1, _$2) {
  return /* () */0;
}

var N1 = [
  f2$1,
  f3$1
];

exports.N = N;
exports.N0 = N0;
exports.N1 = N1;
/* No side effect */
