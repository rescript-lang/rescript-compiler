'use strict';


function ff(x) {
  x.case(3, 2);
  return x.case(3);
}

function h(x) {
  return x.cse(3)(2);
}

function f_ext(x) {
  x.cse(3, 2);
  return x.cse(3);
}

function h_ext(x) {
  return x.cse(3)(2);
}

exports.ff = ff;
exports.h = h;
exports.f_ext = f_ext;
exports.h_ext = h_ext;
/* No side effect */
