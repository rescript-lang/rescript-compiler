'use strict';


function compare(s1, s2) {
  if (s1 === s2) {
    return 0;
  } else if (s1 < s2) {
    return -1;
  } else {
    return 1;
  }
}

function min(x, y) {
  if (x < y) {
    return x;
  } else {
    return y;
  }
}

function max(x, y) {
  if (x > y) {
    return x;
  } else {
    return y;
  }
}

function get(s, i) {
  if (i >= s.length || i < 0) {
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "index out of bounds",
      Error: new Error()
    };
  }
  return s.codePointAt(i);
}

function make(n, ch) {
  return String.fromCharCode(ch).repeat(n);
}

exports.compare = compare;
exports.min = min;
exports.max = max;
exports.get = get;
exports.make = make;
/* No side effect */