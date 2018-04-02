'use strict';


function test(x) {
  return x === undefined;
}

function testAny(x) {
  return x === undefined;
}

function getExn(f) {
  if (f !== undefined) {
    return f;
  } else {
    throw new Error("Js.Undefined.getExn");
  }
}

function bind(x, f) {
  if (x !== undefined) {
    return f(x);
  } else {
    return undefined;
  }
}

function iter(x, f) {
  if (x !== undefined) {
    return f(x);
  } else {
    return /* () */0;
  }
}

function fromOption(x) {
  if (x) {
    return x[0];
  } else {
    return undefined;
  }
}

var from_opt = fromOption;

exports.test = test;
exports.testAny = testAny;
exports.getExn = getExn;
exports.bind = bind;
exports.iter = iter;
exports.fromOption = fromOption;
exports.from_opt = from_opt;
/* No side effect */
