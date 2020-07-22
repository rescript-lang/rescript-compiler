'use strict';


function f(x) {
  if (x === "b") {
    return "b";
  } else if (x === "c") {
    return "c";
  } else {
    return "a";
  }
}

function ff(x) {
  switch (x) {
    case "a" :
        return "a";
    case "b" :
        return "b";
    case "c" :
        return "c";
    default:
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "bb.ml",
              17,
              9
            ],
            Error: new Error()
          };
  }
}

function test(x) {
  var match;
  switch (x) {
    case "a" :
        match = "a";
        break;
    case "b" :
        match = "b";
        break;
    case "c" :
        match = "c";
        break;
    default:
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "bb.ml",
              26,
              13
            ],
            Error: new Error()
          };
  }
  if (match === "b") {
    return "b";
  } else if (match === "c") {
    return "c";
  } else {
    return "a";
  }
}

var test_poly = "a";

var c = f("a");

var d = f("b");

var e = f("c");

exports.f = f;
exports.ff = ff;
exports.test = test;
exports.test_poly = test_poly;
exports.c = c;
exports.d = d;
exports.e = e;
/* c Not a pure module */
