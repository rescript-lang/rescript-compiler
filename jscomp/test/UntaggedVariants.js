'use strict';


function classify(x) {
  if (x.TAG === "I") {
    return "An integer";
  } else {
    return "A string";
  }
}

var i = {
  TAG: "I",
  _0: 42
};

var s = {
  TAG: "S",
  _0: "abc"
};

exports.i = i;
exports.s = s;
exports.classify = classify;
/* No side effect */
