'use strict';


function foo(x) {
  if (x === "\"") {
    return "\"";
  } else {
    return "";
  }
}

function bar(x) {
  switch (x) {
    case "\\" :
        return "\\";
    case "😀" :
        return "😀";
    default:
      return "";
  }
}

var s = "😀";

exports.foo = foo;
exports.s = s;
exports.bar = bar;
/* No side effect */
