'use strict';


function f(x) {
  if (x === "A") {
    return "A";
  } else {
    return "B";
  }
}

var ff = "B";

console.log([
      f("A"),
      ff,
      "A"
    ]);

/*  Not a pure module */
