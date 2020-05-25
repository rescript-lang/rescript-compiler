'use strict';


console.log("list");

console.log("list");

function f(param) {
  if (param !== undefined) {
    return "Some";
  } else {
    return "None";
  }
}

console.log([
      f(3),
      "None",
      "Some"
    ]);

console.log([
      "A",
      "A"
    ]);

/*  Not a pure module */
