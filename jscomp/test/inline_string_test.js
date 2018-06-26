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

console.log(/* tuple */[
      f(3),
      "None",
      "Some"
    ]);

console.log(/* tuple */[
      "A",
      "A"
    ]);

/*  Not a pure module */
