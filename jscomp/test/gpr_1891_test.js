'use strict';


function foo(x) {
  if (typeof x === "number") {
    console.log("2");
    return /* () */0;
  } else if (x[0] !== 3505894) {
    console.log("2");
    return /* () */0;
  } else if (x[1] !== 3) {
    console.log("2");
    return /* () */0;
  } else {
    console.log("1");
    return /* () */0;
  }
}

function foo2(x) {
  if (typeof x === "number" || x[0] !== 3505894 || x[1] !== 3) {
    return "xxx";
  } else {
    return "xxxx";
  }
}

function foo3(x) {
  if (typeof x === "number" || x[0] !== 3505894 || x[1] !== 3) {
    return 2;
  } else {
    return 1;
  }
}

exports.foo  = foo;
exports.foo2 = foo2;
exports.foo3 = foo3;
/* No side effect */
