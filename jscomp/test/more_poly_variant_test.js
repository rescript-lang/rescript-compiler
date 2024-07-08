// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';


function map(f) {
  return function (x) {
    if (typeof x !== "object") {
      return "Nil";
    }
    let match = x.VAL;
    return {
      NAME: "Cons",
      VAL: [
        f(match[0]),
        map(f)(match[1])
      ]
    };
  };
}

function split_cases(x) {
  if (typeof x === "object" && x.NAME === "Snoc") {
    return {
      NAME: "B",
      VAL: x
    };
  } else {
    return {
      NAME: "A",
      VAL: x
    };
  }
}

function f(x) {
  if (typeof x === "object") {
    return "myvariant";
  } else {
    return "Tag3";
  }
}

function g1(x) {
  if (x.NAME === "Tag2") {
    return "Tag2";
  } else {
    return "Tag1";
  }
}

function g(x) {
  if (typeof x === "object") {
    return g1(x);
  } else {
    return "Tag3";
  }
}

function f1(x) {
  if (x === "As") {
    return "A";
  } else {
    return "other";
  }
}

function f2(x) {
  if (typeof x === "object") {
    console.log(x);
    return 2;
  } else if (x === "h") {
    return 2;
  } else if (x === "hello") {
    return 3;
  } else if (x === "Tag4") {
    console.log(x);
    return 2;
  } else {
    return 333;
  }
}

exports.map = map;
exports.split_cases = split_cases;
exports.f = f;
exports.g1 = g1;
exports.g = g;
exports.f1 = f1;
exports.f2 = f2;
/* No side effect */
