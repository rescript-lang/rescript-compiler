'use strict';


function map(f, param) {
  if (typeof param === "string") {
    return "Nil";
  }
  var match = param.VAL;
  return {
          NAME: "Cons",
          VAL: [
            f(match[0]),
            map(f, match[1])
          ]
        };
}

function split_cases(x) {
  if (typeof x === "string" || x.NAME !== "Snoc") {
    return {
            NAME: "A",
            VAL: x
          };
  } else {
    return {
            NAME: "B",
            VAL: x
          };
  }
}

function f(param) {
  if (typeof param === "string") {
    return "Tag3";
  } else {
    return "myvariant";
  }
}

function g1(param) {
  if (param.NAME === "Tag2") {
    return "Tag2";
  } else {
    return "Tag1";
  }
}

function g(x) {
  if (typeof x === "string") {
    return "Tag3";
  } else {
    return g1(x);
  }
}

function f1(param) {
  switch (param) {
    case "A" :
    case "B" :
    case "C" :
        return "other";
    case "As" :
        return "A";
    
  }
}

function f2(x) {
  if (typeof x === "string") {
    if (x === "h") {
      return 2;
    } else if (x === "hello") {
      return 3;
    } else if (x === "Tag4") {
      console.log(x);
      return 2;
    } else {
      return 333;
    }
  } else {
    console.log(x);
    return 2;
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
