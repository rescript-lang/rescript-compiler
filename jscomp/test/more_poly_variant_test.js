'use strict';


function map(f, param) {
  if (typeof param === "number") {
    return /* Nil */3902385;
  }
  var match = param.VAL;
  return {
          HASH: /* Cons */748545553,
          VAL: [
            f(match[0]),
            map(f, match[1])
          ]
        };
}

function split_cases(x) {
  if (typeof x === "number" || x.HASH !== /* Snoc */925929103) {
    return {
            HASH: /* A */65,
            VAL: x
          };
  } else {
    return {
            HASH: /* B */66,
            VAL: x
          };
  }
}

function f(param) {
  if (typeof param === "number") {
    return "Tag3";
  } else {
    return "myvariant";
  }
}

function g1(param) {
  if (param.HASH === /* Tag2 */936370360) {
    return "Tag2";
  } else {
    return "Tag1";
  }
}

function g(x) {
  if (typeof x === "number") {
    return "Tag3";
  } else {
    return g1(x);
  }
}

function f1(param) {
  if (param === /* As */14610) {
    return "A";
  } else {
    return "other";
  }
}

function f2(x) {
  if (typeof x === "number") {
    if (x === /* h */104) {
      return 2;
    } else if (x === /* hello */616641298) {
      return 3;
    } else if (x === /* Tag4 */936370362) {
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
