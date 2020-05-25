'use strict';


function f(x) {
  if (!(x > 3 || x < 1)) {
    return /* "a" */97;
  }
  throw {
        RE_EXN_ID: "Match_failure",
        _1: [
          "test_incomplete.ml",
          3,
          2
        ],
        Error: new Error()
      };
}

function f2(x) {
  if (x !== undefined) {
    return 0;
  } else {
    return 1;
  }
}

function f3(x) {
  switch (x.TAG | 0) {
    case /* A */0 :
    case /* C */2 :
        return x._0 + 1 | 0;
    case /* B */1 :
    case /* D */3 :
        return x._0 + 2 | 0;
    
  }
}

exports.f = f;
exports.f2 = f2;
exports.f3 = f3;
/* No side effect */
