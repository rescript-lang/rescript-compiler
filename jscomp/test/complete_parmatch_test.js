'use strict';


function f(x) {
  switch (x) {
    case 1 :
        return /* 'a' */97;
    case 2 :
        return /* 'b' */98;
    case 3 :
        return /* 'c' */99;
    default:
      return /* 'x' */120;
  }
}

exports.f = f;
/* No side effect */
