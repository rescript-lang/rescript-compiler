'use strict';


function f9(param) {
  if (typeof param === "number") {
    switch (param) {
      case /* T60 */0 :
      case /* T61 */1 :
      case /* T62 */2 :
          return 1;
      default:
        return 3;
    }
  } else {
    switch (param.TAG | 0) {
      case /* T64 */0 :
      case /* T65 */1 :
          return 2;
      default:
        return 3;
    }
  }
}

exports.f9 = f9;
/* No side effect */
