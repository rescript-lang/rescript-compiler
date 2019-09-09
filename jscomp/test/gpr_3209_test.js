'use strict';


function f9(param) {
  if (typeof param === "number") {
    if (param === /* T63 */3) {
      return 3;
    } else {
      return 1;
    }
  } else {
    switch (param.tag | 0) {
      case /* T64 */0 :
      case /* T65 */1 :
          return 2;
      case /* T66 */2 :
      case /* T68 */3 :
          return 3;
      
    }
  }
}

exports.f9 = f9;
/* No side effect */
