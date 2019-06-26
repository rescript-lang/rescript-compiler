'use strict';


function f9(param) {
  if (typeof param === "number") {
    switch (param) {
      case 0 : 
      case 1 : 
      case 2 : 
          return 1;
      default:
        return 3;
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
      case 1 : 
          return 2;
      default:
        return 3;
    }
  }
}

exports.f9 = f9;
/* No side effect */
