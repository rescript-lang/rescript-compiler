'use strict';


function f9(param) {
  if (typeof param === "number") {
    if (param === 3) {
      return 3;
    } else {
      return 1;
    }
  } else {
    switch (param.tag | 0) {
      case 0 : 
      case 1 : 
          return 2;
      case 2 : 
      case 3 : 
          return 3;
      
    }
  }
}

exports.f9 = f9;
/* No side effect */
