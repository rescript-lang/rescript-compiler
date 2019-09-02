'use strict';


function f9(param) {
  if (typeof param === "string") {
    if (param === "T63") {
      return 3;
    } else {
      return 1;
    }
  } else {
    switch (/* XXX */param.tag) {
      case "T64" :
      case "T65" :
          return 2;
      case "T66" :
      case "T68" :
          return 3;
      
    }
  }
}

exports.f9 = f9;
/* No side effect */
