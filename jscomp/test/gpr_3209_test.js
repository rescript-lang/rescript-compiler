'use strict';


function f9(param) {
  if (typeof param === "string") {
    switch (param) {
      case "T60" :
      case "T61" :
      case "T62" :
          return 1;
      default:
        return 3;
    }
  } else {
    switch (param.TAG) {
      case "T64" :
      case "T65" :
          return 2;
      default:
        return 3;
    }
  }
}

exports.f9 = f9;
/* No side effect */
