'use strict';


function fake_c2(a_type, b_type) {
  var exit = 0;
  switch (a_type) {
    case "number" : 
        if (b_type === "number") {
          return 33;
        } else {
          exit = 1;
        }
    case "string" : 
        return 1;
    case "undefined" : 
        return -1;
    default:
      exit = 1;
  }
  if (exit === 1) {
    if (b_type === "undefined") {
      return 1;
    } else if (a_type === "number") {
      return 3;
    } else {
      return 0;
    }
  }
  
}

console.log(String(fake_c2("number", "xx")));

exports.fake_c2 = fake_c2;
/*  Not a pure module */
