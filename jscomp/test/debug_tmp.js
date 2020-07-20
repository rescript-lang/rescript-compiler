'use strict';


function u(x) {
  if (x === /* h */104 || x === /* f */102 || x === /* e */101) {
    console.log("v");
    console.log(x);
  } else {
    console.log("u");
    console.log(x);
  }
  
}

exports.u = u;
/* No side effect */
