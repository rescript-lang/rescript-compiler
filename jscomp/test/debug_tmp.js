'use strict';


function u(x) {
  var exit = 0;
  exit = x === /* b */98 || x === /* c */99 || x === /* d */100 || !(x === /* e */101 || x === /* f */102 || x === /* h */104) ? 1 : 2;
  switch (exit) {
    case 1 :
        console.log("u");
        console.log(x);
        return ;
    case 2 :
        console.log("v");
        console.log(x);
        return ;
    
  }
}

exports.u = u;
/* No side effect */
