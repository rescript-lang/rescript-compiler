'use strict';


function f(x) {
  switch (x) {
    case 0 : 
        return /* X1 */0;
    case 1 : 
        return /* X2 */1;
    case 2 : 
        return /* X3 */2;
    case 3 : 
        return /* X4 */3;
    
  }
}

function f2(x) {
  switch (x) {
    case 0 : 
        return /* X1 */0;
    case 1 : 
        return /* X2 */1;
    case 2 : 
        return /* X3 */2;
    case 3 : 
        return /* X4 */3;
    
  }
}

exports.f  = f;
exports.f2 = f2;
/* No side effect */
