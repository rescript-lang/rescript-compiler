'use strict';


function f(xs) {
  if (xs !== undefined) {
    console.log("side effect");
    
  } else {
    
  }
  console.log("nothing to see here", xs);
  
}

exports.f = f;
/* No side effect */
