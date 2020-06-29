'use strict';


function f(xs) {
  if (xs !== undefined) {
    console.log("side effect");
    ({
        hd: xs,
        tl: {
          hd: xs,
          tl: /* [] */0
        }
      });
  } else {
    ({
        hd: 1,
        tl: {
          hd: 2,
          tl: /* [] */0
        }
      });
  }
  console.log("nothing to see here", xs);
  
}

exports.f = f;
/* No side effect */
