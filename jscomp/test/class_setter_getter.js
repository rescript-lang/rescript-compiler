'use strict';


function fff(x) {
  x.height = 2;
  
}

function ff(x, z) {
  return {
          hd: x.height,
          tl: {
            hd: z.height,
            tl: /* [] */0
          }
        };
}

exports.fff = fff;
exports.ff = ff;
/* No side effect */
