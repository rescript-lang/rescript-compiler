'use strict';


function fff(x) {
  return x.height = 2;
}

function ff(x, z) {
  return /* :: */[
          x.height,
          /* :: */[
            z.height,
            /* [] */0
          ]
        ];
}

exports.fff = fff;
exports.ff = ff;
/* No side effect */
