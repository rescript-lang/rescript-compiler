'use strict';


function fff(x) {
  x.height = 2;
  return /* () */0;
}

function ff(x, z) {
  return /* constructor */{
          tag: "::",
          Arg0: x.height,
          Arg1: /* constructor */{
            tag: "::",
            Arg0: z.height,
            Arg1: "[]"
          }
        };
}

exports.fff = fff;
exports.ff = ff;
/* No side effect */
