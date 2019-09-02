'use strict';


function map(f, param) {
  if (param !== "[]") {
    var r = f(param.Arg0);
    return /* constructor */{
            tag: "::",
            Arg0: r,
            Arg1: map(f, param.Arg1)
          };
  } else {
    return "[]";
  }
}

exports.map = map;
/* No side effect */
