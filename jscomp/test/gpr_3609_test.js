'use strict';


function func(state) {
  if (typeof state === "string") {
    return 0;
  } else {
    return 0 + state.Arg0 | 0;
  }
}

exports.func = func;
/* No side effect */
