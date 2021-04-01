'use strict';


function ff(param) {
  if (param >= 123 || param < 0) {
    return 2;
  } else {
    return 1;
  }
}

exports.ff = ff;
/* No side effect */
