'use strict';


function ff(x) {
  if (x >= 123 || x < 0) {
    return 2;
  } else {
    return 1;
  }
}

exports.ff = ff;
/* No side effect */
