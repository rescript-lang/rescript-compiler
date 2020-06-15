'use strict';


function f(x) {
  var match = x._0;
  if (match !== undefined) {
    if (typeof match === "number") {
      if (match === /* c */99) {
        return 2;
      } else {
        return 0;
      }
    } else if (match.HASH === /* e */101) {
      return 4;
    } else {
      return 3;
    }
  } else {
    return 1;
  }
}

exports.f = f;
/* No side effect */
