'use strict';


function raiseWhenNotFound(x) {
  if (x == null) {
    throw new Error("Not_found", {
      cause: {
        RE_EXN_ID: "Not_found"
      }
    });
  }
  return x;
}

exports.raiseWhenNotFound = raiseWhenNotFound;
/* No side effect */
