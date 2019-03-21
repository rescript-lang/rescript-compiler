'use strict';


function fromString(i) {
  var i$1 = parseInt(i, 10);
  if (i$1 === Number.NaN) {
    return undefined;
  } else {
    return i$1;
  }
}

exports.fromString = fromString;
/* No side effect */
