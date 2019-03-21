'use strict';


function fromString(i) {
  var i$1 = parseFloat(i);
  if (isNaN(i$1)) {
    return undefined;
  } else {
    return i$1;
  }
}

exports.fromString = fromString;
/* No side effect */
