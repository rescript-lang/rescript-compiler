'use strict';


function get(s, i) {
  if (i >= s.length || i < 0) {
    throw {
          ExceptionID: "Invalid_argument",
          _1: "index out of bounds"
        };
  }
  return s.charCodeAt(i);
}

exports.get = get;
/* No side effect */
