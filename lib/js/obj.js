'use strict';


function is_block(a) {
  return typeof a !== "number";
}

exports.is_block = is_block;
/* No side effect */
