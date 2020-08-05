'use strict';


function caml_is_printable(c) {
  if (c > 31) {
    return c < 127;
  } else {
    return false;
  }
}

exports.caml_is_printable = caml_is_printable;
/* No side effect */
