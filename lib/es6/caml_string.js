

import * as Caml_builtin_exceptions from "./caml_builtin_exceptions.js";

function get(s, i) {
  if (i >= s.length || i < 0) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "index out of bounds"
        ];
  }
  return s.charCodeAt(i);
}

export {
  get ,
  
}
/* No side effect */
