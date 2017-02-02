'use strict';

import * as Caml_builtin_exceptions from "./caml_builtin_exceptions";

function get(s, i) {
  if (i < 0 || i >= s.length) {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "index out of bounds"
        ];
  }
  else {
    return s[i];
  }
}

export{
  get ,
  
}
/* No side effect */
