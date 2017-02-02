'use strict';

import * as Caml_builtin_exceptions from "./caml_builtin_exceptions";

function caml_convert_raw_backtrace_slot() {
  throw [
        Caml_builtin_exceptions.failure,
        "caml_convert_raw_backtrace_slot unimplemented"
      ];
}

export {
  caml_convert_raw_backtrace_slot ,
  
}
/* No side effect */
