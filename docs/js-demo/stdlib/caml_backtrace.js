'use strict';
define(["exports", "./caml_builtin_exceptions.js"],
  function(exports, Caml_builtin_exceptions){
    'use strict';
    function caml_convert_raw_backtrace_slot() {
      throw [
            Caml_builtin_exceptions.failure,
            "caml_convert_raw_backtrace_slot unimplemented"
          ];
    }
    
    exports.caml_convert_raw_backtrace_slot = caml_convert_raw_backtrace_slot;
    
  })
/* No side effect */
