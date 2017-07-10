'use strict';
define(["exports", "./caml_builtin_exceptions.js"],
  function(exports, Caml_builtin_exceptions){
    'use strict';
    function get(s, i) {
      if (i < 0 || i >= s.length) {
        throw [
              Caml_builtin_exceptions.invalid_argument,
              "index out of bounds"
            ];
      } else {
        return s[i];
      }
    }
    
    exports.get = get;
    
  })
/* No side effect */
