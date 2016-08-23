'use strict';
define(["exports"],
  function(exports){
    'use strict';
    function js_is_nil_undef(x) {
      if (x === null) {
        return /* true */1;
      }
      else {
        return +(x === undefined);
      }
    }
    
    function js_from_nullable_def(x) {
      if (x === null || x === undefined) {
        return /* None */0;
      }
      else {
        return /* Some */[x];
      }
    }
    
    function option_get(x) {
      if (x) {
        return x[0];
      }
      else {
        return undefined;
      }
    }
    
    exports.js_is_nil_undef      = js_is_nil_undef;
    exports.js_from_nullable_def = js_from_nullable_def;
    exports.option_get           = option_get;
    
  })
/* No side effect */
