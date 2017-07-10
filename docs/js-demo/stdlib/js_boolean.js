'use strict';
define(["exports"],
  function(exports){
    'use strict';
    function to_js_boolean(b) {
      if (b) {
        return true;
      } else {
        return false;
      }
    }
    
    exports.to_js_boolean = to_js_boolean;
    
  })
/* No side effect */
