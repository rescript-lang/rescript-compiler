'use strict';
define(["exports"],
  function(exports){
    'use strict';
    function bind(x, f) {
      if (x !== undefined) {
        return f(x);
      }
      else {
        return undefined;
      }
    }
    
    exports.bind = bind;
    
  })
/* No side effect */
