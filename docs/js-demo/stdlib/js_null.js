'use strict';
define(["exports"],
  function(exports){
    'use strict';
    function bind(x, f) {
      if (x !== null) {
        return f(x);
      }
      else {
        return null;
      }
    }
    
    exports.bind = bind;
    
  })
/* No side effect */
