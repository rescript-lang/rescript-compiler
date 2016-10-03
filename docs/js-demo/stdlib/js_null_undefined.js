'use strict';
define(["exports", "./js_primitive"],
  function(exports, Js_primitive){
    'use strict';
    function bind(x, f) {
      if (Js_primitive.js_is_nil_undef(x)) {
        return undefined;
      }
      else {
        return f(x);
      }
    }
    
    exports.bind = bind;
    
  })
/* No side effect */
