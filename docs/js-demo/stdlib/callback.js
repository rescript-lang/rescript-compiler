'use strict';
define(["exports", "./obj.js"],
  function(exports, Obj){
    'use strict';
    function register(_, _$1) {
      return /* () */0;
    }
    
    function register_exception(_, exn) {
      (exn.tag | 0) === Obj.object_tag ? exn : exn[0];
      return /* () */0;
    }
    
    exports.register           = register;
    exports.register_exception = register_exception;
    
  })
/* No side effect */
