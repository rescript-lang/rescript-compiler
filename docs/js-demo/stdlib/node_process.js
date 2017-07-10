'use strict';
define(["exports", "./js_dict.js", "process"],
  function(exports, Js_dict, Process){
    'use strict';
    function putEnvVar(key, $$var) {
      Process.env[key] = $$var;
      return /* () */0;
    }
    
    function deleteEnvVar(s) {
      return Js_dict.unsafeDeleteKey(Process.env, s);
    }
    
    exports.putEnvVar    = putEnvVar;
    exports.deleteEnvVar = deleteEnvVar;
    
  })
/* Js_dict Not a pure module */
