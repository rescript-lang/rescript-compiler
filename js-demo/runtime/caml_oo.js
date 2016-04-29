// GENERATED CODE BY BUCKLESCRIPT VERSION 0.3 , PLEASE EDIT WITH CARE
'use strict';
define(["exports", "./caml_builtin_exceptions", "./caml_array"],
  function(exports, Caml_builtin_exceptions, Caml_array){
    'use strict';
    var caml_methods_cache = Caml_array.caml_make_vect(1000, 0);
    
    function caml_get_public_method(obj, tag, cacheid) {
      var meths = obj[0];
      var offs = caml_methods_cache[cacheid];
      if (meths[offs] === tag) {
        return meths[offs - 1 | 0];
      }
      else {
        var aux = function (_i) {
          while(true) {
            var i = _i;
            if (i < 3) {
              throw [
                    Caml_builtin_exceptions.assert_failure,
                    [
                      "caml_oo.ml",
                      43,
                      20
                    ]
                  ];
            }
            else if (meths[i] === tag) {
              caml_methods_cache[cacheid] = i;
              return i;
            }
            else {
              _i = i - 2 | 0;
              continue ;
              
            }
          };
        };
        return meths[aux((meths[0] << 1) + 1 | 0) - 1 | 0];
      }
    }
    
    exports.caml_get_public_method = caml_get_public_method;
    
  })
/* No side effect */
