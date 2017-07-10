'use strict';
define(["exports", "./caml_obj.js", "./caml_array.js", "./camlinternalOO.js", "./caml_builtin_exceptions.js"],
  function(exports, Caml_obj, Caml_array, CamlinternalOO, Caml_builtin_exceptions){
    'use strict';
    function init_mod(loc, shape) {
      var undef_module = function () {
        throw [
              Caml_builtin_exceptions.undefined_recursive_module,
              loc
            ];
      };
      var loop = function (shape, struct_, idx) {
        if (typeof shape === "number") {
          switch (shape) {
            case 0 : 
            case 1 : 
                return Caml_array.caml_array_set(struct_, idx, undef_module);
            case 2 : 
                return Caml_array.caml_array_set(struct_, idx, CamlinternalOO.dummy_class(loc));
            
          }
        } else if (shape.tag) {
          return Caml_array.caml_array_set(struct_, idx, shape[0]);
        } else {
          var comps = shape[0];
          var v = /* array */[];
          Caml_array.caml_array_set(struct_, idx, v);
          var len = comps.length;
          for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
            loop(Caml_array.caml_array_get(comps, i), v, i);
          }
          return /* () */0;
        }
      };
      var res = /* array */[];
      loop(shape, res, 0);
      return Caml_array.caml_array_get(res, 0);
    }
    
    function update_mod(shape, o, n) {
      var aux = function (shape, o, n, parent, i) {
        if (typeof shape === "number") {
          switch (shape) {
            case 0 : 
                parent[i] = n;
                return /* () */0;
            case 1 : 
            case 2 : 
                return Caml_obj.caml_update_dummy(o, n);
            
          }
        } else if (shape.tag) {
          return /* () */0;
        } else {
          var comps = shape[0];
          for(var i$1 = 0 ,i_finish = comps.length - 1 | 0; i$1 <= i_finish; ++i$1){
            aux(Caml_array.caml_array_get(comps, i$1), o[i$1], n[i$1], o, i$1);
          }
          return /* () */0;
        }
      };
      if (typeof shape === "number") {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "camlinternalMod.ml",
                122,
                10
              ]
            ];
      } else if (shape.tag) {
        throw [
              Caml_builtin_exceptions.assert_failure,
              [
                "camlinternalMod.ml",
                122,
                10
              ]
            ];
      } else {
        var comps = shape[0];
        for(var i = 0 ,i_finish = comps.length - 1 | 0; i <= i_finish; ++i){
          aux(Caml_array.caml_array_get(comps, i), o[i], n[i], o, i);
        }
        return /* () */0;
      }
    }
    
    exports.init_mod   = init_mod;
    exports.update_mod = update_mod;
    
  })
/* No side effect */
