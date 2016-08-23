'use strict';
define(["exports", "./caml_obj", "./caml_array"],
  function(exports, Caml_obj, Caml_array){
    'use strict';
    function caml_weak_create(n) {
      return new Array(n);
    }
    
    function caml_weak_set(xs, i, v) {
      if (v) {
        xs[i] = v[0];
        return /* () */0;
      }
      else {
        return /* () */0;
      }
    }
    
    function caml_weak_get(xs, i) {
      v = xs[i];
      if (v === undefined) {
        return /* None */0;
      }
      else {
        return [v];
      }
    }
    
    function caml_weak_get_copy(xs, i) {
      var match = xs[i];
      if (match !== undefined) {
        return /* Some */[Caml_obj.caml_obj_dup(match)];
      }
      else {
        return /* None */0;
      }
    }
    
    function caml_weak_check(xs, i) {
      return xs[i] !== undefined;
    }
    
    var caml_weak_blit = Caml_array.caml_array_blit;
    
    exports.caml_weak_create   = caml_weak_create;
    exports.caml_weak_set      = caml_weak_set;
    exports.caml_weak_get      = caml_weak_get;
    exports.caml_weak_get_copy = caml_weak_get_copy;
    exports.caml_weak_check    = caml_weak_check;
    exports.caml_weak_blit     = caml_weak_blit;
    
  })
/* No side effect */
