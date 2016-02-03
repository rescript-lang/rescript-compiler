// Generated CODE, PLEASE EDIT WITH CARE
'use strict';
define(["../runtime/caml_obj_runtime","./obj","./camlinternalLazy"],
  function(Caml_obj_runtime,Obj,CamlinternalLazy){
    'use strict';
    function from_fun(f) {
      var x = Caml_obj_runtime.caml_obj_block(Obj.lazy_tag, 1);
      x[0] = f;
      return x;
    }
    
    function from_val(v) {
      var t = Caml_obj_runtime.caml_obj_tag(v);
      if (t === Obj.forward_tag || t === Obj.lazy_tag || t === Obj.double_tag) {
        return Caml_obj_runtime.caml_lazy_make_forward(v);
      }
      else {
        return v;
      }
    }
    
    function is_val(l) {
      return +(Caml_obj_runtime.caml_obj_tag(l) !== Obj.lazy_tag);
    }
    
    var Undefined = CamlinternalLazy.Undefined;
    
    var force_val = CamlinternalLazy.force_val;
    
    var lazy_from_fun = from_fun;
    
    var lazy_from_val = from_val;
    
    var lazy_is_val = is_val;
    return {
      Undefined : Undefined, 
      force_val : force_val, 
      from_fun : from_fun, 
      from_val : from_val, 
      is_val : is_val, 
      lazy_from_fun : lazy_from_fun, 
      lazy_from_val : lazy_from_val, 
      lazy_is_val : lazy_is_val
    }
  })
/* No side effect */
