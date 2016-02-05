// Generated CODE, PLEASE EDIT WITH CARE
'use strict';
define(["../runtime/caml_obj_runtime","./obj","../runtime/caml_exceptions"],
  function(Caml_obj_runtime,Obj,Caml_exceptions){
    'use strict';
    var Undefined = [
      248,
      "CamlinternalLazy.Undefined",
      ++ Caml_exceptions.caml_oo_last_id
    ];
    
    function raise_undefined() {
      throw Undefined;
    }
    
    function force_lazy_block(blk) {
      var closure = blk[0];
      blk[0] = raise_undefined;
      try {
        var result = closure(/* () */0);
        blk[0] = result;
        Caml_obj_runtime.caml_obj_set_tag(blk, Obj.forward_tag);
        return result;
      }
      catch (e){
        blk[0] = function () {
          throw e;
        };
        throw e;
      }
    }
    
    function force_val_lazy_block(blk) {
      var closure = blk[0];
      blk[0] = raise_undefined;
      var result = closure(/* () */0);
      blk[0] = result;
      Caml_obj_runtime.caml_obj_set_tag(blk, Obj.forward_tag);
      return result;
    }
    
    function force(lzv) {
      var t = Caml_obj_runtime.caml_obj_tag(lzv);
      if (t === Obj.forward_tag) {
        return lzv[0];
      }
      else if (t !== Obj.lazy_tag) {
        return lzv;
      }
      else {
        return force_lazy_block(lzv);
      }
    }
    
    function force_val(lzv) {
      var t = Caml_obj_runtime.caml_obj_tag(lzv);
      if (t === Obj.forward_tag) {
        return lzv[0];
      }
      else if (t !== Obj.lazy_tag) {
        return lzv;
      }
      else {
        return force_val_lazy_block(lzv);
      }
    }
    return {
      Undefined : Undefined, 
      force_lazy_block : force_lazy_block, 
      force_val_lazy_block : force_val_lazy_block, 
      force : force, 
      force_val : force_val
    }
  })
/* No side effect */
