'use strict';
define(["exports", "./obj.js", "./curry.js", "./caml_exceptions.js"],
  function(exports, Obj, Curry, Caml_exceptions){
    'use strict';
    var Undefined = Caml_exceptions.create("CamlinternalLazy.Undefined");
    
    function raise_undefined() {
      throw Undefined;
    }
    
    function force_lazy_block(blk) {
      var closure = blk[0];
      blk[0] = raise_undefined;
      try {
        var result = Curry._1(closure, /* () */0);
        blk[0] = result;
        blk.tag = Obj.forward_tag;
        return result;
      }
      catch (e){
        blk[0] = (function () {
            throw e;
          });
        throw e;
      }
    }
    
    function force_val_lazy_block(blk) {
      var closure = blk[0];
      blk[0] = raise_undefined;
      var result = Curry._1(closure, /* () */0);
      blk[0] = result;
      blk.tag = Obj.forward_tag;
      return result;
    }
    
    function force(lzv) {
      var t = lzv.tag | 0;
      if (t === Obj.forward_tag) {
        return lzv[0];
      } else if (t !== Obj.lazy_tag) {
        return lzv;
      } else {
        return force_lazy_block(lzv);
      }
    }
    
    function force_val(lzv) {
      var t = lzv.tag | 0;
      if (t === Obj.forward_tag) {
        return lzv[0];
      } else if (t !== Obj.lazy_tag) {
        return lzv;
      } else {
        return force_val_lazy_block(lzv);
      }
    }
    
    exports.Undefined            = Undefined;
    exports.force_lazy_block     = force_lazy_block;
    exports.force_val_lazy_block = force_val_lazy_block;
    exports.force                = force;
    exports.force_val            = force_val;
    
  })
/* No side effect */
