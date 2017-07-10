'use strict';
define(["exports", "./pervasives.js"],
  function(exports, Pervasives){
    'use strict';
    function unsafe_ceil(prim) {
      return Math.ceil(prim);
    }
    
    function ceil_int(f) {
      if (f > Pervasives.max_int) {
        return Pervasives.max_int;
      } else if (f < Pervasives.min_int) {
        return Pervasives.min_int;
      } else {
        return Math.ceil(f);
      }
    }
    
    function unsafe_floor(prim) {
      return Math.floor(prim);
    }
    
    function floor_int(f) {
      if (f > Pervasives.max_int) {
        return Pervasives.max_int;
      } else if (f < Pervasives.min_int) {
        return Pervasives.min_int;
      } else {
        return Math.floor(f);
      }
    }
    
    function random_int(min, max) {
      return floor_int(Math.random() * (max - min | 0)) + min | 0;
    }
    
    var ceil = ceil_int;
    
    var floor = floor_int;
    
    exports.unsafe_ceil  = unsafe_ceil;
    exports.ceil_int     = ceil_int;
    exports.ceil         = ceil;
    exports.unsafe_floor = unsafe_floor;
    exports.floor_int    = floor_int;
    exports.floor        = floor;
    exports.random_int   = random_int;
    
  })
/* No side effect */
