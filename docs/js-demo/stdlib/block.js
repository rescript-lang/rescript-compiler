'use strict';
define(["exports"],
  function(exports){
    'use strict';
    function __(tag, block) {
      block.tag = tag;
      return block;
    }
    
    exports.__ = __;
    
  })
/* No side effect */
