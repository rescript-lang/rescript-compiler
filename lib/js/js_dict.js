'use strict';


var unsafeDeleteKey = (
  function(dict,key){
     delete dict[key];
     return 0
   }
);

exports.unsafeDeleteKey = unsafeDeleteKey;
/* unsafeDeleteKey Not a pure module */
