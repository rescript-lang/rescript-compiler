'use strict';


function isfree(id, _id$p) {
  while(true) {
    var id$p = _id$p;
    switch (id$p.TAG | 0) {
      case /* Pident */0 :
          return id === id$p._0;
      case /* Pdot */1 :
          _id$p = id$p._0;
          continue ;
      case /* Papply */2 :
          if (isfree(id, id$p._0)) {
            return true;
          }
          _id$p = id$p._1;
          continue ;
      
    }
  };
}

exports.isfree = isfree;
/* No side effect */
