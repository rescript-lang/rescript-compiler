'use strict';


function isfree(id, _id$p) {
  while(true) {
    var id$p = _id$p;
    switch (id$p.TAG) {
      case "Pident" :
          return id === id$p._0;
      case "Pdot" :
          _id$p = id$p._0;
          continue ;
      case "Papply" :
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
