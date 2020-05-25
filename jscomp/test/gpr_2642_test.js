'use strict';


function isfree(id, _id$prime) {
  while(true) {
    var id$prime = _id$prime;
    switch (id$prime.TAG | 0) {
      case /* Pident */0 :
          return id === id$prime._0;
      case /* Pdot */1 :
          _id$prime = id$prime._0;
          continue ;
      case /* Papply */2 :
          if (isfree(id, id$prime._0)) {
            return true;
          }
          _id$prime = id$prime._1;
          continue ;
      
    }
  };
}

exports.isfree = isfree;
/* No side effect */
