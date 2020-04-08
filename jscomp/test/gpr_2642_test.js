'use strict';


function isfree(id, _id$prime) {
  while(true) {
    var id$prime = _id$prime;
    switch (id$prime.tag | 0) {
      case /* Pident */0 :
          return id === id$prime[0];
      case /* Pdot */1 :
          _id$prime = id$prime[0];
          continue ;
      case /* Papply */2 :
          if (isfree(id, id$prime[0])) {
            return true;
          }
          _id$prime = id$prime[1];
          continue ;
      
    }
  };
}

exports.isfree = isfree;
/* No side effect */
