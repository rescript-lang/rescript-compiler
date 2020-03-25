'use strict';


function isfree(id, _param) {
  while(true) {
    var param = _param;
    switch (param.tag | 0) {
      case /* Pident */0 :
          return id === param[0];
      case /* Pdot */1 :
          _param = param[0];
          continue ;
      case /* Papply */2 :
          if (isfree(id, param[0])) {
            return true;
          }
          _param = param[1];
          continue ;
      
    }
  };
}

exports.isfree = isfree;
/* No side effect */
