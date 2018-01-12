'use strict';


function isfree(id, _param) {
  while(true) {
    var param = _param;
    switch (param.tag | 0) {
      case 0 : 
          return id === param[0];
      case 1 : 
          _param = param[0];
          continue ;
      case 2 : 
          if (isfree(id, param[0])) {
            return true;
          } else {
            _param = param[1];
            continue ;
          }
      
    }
  };
}

exports.isfree = isfree;
/* No side effect */
