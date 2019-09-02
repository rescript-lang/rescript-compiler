'use strict';


function isfree(id, _param) {
  while(true) {
    var param = _param;
    switch (/* XXX */param.tag) {
      case "Pident" :
          return id === param.Arg0;
      case "Pdot" :
          _param = param.Arg0;
          continue ;
      case "Papply" :
          if (isfree(id, param.Arg0)) {
            return true;
          } else {
            _param = param.Arg1;
            continue ;
          }
      
    }
  };
}

exports.isfree = isfree;
/* No side effect */
