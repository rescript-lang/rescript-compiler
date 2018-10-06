'use strict';


function actionKey(key, a, b, c, d, e) {
  var exit = 0;
  switch (key) {
    case 98 : 
        return c;
    case 106 : 
        return d;
    case 107 : 
        return e;
    case 116 : 
        return b;
    case 100 : 
    case 101 : 
    case 102 : 
    case 103 : 
    case 104 : 
    case 105 : 
    case 108 : 
    case 109 : 
    case 110 : 
    case 111 : 
    case 112 : 
    case 113 : 
    case 114 : 
    case 115 : 
    case 117 : 
        exit = 1;
        break;
    case 99 : 
    case 118 : 
        return a;
    default:
      exit = 1;
  }
  if (exit === 1) {
    return (function () {
        return /* () */0;
      });
  }
  
}

exports.actionKey = actionKey;
/* No side effect */
