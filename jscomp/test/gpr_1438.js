'use strict';


function actionKey(key, a, b, c, d, e) {
  var exit = 0;
  var switcher = key - 98 | 0;
  if (switcher > 20 || switcher < 0) {
    exit = 1;
  } else {
    switch (switcher) {
      case 0 : 
          return c;
      case 8 : 
          return d;
      case 9 : 
          return e;
      case 18 : 
          return b;
      case 2 : 
      case 3 : 
      case 4 : 
      case 5 : 
      case 6 : 
      case 7 : 
      case 10 : 
      case 11 : 
      case 12 : 
      case 13 : 
      case 14 : 
      case 15 : 
      case 16 : 
      case 17 : 
      case 19 : 
          exit = 1;
          break;
      case 1 : 
      case 20 : 
          return a;
      
    }
  }
  if (exit === 1) {
    return (function () {
        return /* () */0;
      });
  }
  
}

exports.actionKey = actionKey;
/* No side effect */
