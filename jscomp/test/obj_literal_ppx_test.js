'use strict';


var a = {
  x: 3,
  y: {
    hd: 1,
    tl: {
      hd: 2,
      tl: {
        hd: 3,
        tl: /* [] */0
      }
    }
  }
};

exports.a = a;
/* No side effect */
