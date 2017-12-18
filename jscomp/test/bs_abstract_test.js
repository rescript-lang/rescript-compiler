'use strict';


var v = {
  hd: 3,
  tl: null
};

v.tl = v;

exports.v = v;
/* v Not a pure module */
