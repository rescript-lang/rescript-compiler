'use strict';


function search(x, array) {
  var _i = 0;
  var xs = array;
  var k = x;
  while(true) {
    var i = _i;
    var match = xs[i];
    if (match[0] === k) {
      return match[1];
    } else {
      _i = i + 1 | 0;
      continue ;
      
    }
  };
}

exports.search = search;
/* No side effect */
