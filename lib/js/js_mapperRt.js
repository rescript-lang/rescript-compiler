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

function revSearch(len, array, x) {
  var _i = 0;
  var len$1 = len;
  var xs = array;
  var k = x;
  while(true) {
    var i = _i;
    if (i === len$1) {
      return /* None */0;
    } else {
      var match = xs[i];
      if (match[1] === k) {
        return /* Some */[match[0]];
      } else {
        _i = i + 1 | 0;
        continue ;
        
      }
    }
  };
}

exports.search    = search;
exports.revSearch = revSearch;
/* No side effect */
