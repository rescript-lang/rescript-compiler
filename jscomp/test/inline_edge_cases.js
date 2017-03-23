'use strict';


function test3(_n) {
  while(true) {
    var n = _n;
    if (n) {
      _n = n - 1 | 0;
      continue ;
      
    } else {
      return (n + 5 | 0) + 4 | 0;
    }
  };
}

function test2(_n) {
  while(true) {
    var n = _n;
    if (n) {
      _n = n - 1 | 0;
      continue ;
      
    } else {
      return test3(n) + 3 | 0;
    }
  };
}

function test0(_n) {
  while(true) {
    var n = _n;
    if (n) {
      _n = n - 1 | 0;
      continue ;
      
    } else {
      var _n$1 = n;
      while(true) {
        var n$1 = _n$1;
        if (n$1) {
          _n$1 = n$1 - 1 | 0;
          continue ;
          
        } else {
          return test2(n$1) + 2 | 0;
        }
      };
    }
  };
}

var v = test0(10);

test0(10) + 2 | 0;

exports.v = v;
/* v Not a pure module */
