// Generated CODE, PLEASE EDIT WITH CARE
"use strict";


function test4(n) {
  return n + 5;
}

function test3(_n) {
  while(true) {
    var n = _n;
    if (n) {
      _n = n - 1;
    }
    else {
      return test4(n) + 4;
    }
  };
}

function test2(_n) {
  while(true) {
    var n = _n;
    if (n) {
      _n = n - 1;
    }
    else {
      return test3(n) + 3;
    }
  };
}

function test0(_n) {
  while(true) {
    var n = _n;
    if (n) {
      _n = n - 1;
    }
    else {
      var _n$1 = n;
      while(true) {
        var n$1 = _n$1;
        if (n$1) {
          _n$1 = n$1 - 1;
        }
        else {
          return test2(n$1) + 2;
        }
      };
    }
  };
}

var v = test0(10);

test0(10) + 2;

exports.v = v;
/* v Not a pure module */
