// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

function test4(n) {
  return n + 5;
}

function test3(_n) {
  while(/* true */1) {
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
  while(/* true */1) {
    var n = _n;
    if (n) {
      _n = n - 1;
    }
    else {
      return test3(n) + 3;
    }
  };
}

function test1(_n) {
  while(/* true */1) {
    var n = _n;
    if (n) {
      _n = n - 1;
    }
    else {
      return test2(n) + 2;
    }
  };
}

function test0(_n) {
  while(/* true */1) {
    var n = _n;
    if (n) {
      _n = n - 1;
    }
    else {
      return test1(n);
    }
  };
}

var v = test0(10);

test0(10) + 2;

exports.v = v;
/* v Not a pure module */
