'use strict';


function f(_, _$1) {
  while(true) {
    continue ;
  };
}

function f1(_x, _y, _z) {
  while(true) {
    var z = _z;
    var y = _y;
    var x = _x;
    _z = x;
    _y = z;
    _x = y;
    continue ;
  };
}

function f2(_, _y) {
  while(true) {
    var y = _y;
    _y = y + 10 | 0;
    continue ;
  };
}

function f3(_x, _y) {
  while(true) {
    var y = _y;
    var x = _x;
    _y = x + 10 | 0;
    _x = y;
    continue ;
  };
}

function f4(_x, _y) {
  while(true) {
    var y = _y;
    var x = _x;
    _y = y + x | 0;
    _x = x + 10 | 0;
    continue ;
  };
}

function f5(_x, _y, z) {
  while(true) {
    var y = _y;
    _y = z + 20 | 0;
    _x = y + 10 | 0;
    continue ;
  };
}

function f6(b) {
  while(true) {
    if (b) {
      continue ;
    } else {
      return false;
    }
  };
}

function f7(b) {
  while(true) {
    if (b) {
      return true;
    } else {
      continue ;
    }
  };
}

function f8(_x, _y) {
  while(true) {
    var y = _y;
    var x = _x;
    if (x > 10) {
      _y = y + 1 | 0;
      continue ;
    } else if (x < 5) {
      _x = x - 1 | 0;
      continue ;
    } else if (x > 6) {
      _x = x - 2 | 0;
      continue ;
    } else {
      return f8(x, y + 1 | 0) + f8(x - 1 | 0, y) | 0;
    }
  };
}

exports.f = f;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
/* No side effect */
