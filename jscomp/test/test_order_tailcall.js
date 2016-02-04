// Generated CODE, PLEASE EDIT WITH CARE
'use strict';


function f(_, _$1) {
  while(true) {
    
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
  };
}

function f2(_, _y) {
  while(true) {
    var y = _y;
    _y = y + 10;
  };
}

function f3(_x, _y) {
  while(true) {
    var y = _y;
    var x = _x;
    _y = x + 10;
    _x = y;
  };
}

function f4(_x, _y) {
  while(true) {
    var y = _y;
    var x = _x;
    _y = y + x;
    _x = x + 10;
  };
}

function f5(_x, _y, z) {
  while(true) {
    var y = _y;
    _y = z + 20;
    _x = y + 10;
  };
}

function f6(b) {
  while(true) {
    if (b) {
      if (b) {
        if (b) {
          if (!b) {
            return /* false */0;
          }
          
        }
        else {
          return /* false */0;
        }
      }
      else {
        return /* false */0;
      }
    }
    else {
      return /* false */0;
    }
  };
}

function f7(b) {
  while(true) {
    if (b) {
      return /* true */1;
    }
    else if (b) {
      return /* true */1;
    }
    else if (b) {
      return /* true */1;
    }
    else if (b) {
      return /* true */1;
    }
    
  };
}

function f8(_x, _y) {
  while(true) {
    var y = _y;
    var x = _x;
    if (x > 10) {
      _y = y + 1;
    }
    else if (x < 5) {
      _x = x - 1;
    }
    else if (x > 6) {
      _x = x - 2;
    }
    else {
      return f8(x, y + 1) + f8(x - 1, y);
    }
  };
}

exports.f  = f;
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f4 = f4;
exports.f5 = f5;
exports.f6 = f6;
exports.f7 = f7;
exports.f8 = f8;
/* No side effect */
