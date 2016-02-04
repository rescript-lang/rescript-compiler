// Generated CODE, PLEASE EDIT WITH CARE
'use strict';


function ff(x) {
  var a;
  switch (x) {
    case "0" : 
    case "1" : 
    case "2" : 
        a = 3;
        break;
    case "3" : 
        a = 4;
        break;
    case "4" : 
        a = 6;
        break;
    case "7" : 
        a = 7;
        break;
    default:
      a = 8;
  }
  return a + 3;
}

function f(x) {
  var y;
  y = x[0] ? 4 : 3;
  return y + 32;
}

function f2(x) {
  var v = 0;
  var y;
  v = 1;
  if (x[0]) {
    var z = 33;
    y = z + 4;
  }
  else {
    var z$1 = 33;
    y = z$1 + 3;
  }
  return y + 32;
}

function f3(x) {
  var v = 0;
  var y;
  v = 1;
  if (x[0]) {
    y = 4;
  }
  else {
    y = 3;
  }
  return y + 32;
}

exports.ff = ff;
exports.f  = f;
exports.f2 = f2;
exports.f3 = f3;
/* No side effect */
