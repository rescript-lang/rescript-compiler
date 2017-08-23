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
  return a + 3 | 0;
}

function f(x) {
  var y;
  y = x.tag ? 4 : 3;
  return y + 32 | 0;
}

function f2(x) {
  var v = 0;
  var y;
  v = 1;
  if (x.tag) {
    var z = 33;
    y = z + 4 | 0;
  } else {
    var z$1 = 33;
    y = z$1 + 3 | 0;
  }
  return y + 32 | 0;
}

function f3(x) {
  var v = 0;
  var y;
  v = 1;
  y = x.tag ? 4 : 3;
  return y + 32 | 0;
}

exports.ff = ff;
exports.f  = f;
exports.f2 = f2;
exports.f3 = f3;
/* No side effect */
