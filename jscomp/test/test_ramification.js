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
  y = /* XXX */x.tag === "A" ? 3 : 4;
  return y + 32 | 0;
}

function f2(x) {
  var v = 0;
  var y;
  v = 1;
  if (/* XXX */x.tag === "A") {
    var z = 33;
    y = z + 3 | 0;
  } else {
    var z$1 = 33;
    y = z$1 + 4 | 0;
  }
  return y + 32 | 0;
}

function f3(x) {
  var v = 0;
  var y;
  v = 1;
  y = /* XXX */x.tag === "A" ? 3 : 4;
  return y + 32 | 0;
}

exports.ff = ff;
exports.f = f;
exports.f2 = f2;
exports.f3 = f3;
/* No side effect */
