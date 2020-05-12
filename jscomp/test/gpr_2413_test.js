'use strict';


function f(param) {
  switch (param.tag | 0) {
    case /* A */0 :
        var a = param[0];
        if (a.tag) {
          var a$1 = a[0];
          return a$1 - a$1 | 0;
        }
        var a$2 = a[0];
        return a$2 + a$2 | 0;
    case /* B */1 :
    case /* C */2 :
        break;
    
  }
  var a$3 = param[0][0];
  return Math.imul(a$3, a$3);
}

function ff(c) {
  c.contents = c.contents + 1 | 0;
  var match = (1 + c.contents | 0) + 1 | 0;
  if (match > 3 || match < 0) {
    return 0;
  } else {
    return match + 1 | 0;
  }
}

exports.f = f;
exports.ff = ff;
/* No side effect */
