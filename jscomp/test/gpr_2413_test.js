'use strict';

var Caml_int32 = require("../../lib/js/caml_int32.js");

function f(param) {
  var exit = 0;
  switch (param.tag | 0) {
    case 0 : 
        var match = param[0];
        if (match.tag) {
          var a = match[0];
          return a - a | 0;
        } else {
          var a$1 = match[0];
          return a$1 + a$1 | 0;
        }
    case 1 : 
    case 2 : 
        exit = 1;
        break;
    
  }
  if (exit === 1) {
    var a$2 = param[0][0];
    return Caml_int32.imul(a$2, a$2);
  }
  
}

function ff(c) {
  c[0] = c[0] + 1 | 0;
  var match = (1 + c[0] | 0) + 1 | 0;
  if (match > 3 || match < 0) {
    return 0;
  } else {
    return match + 1 | 0;
  }
}

exports.f = f;
exports.ff = ff;
/* No side effect */
