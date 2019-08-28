'use strict';

var Caml_int32 = require("../../lib/js/caml_int32.js");
var Pervasives = require("../../lib/js/pervasives.js");

function f(param) {
  switch (param.tag | 0) {
    case /* A */0 :
        var match = param[0];
        if (match.tag) {
          var a = match[0];
          return a - a | 0;
        } else {
          var a$1 = match[0];
          return a$1 + a$1 | 0;
        }
    case /* B */1 :
    case /* C */2 :
        break;
    
  }
  var a$2 = param[0][0];
  return Caml_int32.imul(a$2, a$2);
}

function ff(c) {
  Pervasives.incr(c);
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
