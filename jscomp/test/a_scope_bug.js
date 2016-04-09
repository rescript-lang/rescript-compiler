// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_primitive = require("../runtime/caml_primitive");

function odd(_z) {
  while(true) {
    var z = _z;
    var even = Caml_primitive.imul(z, z);
    var a = (even + 4 | 0) + even | 0;
    console.log("" + a);
    _z = 32;
    continue ;
    
  };
}

var even = odd

exports.odd  = odd;
exports.even = even;
/* No side effect */
