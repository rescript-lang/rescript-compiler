'use strict';

var Caml_int32 = require("../../lib/js/caml_int32");

function u() {
  var exit = 0;
  var n;
  try {
    n = 3;
    exit = 1;
  }
  catch (exn){
    return 42;
  }
  if (exit === 1) {
    return Caml_int32.div(3, 0);
  }
  
}

exports.u = u;
/* No side effect */
