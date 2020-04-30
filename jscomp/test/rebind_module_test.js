'use strict';

var Rebind_module = require("./rebind_module.js");

function x(v) {
  if (v.ExceptionID === Rebind_module.AA.ExceptionID) {
    return 0;
  } else {
    return 1;
  }
}

exports.x = x;
/* No side effect */
