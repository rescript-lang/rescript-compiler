'use strict';

var Rebind_module = require("./rebind_module.js");

function x(v) {
  if (v === Rebind_module.A) {
    return 0;
  } else {
    return 1;
  }
}

exports.x = x;
/* No side effect */
