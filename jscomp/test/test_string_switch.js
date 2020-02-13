'use strict';

var Sys = require("../../lib/js/sys.js");

var os_version;

switch (Sys.os_type) {
  case "Cygwin" :
      os_version = 2;
      break;
  case "Unix" :
      os_version = 1;
      break;
  default:
    os_version = 3;
}

exports.os_version = os_version;
/* os_version Not a pure module */
