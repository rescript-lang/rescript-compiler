'use strict';

var Demo$Liba = require("liba/src/demo.bs.js");
var Demo$Libb = require("libb/src/demo.js");

var v = Demo$Liba.v + Demo$Libb.v | 0;

exports.v = v;
/* No side effect */
