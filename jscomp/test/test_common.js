'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var U = Caml_exceptions.create("Test_common.U");

var H = Caml_exceptions.create("Test_common.H");

exports.U = U;
exports.H = H;
/* No side effect */
