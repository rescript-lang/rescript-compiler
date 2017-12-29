'use strict';

var Exception_def = require("./exception_def.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var E = Caml_exceptions.create("Exception_rebind_test.A.E");

var A = /* module */[/* E */E];

var B = /* module */[/* F */E];

var H = Exception_def.A;

exports.A = A;
exports.B = B;
exports.H = H;
/* Exception_def Not a pure module */
