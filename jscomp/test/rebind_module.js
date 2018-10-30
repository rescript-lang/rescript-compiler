'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var A = Caml_exceptions.create("Rebind_module.A");

var AA = Caml_exceptions.create("Rebind_module.AA");

exports.A = A;
exports.AA = AA;
/* No side effect */
