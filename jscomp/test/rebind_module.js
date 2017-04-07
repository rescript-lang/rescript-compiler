'use strict';

var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var A = Caml_exceptions.create("Rebind_module.A");

exports.A = A;
/* No side effect */
