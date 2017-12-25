'use strict';

var Caml_primitive = require("../../lib/js/caml_primitive.js");

var compare = Caml_primitive.caml_int_compare;

exports.compare = compare;
/* No side effect */
