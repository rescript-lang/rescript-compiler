'use strict';

var Caml_option = require("../../lib/js/caml_option.js");

var u = Caml_option.some(Caml_option.some(Caml_option.some(undefined)));

exports.u = u;
/* No side effect */
