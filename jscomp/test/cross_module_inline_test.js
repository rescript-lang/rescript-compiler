'use strict';

var Caml_string = require("../../lib/js/caml_string.js");

var v = Caml_string.caml_is_printable(/* "a" */97);

exports.v = v;
/* v Not a pure module */
