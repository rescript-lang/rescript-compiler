'use strict';

var Caml_char = require("../../lib/js/caml_char.js");

var v = Caml_char.caml_is_printable(/* "a" */97);

exports.v = v;
/* v Not a pure module */
