'use strict';

var Test_char = require("./test_char.js");

var v = Test_char.caml_is_printable(/* 'a' */97);

exports.v = v;
/* v Not a pure module */
