'use strict';

var FileJs = require("./File.js");

var bar = FileJs.foo;

exports.bar = bar;
/* bar Not a pure module */
