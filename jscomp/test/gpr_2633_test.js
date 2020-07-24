'use strict';

var Curry = require("../../lib/js/curry.js");

function on1(foo, $$event) {
  foo.on($$event.NAME, $$event.VAL);
  
}

function on2(foo, h, $$event) {
  foo.on(Curry._1(h, $$event).NAME, Curry._1(h, $$event).VAL);
  
}

exports.on1 = on1;
exports.on2 = on2;
/* No side effect */
