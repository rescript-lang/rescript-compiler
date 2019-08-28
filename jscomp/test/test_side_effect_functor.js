'use strict';

var Pervasives = require("../../lib/js/pervasives.js");

var v = /* record */{
  contents: 0
};

Pervasives.incr(v);

console.log(String(v.contents));

function unuse_v(param) {
  return 35;
}

var h = unuse_v;

exports.h = h;
/*  Not a pure module */
