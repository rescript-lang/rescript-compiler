'use strict';

var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

function f(x) {
  var tag = x.tag | 0;
  var y = tag === 250 ? x[0] : (
      tag === 246 ? CamlinternalLazy.force_lazy_block(x) : x
    );
  return y + "abc";
}

var x = "def";

var tag = x.tag | 0;

if (tag !== 250) {
  if (tag === 246) {
    CamlinternalLazy.force_lazy_block(x);
  }
  
}

var u = f(x);

exports.f = f;
exports.u = u;
/*  Not a pure module */
