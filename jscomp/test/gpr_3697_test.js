'use strict';

var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

function fix(param) {
  return /* Fix */[CamlinternalLazy.from_fun((function () {
                  return fix(undefined);
                }))];
}

function unfixLeak(_f) {
  while(true) {
    var f = _f;
    _f = CamlinternalLazy.force(f[0]);
    continue ;
  };
}

function unfix(p) {
  while(true) {
    var match = p.contents;
    p.contents = CamlinternalLazy.force(match[0]);
  };
  
}

exports.fix = fix;
exports.unfixLeak = unfixLeak;
exports.unfix = unfix;
/* No side effect */
