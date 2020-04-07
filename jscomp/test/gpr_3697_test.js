'use strict';

var Caml_obj = require("../../lib/js/caml_obj.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

function fix(param) {
  return /* Fix */[Caml_obj.caml_lazy_make((function (param) {
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
