'use strict';

var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

function fix(param) {
  return /* Fix */{
          _0: {
            RE_LAZY_DONE: false,
            value: (function () {
                return fix(undefined);
              })
          }
        };
}

function unfixLeak(_f) {
  while(true) {
    var f = _f;
    _f = CamlinternalLazy.force(f._0);
    continue ;
  };
}

function unfix(p) {
  while(true) {
    var match = p.contents;
    p.contents = CamlinternalLazy.force(match._0);
  };
  
}

exports.fix = fix;
exports.unfixLeak = unfixLeak;
exports.unfix = unfix;
/* No side effect */
