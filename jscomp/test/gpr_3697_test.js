'use strict';

var Block = require("../../lib/js/block.js");
var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

function fix(param) {
  return /* Fix */[Block.__(246, [(function (param) {
                  return fix(/* () */0);
                })])];
}

function unfixLeak(_param) {
  while(true) {
    var param = _param;
    var f = param[0];
    var tag = f.tag | 0;
    _param = tag === 250 ? f[0] : (
        tag === 246 ? CamlinternalLazy.force_lazy_block(f) : f
      );
    continue ;
  };
}

function unfix(p) {
  while(true) {
    var match = p[0];
    var match$1 = match[0];
    var tag = match$1.tag | 0;
    p[0] = tag === 250 ? match$1[0] : (
        tag === 246 ? CamlinternalLazy.force_lazy_block(match$1) : match$1
      );
  };
  return /* () */0;
}

exports.fix = fix;
exports.unfixLeak = unfixLeak;
exports.unfix = unfix;
/* No side effect */
