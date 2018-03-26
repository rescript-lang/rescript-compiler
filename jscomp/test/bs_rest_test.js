'use strict';



function x(v){return [v]}

;

x("3");

var v = x(3);

var xxx = x;

var u = xxx(3);

var xx = xxx("3");

exports.v = v;
exports.xxx = xxx;
exports.u = u;
exports.xx = xx;
/*  Not a pure module */
