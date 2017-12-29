'use strict';



function x(v){return [v]}

;

x("3");

var v = x(3);

var include = /* module */[/* x */x];

var xxx = include[0];

var u = xxx(3);

var xx = xxx("3");

exports.v = v;
exports.xxx = xxx;
exports.u = u;
exports.xx = xx;
/*  Not a pure module */
