'use strict';


var u = (console.log(3), /* () */0);

var v = console.log(3);

exports.u = u;
exports.v = v;
/* u Not a pure module */
