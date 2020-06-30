'use strict';


var coalesce = ((a, b) => a ?? b);

var u = (a => a?.b);

exports.coalesce = coalesce;
exports.u = u;
/* No side effect */
