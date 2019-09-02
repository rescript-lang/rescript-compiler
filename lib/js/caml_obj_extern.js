'use strict';


function size_of_t (o){
return Array.isArray(o) ? o.length : Object.keys(o).length
};

function length (o){
return Array.isArray(o) ? o.length : Object.keys(o).length
};

exports.size_of_t = size_of_t;
exports.length = length;
/* No side effect */
