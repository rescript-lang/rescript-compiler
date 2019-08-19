'use strict';


function init_mod(loc, shape) {
  return ({});
}

function module_assign (o,n){
Object.assign(o, n);
};

function update_mod(shape, o, n) {
  return module_assign(o, n);
}

exports.init_mod = init_mod;
exports.update_mod = update_mod;
/* No side effect */
