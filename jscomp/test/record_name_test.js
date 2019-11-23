'use strict';


function f(x) {
  return {
          THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE: x
        };
}

function set(x) {
  x.THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE = 3;
  return (x.THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE << 1);
}

function f2(u) {
  return u.x.x.x.y;
}

exports.f = f;
exports.set = set;
exports.f2 = f2;
/* No side effect */
