'use strict';


function f(re) {
  re.exec("banana");
  return 3;
}

exports.f = f;
/* No side effect */
