'use strict';


function not_implemented(s) {
  var str = s + " not implemented by BuckleScript yet\n";
  throw new Error(str);
}

exports.not_implemented = not_implemented;
/* No side effect */
