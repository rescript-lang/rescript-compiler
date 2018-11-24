'use strict';


var not_implemented = function (s){
  throw new Error(s + " not implemented by BuckleScript yet\n")
};

exports.not_implemented = not_implemented;
/* No side effect */
