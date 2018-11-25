'use strict';


function not_implemented (s){
  throw new Error(s + " not implemented by BuckleScript yet\n")
};

exports.not_implemented = not_implemented;
/* No side effect */
