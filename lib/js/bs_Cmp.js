'use strict';


function Make(M) {
  var cmp = M[/* cmp */0];
  return /* module */[/* cmp */cmp];
}

function make(cmp) {
  return /* module */[/* cmp */cmp];
}

exports.Make = Make;
exports.make = make;
/* No side effect */
