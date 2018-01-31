'use strict';


function comparable(cmp) {
  return /* module */[/* cmp */cmp];
}

function hashable(hash, eq) {
  return /* module */[
          /* hash */hash,
          /* eq */eq
        ];
}

exports.comparable = comparable;
exports.hashable = hashable;
/* No side effect */
