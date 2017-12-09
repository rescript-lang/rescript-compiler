'use strict';


function make(eq, hash) {
  return /* module */[
          /* hash */hash,
          /* eq */eq
        ];
}

exports.make = make;
/* No side effect */
