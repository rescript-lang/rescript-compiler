'use strict';


function make(length, maxByteLength) {
  return new SharedArrayBuffer(length, {
              maxByteLength: maxByteLength
            });
}

exports.make = make;
/* No side effect */
