'use strict';


function make(length, maxByteLength) {
  return new ArrayBuffer(length, {
              maxByteLength: maxByteLength
            });
}

exports.make = make;
/* No side effect */
