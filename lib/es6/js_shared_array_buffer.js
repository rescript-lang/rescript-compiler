


function make(length, maxByteLength) {
  return new SharedArrayBuffer(length, {
              maxByteLength: maxByteLength
            });
}

export {
  make ,
}
/* No side effect */
