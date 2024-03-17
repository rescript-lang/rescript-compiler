


function make(length, maxByteLength) {
  return new ArrayBuffer(length, {
              maxByteLength: maxByteLength
            });
}

export {
  make ,
}
/* No side effect */
