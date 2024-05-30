'use strict';


function raiseWhenNotFound(x) {
  if (x == null) {
    throw new Error("Not_found", {
          cause: {
            RE_EXN_ID: "Not_found"
          }
        });
  }
  return x;
}

function fromInt(len, xs, $$enum) {
  let _i = 0;
  while(true) {
    let i = _i;
    if (i === len) {
      return;
    }
    let k = xs[i];
    if (k === $$enum) {
      return i;
    }
    _i = i + 1 | 0;
    continue;
  };
}

function fromIntAssert(len, xs, $$enum) {
  let _i = 0;
  while(true) {
    let i = _i;
    if (i === len) {
      throw new Error("Not_found", {
            cause: {
              RE_EXN_ID: "Not_found"
            }
          });
    }
    let k = xs[i];
    if (k === $$enum) {
      return i;
    }
    _i = i + 1 | 0;
    continue;
  };
}

exports.raiseWhenNotFound = raiseWhenNotFound;
exports.fromInt = fromInt;
exports.fromIntAssert = fromIntAssert;
/* No side effect */
