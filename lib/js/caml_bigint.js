'use strict';


function div(x, y) {
  if (y === 0n) {
    throw {
          RE_EXN_ID: "Division_by_zero",
          Error: new Error()
        };
  }
  return x / y;
}

function mod_(x, y) {
  if (y === 0n) {
    throw {
          RE_EXN_ID: "Division_by_zero",
          Error: new Error()
        };
  }
  return x % y;
}

exports.div = div;
exports.mod_ = mod_;
/* No side effect */
