'use strict';


function div(x, y) {
  if (y === 0) {
    throw {
          RE_EXN_ID: "Division_by_zero",
          Error: new Error()
        };
  }
  return x / y | 0;
}

function mod_(x, y) {
  if (y === 0) {
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
