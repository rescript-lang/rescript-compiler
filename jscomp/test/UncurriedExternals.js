'use strict';


function dd(param) {
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

var h = sum(1.0, 2.0);

var M = {
  sum: sum
};

var hh = M.sum(1.0, 2.0);

var mf = 3 % 4;

var StandardNotation = {
  dd: dd,
  h: h,
  M: M,
  hh: hh,
  mf: mf
};

function dd$1(param) {
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

var h$1 = sum(1.0, 2.0);

var M$1 = {
  sum: sum
};

var hh$1 = M$1.sum(1.0, 2.0);

var mf$1 = 3 % 4;

exports.StandardNotation = StandardNotation;
exports.dd = dd$1;
exports.h = h$1;
exports.M = M$1;
exports.hh = hh$1;
exports.mf = mf$1;
/* h Not a pure module */
