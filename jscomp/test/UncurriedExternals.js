'use strict';


function dd(param) {
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

var h = sum(1.0, 2.0);

var M = {
  sum: (function (prim0, prim1) {
      return sum(prim0, prim1);
    })
};

var hh = M.sum(1.0, 2.0);

var mf = 3 % 4;

function tg(arr) {
  return arr[0];
}

var tc = Object.assign({}, "abc");

var te = (function (prim) {
      return prim;
    })({
      RE_EXN_ID: "Not_found"
    });

var tcr = {};

var StandardNotation = {
  dd: dd,
  h: h,
  M: M,
  hh: hh,
  mf: mf,
  tg: tg,
  tc: tc,
  te: te,
  tcr: tcr
};

function dd$1(param) {
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

var h$1 = sum(1.0, 2.0);

var M$1 = {
  sum: (function (prim0, prim1) {
      return sum(prim0, prim1);
    })
};

var hh$1 = M$1.sum(1.0, 2.0);

var mf$1 = 3 % 4;

function tg$1(arr) {
  return arr[0];
}

var tc$1 = Object.assign({}, "abc");

var te$1 = (function (prim) {
      return prim;
    })({
      RE_EXN_ID: "Not_found"
    });

var tcr$1 = {};

exports.StandardNotation = StandardNotation;
exports.dd = dd$1;
exports.h = h$1;
exports.M = M$1;
exports.hh = hh$1;
exports.mf = mf$1;
exports.tg = tg$1;
exports.tc = tc$1;
exports.te = te$1;
exports.tcr = tcr$1;
/* h Not a pure module */
