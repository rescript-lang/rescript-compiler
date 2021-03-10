'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var id = {
  contents: 0
};

function u(b) {
  if (b === 0) {
    return 0;
  } else {
    return 1;
  }
}

function u1(b) {
  return b === 0;
}

function u2(b) {
  return b !== 0;
}

Mt.eq_suites(id, suites, "File \"gpr_4924_test.ml\", line 25, characters 23-30", u2(/* A */0), false);

Mt.eq_suites(id, suites, "File \"gpr_4924_test.ml\", line 26, characters 23-30", u2(/* B */1), true);

Mt.eq_suites(id, suites, "File \"gpr_4924_test.ml\", line 27, characters 23-30", u2(/* C */{
          _0: 2
        }), true);

function u3(b) {
  if (b === 0) {
    return 3;
  } else {
    return 4;
  }
}

function u4(b) {
  if (b === 0) {
    return 3;
  } else {
    return 4;
  }
}

function u5(b) {
  return b !== 0;
}

function u6(b) {
  return b === 0;
}

Mt.from_pair_suites("File \"gpr_4924_test.ml\", line 49, characters 20-27", suites.contents);

var from_pair_suites = Mt.from_pair_suites;

var eq_suites = Mt.eq_suites;

exports.from_pair_suites = from_pair_suites;
exports.eq_suites = eq_suites;
exports.suites = suites;
exports.id = id;
exports.u = u;
exports.u1 = u1;
exports.u2 = u2;
exports.u3 = u3;
exports.u4 = u4;
exports.u5 = u5;
exports.u6 = u6;
/*  Not a pure module */
