'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function u(b) {
  if (/* tag */typeof b === "number" && b === /* A */0) {
    return 0;
  } else {
    return 1;
  }
}

function u1(b) {
  if (/* tag */typeof b === "number" && b === /* A */0) {
    return true;
  } else {
    return false;
  }
}

function u2(b) {
  if (/* tag */typeof b === "number" && b === /* A */0) {
    return false;
  } else {
    return true;
  }
}

Mt.eq_suites(test_id, suites, "File \"gpr_4924_test.ml\", line 25, characters 30-37", false, false);

Mt.eq_suites(test_id, suites, "File \"gpr_4924_test.ml\", line 26, characters 30-37", true, true);

Mt.eq_suites(test_id, suites, "File \"gpr_4924_test.ml\", line 27, characters 30-37", true, true);

function u3(b) {
  if (/* tag */typeof b === "number" && b === /* A */0) {
    return 3;
  } else {
    return 4;
  }
}

function u4(b) {
  if (/* tag */typeof b === "number" && b === /* A */0) {
    return 3;
  } else {
    return 4;
  }
}

function u5(b) {
  if (/* tag */typeof b === "number" && b === /* A */0) {
    return false;
  } else {
    return true;
  }
}

function u6(b) {
  if (/* tag */typeof b === "number" && b === /* A */0) {
    return true;
  } else {
    return false;
  }
}

Mt.from_pair_suites("File \"gpr_4924_test.ml\", line 49, characters 20-27", suites.contents);

var from_pair_suites = Mt.from_pair_suites;

var eq_suites = Mt.eq_suites;

exports.from_pair_suites = from_pair_suites;
exports.eq_suites = eq_suites;
exports.suites = suites;
exports.test_id = test_id;
exports.u = u;
exports.u1 = u1;
exports.u2 = u2;
exports.u3 = u3;
exports.u4 = u4;
exports.u5 = u5;
exports.u6 = u6;
/*  Not a pure module */
