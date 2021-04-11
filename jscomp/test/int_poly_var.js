'use strict';

var Mt = require("./mt.js");

var id = {
  contents: 0
};

var suites = {
  contents: /* [] */0
};

function nest(x) {
  if (x.TAG === /* A */0) {
    var match = x._0;
    if (match === 0) {
      return 0;
    } else if (match === 1) {
      return 1;
    } else {
      return 2;
    }
  }
  var match$1 = x._0;
  if (x._1 !== 0) {
    if (match$1 === 1) {
      return 4;
    } else if (match$1 === 2) {
      return 5;
    } else {
      return 6;
    }
  } else {
    return 3;
  }
}

function f2(x, b) {
  if (x === 1) {
    return b;
  } else if (x === 2) {
    return 0;
  } else if (x === "c") {
    return 33;
  } else {
    return 3;
  }
}

function f3(x, b) {
  if (typeof x !== "object") {
    return 3;
  }
  var variant = x.NAME;
  if (variant === 32) {
    return x.VAL[0];
  } else if (variant === 333) {
    return x.VAL[1];
  } else {
    return x.VAL;
  }
}

function h(x) {
  return x === 0;
}

var hihi = f3(3, 0);

var hh10 = "3" === 3;

var tuple_0 = nest({
      TAG: /* A */0,
      _0: 0
    });

var tuple_1 = nest({
      TAG: /* A */0,
      _0: 1
    });

var tuple_2 = nest({
      TAG: /* A */0,
      _0: 2
    });

var tuple_3 = nest({
      TAG: /* B */1,
      _0: 1,
      _1: 0
    });

var tuple_4 = nest({
      TAG: /* B */1,
      _0: 1,
      _1: 1
    });

var tuple_5 = nest({
      TAG: /* B */1,
      _0: 2,
      _1: 1
    });

var tuple_6 = nest({
      TAG: /* B */1,
      _0: 2,
      _1: 2
    });

var tuple_7 = nest({
      TAG: /* B */1,
      _0: 0,
      _1: 0
    });

var tuple_8 = nest({
      TAG: /* B */1,
      _0: 0,
      _1: 1
    });

var tuple = [
  tuple_0,
  tuple_1,
  tuple_2,
  tuple_3,
  tuple_4,
  tuple_5,
  tuple_6,
  tuple_7,
  tuple_8,
  true,
  hh10
];

Mt.eq_suites(id, suites, "File \"int_poly_var.res\", line 79, characters 22-29", hihi, 3);

Mt.eq_suites(id, suites, "File \"int_poly_var.res\", line 80, characters 22-29", tuple, [
      0,
      1,
      2,
      3,
      4,
      5,
      5,
      3,
      6,
      true,
      false
    ]);

function hh0(x) {
  return x;
}

function hh1(x) {
  return x;
}

function f(x) {
  return x;
}

Mt.from_pair_suites("int_poly_var.res", suites.contents);

var eq_suites = Mt.eq_suites;

var u = 1;

var g = /* 'b' */98;

var hh9 = true;

var begin = 3;

exports.eq_suites = eq_suites;
exports.id = id;
exports.suites = suites;
exports.u = u;
exports.nest = nest;
exports.f2 = f2;
exports.f3 = f3;
exports.h = h;
exports.g = g;
exports.hihi = hihi;
exports.hh9 = hh9;
exports.hh10 = hh10;
exports.tuple = tuple;
exports.begin = begin;
exports.hh0 = hh0;
exports.hh1 = hh1;
exports.f = f;
/* hihi Not a pure module */
