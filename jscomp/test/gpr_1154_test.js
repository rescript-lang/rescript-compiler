'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Int64 = require("../../lib/js/int64.js");
var Caml_int64 = require("../../lib/js/caml_int64.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
  Pervasives.incr(test_id);
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  return /* () */0;
}

function f(x) {
  Pervasives.print_string("f");
  return x;
}

function g(x) {
  return Caml_int64.or_(x, (Pervasives.print_string("f"), x));
}

var v = /* record */{
  contents: 0
};

function f2(x) {
  Pervasives.incr(v);
  return x;
}

function g2(x) {
  return Caml_int64.or_(x, (Pervasives.incr(v), x));
}

var a = Caml_int64.or_(Int64.one, (Pervasives.incr(v), Int64.one));

eq("File \"gpr_1154_test.ml\", line 27, characters 12-19", v.contents, 1);

Mt.from_pair_suites("Gpr_1154_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.f = f;
exports.g = g;
exports.v = v;
exports.f2 = f2;
exports.g2 = g2;
exports.a = a;
/* a Not a pure module */
