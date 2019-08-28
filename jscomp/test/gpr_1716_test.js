'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
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

var a = [];

var b = [];

Caml_obj.caml_update_dummy(a, /* record */{
      b: b
    });

Caml_obj.caml_update_dummy(b, /* record */{
      a: a
    });

function is_inifite(x) {
  return x.b.a === x;
}

eq("File \"gpr_1716_test.ml\", line 26, characters 6-13", true, is_inifite(a));

Mt.from_pair_suites("Gpr_1716_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.a = a;
exports.b = b;
exports.is_inifite = is_inifite;
/*  Not a pure module */
