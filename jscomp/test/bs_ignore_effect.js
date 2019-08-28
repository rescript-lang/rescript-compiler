'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
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


function add(x,y){
  return x + y
}

;

var v = /* record */{
  contents: 0
};

var h = (Pervasives.incr(v), {
    hi: 2,
    lo: 0
  });

var z = (Pervasives.incr(v), add(3.0, 2.0));

eq("File \"bs_ignore_effect.ml\", line 26, characters 5-12", v.contents, 2);

eq("File \"bs_ignore_effect.ml\", line 27, characters 5-12", z, 5.0);

Mt.from_pair_suites("Bs_ignore_effect", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.h = h;
exports.z = z;
/*  Not a pure module */
