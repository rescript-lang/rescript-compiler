// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var CamlinternalLazy = require("../stdlib/camlinternalLazy");
var Mt               = require("./mt");
var Assert           = require("assert");

var u = [3];

var v = {
  0: function () {
    u[0] = 32;
    return /* () */0;
  },
  length: 1,
  tag: 246
};

function lazy_test() {
  var h = u[0];
  var tag = v.tag | 0;
  if (tag !== 250) {
    if (tag === 246) {
      CamlinternalLazy.force_lazy_block(v);
    }
    else {
      ;
    }
  }
  var g = u[0];
  return /* tuple */[
          h,
          g
        ];
}

Mt.from_suites("lazy", /* :: */[
      /* tuple */[
        "simple",
        function () {
          var prim = lazy_test(/* () */0);
          return Assert.deepEqual(prim, /* tuple */[
                      3,
                      32
                    ]);
        }
      ],
      /* [] */0
    ]);

exports.u         = u;
exports.v         = v;
exports.lazy_test = lazy_test;
/*  Not a pure module */
