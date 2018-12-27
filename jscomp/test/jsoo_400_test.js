'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_int32 = require("../../lib/js/caml_int32.js");

function u(param) {
  var exit = 0;
  var n;
  try {
    n = 3;
    exit = 1;
  }
  catch (exn){
    return 42;
  }
  if (exit === 1) {
    return Caml_int32.div(3, 0);
  }
  
}

Mt.from_pair_suites("Jsoo_400_test", /* :: */[
      /* tuple */[
        "File \"test/jsoo_400_test.ml\", line 8, characters 3-10",
        (function (param) {
            return /* ThrowAny */Block.__(7, [(function (param) {
                          u(/* () */0);
                          return /* () */0;
                        })]);
          })
      ],
      /* [] */0
    ]);

exports.u = u;
/*  Not a pure module */
