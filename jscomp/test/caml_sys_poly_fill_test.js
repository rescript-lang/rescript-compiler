'use strict';

var Mt           = require("./mt.js");
var Block        = require("../../lib/js/block.js");
var Caml_sys     = require("../../lib/js/caml_sys.js");
var Node_process = require("../../lib/js/node_process.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

Node_process.putEnvVar("caml_sys_poly_fill_test.ml", "X");

var v = Caml_sys.caml_sys_getenv("caml_sys_poly_fill_test.ml");

eq("File \"caml_sys_poly_fill_test.ml\", line 11, characters 5-12", "X", (Node_process.deleteEnvVar("caml_sys_poly_fill_test.ml"), v));

Node_process.putEnvVar("caml_sys_poly_fill_test.ml", "Y");

var v$1 = Caml_sys.caml_sys_getenv("caml_sys_poly_fill_test.ml");

eq("File \"caml_sys_poly_fill_test.ml\", line 17, characters 5-12", "Y", (Node_process.deleteEnvVar("caml_sys_poly_fill_test.ml"), v$1));

Node_process.deleteEnvVar("caml_sys_poly_fill_test.ml");

var tmp;

try {
  tmp = Caml_sys.caml_sys_getenv("caml_sys_poly_fill_test.ml");
}
catch (exn){
  tmp = "Z";
}

eq("File \"caml_sys_poly_fill_test.ml\", line 23, characters 5-12", "Z", tmp);

Mt.from_pair_suites("caml_sys_poly_fill_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */
