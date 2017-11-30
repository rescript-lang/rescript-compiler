'use strict';

var Mt                      = require("./mt.js");
var Block                   = require("../../lib/js/block.js");
var Js_exn                  = require("../../lib/js/js_exn.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

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

var y;

try {
  throw [
        Caml_builtin_exceptions.failure,
        "boo"
      ];
}
catch (raw_exn){
  var exn = Js_exn.internalToOCamlException(raw_exn);
  if (exn[0] === Caml_builtin_exceptions.failure) {
    y = /* Some */[exn[1]];
  } else {
    throw exn;
  }
}

var x;

var exit = 0;

try {
  throw [
        Caml_builtin_exceptions.failure,
        "boo"
      ];
}
catch (raw_exn$1){
  var exn$1 = Js_exn.internalToOCamlException(raw_exn$1);
  if (exn$1[0] === Caml_builtin_exceptions.failure) {
    x = /* Some */[exn$1[1]];
  } else {
    throw exn$1;
  }
}

if (exit === 1) {
  console.log("ok");
  x = /* None */0;
}

eq("File \"gpr_2316_test.ml\", line 20, characters 5-12", y, /* Some */["boo"]);

eq("File \"gpr_2316_test.ml\", line 21, characters 5-12", x, /* Some */["boo"]);

Mt.from_pair_suites("gpr_2316_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
exports.y       = y;
exports.x       = x;
/* y Not a pure module */
