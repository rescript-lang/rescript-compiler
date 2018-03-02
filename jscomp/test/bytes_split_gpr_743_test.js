'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Bytes = require("../../lib/js/bytes.js");
var Caml_string = require("../../lib/js/caml_string.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
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

var b = Caml_string.caml_create_string(3);

b[0] = /* "a" */97;

b[1] = /* "b" */98;

b[2] = /* "c" */99;

Bytes.blit(b, 0, b, 1, 2);

var res = Caml_string.bytes_to_string(b);

console.log(res);

eq("File \"bytes_split_gpr_743_test.ml\", line 17, characters 5-12", /* tuple */[
      "aab",
      res
    ]);

var b$1 = Caml_string.caml_create_string(3);

b$1[0] = /* "a" */97;

b$1[1] = /* "b" */98;

b$1[2] = /* "c" */99;

Bytes.blit(b$1, 1, b$1, 0, 2);

var res2 = Caml_string.bytes_to_string(b$1);

console.log(res2);

eq("File \"bytes_split_gpr_743_test.ml\", line 32, characters 5-12", /* tuple */[
      "bcc",
      res2
    ]);

Mt.from_pair_suites("bytes_split_gpr_743_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
