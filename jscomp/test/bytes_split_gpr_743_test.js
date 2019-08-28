'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Bytes = require("../../lib/js/bytes.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Pervasives = require("../../lib/js/pervasives.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
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

var b = [
  0,
  0,
  0
];

b[0] = /* "a" */97;

b[1] = /* "b" */98;

b[2] = /* "c" */99;

Bytes.blit(b, 0, b, 1, 2);

var res = Caml_bytes.bytes_to_string(b);

console.log(res);

eq("File \"bytes_split_gpr_743_test.ml\", line 17, characters 5-12", /* tuple */[
      "aab",
      res
    ]);

var b$1 = [
  0,
  0,
  0
];

b$1[0] = /* "a" */97;

b$1[1] = /* "b" */98;

b$1[2] = /* "c" */99;

Bytes.blit(b$1, 1, b$1, 0, 2);

var res2 = Caml_bytes.bytes_to_string(b$1);

console.log(res2);

eq("File \"bytes_split_gpr_743_test.ml\", line 32, characters 5-12", /* tuple */[
      "bcc",
      res2
    ]);

Mt.from_pair_suites("Bytes_split_gpr_743_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/*  Not a pure module */
