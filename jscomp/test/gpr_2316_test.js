'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
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
  
}

var y;

try {
  throw {
        CamlExt: Caml_builtin_exceptions.failure,
        _1: "boo"
      };
}
catch (raw_msg){
  var msg = Caml_js_exceptions.internalToOCamlException(raw_msg);
  if (msg.CamlExt.CamlId === Caml_builtin_exceptions.failure.CamlId) {
    y = msg._1;
  } else {
    throw msg;
  }
}

var x;

var exit = 0;

try {
  throw {
        CamlExt: Caml_builtin_exceptions.failure,
        _1: "boo"
      };
}
catch (raw_msg$1){
  var msg$1 = Caml_js_exceptions.internalToOCamlException(raw_msg$1);
  if (msg$1.CamlExt.CamlId === Caml_builtin_exceptions.failure.CamlId) {
    x = msg$1._1;
  } else {
    throw msg$1;
  }
}

if (exit === 1) {
  console.log("ok");
  x = undefined;
}

eq("File \"gpr_2316_test.ml\", line 20, characters 5-12", y, "boo");

eq("File \"gpr_2316_test.ml\", line 21, characters 5-12", x, "boo");

Mt.from_pair_suites("Gpr_2316_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.y = y;
exports.x = x;
/* y Not a pure module */
