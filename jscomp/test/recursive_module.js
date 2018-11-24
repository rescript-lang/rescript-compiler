'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");
var Caml_module = require("../../lib/js/caml_module.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = /* record */[/* contents : [] */0];

var test_id = /* record */[/* contents */0];

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var Int32 = Caml_module.init_mod([
      "recursive_module.ml",
      14,
      6
    ], [[
        0,
        0,
        0,
        0,
        0
      ]]);

Caml_module.update_mod([[
        0,
        0,
        0,
        0,
        0
      ]], Int32, Int32);

var Int3 = Caml_module.init_mod([
      "recursive_module.ml",
      19,
      6
    ], [[0]]);

Caml_module.update_mod([[0]], Int3, Int3);

var tmp;

try {
  Curry._1(Int3[/* u */0], 3);
  tmp = 3;
}
catch (raw_exn){
  var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
  if (exn[0] === Caml_builtin_exceptions.undefined_recursive_module) {
    tmp = 4;
  } else {
    throw exn;
  }
}

eq("File \"recursive_module.ml\", line 24, characters 6-13", 4, tmp);

Mt.from_pair_suites("recursive_module.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.Int32 = Int32;
exports.Int3 = Int3;
/* Int32 Not a pure module */
