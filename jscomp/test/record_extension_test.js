'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_format = require("../../lib/js/caml_format.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

var Inline_record = Caml_exceptions.create("Record_extension_test.Inline_record");

function f(x) {
  if (x.ExceptionID === Inline_record.ExceptionID) {
    return x.x + Caml_format.caml_int_of_string(x.y) | 0;
  }
  
}

var v0_000 = Inline_record.ExceptionID;

var v0_003 = Inline_record.Debug;

var v0 = {
  ExceptionID: v0_000,
  x: 3,
  y: "4",
  Debug: v0_003
};

eq("File \"record_extension_test.ml\", line 19, characters 6-13", f(v0), 7);

function f2(x) {
  if (typeof x === "number" || x.tag) {
    return 0;
  } else {
    return x[/* x */0];
  }
}

function f2_with(x) {
  if (typeof x === "number" || x.tag) {
    return x;
  } else {
    return /* C */Block.__(0, [
              /* x */0,
              /* y */x[/* y */1]
            ]);
  }
}

var A = Caml_exceptions.create("Record_extension_test.A");

var B = Caml_exceptions.create("Record_extension_test.B");

var C = Caml_exceptions.create("Record_extension_test.C");

function u(f) {
  try {
    return Curry._1(f, undefined);
  }
  catch (raw_x){
    var x = Caml_js_exceptions.internalToOCamlException(raw_x);
    if (x.ExceptionID === A.ExceptionID) {
      return x.name + x.x | 0;
    } else if (x.ExceptionID === B.ExceptionID) {
      return x._1 + x._2 | 0;
    } else if (x.ExceptionID === C.ExceptionID) {
      return x.name;
    } else {
      return -1;
    }
  }
}

Mt.from_pair_suites("File \"record_extension_test.ml\", line 56, characters 22-29", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.Inline_record = Inline_record;
exports.f = f;
exports.v0 = v0;
exports.f2 = f2;
exports.f2_with = f2_with;
exports.A = A;
exports.B = B;
exports.C = C;
exports.u = u;
/*  Not a pure module */
