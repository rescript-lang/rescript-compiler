'use strict';

var Mt = require("./mt.js");
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

eq("File \"406_primitive_test.ml\", line 18, characters 6-13", 32, 32);

var backend_type = /* Other */{
  _0: "BS"
};

eq("File \"406_primitive_test.ml\", line 29, characters 6-13", backend_type, /* Other */{
      _0: "BS"
    });

function f(param) {
  var A = Caml_exceptions.create("A");
  try {
    for(var i = 0; i <= 200; ++i){
      if (i === 10) {
        throw {
              RE_EXN_ID: A,
              _1: 0,
              Error: new Error()
            };
      }
      
    }
    return ;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.RE_EXN_ID === A) {
      return ;
    }
    throw exn;
  }
}

Mt.from_pair_suites("406_primitive_test", suites.contents);

var v = 32;

var max_array_length = /* Max_wosize */2147483647;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.v = v;
exports.backend_type = backend_type;
exports.max_array_length = max_array_length;
exports.f = f;
/*  Not a pure module */
