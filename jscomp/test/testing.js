'use strict';

var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Scanf = require("../../lib/js/scanf.js");
var Printf = require("../../lib/js/printf.js");
var Caml_io = require("../../lib/js/caml_io.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var all_tests_ok = {
  contents: true
};

function finish(param) {
  var match = all_tests_ok.contents;
  if (match) {
    console.log("\nAll tests succeeded.");
  } else {
    console.log("\n\n********* Test suite failed. ***********\n");
  }
  
}

Pervasives.at_exit(finish);

var test_num = {
  contents: -1
};

function print_test_number(param) {
  Pervasives.print_string(" ");
  Pervasives.print_int(test_num.contents);
  return Caml_io.caml_ml_flush(Pervasives.stdout);
}

function print_failure_test_fail(param) {
  all_tests_ok.contents = false;
  return Pervasives.print_string(Curry._1(Printf.sprintf(/* Format */{
                      _0: {
                        tag: /* String_literal */11,
                        _0: "\n********* Failure Test number ",
                        _1: {
                          tag: /* Int */4,
                          _0: /* Int_i */3,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: {
                            tag: /* String_literal */11,
                            _0: " incorrectly failed ***********\n",
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "\n********* Failure Test number %i incorrectly failed ***********\n"
                    }), test_num.contents));
}

function print_failure_test_succeed(param) {
  all_tests_ok.contents = false;
  return Pervasives.print_string(Curry._1(Printf.sprintf(/* Format */{
                      _0: {
                        tag: /* String_literal */11,
                        _0: "\n********* Failure Test number ",
                        _1: {
                          tag: /* Int */4,
                          _0: /* Int_i */3,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: {
                            tag: /* String_literal */11,
                            _0: " failed to fail ***********\n",
                            _1: /* End_of_format */0
                          }
                        }
                      },
                      _1: "\n********* Failure Test number %i failed to fail ***********\n"
                    }), test_num.contents));
}

function test(b) {
  test_num.contents = test_num.contents + 1 | 0;
  print_test_number(undefined);
  if (!b) {
    all_tests_ok.contents = false;
    return Pervasives.print_string(Curry._1(Printf.sprintf(/* Format */{
                        _0: {
                          tag: /* String_literal */11,
                          _0: "\n********* Test number ",
                          _1: {
                            tag: /* Int */4,
                            _0: /* Int_i */3,
                            _1: /* No_padding */0,
                            _2: /* No_precision */0,
                            _3: {
                              tag: /* String_literal */11,
                              _0: " failed ***********\n",
                              _1: /* End_of_format */0
                            }
                          }
                        },
                        _1: "\n********* Test number %i failed ***********\n"
                      }), test_num.contents));
  }
  
}

function test_raises_exc_p(pred, f, x) {
  test_num.contents = test_num.contents + 1 | 0;
  print_test_number(undefined);
  try {
    Curry._1(f, x);
    print_failure_test_succeed(undefined);
    return false;
  }
  catch (raw_x){
    var x$1 = Caml_js_exceptions.internalToOCamlException(raw_x);
    if (Curry._1(pred, x$1)) {
      return true;
    } else {
      print_failure_test_fail(undefined);
      return false;
    }
  }
}

function test_raises_some_exc(f) {
  return function (param) {
    return test_raises_exc_p((function (param) {
                  return true;
                }), f, param);
  };
}

function test_raises_this_exc(exc) {
  return function (param, param$1) {
    return test_raises_exc_p((function (x) {
                  return Caml_obj.caml_equal(x, exc);
                }), param, param$1);
  };
}

function failure_test(f, x, s) {
  return test_raises_exc_p((function (x) {
                return Caml_obj.caml_equal(x, {
                            RE_EXN_ID: "Failure",
                            _1: s
                          });
              }), f, x);
}

function scan_failure_test(f, x) {
  return test_raises_exc_p((function (param) {
                return param.RE_EXN_ID === Scanf.Scan_failure;
              }), f, x);
}

exports.test = test;
exports.failure_test = failure_test;
exports.test_raises_some_exc = test_raises_some_exc;
exports.test_raises_this_exc = test_raises_this_exc;
exports.test_raises_exc_p = test_raises_exc_p;
exports.scan_failure_test = scan_failure_test;
/*  Not a pure module */
