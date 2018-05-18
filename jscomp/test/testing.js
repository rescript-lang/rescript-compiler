'use strict';

var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Scanf = require("../../lib/js/scanf.js");
var Js_exn = require("../../lib/js/js_exn.js");
var Printf = require("../../lib/js/printf.js");
var Caml_io = require("../../lib/js/caml_io.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var all_tests_ok = [true];

function finish() {
  var match = all_tests_ok[0];
  if (match) {
    console.log("\nAll tests succeeded.");
    return /* () */0;
  } else {
    console.log("\n\n********* Test suite failed. ***********\n");
    return /* () */0;
  }
}

Pervasives.at_exit(finish);

var test_num = [-1];

function print_test_number() {
  Pervasives.print_string(" ");
  Pervasives.print_int(test_num[0]);
  return Caml_io.caml_ml_flush(Pervasives.stdout);
}

function print_failure_test_fail() {
  all_tests_ok[0] = false;
  return Pervasives.print_string(Curry._1(Printf.sprintf(/* Format */[
                      /* String_literal */Block.__(11, [
                          "\n********* Failure Test number ",
                          /* Int */Block.__(4, [
                              /* Int_i */3,
                              /* No_padding */0,
                              /* No_precision */0,
                              /* String_literal */Block.__(11, [
                                  " incorrectly failed ***********\n",
                                  /* End_of_format */0
                                ])
                            ])
                        ]),
                      "\n********* Failure Test number %i incorrectly failed ***********\n"
                    ]), test_num[0]));
}

function print_failure_test_succeed() {
  all_tests_ok[0] = false;
  return Pervasives.print_string(Curry._1(Printf.sprintf(/* Format */[
                      /* String_literal */Block.__(11, [
                          "\n********* Failure Test number ",
                          /* Int */Block.__(4, [
                              /* Int_i */3,
                              /* No_padding */0,
                              /* No_precision */0,
                              /* String_literal */Block.__(11, [
                                  " failed to fail ***********\n",
                                  /* End_of_format */0
                                ])
                            ])
                        ]),
                      "\n********* Failure Test number %i failed to fail ***********\n"
                    ]), test_num[0]));
}

function test(b) {
  test_num[0] = test_num[0] + 1 | 0;
  print_test_number(/* () */0);
  if (b) {
    return 0;
  } else {
    all_tests_ok[0] = false;
    return Pervasives.print_string(Curry._1(Printf.sprintf(/* Format */[
                        /* String_literal */Block.__(11, [
                            "\n********* Test number ",
                            /* Int */Block.__(4, [
                                /* Int_i */3,
                                /* No_padding */0,
                                /* No_precision */0,
                                /* String_literal */Block.__(11, [
                                    " failed ***********\n",
                                    /* End_of_format */0
                                  ])
                              ])
                          ]),
                        "\n********* Test number %i failed ***********\n"
                      ]), test_num[0]));
  }
}

function test_raises_exc_p(pred, f, x) {
  test_num[0] = test_num[0] + 1 | 0;
  print_test_number(/* () */0);
  try {
    Curry._1(f, x);
    print_failure_test_succeed(/* () */0);
    return false;
  }
  catch (raw_x){
    var x$1 = Js_exn.internalToOCamlException(raw_x);
    if (Curry._1(pred, x$1)) {
      return true;
    } else {
      print_failure_test_fail(/* () */0);
      return false;
    }
  }
}

function test_raises_some_exc(f) {
  return (function (param) {
      return test_raises_exc_p((function () {
                    return true;
                  }), f, param);
    });
}

function test_raises_this_exc(exc) {
  return (function (param, param$1) {
      return test_raises_exc_p((function (x) {
                    return Caml_obj.caml_equal(x, exc);
                  }), param, param$1);
    });
}

function failure_test(f, x, s) {
  var s$1 = s;
  var f$1 = f;
  var x$1 = x;
  return test_raises_exc_p((function (x) {
                return Caml_obj.caml_equal(x, [
                            Caml_builtin_exceptions.failure,
                            s$1
                          ]);
              }), f$1, x$1);
}

function scan_failure_test(f, x) {
  return test_raises_exc_p((function (param) {
                return param[0] === Scanf.Scan_failure;
              }), f, x);
}

exports.test = test;
exports.failure_test = failure_test;
exports.test_raises_some_exc = test_raises_some_exc;
exports.test_raises_this_exc = test_raises_this_exc;
exports.test_raises_exc_p = test_raises_exc_p;
exports.scan_failure_test = scan_failure_test;
/*  Not a pure module */
