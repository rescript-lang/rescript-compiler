'use strict';

var Curry                   = require("../../lib/js/curry.js");
var Caml_array              = require("../../lib/js/caml_array.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

function assert_bool(b) {
  if (b) {
    return /* () */0;
  } else {
    throw [
          Caml_builtin_exceptions.invalid_argument,
          "Assertion Failure."
        ];
  }
}

function fail() {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "js_promise_basic_test.ml",
          12,
          2
        ]
      ];
}

function thenTest() {
  var p = Promise.resolve(4);
  return p.then(function (x) {
              return Promise.resolve(assert_bool(+(x === 4)));
            });
}

function andThenTest() {
  var p = Promise.resolve(6);
  return p.then(function () {
                return Promise.resolve(12);
              }).then(function (y) {
              return Promise.resolve(assert_bool(+(y === 12)));
            });
}

function catchTest() {
  var p = Promise.reject("error");
  return p.then(fail).catch(function (error) {
              return Promise.resolve(assert_bool(+(error === "error")));
            });
}

function orResolvedTest() {
  var p = Promise.resolve(42);
  return p.catch(function () {
                  return Promise.resolve(22);
                }).then(function (value) {
                return Promise.resolve(assert_bool(+(value === 42)));
              }).catch(fail);
}

function orRejectedTest() {
  var p = Promise.reject("error");
  return p.catch(function () {
                  return Promise.resolve(22);
                }).then(function (value) {
                return Promise.resolve(assert_bool(+(value === 22)));
              }).catch(fail);
}

function orElseResolvedTest() {
  var p = Promise.resolve(42);
  return p.catch(function () {
                  return Promise.resolve(22);
                }).then(function (value) {
                return Promise.resolve(assert_bool(+(value === 42)));
              }).catch(fail);
}

function orElseRejectedResolveTest() {
  var p = Promise.reject("error");
  return p.catch(function () {
                  return Promise.resolve(22);
                }).then(function (value) {
                return Promise.resolve(assert_bool(+(value === 22)));
              }).catch(fail);
}

function orElseRejectedRejectTest() {
  var p = Promise.reject("error");
  return p.catch(function () {
                  return Promise.reject("error 2");
                }).then(fail).catch(function (error) {
              return Promise.resolve(assert_bool(+(error === "error 2")));
            });
}

function resolveTest() {
  var p1 = Promise.resolve(10);
  return p1.then(function (x) {
              return Promise.resolve(assert_bool(+(x === 10)));
            });
}

function rejectTest() {
  var p = Promise.reject("error");
  return p.catch(function (error) {
              return Promise.resolve(assert_bool(+(error === "error")));
            });
}

function thenCatchChainResolvedTest() {
  var p = Promise.resolve(20);
  return p.then(function (value) {
                return Promise.resolve(assert_bool(+(value === 20)));
              }).catch(fail);
}

function thenCatchChainRejectedTest() {
  var p = Promise.reject("error");
  return p.then(fail).catch(function (error) {
              return Promise.resolve(assert_bool(+(error === "error")));
            });
}

var h = Promise.resolve(/* () */0);

function allResolvedTest() {
  var p1 = Promise.resolve(1);
  var p2 = Promise.resolve(2);
  var p3 = Promise.resolve(3);
  var promises = /* array */[
    p1,
    p2,
    p3
  ];
  return Promise.all(promises).then(function (resolved) {
              assert_bool(+(Caml_array.caml_array_get(resolved, 0) === 1));
              assert_bool(+(Caml_array.caml_array_get(resolved, 1) === 2));
              assert_bool(+(Caml_array.caml_array_get(resolved, 2) === 3));
              return h;
            });
}

function allRejectTest() {
  var p1 = Promise.resolve(1);
  var p2 = Promise.resolve(3);
  var p3 = Promise.reject("error");
  var promises = /* array */[
    p1,
    p2,
    p3
  ];
  return Promise.all(promises).then(fail).catch(function (error) {
              assert_bool(+(error === "error"));
              return h;
            });
}

function raceTest() {
  var p1 = Promise.resolve("first");
  var p2 = Promise.resolve("second");
  var p3 = Promise.resolve("third");
  var promises = /* array */[
    p1,
    p2,
    p3
  ];
  return Promise.race(promises).then(function () {
                return h;
              }).catch(fail);
}

function createPromiseRejectTest() {
  return new Promise(function (_, reject) {
                return Curry._1(reject, "error");
              }).catch(function (error) {
              assert_bool(+(error === "error"));
              return h;
            });
}

function createPromiseFulfillTest() {
  return new Promise(function (resolve, _) {
                  return Curry._1(resolve, "success");
                }).then(function (resolved) {
                assert_bool(+(resolved === "success"));
                return h;
              }).catch(fail);
}

thenTest(/* () */0);

andThenTest(/* () */0);

catchTest(/* () */0);

orResolvedTest(/* () */0);

orRejectedTest(/* () */0);

orElseResolvedTest(/* () */0);

orElseRejectedResolveTest(/* () */0);

orElseRejectedRejectTest(/* () */0);

thenCatchChainResolvedTest(/* () */0);

thenCatchChainRejectedTest(/* () */0);

allResolvedTest(/* () */0);

allRejectTest(/* () */0);

raceTest(/* () */0);

createPromiseRejectTest(/* () */0);

createPromiseFulfillTest(/* () */0);

exports.assert_bool                = assert_bool;
exports.fail                       = fail;
exports.thenTest                   = thenTest;
exports.andThenTest                = andThenTest;
exports.catchTest                  = catchTest;
exports.orResolvedTest             = orResolvedTest;
exports.orRejectedTest             = orRejectedTest;
exports.orElseResolvedTest         = orElseResolvedTest;
exports.orElseRejectedResolveTest  = orElseRejectedResolveTest;
exports.orElseRejectedRejectTest   = orElseRejectedRejectTest;
exports.resolveTest                = resolveTest;
exports.rejectTest                 = rejectTest;
exports.thenCatchChainResolvedTest = thenCatchChainResolvedTest;
exports.thenCatchChainRejectedTest = thenCatchChainRejectedTest;
exports.h                          = h;
exports.allResolvedTest            = allResolvedTest;
exports.allRejectTest              = allRejectTest;
exports.raceTest                   = raceTest;
exports.createPromiseRejectTest    = createPromiseRejectTest;
exports.createPromiseFulfillTest   = createPromiseFulfillTest;
/* h Not a pure module */
