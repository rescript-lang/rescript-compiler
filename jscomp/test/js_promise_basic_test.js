'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Js_promise = require("../../lib/js/js_promise.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = {
    hd: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    tl: suites.contents
  };
  
}

function assert_bool(b) {
  if (b) {
    return ;
  }
  throw {
        RE_EXN_ID: "Invalid_argument",
        _1: "Assertion Failure.",
        Error: new Error()
      };
}

function fail(param) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "js_promise_basic_test.ml",
          19,
          2
        ],
        Error: new Error()
      };
}

function thenTest(param) {
  var p = Promise.resolve(4);
  var arg1 = function (x) {
    return Promise.resolve(assert_bool(x === 4));
  };
  return p.then(arg1);
}

function andThenTest(param) {
  var p = Promise.resolve(6);
  var arg1 = function (param) {
    return Promise.resolve(12);
  };
  var obj = p.then(arg1);
  var arg1$1 = function (y) {
    return Promise.resolve(assert_bool(y === 12));
  };
  return obj.then(arg1$1);
}

var h = Promise.resolve(undefined);

function assertIsNotFound(x) {
  var match = Caml_exceptions.is_extension(x) && x.RE_EXN_ID === "Not_found" ? 0 : undefined;
  if (match !== undefined) {
    return h;
  }
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "js_promise_basic_test.ml",
          36,
          9
        ],
        Error: new Error()
      };
}

function catchTest(param) {
  var p = Promise.reject({
        RE_EXN_ID: "Not_found"
      });
  var obj = p.then(fail);
  var arg1 = assertIsNotFound;
  return obj.catch(arg1);
}

function orResolvedTest(param) {
  var p = Promise.resolve(42);
  var arg1 = function (param) {
    return Promise.resolve(22);
  };
  var obj = p.catch(arg1);
  var arg1$1 = function (value) {
    return Promise.resolve(assert_bool(value === 42));
  };
  var obj$1 = obj.then(arg1$1);
  return obj$1.catch(fail);
}

function orRejectedTest(param) {
  var p = Promise.reject({
        RE_EXN_ID: "Not_found"
      });
  var arg1 = function (param) {
    return Promise.resolve(22);
  };
  var obj = p.catch(arg1);
  var arg1$1 = function (value) {
    return Promise.resolve(assert_bool(value === 22));
  };
  var obj$1 = obj.then(arg1$1);
  return obj$1.catch(fail);
}

function orElseResolvedTest(param) {
  var p = Promise.resolve(42);
  var arg1 = function (param) {
    return Promise.resolve(22);
  };
  var obj = p.catch(arg1);
  var arg1$1 = function (value) {
    return Promise.resolve(assert_bool(value === 42));
  };
  var obj$1 = obj.then(arg1$1);
  return obj$1.catch(fail);
}

function orElseRejectedResolveTest(param) {
  var p = Promise.reject({
        RE_EXN_ID: "Not_found"
      });
  var arg1 = function (param) {
    return Promise.resolve(22);
  };
  var obj = p.catch(arg1);
  var arg1$1 = function (value) {
    return Promise.resolve(assert_bool(value === 22));
  };
  var obj$1 = obj.then(arg1$1);
  return obj$1.catch(fail);
}

function orElseRejectedRejectTest(param) {
  var p = Promise.reject({
        RE_EXN_ID: "Not_found"
      });
  var arg1 = function (param) {
    return Promise.reject({
                RE_EXN_ID: "Stack_overflow"
              });
  };
  var obj = p.catch(arg1);
  var obj$1 = obj.then(fail);
  var arg1$1 = function (error) {
    var match = Caml_exceptions.is_extension(error) && error.RE_EXN_ID === "Stack_overflow" ? 0 : undefined;
    if (match !== undefined) {
      return h;
    }
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "js_promise_basic_test.ml",
            77,
            18
          ],
          Error: new Error()
        };
  };
  return obj$1.catch(arg1$1);
}

function resolveTest(param) {
  var p1 = Promise.resolve(10);
  var arg1 = function (x) {
    return Promise.resolve(assert_bool(x === 10));
  };
  return p1.then(arg1);
}

function rejectTest(param) {
  var p = Promise.reject({
        RE_EXN_ID: "Not_found"
      });
  var arg1 = assertIsNotFound;
  return p.catch(arg1);
}

function thenCatchChainResolvedTest(param) {
  var p = Promise.resolve(20);
  var arg1 = function (value) {
    return Promise.resolve(assert_bool(value === 20));
  };
  var obj = p.then(arg1);
  return obj.catch(fail);
}

function thenCatchChainRejectedTest(param) {
  var p = Promise.reject({
        RE_EXN_ID: "Not_found"
      });
  var obj = p.then(fail);
  var arg1 = assertIsNotFound;
  return obj.catch(arg1);
}

function allResolvedTest(param) {
  var p1 = Promise.resolve(1);
  var p2 = Promise.resolve(2);
  var p3 = Promise.resolve(3);
  var promises = [
    p1,
    p2,
    p3
  ];
  var obj = Promise.all(promises);
  var arg1 = function (resolved) {
    assert_bool(Caml_array.get(resolved, 0) === 1);
    assert_bool(Caml_array.get(resolved, 1) === 2);
    assert_bool(Caml_array.get(resolved, 2) === 3);
    return h;
  };
  return obj.then(arg1);
}

function allRejectTest(param) {
  var p1 = Promise.resolve(1);
  var p2 = Promise.resolve(3);
  var p3 = Promise.reject({
        RE_EXN_ID: "Not_found"
      });
  var promises = [
    p1,
    p2,
    p3
  ];
  var obj = Promise.all(promises);
  var obj$1 = obj.then(fail);
  var arg1 = function (error) {
    assert_bool(error === ({
            RE_EXN_ID: "Not_found"
          }));
    return h;
  };
  return obj$1.catch(arg1);
}

function raceTest(param) {
  var p1 = Promise.resolve("first");
  var p2 = Promise.resolve("second");
  var p3 = Promise.resolve("third");
  var promises = [
    p1,
    p2,
    p3
  ];
  var obj = Promise.race(promises);
  var arg1 = function (resolved) {
    return h;
  };
  var obj$1 = obj.then(arg1);
  return obj$1.catch(fail);
}

function createPromiseRejectTest(param) {
  var obj = new Promise((function (resolve, reject) {
          return reject({
                      RE_EXN_ID: "Not_found"
                    });
        }));
  var arg1 = function (error) {
    assert_bool(error === ({
            RE_EXN_ID: "Not_found"
          }));
    return h;
  };
  return obj.catch(arg1);
}

function createPromiseFulfillTest(param) {
  var obj = new Promise((function (resolve, param) {
          return resolve("success");
        }));
  var arg1 = function (resolved) {
    assert_bool(resolved === "success");
    return h;
  };
  var obj$1 = obj.then(arg1);
  return obj$1.catch(fail);
}

thenTest(undefined);

andThenTest(undefined);

catchTest(undefined);

orResolvedTest(undefined);

orRejectedTest(undefined);

orElseResolvedTest(undefined);

orElseRejectedResolveTest(undefined);

orElseRejectedRejectTest(undefined);

thenCatchChainResolvedTest(undefined);

thenCatchChainRejectedTest(undefined);

allResolvedTest(undefined);

allRejectTest(undefined);

raceTest(undefined);

createPromiseRejectTest(undefined);

createPromiseFulfillTest(undefined);

var obj = Promise.all([
      Promise.resolve(2),
      Promise.resolve(3)
    ]);

function arg1(param) {
  eq("File \"js_promise_basic_test.ml\", line 169, characters 7-14", [
        param[0],
        param[1]
      ], [
        2,
        3
      ]);
  return Promise.resolve(undefined);
}

obj.then(arg1);

console.log(List.length(suites.contents));

console.log("hey");

Mt.from_pair_suites("Js_promise_basic_test", suites.contents);

var twop = Promise.resolve(2);

function re(prim) {
  return Promise.resolve(prim);
}

Mt.from_promise_suites("Js_promise_basic_test", {
      hd: [
        "File \"js_promise_basic_test.ml\", line 187, characters 4-11",
        twop.then(function (x) {
              return Promise.resolve({
                          TAG: /* Eq */0,
                          _0: x,
                          _1: 2
                        });
            })
      ],
      tl: {
        hd: [
          "File \"js_promise_basic_test.ml\", line 190, characters 4-11",
          twop.then(function (x) {
                return Promise.resolve({
                            TAG: /* Neq */1,
                            _0: x,
                            _1: 3
                          });
              })
        ],
        tl: /* [] */0
      }
    });

var then_ = Js_promise.then_;

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.assert_bool = assert_bool;
exports.fail = fail;
exports.thenTest = thenTest;
exports.andThenTest = andThenTest;
exports.h = h;
exports.assertIsNotFound = assertIsNotFound;
exports.catchTest = catchTest;
exports.orResolvedTest = orResolvedTest;
exports.orRejectedTest = orRejectedTest;
exports.orElseResolvedTest = orElseResolvedTest;
exports.orElseRejectedResolveTest = orElseRejectedResolveTest;
exports.orElseRejectedRejectTest = orElseRejectedRejectTest;
exports.resolveTest = resolveTest;
exports.rejectTest = rejectTest;
exports.thenCatchChainResolvedTest = thenCatchChainResolvedTest;
exports.thenCatchChainRejectedTest = thenCatchChainRejectedTest;
exports.allResolvedTest = allResolvedTest;
exports.allRejectTest = allRejectTest;
exports.raceTest = raceTest;
exports.createPromiseRejectTest = createPromiseRejectTest;
exports.createPromiseFulfillTest = createPromiseFulfillTest;
exports.twop = twop;
exports.then_ = then_;
exports.re = re;
/* h Not a pure module */
