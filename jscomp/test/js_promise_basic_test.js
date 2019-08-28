'use strict';

var Mt = require("./mt.js");
var List = require("../../lib/js/list.js");
var Block = require("../../lib/js/block.js");
var Curry = require("../../lib/js/curry.js");
var Caml_array = require("../../lib/js/caml_array.js");
var Pervasives = require("../../lib/js/pervasives.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = /* record */{
  contents: /* [] */0
};

var test_id = /* record */{
  contents: 0
};

function eq(loc, x, y) {
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

function fail(param) {
  throw [
        Caml_builtin_exceptions.assert_failure,
        /* tuple */[
          "js_promise_basic_test.ml",
          19,
          2
        ]
      ];
}

function thenTest(param) {
  var p = Promise.resolve(4);
  return p.then((function (x) {
                return Promise.resolve(assert_bool(x === 4));
              }));
}

function andThenTest(param) {
  var p = Promise.resolve(6);
  return p.then((function (param) {
                  return Promise.resolve(12);
                })).then((function (y) {
                return Promise.resolve(assert_bool(y === 12));
              }));
}

var h = Promise.resolve(/* () */0);

function assertIsNotFound(x) {
  var match = Caml_exceptions.caml_is_extension(x) && x === Caml_builtin_exceptions.not_found ? 0 : undefined;
  if (match !== undefined) {
    return h;
  } else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          /* tuple */[
            "js_promise_basic_test.ml",
            36,
            9
          ]
        ];
  }
}

function catchTest(param) {
  var p = Promise.reject(Caml_builtin_exceptions.not_found);
  return p.then(fail).catch(assertIsNotFound);
}

function orResolvedTest(param) {
  var p = Promise.resolve(42);
  return p.catch((function (param) {
                    return Promise.resolve(22);
                  })).then((function (value) {
                  return Promise.resolve(assert_bool(value === 42));
                })).catch(fail);
}

function orRejectedTest(param) {
  var p = Promise.reject(Caml_builtin_exceptions.not_found);
  return p.catch((function (param) {
                    return Promise.resolve(22);
                  })).then((function (value) {
                  return Promise.resolve(assert_bool(value === 22));
                })).catch(fail);
}

function orElseResolvedTest(param) {
  var p = Promise.resolve(42);
  return p.catch((function (param) {
                    return Promise.resolve(22);
                  })).then((function (value) {
                  return Promise.resolve(assert_bool(value === 42));
                })).catch(fail);
}

function orElseRejectedResolveTest(param) {
  var p = Promise.reject(Caml_builtin_exceptions.not_found);
  return p.catch((function (param) {
                    return Promise.resolve(22);
                  })).then((function (value) {
                  return Promise.resolve(assert_bool(value === 22));
                })).catch(fail);
}

function orElseRejectedRejectTest(param) {
  var p = Promise.reject(Caml_builtin_exceptions.not_found);
  return p.catch((function (param) {
                    return Promise.reject(Caml_builtin_exceptions.stack_overflow);
                  })).then(fail).catch((function (error) {
                var match = Caml_exceptions.caml_is_extension(error) && error === Caml_builtin_exceptions.stack_overflow ? 0 : undefined;
                if (match !== undefined) {
                  return h;
                } else {
                  throw [
                        Caml_builtin_exceptions.assert_failure,
                        /* tuple */[
                          "js_promise_basic_test.ml",
                          77,
                          18
                        ]
                      ];
                }
              }));
}

function resolveTest(param) {
  var p1 = Promise.resolve(10);
  return p1.then((function (x) {
                return Promise.resolve(assert_bool(x === 10));
              }));
}

function rejectTest(param) {
  var p = Promise.reject(Caml_builtin_exceptions.not_found);
  return p.catch(assertIsNotFound);
}

function thenCatchChainResolvedTest(param) {
  var p = Promise.resolve(20);
  return p.then((function (value) {
                  return Promise.resolve(assert_bool(value === 20));
                })).catch(fail);
}

function thenCatchChainRejectedTest(param) {
  var p = Promise.reject(Caml_builtin_exceptions.not_found);
  return p.then(fail).catch(assertIsNotFound);
}

function allResolvedTest(param) {
  var p1 = Promise.resolve(1);
  var p2 = Promise.resolve(2);
  var p3 = Promise.resolve(3);
  var promises = /* array */[
    p1,
    p2,
    p3
  ];
  return Promise.all(promises).then((function (resolved) {
                assert_bool(Caml_array.caml_array_get(resolved, 0) === 1);
                assert_bool(Caml_array.caml_array_get(resolved, 1) === 2);
                assert_bool(Caml_array.caml_array_get(resolved, 2) === 3);
                return h;
              }));
}

function allRejectTest(param) {
  var p1 = Promise.resolve(1);
  var p2 = Promise.resolve(3);
  var p3 = Promise.reject(Caml_builtin_exceptions.not_found);
  var promises = /* array */[
    p1,
    p2,
    p3
  ];
  return Promise.all(promises).then(fail).catch((function (error) {
                assert_bool(error === Caml_builtin_exceptions.not_found);
                return h;
              }));
}

function raceTest(param) {
  var p1 = Promise.resolve("first");
  var p2 = Promise.resolve("second");
  var p3 = Promise.resolve("third");
  var promises = /* array */[
    p1,
    p2,
    p3
  ];
  return Promise.race(promises).then((function (resolved) {
                  return h;
                })).catch(fail);
}

function createPromiseRejectTest(param) {
  return new Promise((function (resolve, reject) {
                  return reject(Caml_builtin_exceptions.not_found);
                })).catch((function (error) {
                assert_bool(error === Caml_builtin_exceptions.not_found);
                return h;
              }));
}

function createPromiseFulfillTest(param) {
  return new Promise((function (resolve, param) {
                    return resolve("success");
                  })).then((function (resolved) {
                  assert_bool(resolved === "success");
                  return h;
                })).catch(fail);
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

Promise.all(/* tuple */[
        Promise.resolve(2),
        Promise.resolve(3)
      ]).then((function (param) {
        eq("File \"js_promise_basic_test.ml\", line 169, characters 7-14", /* tuple */[
              param[0],
              param[1]
            ], /* tuple */[
              2,
              3
            ]);
        return Promise.resolve(/* () */0);
      }));

console.log(List.length(suites.contents));

console.log("hey");

Mt.from_pair_suites("Js_promise_basic_test", suites.contents);

var twop = Promise.resolve(2);

function then_(prim, prim$1) {
  return prim$1.then(Curry.__1(prim));
}

function re(prim) {
  return Promise.resolve(prim);
}

Mt.from_promise_suites("Js_promise_basic_test", /* :: */[
      /* tuple */[
        "File \"js_promise_basic_test.ml\", line 187, characters 4-11",
        twop.then((function (x) {
                return Promise.resolve(/* Eq */Block.__(0, [
                              x,
                              2
                            ]));
              }))
      ],
      /* :: */[
        /* tuple */[
          "File \"js_promise_basic_test.ml\", line 190, characters 4-11",
          twop.then((function (x) {
                  return Promise.resolve(/* Neq */Block.__(1, [
                                x,
                                3
                              ]));
                }))
        ],
        /* [] */0
      ]
    ]);

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
