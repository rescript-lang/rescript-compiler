'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  return Mt.eq_suites(test_id, suites, loc, x, y);
}

function fib(n, k) {
  if (n === 0 || n === 1) {
    return k(1);
  } else {
    return {
            TAG: /* Suspend */1,
            _0: (function () {
                return fib(n - 1 | 0, (function (v0) {
                              return fib(n - 2 | 0, (function (v1) {
                                            return k(v0 + v1 | 0);
                                          }));
                            }));
              })
          };
  }
}

var u = fib(10, (function (x) {
        return {
                TAG: /* Continue */0,
                _0: x
              };
      }));

function iter(_bounce) {
  while(true) {
    var bounce = _bounce;
    if (!bounce.TAG) {
      return bounce._0;
    }
    _bounce = bounce._0();
    continue ;
  };
}

function isEven(n) {
  if (n !== 0) {
    if (n !== 1) {
      return {
              TAG: /* Suspend */1,
              _0: (function () {
                  return isOdd(n - 1 | 0);
                })
            };
    } else {
      return {
              TAG: /* Continue */0,
              _0: false
            };
    }
  } else {
    return {
            TAG: /* Continue */0,
            _0: true
          };
  }
}

function isOdd(n) {
  if (n !== 0) {
    if (n !== 1) {
      return isEven(n - 1 | 0);
    } else {
      return {
              TAG: /* Continue */0,
              _0: true
            };
    }
  } else {
    return {
            TAG: /* Continue */0,
            _0: false
          };
  }
}

eq("File \"tramp_fib.ml\", line 56, characters 6-13", iter(u), 89);

eq("File \"tramp_fib.ml\", line 58, characters 6-13", iter(isEven(20000)), true);

Mt.from_pair_suites("File \"tramp_fib.ml\", line 60, characters 23-30", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.fib = fib;
exports.u = u;
exports.iter = iter;
exports.isEven = isEven;
exports.isOdd = isOdd;
/* u Not a pure module */
