'use strict';

var Mt = require("./mt.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, x, y) {
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: [
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

function test(dom) {
  var elem = dom.getElementById("haha");
  if (elem == null) {
    return 1;
  } else {
    console.log(elem);
    return 2;
  }
}

function f(x, y) {
  console.log("no inline");
  return x + y | 0;
}

eq("File \"js_nullable_test.ml\", line 26, characters 7-14", false, false);

eq("File \"js_nullable_test.ml\", line 28, characters 7-14", (f(1, 2) == null), false);

eq("File \"js_nullable_test.ml\", line 30, characters 6-13", (null == null), true);

eq("File \"js_nullable_test.ml\", line 34, characters 3-10", false, false);

Mt.from_pair_suites("Js_nullable_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
exports.test = test;
exports.f = f;
/*  Not a pure module */
