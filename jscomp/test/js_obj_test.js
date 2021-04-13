'use strict';

var Mt = require("./mt.js");
var Curry = require("../../lib/js/curry.js");

function f9($$window) {
  return Curry._2($$window.location, 1, 2);
}

function f(u) {
  var h = u.say;
  return Curry._1(h, 32);
}

function f_js(u) {
  return u.say(32);
}

var suites_0 = [
  "js_obj",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: 34,
              _1: ({
                    say: (function (x) {
                        return x + 2 | 0;
                      })
                  }).say(32)
            };
    })
];

var suites_1 = {
  hd: [
    "js_obj2",
    (function (param) {
        return {
                TAG: /* Eq */0,
                _0: 34,
                _1: ({
                      say: (function (x) {
                          return x + 2 | 0;
                        })
                    }).say(32)
              };
      })
  ],
  tl: {
    hd: [
      "empty",
      (function (param) {
          return {
                  TAG: /* Eq */0,
                  _0: 0,
                  _1: Object.keys({}).length
                };
        })
    ],
    tl: {
      hd: [
        "assign",
        (function (param) {
            return {
                    TAG: /* Eq */0,
                    _0: {
                      a: 1
                    },
                    _1: Object.assign({}, {
                          a: 1
                        })
                  };
          })
      ],
      tl: /* [] */0
    }
  }
};

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Js_obj_test", suites);

exports.f9 = f9;
exports.f = f;
exports.f_js = f_js;
exports.suites = suites;
/*  Not a pure module */
