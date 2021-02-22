'use strict';

var Mt = require("./mt.js");
var Caml_oo_curry = require("../../lib/js/caml_oo_curry.js");

function f(u) {
  return Caml_oo_curry.js2(5740587, 1, u, 32);
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

exports.f = f;
exports.f_js = f_js;
exports.suites = suites;
/*  Not a pure module */
