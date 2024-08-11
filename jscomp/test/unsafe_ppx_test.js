// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

let Mt = require("./mt.js");
let Pervasives = require("../../lib/js/pervasives.js");
let Ffi_js_test = require("./ffi_js_test.js");

let x = "\\x01\\x02\\x03";

let max = Math.max;

function $$test(x,y){
  return x + y;
}
;

let regression3 = Math.max;

let regression4 = Math.max;

function g(a) {
  let regression = (function(x,y){
   return ""
});
  let regression2 = Math.max;
  regression(a, Pervasives.failwith);
  regression2(3, 2);
  regression3(3, 2);
  regression4(3, ((x) => {
    return x;
  }));
}

let max2 = Math.max;

function umax(a, b) {
  return max2(a, b);
}

function u(h) {
  return max2(3, h);
}

let max3 = Math.max;

function uu(h) {
  return max2(3, h);
}

let empty = Object.keys(3);

let v = $$test(1, 2);

Mt.from_pair_suites("Unsafe_ppx_test", {
  hd: [
    "unsafe_max",
    (() => {
      return {
        TAG: "Eq",
        _0: 2,
        _1: max(1, 2)
      };
    })
  ],
  tl: {
    hd: [
      "unsafe_test",
      (() => {
        return {
          TAG: "Eq",
          _0: 3,
          _1: v
        };
      })
    ],
    tl: {
      hd: [
        "unsafe_max2",
        (() => {
          return {
            TAG: "Eq",
            _0: 2,
            _1: Math.max(1, 2)
          };
        })
      ],
      tl: {
        hd: [
          "ffi_keys",
          (() => {
            return {
              TAG: "Eq",
              _0: ["a"],
              _1: Ffi_js_test.keys({a : 3})
            };
          })
        ],
        tl: /* [] */0
      }
    }
  }
});

exports.x = x;
exports.max = max;
exports.regression3 = regression3;
exports.regression4 = regression4;
exports.g = g;
exports.max2 = max2;
exports.umax = umax;
exports.u = u;
exports.max3 = max3;
exports.uu = uu;
exports.empty = empty;
exports.v = v;
/* max Not a pure module */
