'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");

function f(x) {
  if (x) {
    return true;
  } else {
    return false;
  }
}

function f2(x) {
  if (x) {
    return true;
  } else {
    return false;
  }
}

function f4(x) {
  if (x) {
    return true;
  } else {
    return false;
  }
}

var f3 = true;

var u = ( 1);

var v = ( true);

var suites_000 = /* tuple */[
  "caml_bool_eq_caml_bool",
  (function () {
      var x = true;
      return /* Eq */Block.__(0, [
                u,
                x ? true : false
              ]);
    })
];

var suites_001 = /* :: */[
  /* tuple */[
    "js_bool_eq_js_bool",
    (function () {
        return /* Eq */Block.__(0, [
                  v,
                  true
                ]);
      })
  ],
  /* :: */[
    /* tuple */[
      "js_bool_eq_ocaml_bool",
      (function () {
          var x = true;
          return /* Eq */Block.__(0, [
                    true,
                    (
                      x ? true : false
                    ) === (true)
                  ]);
        })
    ],
    /* :: */[
      /* tuple */[
        "js_bool_is_ocaml_bool",
        (function () {
            var x = true;
            return /* Eq */Block.__(0, [
                      true,
                      (
                        x ? true : false
                      ) === true
                    ]);
          })
      ],
      /* [] */0
    ]
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("js_bool_test.ml", suites);

exports.f = f;
exports.f2 = f2;
exports.f4 = f4;
exports.f3 = f3;
exports.u = u;
exports.v = v;
exports.suites = suites;
/* f3 Not a pure module */
