'use strict';

var Mt = require("./mt.js");
var Bytes = require("../../lib/js/bytes.js");
var $$Buffer = require("../../lib/js/buffer.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");

var v = "gso";

var suites_0 = [
  "equal",
  (function (param) {
      return {
              TAG: /* Eq */0,
              _0: [
                Caml_bytes.get(Bytes.make(3, /* "a" */97), 0),
                Bytes.make(3, /* "a" */97)[0]
              ],
              _1: [
                /* "a" */97,
                /* "a" */97
              ]
            };
    })
];

var suites_1 = {
  hd: [
    "equal2",
    (function (param) {
        var u = Bytes.make(3, /* "a" */97);
        u[0] = /* "b" */98;
        return {
                TAG: /* Eq */0,
                _0: [
                  u[0],
                  /* "g" */103
                ],
                _1: [
                  /* "b" */98,
                  /* "g" */103
                ]
              };
      })
  ],
  tl: {
    hd: [
      "buffer",
      (function (param) {
          var v = $$Buffer.create(30);
          for(var i = 0; i <= 10; ++i){
            $$Buffer.add_string(v, String(i));
          }
          return {
                  TAG: /* Eq */0,
                  _0: $$Buffer.contents(v),
                  _1: "012345678910"
                };
        })
    ],
    tl: /* [] */0
  }
};

var suites = {
  hd: suites_0,
  tl: suites_1
};

Mt.from_pair_suites("Buffer_test", suites);

exports.v = v;
exports.suites = suites;
/*  Not a pure module */
