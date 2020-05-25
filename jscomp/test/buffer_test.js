'use strict';

var Mt = require("./mt.js");
var Block = require("../../lib/js/block.js");
var Bytes = require("../../lib/js/bytes.js");
var $$Buffer = require("../../lib/js/buffer.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");

var v = "gso";

var suites_0 = /* tuple */[
  "equal",
  (function (param) {
      return {
              tag: /* Eq */0,
              _0: /* tuple */[
                Caml_bytes.get(Bytes.make(3, /* "a" */97), 0),
                Bytes.make(3, /* "a" */97)[0]
              ],
              _1: /* tuple */[
                /* "a" */97,
                /* "a" */97
              ]
            };
    })
];

var suites_1 = /* :: */{
  _0: /* tuple */[
    "equal2",
    (function (param) {
        var u = Bytes.make(3, /* "a" */97);
        u[0] = /* "b" */98;
        return {
                tag: /* Eq */0,
                _0: /* tuple */[
                  u[0],
                  /* "g" */103
                ],
                _1: /* tuple */[
                  /* "b" */98,
                  /* "g" */103
                ]
              };
      })
  ],
  _1: /* :: */{
    _0: /* tuple */[
      "buffer",
      (function (param) {
          var v = $$Buffer.create(30);
          for(var i = 0; i <= 10; ++i){
            $$Buffer.add_string(v, String(i));
          }
          return {
                  tag: /* Eq */0,
                  _0: $$Buffer.contents(v),
                  _1: "012345678910"
                };
        })
    ],
    _1: /* [] */0
  }
};

var suites = /* :: */{
  _0: suites_0,
  _1: suites_1
};

Mt.from_pair_suites("Buffer_test", suites);

exports.v = v;
exports.suites = suites;
/*  Not a pure module */
