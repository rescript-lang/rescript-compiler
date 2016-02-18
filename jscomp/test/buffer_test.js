// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Bytes  = require("../stdlib/bytes");
var Mt     = require("./mt");
var Buffer = require("../stdlib/buffer");

var v = "gso";

var suites_000 = /* tuple */[
  "equal",
  function () {
    return /* Eq */{
            0: /* tuple */[
              Bytes.make(3, /* "a" */97)[0],
              Bytes.make(3, /* "a" */97)[0]
            ],
            1: /* tuple */[
              /* "a" */97,
              /* "a" */97
            ],
            length: 2,
            tag: 0
          };
  }
];

var suites_001 = /* :: */[
  /* tuple */[
    "equal2",
    function () {
      var u = Bytes.make(3, /* "a" */97);
      u[0] = /* "b" */98;
      return /* Eq */{
              0: /* tuple */[
                u[0],
                v.charCodeAt(0)
              ],
              1: /* tuple */[
                /* "b" */98,
                /* "g" */103
              ],
              length: 2,
              tag: 0
            };
    }
  ],
  /* :: */[
    /* tuple */[
      "buffer",
      function () {
        var v = Buffer.create(30);
        for(var i = 0; i<= 10; ++i){
          Buffer.add_string(v, "" + i);
        }
        return /* Eq */{
                0: Buffer.contents(v),
                1: "012345678910",
                length: 2,
                tag: 0
              };
      }
    ],
    /* [] */0
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_pair_suites("buffer_test.ml", suites);

exports.v      = v;
exports.suites = suites;
/*  Not a pure module */
