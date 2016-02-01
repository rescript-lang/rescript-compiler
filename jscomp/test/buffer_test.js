// Generated CODE, PLEASE EDIT WITH CARE
"use strict";

var Bytes           = require("../stdlib/bytes");
var Caml_exceptions = require("../runtime/caml_exceptions");
var Mt              = require("./mt");
var Buffer          = require("../stdlib/buffer");

var v = "gso";

function bytes_equal() {
  if (Bytes.make(3, /* "a" */97)[0] !== /* "a" */97) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "buffer_test.ml",
            9,
            4
          ]
        ];
  }
  if (Bytes.make(3, /* "a" */97)[0] !== /* "a" */97) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "buffer_test.ml",
            10,
            4
          ]
        ];
  }
  var u = Bytes.make(3, /* "a" */97);
  u[0] = /* "b" */98;
  if (u[0] !== /* "b" */98) {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "buffer_test.ml",
            13,
            4
          ]
        ];
  }
  if (v[0] === "g") {
    return 0;
  }
  else {
    throw [
          0,
          Caml_exceptions.Assert_failure,
          [
            0,
            "buffer_test.ml",
            14,
            4
          ]
        ];
  }
}

var suites_001 = [
  /* tuple */0,
  "equal",
  bytes_equal
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "buffer",
    function () {
      var v = Buffer.create(30);
      for(var i = 0; i<= 10; ++i){
        Buffer.add_string(v, "" + i);
      }
      return Mt.assert_equal(Buffer.contents(v), "012345678910");
    }
  ],
  /* [] */0
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_suites("buffer_test.ml", suites);

exports.v           = v;
exports.bytes_equal = bytes_equal;
exports.suites      = suites;
/*  Not a pure module */
