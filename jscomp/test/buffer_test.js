// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Assert                  = require("assert");
var Bytes                   = require("../stdlib/bytes");
var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Mt                      = require("./mt");
var Buffer                  = require("../stdlib/buffer");

var v = "gso";

function bytes_equal() {
  if (Bytes.make(3, /* "a" */97)[0] !== /* "a" */97) {
    throw [
          Caml_builtin_exceptions.Assert_failure,
          [
            "buffer_test.ml",
            9,
            4
          ]
        ];
  }
  if (Bytes.make(3, /* "a" */97)[0] !== /* "a" */97) {
    throw [
          Caml_builtin_exceptions.Assert_failure,
          [
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
          Caml_builtin_exceptions.Assert_failure,
          [
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
          Caml_builtin_exceptions.Assert_failure,
          [
            "buffer_test.ml",
            14,
            4
          ]
        ];
  }
}

var suites_000 = /* tuple */[
  "equal",
  bytes_equal
];

var suites_001 = /* :: */[
  /* tuple */[
    "buffer",
    function () {
      var v = Buffer.create(30);
      for(var i = 0; i<= 10; ++i){
        Buffer.add_string(v, "" + i);
      }
      var prim = Buffer.contents(v);
      return Assert.deepEqual(prim, "012345678910");
    }
  ],
  /* [] */0
];

var suites = /* :: */[
  suites_000,
  suites_001
];

Mt.from_suites("buffer_test.ml", suites);

exports.v           = v;
exports.bytes_equal = bytes_equal;
exports.suites      = suites;
/*  Not a pure module */
