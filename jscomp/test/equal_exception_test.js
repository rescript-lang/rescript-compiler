// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Bytes                   = require("../stdlib/bytes");
var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Mt                      = require("./mt");

var v = "gso";

function is_equal() {
  if (Bytes.make(3, /* "a" */97)[0] !== /* "a" */97) {
    throw [
          0,
          Caml_builtin_exceptions.Assert_failure,
          [
            0,
            "equal_exception_test.ml",
            9,
            4
          ]
        ];
  }
  if (Bytes.make(3, /* "a" */97)[0] !== /* "a" */97) {
    throw [
          0,
          Caml_builtin_exceptions.Assert_failure,
          [
            0,
            "equal_exception_test.ml",
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
          Caml_builtin_exceptions.Assert_failure,
          [
            0,
            "equal_exception_test.ml",
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
          Caml_builtin_exceptions.Assert_failure,
          [
            0,
            "equal_exception_test.ml",
            14,
            4
          ]
        ];
  }
}

function is_exception() {
  try {
    throw Caml_builtin_exceptions.Not_found;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.Not_found) {
      return /* () */0;
    }
    else {
      throw exn;
    }
  }
}

function is_normal_exception() {
  var A = [
    248,
    "A",
    ++ Caml_builtin_exceptions.caml_oo_last_id
  ];
  var v = [
    0,
    A,
    3
  ];
  try {
    throw v;
  }
  catch (exn){
    if (exn[1] === A) {
      if (exn[2] !== 3) {
        throw exn;
      }
      else {
        return /* () */0;
      }
    }
    else {
      throw exn;
    }
  }
}

function is_arbitrary_exception() {
  var A = [
    248,
    "A",
    ++ Caml_builtin_exceptions.caml_oo_last_id
  ];
  try {
    throw A;
  }
  catch (exn){
    return /* () */0;
  }
}

var suites_001 = [
  /* tuple */0,
  "is_equal",
  is_equal
];

var suites_002 = [
  /* :: */0,
  [
    /* tuple */0,
    "is_exception",
    is_exception
  ],
  [
    /* :: */0,
    [
      /* tuple */0,
      "is_normal_exception",
      is_normal_exception
    ],
    [
      /* :: */0,
      [
        /* tuple */0,
        "is_arbitrary_exception",
        is_arbitrary_exception
      ],
      /* [] */0
    ]
  ]
];

var suites = [
  /* :: */0,
  suites_001,
  suites_002
];

Mt.from_suites("exception", suites);

exports.v                      = v;
exports.is_equal               = is_equal;
exports.is_exception           = is_exception;
exports.is_normal_exception    = is_normal_exception;
exports.is_arbitrary_exception = is_arbitrary_exception;
exports.suites                 = suites;
/*  Not a pure module */
