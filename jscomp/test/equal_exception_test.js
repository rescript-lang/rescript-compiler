'use strict';

var Mt = require("./mt.js");
var Bytes = require("../../lib/js/bytes.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var v = "gso";

function is_equal(param) {
  if (Caml_bytes.get(Bytes.make(3, /* "a" */97), 0) !== /* "a" */97) {
    throw {
          CamlExt: Caml_builtin_exceptions.assert_failure,
          _1: /* tuple */[
            "equal_exception_test.ml",
            9,
            4
          ]
        };
  }
  if (Bytes.make(3, /* "a" */97)[0] !== /* "a" */97) {
    throw {
          CamlExt: Caml_builtin_exceptions.assert_failure,
          _1: /* tuple */[
            "equal_exception_test.ml",
            10,
            4
          ]
        };
  }
  var u = Bytes.make(3, /* "a" */97);
  u[0] = /* "b" */98;
  if (u[0] !== /* "b" */98) {
    throw {
          CamlExt: Caml_builtin_exceptions.assert_failure,
          _1: /* tuple */[
            "equal_exception_test.ml",
            13,
            4
          ]
        };
  }
  
}

function is_exception(param) {
  try {
    throw {
          CamlExt: Caml_builtin_exceptions.not_found
        };
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.CamlExt === Caml_builtin_exceptions.not_found) {
      return ;
    }
    throw exn;
  }
}

function is_normal_exception(_x) {
  var A = Caml_exceptions.create("A");
  var v = {
    CamlExt: A,
    _1: 3
  };
  try {
    throw v;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.CamlExt === A) {
      if (exn._1 === 3) {
        return ;
      }
      throw exn;
    }
    throw exn;
  }
}

function is_arbitrary_exception(param) {
  var A = Caml_exceptions.create("A");
  try {
    throw {
          CamlExt: A
        };
  }
  catch (exn){
    return ;
  }
}

var suites_000 = /* tuple */[
  "is_equal",
  is_equal
];

var suites_001 = /* :: */[
  /* tuple */[
    "is_exception",
    is_exception
  ],
  /* :: */[
    /* tuple */[
      "is_normal_exception",
      is_normal_exception
    ],
    /* :: */[
      /* tuple */[
        "is_arbitrary_exception",
        is_arbitrary_exception
      ],
      /* [] */0
    ]
  ]
];

var suites = /* :: */[
  suites_000,
  suites_001
];

var e = {
  CamlExt: Caml_builtin_exceptions.not_found
};

function eq(param) {
  return param.CamlExt === Caml_builtin_exceptions.not_found;
}

var Not_found = Caml_exceptions.create("Equal_exception_test.Not_found");

if (Caml_obj.caml_equal(e, {
        CamlExt: Not_found
      }) !== false) {
  throw {
        CamlExt: Caml_builtin_exceptions.assert_failure,
        _1: /* tuple */[
          "equal_exception_test.ml",
          50,
          3
        ]
      };
}

if (eq({
        CamlExt: Not_found
      }) !== false) {
  throw {
        CamlExt: Caml_builtin_exceptions.assert_failure,
        _1: /* tuple */[
          "equal_exception_test.ml",
          51,
          3
        ]
      };
}

Mt.from_suites("exception", suites);

exports.v = v;
exports.is_equal = is_equal;
exports.is_exception = is_exception;
exports.is_normal_exception = is_normal_exception;
exports.is_arbitrary_exception = is_arbitrary_exception;
exports.suites = suites;
exports.e = e;
exports.eq = eq;
exports.Not_found = Not_found;
/*  Not a pure module */
