'use strict';

var Mt = require("./mt.js");
var Bytes = require("../../lib/js/bytes.js");
var Caml_obj = require("../../lib/js/caml_obj.js");
var Caml_bytes = require("../../lib/js/caml_bytes.js");
var Caml_exceptions = require("../../lib/js/caml_exceptions.js");
var Caml_js_exceptions = require("../../lib/js/caml_js_exceptions.js");

var v = "gso";

function is_equal(param) {
  if (Caml_bytes.get(Bytes.make(3, /* "a" */97), 0) !== /* "a" */97) {
    throw {
          ExceptionID: -9,
          _1: /* tuple */[
            "equal_exception_test.ml",
            9,
            4
          ],
          Debug: "Assert_failure"
        };
  }
  if (Bytes.make(3, /* "a" */97)[0] !== /* "a" */97) {
    throw {
          ExceptionID: -9,
          _1: /* tuple */[
            "equal_exception_test.ml",
            10,
            4
          ],
          Debug: "Assert_failure"
        };
  }
  var u = Bytes.make(3, /* "a" */97);
  u[0] = /* "b" */98;
  if (u[0] !== /* "b" */98) {
    throw {
          ExceptionID: -9,
          _1: /* tuple */[
            "equal_exception_test.ml",
            13,
            4
          ],
          Debug: "Assert_failure"
        };
  }
  
}

function is_exception(param) {
  try {
    throw {
          ExceptionID: -6,
          Debug: "Not_found"
        };
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID === /* Not_found */-6) {
      return ;
    }
    throw exn;
  }
}

function is_normal_exception(_x) {
  var A = Caml_exceptions.create("A");
  var v_000 = A.ExceptionID;
  var v_002 = A.Debug;
  var v = {
    ExceptionID: v_000,
    _1: 3,
    Debug: v_002
  };
  try {
    throw v;
  }
  catch (raw_exn){
    var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
    if (exn.ExceptionID === A.ExceptionID) {
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
          ExceptionID: A.ExceptionID,
          Debug: A.Debug
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
  ExceptionID: -6,
  Debug: "Not_found"
};

function eq(param) {
  return param.ExceptionID === /* Not_found */-6;
}

var Not_found = Caml_exceptions.create("Equal_exception_test.Not_found");

if (Caml_obj.caml_equal(e, {
        ExceptionID: Not_found.ExceptionID,
        Debug: Not_found.Debug
      }) !== false) {
  throw {
        ExceptionID: -9,
        _1: /* tuple */[
          "equal_exception_test.ml",
          50,
          3
        ],
        Debug: "Assert_failure"
      };
}

if (({
      ExceptionID: Not_found.ExceptionID,
      Debug: Not_found.Debug
    }).ExceptionID === /* Not_found */-6 !== false) {
  throw {
        ExceptionID: -9,
        _1: /* tuple */[
          "equal_exception_test.ml",
          51,
          3
        ],
        Debug: "Assert_failure"
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
