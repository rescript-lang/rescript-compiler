'use strict';

var Arg = require("../../lib/js/arg.js");

function anno_fun(arg) {
  
}

var usage_msg = "Usage:\n";

var compile = {
  contents: false
};

var test = {
  contents: true
};

var arg_spec_0 = [
  "-c",
  {
    TAG: /* Set */2,
    _0: compile
  },
  " Compile"
];

var arg_spec_1 = {
  hd: [
    "-d",
    {
      TAG: /* Clear */3,
      _0: test
    },
    " Test"
  ],
  tl: /* [] */0
};

var arg_spec = {
  hd: arg_spec_0,
  tl: arg_spec_1
};

Arg.parse_argv(undefined, [
      "prog.exe",
      "-c",
      "-d"
    ], arg_spec, anno_fun, usage_msg);

if (compile.contents !== true) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "argv_test.ml",
          20,
          0
        ],
        Error: new Error()
      };
}

if (test.contents !== false) {
  throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "argv_test.ml",
          21,
          0
        ],
        Error: new Error()
      };
}

exports.anno_fun = anno_fun;
exports.usage_msg = usage_msg;
exports.compile = compile;
exports.test = test;
exports.arg_spec = arg_spec;
/*  Not a pure module */
