'use strict';

var Arg = require("../../lib/js/arg.js");

function anno_fun(arg) {
  return /* () */0;
}

var usage_msg = "Usage:\n";

var compile = /* record */[/* contents */false];

var test = /* record */[/* contents */true];

var arg_spec = /* constructor */{
  tag: "::",
  Arg0: /* tuple */[
    "-c",
    /* constructor */{
      tag: "Set",
      Arg0: compile
    },
    " Compile"
  ],
  Arg1: /* constructor */{
    tag: "::",
    Arg0: /* tuple */[
      "-d",
      /* constructor */{
        tag: "Clear",
        Arg0: test
      },
      " Test"
    ],
    Arg1: "[]"
  }
};

Arg.parse(arg_spec, anno_fun, usage_msg);

exports.anno_fun = anno_fun;
exports.usage_msg = usage_msg;
exports.compile = compile;
exports.test = test;
exports.arg_spec = arg_spec;
/*  Not a pure module */
