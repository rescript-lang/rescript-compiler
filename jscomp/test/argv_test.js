'use strict';

var Arg = require("../../lib/js/arg.js");
var Block = require("../../lib/js/block.js");

function anno_fun() {
  return /* () */0;
}

var usage_msg = "Usage:\n";

var compile = [/* false */0];

var test = [/* true */1];

var arg_spec_000 = /* tuple */[
  "-c",
  /* Set */Block.__(2, [compile]),
  " Compile"
];

var arg_spec_001 = /* :: */[
  /* tuple */[
    "-d",
    /* Clear */Block.__(3, [test]),
    " Test"
  ],
  /* [] */0
];

var arg_spec = /* :: */[
  arg_spec_000,
  arg_spec_001
];

Arg.parse(arg_spec, anno_fun, usage_msg);

exports.anno_fun = anno_fun;
exports.usage_msg = usage_msg;
exports.compile = compile;
exports.test = test;
exports.arg_spec = arg_spec;
/*  Not a pure module */
