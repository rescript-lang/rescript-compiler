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
    tag: /* Set */2,
    _0: compile
  },
  " Compile"
];

var arg_spec_1 = /* :: */{
  _0: [
    "-d",
    {
      tag: /* Clear */3,
      _0: test
    },
    " Test"
  ],
  _1: /* [] */0
};

var arg_spec = /* :: */{
  _0: arg_spec_0,
  _1: arg_spec_1
};

Arg.parse(arg_spec, anno_fun, usage_msg);

exports.anno_fun = anno_fun;
exports.usage_msg = usage_msg;
exports.compile = compile;
exports.test = test;
exports.arg_spec = arg_spec;
/*  Not a pure module */
