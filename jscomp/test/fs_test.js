'use strict';

var Mt = require("./mt.js");
var Fs = require("fs");
var Path = require("path");
var Block = require("../../lib/js/block.js");

var suites = {
  contents: /* [] */0
};

var test_id = {
  contents: 0
};

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id.contents = test_id.contents + 1 | 0;
  suites.contents = /* :: */{
    _0: /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return {
                  tag: /* Eq */0,
                  _0: x,
                  _1: y
                };
        })
    ],
    _1: suites.contents
  };
  
}

var x = typeof __filename === "undefined" ? undefined : __filename;

var current_file = x !== undefined ? x : "<Not Node JS>";

var x$1 = typeof __dirname === "undefined" ? undefined : __dirname;

var current_dir_name = x$1 !== undefined ? x$1 : "<Not Node Js>";

Fs.readFileSync(current_file, "utf8");

Fs.readdirSync(current_dir_name);

var pathobj = Path.parse(current_dir_name);

var module_ = typeof module === "undefined" ? undefined : module;

if (module_ !== undefined) {
  console.log(/* tuple */[
        module_.id,
        module_.paths
      ]);
  eq("File \"fs_test.ml\", line 45, characters 7-14", /* tuple */[
        pathobj.name,
        "test"
      ]);
}

Mt.from_pair_suites("Fs_test", suites.contents);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/* x Not a pure module */
