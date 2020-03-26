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
  suites.contents = /* :: */[
    /* tuple */[
      loc + (" id " + String(test_id.contents)),
      (function (param) {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites.contents
  ];
  
}

var match = typeof __filename === "undefined" ? void 0 : __filename;

var current_file = match !== void 0 ? match : "<Not Node JS>";

var match$1 = typeof __dirname === "undefined" ? void 0 : __dirname;

var current_dir_name = match$1 !== void 0 ? match$1 : "<Not Node Js>";

Fs.readFileSync(current_file, "utf8");

Fs.readdirSync(current_dir_name);

var pathobj = Path.parse(current_dir_name);

var match$2 = typeof module === "undefined" ? void 0 : module;

if (match$2 !== void 0) {
  console.log(/* tuple */[
        match$2.id,
        match$2.paths
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
/* match Not a pure module */
