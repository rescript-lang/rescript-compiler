'use strict';

var Mt = require("./mt.js");
var Fs = require("fs");
var Path = require("path");
var Block = require("../../lib/js/block.js");
var Child_process = require("child_process");
var App_root_finder = require("./app_root_finder.js");
var Caml_builtin_exceptions = require("../../lib/js/caml_builtin_exceptions.js");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      (function () {
          return /* Eq */Block.__(0, [
                    x,
                    y
                  ]);
        })
    ],
    suites[0]
  ];
  return /* () */0;
}

var match = typeof (__dirname) === "undefined" ? undefined : (__dirname);

if (match !== undefined) {
  var root = App_root_finder.find_package_json(match);
  var bsc_exe = Path.join(root, "lib", "bsc.exe");
  var exit = 0;
  var output;
  try {
    output = Child_process.execSync(bsc_exe + " -where ", {
          encoding: "utf8"
        });
    exit = 1;
  }
  catch (e){
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "installation_test.ml",
            33,
            8
          ]
        ];
  }
  if (exit === 1) {
    var dir = output.trim();
    var files = Fs.readdirSync(dir);
    var exists = files.indexOf("pervasives.cmi");
    var non_exists = files.indexOf("pervasive.cmi");
    var v = +(exists >= 0 && non_exists < 0);
    console.log(v);
  }
  
} else {
  throw [
        Caml_builtin_exceptions.assert_failure,
        [
          "installation_test.ml",
          35,
          18
        ]
      ];
}

Mt.from_pair_suites("installation_test.ml", suites[0]);

exports.suites = suites;
exports.test_id = test_id;
exports.eq = eq;
/* match Not a pure module */
