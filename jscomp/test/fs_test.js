'use strict';

var Fs    = require("fs");
var Path  = require("path");
var Block = require("../../lib/js/block");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, param) {
  var y = param[1];
  var x = param[0];
  test_id[0] = test_id[0] + 1 | 0;
  suites[0] = /* :: */[
    /* tuple */[
      loc + (" id " + test_id[0]),
      function () {
        return /* Eq */Block.__(0, [
                  x,
                  y
                ]);
      }
    ],
    suites[0]
  ];
  return /* () */0;
}

var current_file = (__filename);

var current_dir_name = (__dirname);

Fs.readFileSync(current_file, "utf8");

Fs.readdirSync(current_dir_name);

var pathobj = Path.parse(current_dir_name);

var module_ = (module);

console.log(/* tuple */[
      module_.id,
      module_.paths
    ]);

eq('File "fs_test.ml", line 38, characters 5-12', /* tuple */[
      pathobj.name,
      "test"
    ]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/* current_file Not a pure module */
