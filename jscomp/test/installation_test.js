'use strict';

var Mt            = require("./mt");
var Block         = require("../../lib/js/block");
var Fs            = require("fs");
var Child_process = require("child_process");
var Js_undefined  = require("../../lib/js/js_undefined");
var Path          = require("path");

var suites = [/* [] */0];

var test_id = [0];

function eq(loc, x, y) {
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

Js_undefined.bind((__dirname), function (p) {
      var bsc_exe = Path.join(p, "..", "bin", "bsc.exe");
      var output = Child_process.execSync(bsc_exe + " -where ", {
            encoding: "utf8"
          });
      var dir = output.trim();
      var exists = Fs.readdirSync(dir).includes("pervasives.cmi");
      return eq('File "installation_test.ml", line 24, characters 9-16', exists, true);
    });

Mt.from_pair_suites("installation_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */
