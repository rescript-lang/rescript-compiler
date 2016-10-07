'use strict';

var Mt            = require("./mt");
var Block         = require("../../lib/js/block");
var Path          = require("path");
var Js_undefined  = require("../../lib/js/js_undefined");
var Child_process = require("child_process");
var Fs            = require("fs");

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
      var files = Fs.readdirSync(dir);
      var exists = files.indexOf("pervasives.cmi");
      var non_exists = files.indexOf("pervasive.cmi");
      var v = +(exists >= 0 && non_exists < 0);
      console.log(v);
      return eq('File "installation_test.ml", line 30, characters 9-16', v, /* true */1);
    });

Mt.from_pair_suites("installation_test.ml", suites[0]);

exports.suites  = suites;
exports.test_id = test_id;
exports.eq      = eq;
/*  Not a pure module */
