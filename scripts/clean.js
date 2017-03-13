'use strict';

var Fs                      = require("fs");
var Path                    = require("path");
var Caml_builtin_exceptions = require("../lib/js/caml_builtin_exceptions");

function clean() {
  var match = typeof (__dirname) === "undefined" ? undefined : (__dirname);
  if (match !== undefined) {
    var bin_dir = Path.join(match, "..", "bin");
    var files = Fs.readdirSync(bin_dir);
    console.log(/* tuple */[
          "cleaning now",
          files
        ]);
    files.forEach(function (f) {
          if (!f.startsWith("bs") && f !== ".gitignore" && f !== "ninja.exe") {
            var p = Path.join(bin_dir, f);
            try {
              console.log(/* tuple */[
                    "removing",
                    p,
                    "now"
                  ]);
              Fs.unlinkSync(p);
              return /* () */0;
            }
            catch (exn){
              console.log(/* tuple */[
                    "removing",
                    p,
                    "failure"
                  ]);
              return /* () */0;
            }
          }
          else {
            return 0;
          }
        });
    return /* () */0;
  }
  else {
    throw [
          Caml_builtin_exceptions.assert_failure,
          [
            "clean.ml",
            22,
            12
          ]
        ];
  }
}

var Fs$1 = 0;

exports.Fs    = Fs$1;
exports.clean = clean;
/* fs Not a pure module */
