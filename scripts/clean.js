'use strict';

var Path         = require("path");
var Js_undefined = require("../lib/js/js_undefined");
var Fs           = require("fs");

function clean() {
  return Js_undefined.iter((__dirname), function (dir) {
              var bin_dir = Path.join(dir, "..", "bin");
              var files = Fs.readdirSync(bin_dir);
              console.log(/* tuple */[
                    "cleaning now",
                    files
                  ]);
              return files.forEach(function (f) {
                          if (!f.startsWith("bs") && f !== ".gitignore") {
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
            });
}

var Fs$1 = 0;

exports.Fs    = Fs$1;
exports.clean = clean;
/* path Not a pure module */
