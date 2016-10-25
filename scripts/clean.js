'use strict';

var Path         = require("path");
var Fs           = require("fs");
var Js_undefined = require("../lib/js/js_undefined");

Js_undefined.bind((__dirname), function (dir) {
      var bin_dir = Path.join(dir, "..", "bin");
      var files = Fs.readdirSync(bin_dir);
      console.log(/* tuple */[
            "cleaning now",
            files
          ]);
      files.forEach(function (f) {
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
      return undefined;
    });

/*  Not a pure module */
