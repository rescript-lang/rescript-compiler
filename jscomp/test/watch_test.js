'use strict';

var Fs = require("fs");

function test(path) {
  Fs.watch(path, {
            recursive: true
          }).on("change", (function ($$event, string_buffer) {
            console.log(/* tuple */[
                  $$event,
                  string_buffer
                ]);
            return /* () */0;
          })).close();
  return /* () */0;
}

exports.test = test;
/* fs Not a pure module */
