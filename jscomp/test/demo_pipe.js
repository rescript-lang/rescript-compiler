'use strict';


function register(rl) {
  return rl.on("line", (function (x) {
                  console.log(x);
                  return /* () */0;
                })).on("close", (function (param) {
                console.log("finished");
                return /* () */0;
              }));
}

exports.register = register;
/* No side effect */
