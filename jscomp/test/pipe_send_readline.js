'use strict';


function u(rl) {
  return rl.on("line", function (x) {
                console.log(x);
                return /* () */0;
              }).on("close", function () {
              console.log("finished");
              return /* () */0;
            });
}

exports.u = u;
/* No side effect */
