'use strict';


function u(rl) {
  return rl.on("line", (function (x) {
                  console.log(x);
                  return /* () */0;
                })).on("close", (function () {
                console.log("finished");
                return /* () */0;
              }));
}

function xx(h) {
  return h.send("x").hi;
}

function yy(h) {
  return h.send("x");
}

exports.u = u;
exports.xx = xx;
exports.yy = yy;
/* No side effect */
