'use strict';


function register(rl) {
  return rl.on("line", (function (x) {
                  console.log(x);
                  
                })).on("close", (function (param) {
                console.log("finished");
                
              }));
}

exports.register = register;
/* No side effect */
