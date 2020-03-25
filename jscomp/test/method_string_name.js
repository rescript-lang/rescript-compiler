'use strict';


var f = {
  "Content-Type": 3
};

console.log(f["Content-Type"]);

function ff(x) {
  x.Hi;
  x["Content-Type"] = "hello";
  console.log(({
          "Content-Type": "hello"
        })["Content-Type"]);
  
}

exports.f = f;
exports.ff = ff;
/*  Not a pure module */
