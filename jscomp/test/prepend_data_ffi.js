'use strict';


var v1 = {
  stdio: "inherit",
  v: 3
};

var v2 = {
  stdio: 1,
  v: 2
};

process.on("exit", function (exit_code) {
      return "" + exit_code;
    });

process.on(1, function () {
      return /* () */0;
    });

process.on(function (i) {
      return "" + i;
    }, "exit");

process.on(function (i) {
      return "" + i;
    }, 1);

xx(3, 3, "xxx", "a", "b");

exports.v1 = v1;
exports.v2 = v2;
/* v1 Not a pure module */
