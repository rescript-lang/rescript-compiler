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

function f(x) {
  x.xx(91, /* int array */[
        1,
        2,
        3
      ]);
  x.xx(92, 3, "xxx", /* int array */[
        1,
        2,
        3
      ]);
  x.xx(93, 3, "xxx", 1, 2, 3);
  return x.xx(94, 3, "xxx", 0, "b", 1, 2, 3, 4, 5);
}

exports.v1 = v1;
exports.v2 = v2;
exports.f  = f;
/* v1 Not a pure module */
