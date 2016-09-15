'use strict';

var JoinClasses = require("joinClasses");

function f() {
  return JoinClasses(/* int array */[
              1,
              2,
              3
            ]);
}

exports.f = f;
/* joinClasses Not a pure module */
