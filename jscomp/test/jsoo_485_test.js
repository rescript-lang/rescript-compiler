'use strict';


function f() {
  [3];
  return /* () */0;
}

exports.f = f;
/*  Not a pure module */
