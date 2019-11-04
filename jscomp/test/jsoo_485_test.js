'use strict';


function f(param) {
  ({
      contents: undefined
    }).contents = 3;
  return /* () */0;
}

({
    contents: undefined
  }).contents = 3;

exports.f = f;
/*  Not a pure module */
