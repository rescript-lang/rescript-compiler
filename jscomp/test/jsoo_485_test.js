'use strict';


function f(param) {
  /* record */({
      contents: undefined
    }).contents = 3;
  return /* () */0;
}

/* record */({
    contents: undefined
  }).contents = 3;

exports.f = f;
/*  Not a pure module */
