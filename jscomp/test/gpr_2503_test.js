'use strict';


function makeWrapper(foo, _) {
  var tmp = { };
  if (foo) {
    tmp.foo = foo[0];
  }
  console.log(tmp);
  return /* () */0;
}

makeWrapper(/* Some */[/* a */97], /* () */0);

exports.makeWrapper = makeWrapper;
/*  Not a pure module */
