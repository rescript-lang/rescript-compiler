'use strict';


function test1(r) {
  var x = r.fo;
  var x$1 = (
    x !== undefined ? x.f : undefined
  ).f.f;
  var match = x$1.fo;
  var x$2 = r.fo;
  var x$3 = (
    x$2 !== undefined ? x$2.f : undefined
  ).f.f;
  var x$4 = x$3.fo;
  var match$1 = x$4 !== undefined ? x$4.n : undefined;
  if (match !== undefined) {
    return match.n + match$1 | 0;
  } else {
    return 0;
  }
}

exports.test1 = test1;
/* No side effect */
