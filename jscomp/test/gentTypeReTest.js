'use strict';

var GentTypeReTestGen = require("./gentTypeReTest.gen");

function f(prim) {
  return GentTypeReTestGen.f(prim);
}

exports.f = f;
/* ./gentTypeReTest.gen Not a pure module */
