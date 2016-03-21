// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Random = require("../stdlib/random");

function f() {
  Random.self_init(/* () */0);
  var x = Random.$$int(10000);
  Random.self_init(/* () */0);
  var y = Random.$$int(1000);
  if (x === y) {
    console.log("FAILED");
    return /* () */0;
  }
  else {
    console.log("PASSED");
    return /* () */0;
  }
}

exports.f = f;
/* No side effect */
