'use strict';


var match = (require);

if (match !== undefined) {
  console.log(match.resolve("./test_require.js"));
  if (match.main === (module) && match.main !== undefined) {
    console.log("is main");
  }
  else {
    console.log("not main");
  }
}

/* match Not a pure module */
