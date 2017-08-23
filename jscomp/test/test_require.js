'use strict';


var match = typeof (require) === "undefined" ? undefined : (require);

if (match !== undefined) {
  console.log(match.resolve("./test_require.js"));
  var match$1 = typeof (module) === "undefined" ? undefined : (module);
  var match$2 = match.main;
  if (match$1 !== undefined) {
    if (match$2 !== undefined && match$1 === match$2) {
      console.log("is main");
    } else {
      console.log("not main");
    }
  } else {
    console.log("not main");
  }
}

/* match Not a pure module */
