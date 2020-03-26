'use strict';


var match = typeof require === "undefined" ? void 0 : require;

if (match !== void 0) {
  console.log(match.resolve("./test_require.js"));
  var match$1 = typeof module === "undefined" ? void 0 : module;
  var match$2 = match.main;
  if (match$1 !== void 0 && match$2 !== void 0 && match$1 === match$2) {
    console.log("is main");
  } else {
    console.log("not main");
  }
}

/* match Not a pure module */
