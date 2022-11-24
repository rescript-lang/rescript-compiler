'use strict';


var u = typeof require === "undefined" ? undefined : require;

if (u !== undefined) {
  var match = typeof module === "undefined" ? undefined : module;
  var match$1 = u.main;
  if (match !== undefined && match$1 !== undefined && match === match$1) {
    console.log("is main");
  } else {
    console.log("not main");
  }
}

/* u Not a pure module */
