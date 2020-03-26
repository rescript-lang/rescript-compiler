'use strict';

var Fs = require("fs");

var match = typeof __filename === "undefined" ? void 0 : __filename;

if (match !== void 0) {
  console.log(Fs.readFileSync(match, "utf8"));
}

/* match Not a pure module */
