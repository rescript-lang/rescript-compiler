'use strict';

var Fs = require("fs");

var match = typeof (__filename) === "undefined" ? undefined : (__filename);

if ((
    match === undefined ? /* None */0 : [match]
  ) !== /* None */0) {
  console.log(Fs.readFileSync(match, "utf8"));
}

/* match Not a pure module */
