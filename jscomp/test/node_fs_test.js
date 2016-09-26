'use strict';

var Js_undefined = require("../../lib/js/js_undefined");
var Fs           = require("fs");

Js_undefined.bind((__filename), function (f) {
      console.log(Fs.readFileSync(f, "utf8"));
      return /* () */0;
    });

/*  Not a pure module */
