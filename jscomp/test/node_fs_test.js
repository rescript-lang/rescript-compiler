'use strict';

var Fs           = require("fs");
var Js_undefined = require("../../lib/js/js_undefined");

Js_undefined.bind((__filename), function (f) {
      console.log(Fs.readFileSync(f, "utf8"));
      return /* () */0;
    });

/*  Not a pure module */
