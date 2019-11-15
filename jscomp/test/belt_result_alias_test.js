'use strict';

var Block = require("../../lib/js/block.js");
var Belt_Result = require("../../lib/js/belt_Result.js");

console.log(Belt_Result.map(/* Ok */Block.__(0, ["Test"]), (function (r) {
            return "Value: " + r;
          })));

Belt_Result.getWithDefault(Belt_Result.map(/* Error */Block.__(1, ["error"]), (function (r) {
            return "Value: " + r;
          })), "success");

/*  Not a pure module */
