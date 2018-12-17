'use strict';

var List = require("../../lib/js/list.js");

function v(x) {
  return /* tuple */[
          List.length(x),
          List.length(x)
        ];
}

var L = 0;

exports.L = L;
exports.v = v;
/* No side effect */
