'use strict';

var CamlinternalLazy = require("../../lib/js/camlinternalLazy.js");

var lazy1 = {
  RE_LAZY_DONE: false,
  value: (function () {
      console.log("Hello, lazy");
      return 1;
    })
};

var lazy2 = {
  RE_LAZY_DONE: true,
  value: 3
};

console.log(lazy1, lazy2);

var la = CamlinternalLazy.force(lazy1);

var lb = CamlinternalLazy.force(lazy2);

console.log(la, lb);

exports.lazy1 = lazy1;
exports.lazy2 = lazy2;
exports.la = la;
exports.lb = lb;
/*  Not a pure module */
