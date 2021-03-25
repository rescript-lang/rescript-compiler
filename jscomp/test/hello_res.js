'use strict';

var List = require("../../lib/js/list.js");

var b = List.length({
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    });

var a = b - 1 | 0;

console.log("hello, res");

List.length({
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    });

console.log(3);

console.log([
      3,
      1
    ]);

var to = 3;

var downto = 1;

exports.a = a;
exports.to = to;
exports.downto = downto;
/* b Not a pure module */
