'use strict';


var x = ({
    x: 3,
    y: 4
  }).x;

({
    x: 3,
    y: 4
  }).x;

var zz = ({
    "5": 3
  })["5"];

({
    "5": 3
  })["5"];

console.log(({
          "5": 3
        }).tag | 0);

exports.x = x;
exports.zz = zz;
/* x Not a pure module */
