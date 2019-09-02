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

console.log(/* XXX */({
          "5": 3
        }).tag);

exports.x = x;
exports.zz = zz;
/* x Not a pure module */
