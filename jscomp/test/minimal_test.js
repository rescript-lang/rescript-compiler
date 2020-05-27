'use strict';


var fake_y = {
  hd: 2,
  tl: {
    hd: 3,
    tl: /* [] */0
  }
};

var fake_z = {
  hd: 1,
  tl: fake_y
};

exports.fake_y = fake_y;
exports.fake_z = fake_z;
/* No side effect */
