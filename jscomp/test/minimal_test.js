'use strict';


var fake_y = /* constructor */{
  tag: "::",
  Arg0: 2,
  Arg1: /* constructor */{
    tag: "::",
    Arg0: 3,
    Arg1: "[]"
  }
};

var fake_z = /* constructor */{
  tag: "::",
  Arg0: 1,
  Arg1: fake_y
};

exports.fake_y = fake_y;
exports.fake_z = fake_z;
/* No side effect */
