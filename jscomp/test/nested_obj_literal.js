'use strict';


var structural_obj = {
  x: {
    y: {
      z: 3
    }
  }
};

var f_record = /* record */{
  x: /* record */{
    y: /* record */{
      z: 3
    }
  }
};

exports.structural_obj = structural_obj;
exports.f_record = f_record;
/* No side effect */
