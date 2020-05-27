'use strict';


var f = {
  x: {
    y: {
      z: 3
    }
  }
};

var f2_0 = {
  hd: {
    x: {
      y: {
        z: 3
      }
    }
  },
  tl: {
    hd: {
      x: {
        y: {
          z: 31
        }
      }
    },
    tl: /* [] */0
  }
};

var f2_1 = [
  {
    x: {
      y: {
        z: 3
      }
    }
  },
  {
    x: {
      y: {
        z: 31
      }
    }
  }
];

var f2 = [
  f2_0,
  f2_1
];

var f3 = {
  x: {
    y: {
      z: 3
    }
  }
};

var f_record = {
  x: {
    y: {
      z: 3
    }
  }
};

exports.f_record = f_record;
exports.f = f;
exports.f2 = f2;
exports.f3 = f3;
/* No side effect */
