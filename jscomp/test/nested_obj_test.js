'use strict';


var f = {
  x: {
    y: {
      z: 3
    }
  }
};

var f2_000 = /* :: */[
  {
    x: {
      y: {
        z: 3
      }
    }
  },
  /* :: */[
    {
      x: {
        y: {
          z: 31
        }
      }
    },
    /* [] */0
  ]
];

var f2_001 = /* array */[
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

var f2 = /* tuple */[
  f2_000,
  f2_001
];

var f3 = {
  x: {
    y: {
      z: 3
    }
  }
};

var f_record = /* record */[/* x : record */[/* y : record */[/* z */3]]];

exports.f_record = f_record;
exports.f = f;
exports.f2 = f2;
exports.f3 = f3;
/* No side effect */
