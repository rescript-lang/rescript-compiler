'use strict';


function even(_n) {
  while(true) {
    var n = _n;
    if (n === 0) {
      return true;
    }
    _n = n - 1 | 0;
    continue ;
  };
}

function even2(n) {
  if (n === 0) {
    return true;
  } else {
    var n$1 = n - 1 | 0;
    if (n$1 === 1) {
      return true;
    } else {
      return even2(n$1 - 1 | 0);
    }
  }
}

var v = {
  contents: 0
};

function obj_get(param) {
  return v.contents;
}

function obj_set(i) {
  v.contents = i;
  
}

var obj = {
  get: obj_get,
  set: obj_set
};

exports.even = even;
exports.even2 = even2;
exports.v = v;
exports.obj = obj;
/* No side effect */
