'use strict';


var emptyOpt = undefined;

function power_2_above(_x, n) {
  while(true) {
    var x = _x;
    if (x >= n) {
      return x;
    } else if ((x << 1) < x) {
      return x;
    } else {
      _x = (x << 1);
      continue ;
      
    }
  };
}

function create0(initialSize) {
  var s = power_2_above(16, initialSize);
  return {
          size: 0,
          buckets: new Array(s),
          initialSize: s
        };
}

function clear0(h) {
  h.size = 0;
  var h_buckets = h.buckets;
  var len = h_buckets.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    h_buckets[i] = emptyOpt;
  }
  return /* () */0;
}

function reset0(h) {
  var len = h.buckets.length;
  var h_initialSize = h.initialSize;
  if (len === h_initialSize) {
    return clear0(h);
  } else {
    h.size = 0;
    h.buckets = new Array(h_initialSize);
    return /* () */0;
  }
}

function length0(h) {
  return h.size;
}

exports.emptyOpt = emptyOpt;
exports.create0 = create0;
exports.clear0 = clear0;
exports.reset0 = reset0;
exports.length0 = length0;
/* No side effect */
