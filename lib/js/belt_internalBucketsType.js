'use strict';


var emptyOpt = undefined;

function power_2_above(_x, n) {
  while(true) {
    var x = _x;
    if (x >= n || (x << 1) < x) {
      return x;
    } else {
      _x = (x << 1);
      continue ;
    }
  };
}

function make(hash, eq, hintSize) {
  var s = power_2_above(16, hintSize);
  return {
          size: 0,
          buckets: new Array(s),
          hash: hash,
          eq: eq
        };
}

function clear(h) {
  h.size = 0;
  var h_buckets = h.buckets;
  var len = h_buckets.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    h_buckets[i] = emptyOpt;
  }
  return /* () */0;
}

function isEmpty(h) {
  return h.size === 0;
}

exports.emptyOpt = emptyOpt;
exports.make = make;
exports.clear = clear;
exports.isEmpty = isEmpty;
/* No side effect */
