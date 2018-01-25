'use strict';

var Bs_Array = require("./bs_Array.js");
var Bs_internalBucketsType = require("./bs_internalBucketsType.js");

function copyAuxCont(_c, _prec) {
  while(true) {
    var prec = _prec;
    var c = _c;
    if (c !== undefined) {
      var ncopy = {
        key: c.key,
        next: Bs_internalBucketsType.emptyOpt
      };
      prec.next = ncopy;
      _prec = ncopy;
      _c = c.next;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function copyBucket(c) {
  if (c !== undefined) {
    var head = {
      key: c.key,
      next: Bs_internalBucketsType.emptyOpt
    };
    copyAuxCont(c.next, head);
    return head;
  } else {
    return c;
  }
}

function copyBuckets(buckets) {
  var len = buckets.length;
  var newBuckets = new Array(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    newBuckets[i] = copyBucket(buckets[i]);
  }
  return newBuckets;
}

function copy(x) {
  return {
          size: x.size,
          buckets: copyBuckets(x.buckets)
        };
}

function bucketLength(_accu, _buckets) {
  while(true) {
    var buckets = _buckets;
    var accu = _accu;
    if (buckets !== undefined) {
      _buckets = buckets.next;
      _accu = accu + 1 | 0;
      continue ;
      
    } else {
      return accu;
    }
  };
}

function doBucketIter(f, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      f(buckets.key);
      _buckets = buckets.next;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function forEach0(h, f) {
  var d = h.buckets;
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    doBucketIter(f, d[i]);
  }
  return /* () */0;
}

function fillArray(_i, arr, _cell) {
  while(true) {
    var cell = _cell;
    var i = _i;
    arr[i] = cell.key;
    var match = cell.next;
    if (match !== undefined) {
      _cell = match;
      _i = i + 1 | 0;
      continue ;
      
    } else {
      return i + 1 | 0;
    }
  };
}

function toArray0(h) {
  var d = h.buckets;
  var current = 0;
  var arr = new Array(h.size);
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    var cell = d[i];
    if (cell !== undefined) {
      current = fillArray(current, arr, cell);
    }
    
  }
  return arr;
}

function doBucketFold(f, _b, _accu) {
  while(true) {
    var accu = _accu;
    var b = _b;
    if (b !== undefined) {
      _accu = f(accu, b.key);
      _b = b.next;
      continue ;
      
    } else {
      return accu;
    }
  };
}

function reduce0(h, init, f) {
  var d = h.buckets;
  var accu = init;
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    accu = doBucketFold(f, d[i], accu);
  }
  return accu;
}

function getMaxBucketLength(h) {
  return Bs_Array.reduce(h.buckets, 0, (function (m, b) {
                var len = bucketLength(0, b);
                if (m > len) {
                  return m;
                } else {
                  return len;
                }
              }));
}

function getBucketHistogram(h) {
  var mbl = getMaxBucketLength(h);
  var histo = Bs_Array.makeBy(mbl + 1 | 0, (function () {
          return 0;
        }));
  Bs_Array.forEach(h.buckets, (function (b) {
          var l = bucketLength(0, b);
          histo[l] = histo[l] + 1 | 0;
          return /* () */0;
        }));
  return histo;
}

function logStats0(h) {
  var histogram = getBucketHistogram(h);
  console.log({
        bindings: h.size,
        buckets: h.buckets.length,
        histogram: histogram
      });
  return /* () */0;
}

var C = 0;

exports.C = C;
exports.copy = copy;
exports.forEach0 = forEach0;
exports.fillArray = fillArray;
exports.toArray0 = toArray0;
exports.reduce0 = reduce0;
exports.logStats0 = logStats0;
exports.getBucketHistogram = getBucketHistogram;
/* No side effect */
