'use strict';

var Curry = require("./curry.js");
var Belt_Array = require("./belt_Array.js");

function copyAuxCont(_c, _prec) {
  while(true) {
    var prec = _prec;
    var c = _c;
    if (c === undefined) {
      return ;
    }
    var ncopy = {
      key: c.key,
      next: undefined
    };
    prec.next = ncopy;
    _prec = ncopy;
    _c = c.next;
    continue ;
  };
}

function copyBucket(c) {
  if (c === undefined) {
    return c;
  }
  var head = {
    key: c.key,
    next: undefined
  };
  copyAuxCont(c.next, head);
  return head;
}

function copyBuckets(buckets) {
  var len = buckets.length;
  var newBuckets = new Array(len);
  for(var i = 0; i < len; ++i){
    newBuckets[i] = copyBucket(buckets[i]);
  }
  return newBuckets;
}

function copy(x) {
  return {
          size: x.size,
          buckets: copyBuckets(x.buckets),
          hash: x.hash,
          eq: x.eq
        };
}

function bucketLength(_accu, _buckets) {
  while(true) {
    var buckets = _buckets;
    var accu = _accu;
    if (buckets === undefined) {
      return accu;
    }
    _buckets = buckets.next;
    _accu = accu + 1 | 0;
    continue ;
  };
}

function doBucketIter(f, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets === undefined) {
      return ;
    }
    f(buckets.key);
    _buckets = buckets.next;
    continue ;
  };
}

function forEachU(h, f) {
  var d = h.buckets;
  for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
    doBucketIter(f, d[i]);
  }
  
}

function forEach(h, f) {
  return forEachU(h, Curry.__1(f));
}

function fillArray(_i, arr, _cell) {
  while(true) {
    var cell = _cell;
    var i = _i;
    arr[i] = cell.key;
    var v = cell.next;
    if (v === undefined) {
      return i + 1 | 0;
    }
    _cell = v;
    _i = i + 1 | 0;
    continue ;
  };
}

function toArray(h) {
  var d = h.buckets;
  var current = 0;
  var arr = new Array(h.size);
  for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
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
    if (b === undefined) {
      return accu;
    }
    _accu = f(accu, b.key);
    _b = b.next;
    continue ;
  };
}

function reduceU(h, init, f) {
  var d = h.buckets;
  var accu = init;
  for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
    accu = doBucketFold(f, d[i], accu);
  }
  return accu;
}

function reduce(h, init, f) {
  return reduceU(h, init, Curry.__2(f));
}

function getMaxBucketLength(h) {
  return Belt_Array.reduceU(h.buckets, 0, (function (m, b) {
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
  var histo = Belt_Array.makeByU(mbl + 1 | 0, (function (param) {
          return 0;
        }));
  Belt_Array.forEachU(h.buckets, (function (b) {
          var l = bucketLength(0, b);
          histo[l] = histo[l] + 1 | 0;
          
        }));
  return histo;
}

function logStats(h) {
  var histogram = getBucketHistogram(h);
  console.log({
        bindings: h.size,
        buckets: h.buckets.length,
        histogram: histogram
      });
  
}

var C;

exports.C = C;
exports.copy = copy;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.fillArray = fillArray;
exports.toArray = toArray;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.logStats = logStats;
exports.getBucketHistogram = getBucketHistogram;
/* No side effect */
