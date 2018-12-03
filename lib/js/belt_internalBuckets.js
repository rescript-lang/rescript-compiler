'use strict';

var Curry = require("./curry.js");
var Belt_Array = require("./belt_Array.js");
var Caml_option = require("./caml_option.js");

function copyAuxCont(_c, _prec) {
  while(true) {
    var prec = _prec;
    var c = _c;
    if (c !== undefined) {
      var ncopy = {
        key: c.key,
        value: c.value,
        next: undefined
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
      value: c.value,
      next: undefined
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
          buckets: copyBuckets(x.buckets),
          hash: x.hash,
          eq: x.eq
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

function do_bucket_iter(f, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      f(buckets.key, buckets.value);
      _buckets = buckets.next;
      continue ;
    } else {
      return /* () */0;
    }
  };
}

function forEachU(h, f) {
  var d = h.buckets;
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    do_bucket_iter(f, d[i]);
  }
  return /* () */0;
}

function forEach(h, f) {
  return forEachU(h, Curry.__2(f));
}

function do_bucket_fold(f, _b, _accu) {
  while(true) {
    var accu = _accu;
    var b = _b;
    if (b !== undefined) {
      _accu = f(accu, b.key, b.value);
      _b = b.next;
      continue ;
    } else {
      return accu;
    }
  };
}

function reduceU(h, init, f) {
  var d = h.buckets;
  var accu = init;
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    accu = do_bucket_fold(f, d[i], accu);
  }
  return accu;
}

function reduce(h, init, f) {
  return reduceU(h, init, Curry.__3(f));
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
          return /* () */0;
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
  return /* () */0;
}

function filterMapInplaceBucket(f, h, i, _prec, _cell) {
  while(true) {
    var cell = _cell;
    var prec = _prec;
    var n = cell.next;
    var match = f(cell.key, cell.value);
    if (match !== undefined) {
      if (prec !== undefined) {
        cell.next = cell;
      } else {
        h.buckets[i] = cell;
      }
      cell.value = Caml_option.valFromOption(match);
      if (n !== undefined) {
        _cell = n;
        _prec = cell;
        continue ;
      } else {
        cell.next = n;
        return /* () */0;
      }
    } else {
      h.size = h.size - 1 | 0;
      if (n !== undefined) {
        _cell = n;
        continue ;
      } else if (prec !== undefined) {
        prec.next = n;
        return /* () */0;
      } else {
        h.buckets[i] = prec;
        return /* () */0;
      }
    }
  };
}

function keepMapInPlaceU(h, f) {
  var h_buckets = h.buckets;
  for(var i = 0 ,i_finish = h_buckets.length - 1 | 0; i <= i_finish; ++i){
    var v = h_buckets[i];
    if (v !== undefined) {
      filterMapInplaceBucket(f, h, i, undefined, v);
    }
    
  }
  return /* () */0;
}

function keepMapInPlace(h, f) {
  return keepMapInPlaceU(h, Curry.__2(f));
}

function fillArray(_i, arr, _cell) {
  while(true) {
    var cell = _cell;
    var i = _i;
    arr[i] = /* tuple */[
      cell.key,
      cell.value
    ];
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

function fillArrayMap(_i, arr, _cell, f) {
  while(true) {
    var cell = _cell;
    var i = _i;
    arr[i] = f(cell);
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

function linear(h, f) {
  var d = h.buckets;
  var current = 0;
  var arr = new Array(h.size);
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    var cell = d[i];
    if (cell !== undefined) {
      current = fillArrayMap(current, arr, cell, f);
    }
    
  }
  return arr;
}

function keysToArray(h) {
  return linear(h, (function (x) {
                return x.key;
              }));
}

function valuesToArray(h) {
  return linear(h, (function (x) {
                return x.value;
              }));
}

function toArray(h) {
  return linear(h, (function (x) {
                return /* tuple */[
                        x.key,
                        x.value
                      ];
              }));
}

var C = 0;

exports.C = C;
exports.copy = copy;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.logStats = logStats;
exports.keepMapInPlaceU = keepMapInPlaceU;
exports.keepMapInPlace = keepMapInPlace;
exports.fillArray = fillArray;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.toArray = toArray;
exports.getBucketHistogram = getBucketHistogram;
/* No side effect */
