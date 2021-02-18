

import * as Curry from "./curry.js";
import * as Belt_Array from "./belt_Array.js";
import * as Caml_option from "./caml_option.js";

function copyBucket(c) {
  if (c === undefined) {
    return c;
  }
  var head = {
    key: c.key,
    value: c.value,
    next: undefined
  };
  copyAuxCont(c.next, head);
  return head;
}

function copyAuxCont(_c, _prec) {
  while(true) {
    var prec = _prec;
    var c = _c;
    if (c === undefined) {
      return ;
    }
    var ncopy = {
      key: c.key,
      value: c.value,
      next: undefined
    };
    prec.next = ncopy;
    _prec = ncopy;
    _c = c.next;
    continue ;
  };
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

function do_bucket_iter(f, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets === undefined) {
      return ;
    }
    f(buckets.key, buckets.value);
    _buckets = buckets.next;
    continue ;
  };
}

function forEachU(h, f) {
  var d = h.buckets;
  for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
    do_bucket_iter(f, d[i]);
  }
  
}

function forEach(h, f) {
  return forEachU(h, Curry.__2(f));
}

function do_bucket_fold(f, _b, _accu) {
  while(true) {
    var accu = _accu;
    var b = _b;
    if (b === undefined) {
      return accu;
    }
    _accu = f(accu, b.key, b.value);
    _b = b.next;
    continue ;
  };
}

function reduceU(h, init, f) {
  var d = h.buckets;
  var accu = init;
  for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
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

function filterMapInplaceBucket(f, h, i, _prec, _cell) {
  while(true) {
    var cell = _cell;
    var prec = _prec;
    var n = cell.next;
    var data = f(cell.key, cell.value);
    if (data !== undefined) {
      if (prec !== undefined) {
        cell.next = cell;
      } else {
        h.buckets[i] = cell;
      }
      cell.value = Caml_option.valFromOption(data);
      if (n === undefined) {
        cell.next = n;
        return ;
      }
      _cell = n;
      _prec = cell;
      continue ;
    }
    h.size = h.size - 1 | 0;
    if (n === undefined) {
      if (prec !== undefined) {
        prec.next = n;
      } else {
        h.buckets[i] = prec;
      }
      return ;
    }
    _cell = n;
    continue ;
  };
}

function keepMapInPlaceU(h, f) {
  var h_buckets = h.buckets;
  for(var i = 0 ,i_finish = h_buckets.length; i < i_finish; ++i){
    var v = h_buckets[i];
    if (v !== undefined) {
      filterMapInplaceBucket(f, h, i, undefined, v);
    }
    
  }
  
}

function keepMapInPlace(h, f) {
  return keepMapInPlaceU(h, Curry.__2(f));
}

function fillArray(_i, arr, _cell) {
  while(true) {
    var cell = _cell;
    var i = _i;
    arr[i] = [
      cell.key,
      cell.value
    ];
    var v = cell.next;
    if (v === undefined) {
      return i + 1 | 0;
    }
    _cell = v;
    _i = i + 1 | 0;
    continue ;
  };
}

function fillArrayMap(_i, arr, _cell, f) {
  while(true) {
    var cell = _cell;
    var i = _i;
    arr[i] = f(cell);
    var v = cell.next;
    if (v === undefined) {
      return i + 1 | 0;
    }
    _cell = v;
    _i = i + 1 | 0;
    continue ;
  };
}

function linear(h, f) {
  var d = h.buckets;
  var current = 0;
  var arr = new Array(h.size);
  for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
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
                return [
                        x.key,
                        x.value
                      ];
              }));
}

var C;

export {
  C ,
  copy ,
  forEachU ,
  forEach ,
  reduceU ,
  reduce ,
  logStats ,
  keepMapInPlaceU ,
  keepMapInPlace ,
  fillArray ,
  keysToArray ,
  valuesToArray ,
  toArray ,
  getBucketHistogram ,
  
}
/* No side effect */
