'use strict';

var Bs_internalBuckets = require("./bs_internalBuckets.js");
var Bs_internalBucketsType = require("./bs_internalBucketsType.js");

function copyBucketReHash(hash, h_buckets, ndata_tail, _, _old_bucket) {
  while(true) {
    var old_bucket = _old_bucket;
    if (old_bucket !== undefined) {
      var nidx = hash(old_bucket.key) & (h_buckets.length - 1 | 0);
      var match = ndata_tail[nidx];
      if (match !== undefined) {
        match.next = old_bucket;
      } else {
        h_buckets[nidx] = old_bucket;
      }
      ndata_tail[nidx] = old_bucket;
      _old_bucket = old_bucket.next;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function resize(hash, h) {
  var odata = h.buckets;
  var osize = odata.length;
  var nsize = (osize << 1);
  if (nsize >= osize) {
    var h_buckets = new Array(nsize);
    var ndata_tail = new Array(nsize);
    h.buckets = h_buckets;
    for(var i = 0 ,i_finish = osize - 1 | 0; i <= i_finish; ++i){
      copyBucketReHash(hash, h_buckets, ndata_tail, h, odata[i]);
    }
    for(var i$1 = 0 ,i_finish$1 = nsize - 1 | 0; i$1 <= i_finish$1; ++i$1){
      var match = ndata_tail[i$1];
      if (match !== undefined) {
        match.next = Bs_internalBucketsType.emptyOpt;
      }
      
    }
    return /* () */0;
  } else {
    return 0;
  }
}

function replaceInBucket(eq, key, info, _cell) {
  while(true) {
    var cell = _cell;
    if (eq(cell.key, key)) {
      cell.value = info;
      return /* false */0;
    } else {
      var match = cell.next;
      if (match !== undefined) {
        _cell = match;
        continue ;
        
      } else {
        return /* true */1;
      }
    }
  };
}

function setDone0(hash, eq, h, key, value) {
  var h_buckets = h.buckets;
  var i = hash(key) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (l !== undefined) {
    if (replaceInBucket(eq, key, value, l)) {
      h_buckets[i] = {
        key: key,
        value: value,
        next: l
      };
      h.size = h.size + 1 | 0;
      if (h.size > (h.buckets.length << 1)) {
        return resize(hash, h);
      } else {
        return 0;
      }
    } else {
      return 0;
    }
  } else {
    h_buckets[i] = {
      key: key,
      value: value,
      next: l
    };
    h.size = h.size + 1 | 0;
    if (h.size > (h.buckets.length << 1)) {
      return resize(hash, h);
    } else {
      return 0;
    }
  }
}

function make(initialize_size, dict) {
  return {
          dict: dict,
          data: Bs_internalBucketsType.make(initialize_size)
        };
}

function clear(h) {
  return Bs_internalBucketsType.clear(h.data);
}

function size(h) {
  return h.data.size;
}

function forEach(h, f) {
  return Bs_internalBuckets.forEach(h.data, f);
}

function reduce(h, init, f) {
  return Bs_internalBuckets.reduce(h.data, init, f);
}

function logStats(h) {
  return Bs_internalBuckets.logStats(h.data);
}

function set(h, key, info) {
  var M = h.dict;
  return setDone0(M[/* hash */0], M[/* eq */1], h.data, key, info);
}

function remove(h, key) {
  var M = h.dict;
  var hash = M[/* hash */0];
  var eq = M[/* eq */1];
  var h$1 = h.data;
  var key$1 = key;
  var h_buckets = h$1.buckets;
  var i = hash(key$1) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[i];
  if (bucket !== undefined) {
    if (eq(bucket.key, key$1)) {
      h_buckets[i] = bucket.next;
      h$1.size = h$1.size - 1 | 0;
      return /* () */0;
    } else {
      var h$2 = h$1;
      var key$2 = key$1;
      var _prec = bucket;
      var _bucket = bucket.next;
      var eq$1 = eq;
      while(true) {
        var bucket$1 = _bucket;
        var prec = _prec;
        if (bucket$1 !== undefined) {
          var cell_next = bucket$1.next;
          if (eq$1(bucket$1.key, key$2)) {
            prec.next = cell_next;
            h$2.size = h$2.size - 1 | 0;
            return /* () */0;
          } else {
            _bucket = cell_next;
            _prec = bucket$1;
            continue ;
            
          }
        } else {
          return /* () */0;
        }
      };
    }
  } else {
    return /* () */0;
  }
}

function get(h, key) {
  var M = h.dict;
  var hash = M[/* hash */0];
  var eq = M[/* eq */1];
  var h$1 = h.data;
  var key$1 = key;
  var h_buckets = h$1.buckets;
  var nid = hash(key$1) & (h_buckets.length - 1 | 0);
  var match = h_buckets[nid];
  if (match !== undefined) {
    if (eq(key$1, match.key)) {
      return /* Some */[match.value];
    } else {
      var match$1 = match.next;
      if (match$1 !== undefined) {
        if (eq(key$1, match$1.key)) {
          return /* Some */[match$1.value];
        } else {
          var match$2 = match$1.next;
          if (match$2 !== undefined) {
            if (eq(key$1, match$2.key)) {
              return /* Some */[match$2.value];
            } else {
              var eq$1 = eq;
              var key$2 = key$1;
              var _buckets = match$2.next;
              while(true) {
                var buckets = _buckets;
                if (buckets !== undefined) {
                  if (eq$1(key$2, buckets.key)) {
                    return /* Some */[buckets.value];
                  } else {
                    _buckets = buckets.next;
                    continue ;
                    
                  }
                } else {
                  return /* None */0;
                }
              };
            }
          } else {
            return /* None */0;
          }
        }
      } else {
        return /* None */0;
      }
    }
  } else {
    return /* None */0;
  }
}

function has(h, key) {
  var M = h.dict;
  var hash = M[/* hash */0];
  var eq = M[/* eq */1];
  var h$1 = h.data;
  var key$1 = key;
  var h_buckets = h$1.buckets;
  var nid = hash(key$1) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[nid];
  if (bucket !== undefined) {
    var eq$1 = eq;
    var key$2 = key$1;
    var _cell = bucket;
    while(true) {
      var cell = _cell;
      if (eq$1(cell.key, key$2)) {
        return /* true */1;
      } else {
        var match = cell.next;
        if (match !== undefined) {
          _cell = match;
          continue ;
          
        } else {
          return /* false */0;
        }
      }
    };
  } else {
    return /* false */0;
  }
}

function keepMapInPlace(h, f) {
  return Bs_internalBuckets.keepMapInPlace(h.data, f);
}

function toArray(h) {
  return Bs_internalBuckets.toArray(h.data);
}

function ofArray0(arr, hash, eq) {
  var len = arr.length;
  var v = Bs_internalBucketsType.make(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr[i];
    setDone0(hash, eq, v, match[0], match[1]);
  }
  return v;
}

function ofArray(arr, dict) {
  return {
          dict: dict,
          data: ofArray0(arr, dict[/* hash */0], dict[/* eq */1])
        };
}

function mergeMany(h, arr) {
  var M = h.dict;
  var h$1 = h.data;
  var arr$1 = arr;
  var hash = M[/* hash */0];
  var eq = M[/* eq */1];
  var len = arr$1.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr$1[i];
    setDone0(hash, eq, h$1, match[0], match[1]);
  }
  return /* () */0;
}

function copy(h) {
  return {
          dict: h.dict,
          data: Bs_internalBuckets.copy(h.data)
        };
}

function keysToArray(h) {
  return Bs_internalBuckets.keysToArray(h.data);
}

function valuesToArray(h) {
  return Bs_internalBuckets.valuesToArray(h.data);
}

function getBucketHistogram(h) {
  return Bs_internalBuckets.getBucketHistogram(h.data);
}

function isEmpty(h) {
  return +(h.data.size === 0);
}

exports.make = make;
exports.clear = clear;
exports.isEmpty = isEmpty;
exports.set = set;
exports.copy = copy;
exports.get = get;
exports.has = has;
exports.remove = remove;
exports.forEach = forEach;
exports.reduce = reduce;
exports.keepMapInPlace = keepMapInPlace;
exports.size = size;
exports.logStats = logStats;
exports.toArray = toArray;
exports.ofArray = ofArray;
exports.mergeMany = mergeMany;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.getBucketHistogram = getBucketHistogram;
/* No side effect */
