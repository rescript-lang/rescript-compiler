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

function remove0(hash, eq, h, key) {
  var h_buckets = h.buckets;
  var i = hash(key) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[i];
  if (bucket !== undefined) {
    if (eq(bucket.key, key)) {
      h_buckets[i] = bucket.next;
      h.size = h.size - 1 | 0;
      return /* () */0;
    } else {
      var h$1 = h;
      var key$1 = key;
      var _prec = bucket;
      var _bucket = bucket.next;
      var eq$1 = eq;
      while(true) {
        var bucket$1 = _bucket;
        var prec = _prec;
        if (bucket$1 !== undefined) {
          var cell_next = bucket$1.next;
          if (eq$1(bucket$1.key, key$1)) {
            prec.next = cell_next;
            h$1.size = h$1.size - 1 | 0;
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

function get0(hash, eq, h, key) {
  var h_buckets = h.buckets;
  var nid = hash(key) & (h_buckets.length - 1 | 0);
  var match = h_buckets[nid];
  if (match !== undefined) {
    if (eq(key, match.key)) {
      return /* Some */[match.value];
    } else {
      var match$1 = match.next;
      if (match$1 !== undefined) {
        if (eq(key, match$1.key)) {
          return /* Some */[match$1.value];
        } else {
          var match$2 = match$1.next;
          if (match$2 !== undefined) {
            if (eq(key, match$2.key)) {
              return /* Some */[match$2.value];
            } else {
              var eq$1 = eq;
              var key$1 = key;
              var _buckets = match$2.next;
              while(true) {
                var buckets = _buckets;
                if (buckets !== undefined) {
                  if (eq$1(key$1, buckets.key)) {
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

function has0(hash, eq, h, key) {
  var h_buckets = h.buckets;
  var nid = hash(key) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[nid];
  if (bucket !== undefined) {
    var eq$1 = eq;
    var key$1 = key;
    var _cell = bucket;
    while(true) {
      var cell = _cell;
      if (eq$1(cell.key, key$1)) {
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

function size0(prim) {
  return prim.size;
}

function create(initialize_size, dict) {
  return {
          dict: dict,
          data: Bs_internalBucketsType.create0(initialize_size)
        };
}

function clear(h) {
  return Bs_internalBucketsType.clear0(h.data);
}

function size(h) {
  return h.data.size;
}

function forEach(h, f) {
  return Bs_internalBuckets.forEach0(h.data, f);
}

function reduce(h, init, f) {
  return Bs_internalBuckets.reduce0(h.data, init, f);
}

function logStats(h) {
  return Bs_internalBuckets.logStats0(h.data);
}

function setDone(h, key, info) {
  var M = h.dict;
  return setDone0(M[/* hash */0], M[/* eq */1], h.data, key, info);
}

function set(h, key, info) {
  setDone(h, key, info);
  return h;
}

function removeDone(h, key) {
  var M = h.dict;
  return remove0(M[/* hash */0], M[/* eq */1], h.data, key);
}

function remove(h, key) {
  removeDone(h, key);
  return h;
}

function get(h, key) {
  var M = h.dict;
  return get0(M[/* hash */0], M[/* eq */1], h.data, key);
}

function has(h, key) {
  var M = h.dict;
  return has0(M[/* hash */0], M[/* eq */1], h.data, key);
}

function filterMapDone(h, f) {
  return Bs_internalBuckets.filterMapInplace0(h.data, f);
}

function filterMap(h, f) {
  Bs_internalBuckets.filterMapInplace0(h.data, f);
  return h;
}

function toArray(h) {
  return Bs_internalBuckets.toArray0(h.data);
}

function ofArray0(arr, hash, eq) {
  var len = arr.length;
  var v = Bs_internalBucketsType.create0(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr[i];
    setDone0(hash, eq, v, match[0], match[1]);
  }
  return v;
}

function mergeArrayDone0(h, arr, hash, eq) {
  var len = arr.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr[i];
    setDone0(hash, eq, h, match[0], match[1]);
  }
  return /* () */0;
}

function mergeArray0(h, arr, hash, eq) {
  mergeArrayDone0(h, arr, hash, eq);
  return h;
}

function ofArray(arr, dict) {
  return {
          dict: dict,
          data: ofArray0(arr, dict[/* hash */0], dict[/* eq */1])
        };
}

function mergeArrayDone(h, arr) {
  var M = h.dict;
  return mergeArrayDone0(h.data, arr, M[/* hash */0], M[/* eq */1]);
}

function mergeArray(h, arr) {
  mergeArrayDone(h, arr);
  return h;
}

function copy(h) {
  return {
          dict: h.dict,
          data: Bs_internalBuckets.copy(h.data)
        };
}

function keysToArray(h) {
  return Bs_internalBuckets.keys0(h.data);
}

function valuesToArray(h) {
  return Bs_internalBuckets.values0(h.data);
}

function getBucketHistogram(h) {
  return Bs_internalBuckets.getBucketHistogram(h.data);
}

function getData(prim) {
  return prim.data;
}

function getDict(prim) {
  return prim.dict;
}

function packDictData(prim, prim$1) {
  return {
          dict: prim,
          data: prim$1
        };
}

var create0 = Bs_internalBucketsType.create0;

var clear0 = Bs_internalBucketsType.clear0;

var forEach0 = Bs_internalBuckets.forEach0;

var reduce0 = Bs_internalBuckets.reduce0;

var filterMapDone0 = Bs_internalBuckets.filterMapInplace0;

var logStats0 = Bs_internalBuckets.logStats0;

var toArray0 = Bs_internalBuckets.toArray0;

var keysToArray0 = Bs_internalBuckets.keys0;

var valuesToArray0 = Bs_internalBuckets.values0;

exports.create = create;
exports.clear = clear;
exports.setDone = setDone;
exports.set = set;
exports.copy = copy;
exports.get = get;
exports.has = has;
exports.removeDone = removeDone;
exports.remove = remove;
exports.forEach = forEach;
exports.reduce = reduce;
exports.filterMapDone = filterMapDone;
exports.filterMap = filterMap;
exports.size = size;
exports.logStats = logStats;
exports.toArray = toArray;
exports.ofArray = ofArray;
exports.mergeArrayDone = mergeArrayDone;
exports.mergeArray = mergeArray;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.getBucketHistogram = getBucketHistogram;
exports.getData = getData;
exports.getDict = getDict;
exports.packDictData = packDictData;
exports.create0 = create0;
exports.clear0 = clear0;
exports.setDone0 = setDone0;
exports.get0 = get0;
exports.has0 = has0;
exports.remove0 = remove0;
exports.forEach0 = forEach0;
exports.reduce0 = reduce0;
exports.filterMapDone0 = filterMapDone0;
exports.size0 = size0;
exports.logStats0 = logStats0;
exports.toArray0 = toArray0;
exports.ofArray0 = ofArray0;
exports.mergeArray0 = mergeArray0;
exports.keysToArray0 = keysToArray0;
exports.valuesToArray0 = valuesToArray0;
/* No side effect */
