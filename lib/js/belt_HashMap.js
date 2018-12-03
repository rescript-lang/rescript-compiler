'use strict';

var Caml_option = require("./caml_option.js");
var Belt_internalBuckets = require("./belt_internalBuckets.js");
var Belt_internalBucketsType = require("./belt_internalBucketsType.js");

function size(prim) {
  return prim.size;
}

function copyBucketReHash(hash, h_buckets, ndata_tail, _old_bucket) {
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

function replaceInBucket(eq, key, info, _cell) {
  while(true) {
    var cell = _cell;
    if (eq(cell.key, key)) {
      cell.value = info;
      return false;
    } else {
      var match = cell.next;
      if (match !== undefined) {
        _cell = match;
        continue ;
      } else {
        return true;
      }
    }
  };
}

function set0(h, key, value, eq, hash) {
  var h_buckets = h.buckets;
  var buckets_len = h_buckets.length;
  var i = hash(key) & (buckets_len - 1 | 0);
  var l = h_buckets[i];
  if (l !== undefined) {
    if (replaceInBucket(eq, key, value, l)) {
      h_buckets[i] = {
        key: key,
        value: value,
        next: l
      };
      h.size = h.size + 1 | 0;
    }
    
  } else {
    h_buckets[i] = {
      key: key,
      value: value,
      next: undefined
    };
    h.size = h.size + 1 | 0;
  }
  if (h.size > (buckets_len << 1)) {
    var hash$1 = hash;
    var h$1 = h;
    var odata = h$1.buckets;
    var osize = odata.length;
    var nsize = (osize << 1);
    if (nsize >= osize) {
      var h_buckets$1 = new Array(nsize);
      var ndata_tail = new Array(nsize);
      h$1.buckets = h_buckets$1;
      for(var i$1 = 0 ,i_finish = osize - 1 | 0; i$1 <= i_finish; ++i$1){
        copyBucketReHash(hash$1, h_buckets$1, ndata_tail, odata[i$1]);
      }
      for(var i$2 = 0 ,i_finish$1 = nsize - 1 | 0; i$2 <= i_finish$1; ++i$2){
        var match = ndata_tail[i$2];
        if (match !== undefined) {
          match.next = undefined;
        }
        
      }
      return /* () */0;
    } else {
      return 0;
    }
  } else {
    return 0;
  }
}

function set(h, key, value) {
  return set0(h, key, value, h.eq, h.hash);
}

function remove(h, key) {
  var h_buckets = h.buckets;
  var i = h.hash(key) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[i];
  if (bucket !== undefined) {
    var eq = h.eq;
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

function get(h, key) {
  var h_buckets = h.buckets;
  var nid = h.hash(key) & (h_buckets.length - 1 | 0);
  var match = h_buckets[nid];
  if (match !== undefined) {
    var eq = h.eq;
    if (eq(key, match.key)) {
      return Caml_option.some(match.value);
    } else {
      var match$1 = match.next;
      if (match$1 !== undefined) {
        if (eq(key, match$1.key)) {
          return Caml_option.some(match$1.value);
        } else {
          var match$2 = match$1.next;
          if (match$2 !== undefined) {
            if (eq(key, match$2.key)) {
              return Caml_option.some(match$2.value);
            } else {
              var eq$1 = eq;
              var key$1 = key;
              var _buckets = match$2.next;
              while(true) {
                var buckets = _buckets;
                if (buckets !== undefined) {
                  if (eq$1(key$1, buckets.key)) {
                    return Caml_option.some(buckets.value);
                  } else {
                    _buckets = buckets.next;
                    continue ;
                  }
                } else {
                  return undefined;
                }
              };
            }
          } else {
            return undefined;
          }
        }
      } else {
        return undefined;
      }
    }
  }
  
}

function has(h, key) {
  var h_buckets = h.buckets;
  var nid = h.hash(key) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[nid];
  if (bucket !== undefined) {
    var key$1 = key;
    var _cell = bucket;
    var eq = h.eq;
    while(true) {
      var cell = _cell;
      if (eq(cell.key, key$1)) {
        return true;
      } else {
        var match = cell.next;
        if (match !== undefined) {
          _cell = match;
          continue ;
        } else {
          return false;
        }
      }
    };
  } else {
    return false;
  }
}

function make(hintSize, id) {
  return Belt_internalBucketsType.make(id[/* hash */0], id[/* eq */1], hintSize);
}

function fromArray(arr, id) {
  var hash = id[/* hash */0];
  var eq = id[/* eq */1];
  var len = arr.length;
  var v = Belt_internalBucketsType.make(hash, eq, len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr[i];
    set0(v, match[0], match[1], eq, hash);
  }
  return v;
}

function mergeMany(h, arr) {
  var hash = h.hash;
  var eq = h.eq;
  var len = arr.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr[i];
    set0(h, match[0], match[1], eq, hash);
  }
  return /* () */0;
}

var Int = 0;

var $$String = 0;

var clear = Belt_internalBucketsType.clear;

var isEmpty = Belt_internalBucketsType.isEmpty;

var copy = Belt_internalBuckets.copy;

var forEachU = Belt_internalBuckets.forEachU;

var forEach = Belt_internalBuckets.forEach;

var reduceU = Belt_internalBuckets.reduceU;

var reduce = Belt_internalBuckets.reduce;

var keepMapInPlaceU = Belt_internalBuckets.keepMapInPlaceU;

var keepMapInPlace = Belt_internalBuckets.keepMapInPlace;

var toArray = Belt_internalBuckets.toArray;

var keysToArray = Belt_internalBuckets.keysToArray;

var valuesToArray = Belt_internalBuckets.valuesToArray;

var getBucketHistogram = Belt_internalBuckets.getBucketHistogram;

var logStats = Belt_internalBuckets.logStats;

exports.Int = Int;
exports.$$String = $$String;
exports.make = make;
exports.clear = clear;
exports.isEmpty = isEmpty;
exports.set = set;
exports.copy = copy;
exports.get = get;
exports.has = has;
exports.remove = remove;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.keepMapInPlaceU = keepMapInPlaceU;
exports.keepMapInPlace = keepMapInPlace;
exports.size = size;
exports.toArray = toArray;
exports.keysToArray = keysToArray;
exports.valuesToArray = valuesToArray;
exports.fromArray = fromArray;
exports.mergeMany = mergeMany;
exports.getBucketHistogram = getBucketHistogram;
exports.logStats = logStats;
/* No side effect */
