'use strict';

var Belt_internalSetBuckets = require("./belt_internalSetBuckets.js");
var Belt_internalBucketsType = require("./belt_internalBucketsType.js");

function copyBucket(hash, h_buckets, ndata_tail, _old_bucket) {
  while(true) {
    var old_bucket = _old_bucket;
    if (old_bucket === void 0) {
      return ;
    }
    var nidx = hash(old_bucket.key) & (h_buckets.length - 1 | 0);
    var match = ndata_tail[nidx];
    if (match !== void 0) {
      match.next = old_bucket;
    } else {
      h_buckets[nidx] = old_bucket;
    }
    ndata_tail[nidx] = old_bucket;
    _old_bucket = old_bucket.next;
    continue ;
  };
}

function remove(h, key) {
  var eq = h.eq;
  var h_buckets = h.buckets;
  var i = h.hash(key) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (l === void 0) {
    return ;
  }
  var next_cell = l.next;
  if (eq(l.key, key)) {
    h.size = h.size - 1 | 0;
    h_buckets[i] = next_cell;
    return ;
  } else if (next_cell !== void 0) {
    var _prec = l;
    var _cell = next_cell;
    while(true) {
      var cell = _cell;
      var prec = _prec;
      var cell_next = cell.next;
      if (eq(cell.key, key)) {
        prec.next = cell_next;
        h.size = h.size - 1 | 0;
        return ;
      } else {
        if (cell_next === void 0) {
          return ;
        }
        _cell = cell_next;
        _prec = cell;
        continue ;
      }
    };
  } else {
    return ;
  }
}

function addBucket(h, key, _cell, eq) {
  while(true) {
    var cell = _cell;
    if (eq(cell.key, key)) {
      return ;
    }
    var n = cell.next;
    if (n !== void 0) {
      _cell = n;
      continue ;
    } else {
      h.size = h.size + 1 | 0;
      cell.next = {
        key: key,
        next: void 0
      };
      return ;
    }
  };
}

function add0(h, key, hash, eq) {
  var h_buckets = h.buckets;
  var buckets_len = h_buckets.length;
  var i = hash(key) & (buckets_len - 1 | 0);
  var l = h_buckets[i];
  if (l !== void 0) {
    addBucket(h, key, l, eq);
  } else {
    h.size = h.size + 1 | 0;
    h_buckets[i] = {
      key: key,
      next: void 0
    };
  }
  if (h.size > (buckets_len << 1)) {
    var odata = h.buckets;
    var osize = odata.length;
    var nsize = (osize << 1);
    if (nsize < osize) {
      return ;
    }
    var h_buckets$1 = new Array(nsize);
    var ndata_tail = new Array(nsize);
    h.buckets = h_buckets$1;
    for(var i$1 = 0 ,i_finish = osize - 1 | 0; i$1 <= i_finish; ++i$1){
      copyBucket(hash, h_buckets$1, ndata_tail, odata[i$1]);
    }
    for(var i$2 = 0 ,i_finish$1 = nsize - 1 | 0; i$2 <= i_finish$1; ++i$2){
      var match = ndata_tail[i$2];
      if (match !== void 0) {
        match.next = void 0;
      }
      
    }
    return ;
  }
  
}

function add(h, key) {
  return add0(h, key, h.hash, h.eq);
}

function has(h, key) {
  var eq = h.eq;
  var h_buckets = h.buckets;
  var nid = h.hash(key) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[nid];
  if (bucket !== void 0) {
    var _cell = bucket;
    while(true) {
      var cell = _cell;
      if (eq(cell.key, key)) {
        return true;
      }
      var match = cell.next;
      if (match === void 0) {
        return false;
      }
      _cell = match;
      continue ;
    };
  } else {
    return false;
  }
}

function make(hintSize, id) {
  return Belt_internalBucketsType.make(id.hash, id.eq, hintSize);
}

function size(prim) {
  return prim.size;
}

function fromArray(arr, id) {
  var eq = id.eq;
  var hash = id.hash;
  var len = arr.length;
  var v = Belt_internalBucketsType.make(hash, eq, len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    add0(v, arr[i], hash, eq);
  }
  return v;
}

function mergeMany(h, arr) {
  var eq = h.eq;
  var hash = h.hash;
  var len = arr.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    add0(h, arr[i], hash, eq);
  }
  
}

var Int = /* alias */0;

var $$String = /* alias */0;

var clear = Belt_internalBucketsType.clear;

var isEmpty = Belt_internalBucketsType.isEmpty;

var copy = Belt_internalSetBuckets.copy;

var forEachU = Belt_internalSetBuckets.forEachU;

var forEach = Belt_internalSetBuckets.forEach;

var reduceU = Belt_internalSetBuckets.reduceU;

var reduce = Belt_internalSetBuckets.reduce;

var logStats = Belt_internalSetBuckets.logStats;

var toArray = Belt_internalSetBuckets.toArray;

var getBucketHistogram = Belt_internalSetBuckets.getBucketHistogram;

exports.Int = Int;
exports.$$String = $$String;
exports.make = make;
exports.clear = clear;
exports.isEmpty = isEmpty;
exports.add = add;
exports.copy = copy;
exports.has = has;
exports.remove = remove;
exports.forEachU = forEachU;
exports.forEach = forEach;
exports.reduceU = reduceU;
exports.reduce = reduce;
exports.size = size;
exports.logStats = logStats;
exports.toArray = toArray;
exports.fromArray = fromArray;
exports.mergeMany = mergeMany;
exports.getBucketHistogram = getBucketHistogram;
/* No side effect */
