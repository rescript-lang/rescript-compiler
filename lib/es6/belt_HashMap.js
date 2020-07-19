

import * as Caml_option from "./caml_option.js";
import * as Belt_internalBuckets from "./belt_internalBuckets.js";
import * as Belt_internalBucketsType from "./belt_internalBucketsType.js";

function size(h) {
  return h.size;
}

function copyBucketReHash(hash, h_buckets, ndata_tail, _old_bucket) {
  while(true) {
    var old_bucket = _old_bucket;
    if (old_bucket === undefined) {
      return ;
    }
    var nidx = hash(old_bucket.key) & (h_buckets.length - 1 | 0);
    var tail = ndata_tail[nidx];
    if (tail !== undefined) {
      tail.next = old_bucket;
    } else {
      h_buckets[nidx] = old_bucket;
    }
    ndata_tail[nidx] = old_bucket;
    _old_bucket = old_bucket.next;
    continue ;
  };
}

function replaceInBucket(eq, key, info, _cell) {
  while(true) {
    var cell = _cell;
    if (eq(cell.key, key)) {
      cell.value = info;
      return false;
    }
    var cell$1 = cell.next;
    if (cell$1 === undefined) {
      return true;
    }
    _cell = cell$1;
    continue ;
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
    var odata = h.buckets;
    var osize = odata.length;
    var nsize = (osize << 1);
    if (nsize < osize) {
      return ;
    }
    var h_buckets$1 = new Array(nsize);
    var ndata_tail = new Array(nsize);
    h.buckets = h_buckets$1;
    for(var i$1 = 0; i$1 < osize; ++i$1){
      copyBucketReHash(hash, h_buckets$1, ndata_tail, odata[i$1]);
    }
    for(var i$2 = 0; i$2 < nsize; ++i$2){
      var tail = ndata_tail[i$2];
      if (tail !== undefined) {
        tail.next = undefined;
      }
      
    }
    return ;
  }
  
}

function set(h, key, value) {
  return set0(h, key, value, h.eq, h.hash);
}

function remove(h, key) {
  var h_buckets = h.buckets;
  var i = h.hash(key) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[i];
  if (bucket === undefined) {
    return ;
  }
  var eq = h.eq;
  if (eq(bucket.key, key)) {
    h_buckets[i] = bucket.next;
    h.size = h.size - 1 | 0;
    return ;
  } else {
    var _prec = bucket;
    var _bucket = bucket.next;
    while(true) {
      var bucket$1 = _bucket;
      var prec = _prec;
      if (bucket$1 === undefined) {
        return ;
      }
      var cell_next = bucket$1.next;
      if (eq(bucket$1.key, key)) {
        prec.next = cell_next;
        h.size = h.size - 1 | 0;
        return ;
      }
      _bucket = cell_next;
      _prec = bucket$1;
      continue ;
    };
  }
}

function get(h, key) {
  var h_buckets = h.buckets;
  var nid = h.hash(key) & (h_buckets.length - 1 | 0);
  var cell1 = h_buckets[nid];
  if (cell1 === undefined) {
    return ;
  }
  var eq = h.eq;
  if (eq(key, cell1.key)) {
    return Caml_option.some(cell1.value);
  }
  var cell2 = cell1.next;
  if (cell2 === undefined) {
    return ;
  }
  if (eq(key, cell2.key)) {
    return Caml_option.some(cell2.value);
  }
  var cell3 = cell2.next;
  if (cell3 !== undefined) {
    if (eq(key, cell3.key)) {
      return Caml_option.some(cell3.value);
    } else {
      var _buckets = cell3.next;
      while(true) {
        var buckets = _buckets;
        if (buckets === undefined) {
          return ;
        }
        if (eq(key, buckets.key)) {
          return Caml_option.some(buckets.value);
        }
        _buckets = buckets.next;
        continue ;
      };
    }
  }
  
}

function has(h, key) {
  var h_buckets = h.buckets;
  var nid = h.hash(key) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[nid];
  if (bucket !== undefined) {
    var _cell = bucket;
    var eq = h.eq;
    while(true) {
      var cell = _cell;
      if (eq(cell.key, key)) {
        return true;
      }
      var nextCell = cell.next;
      if (nextCell === undefined) {
        return false;
      }
      _cell = nextCell;
      continue ;
    };
  } else {
    return false;
  }
}

function make(hintSize, id) {
  return Belt_internalBucketsType.make(id.hash, id.eq, hintSize);
}

function fromArray(arr, id) {
  var hash = id.hash;
  var eq = id.eq;
  var len = arr.length;
  var v = Belt_internalBucketsType.make(hash, eq, len);
  for(var i = 0; i < len; ++i){
    var match = arr[i];
    set0(v, match[0], match[1], eq, hash);
  }
  return v;
}

function mergeMany(h, arr) {
  var hash = h.hash;
  var eq = h.eq;
  var len = arr.length;
  for(var i = 0; i < len; ++i){
    var match = arr[i];
    set0(h, match[0], match[1], eq, hash);
  }
  
}

var Int;

var $$String;

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

export {
  Int ,
  $$String ,
  make ,
  clear ,
  isEmpty ,
  set ,
  copy ,
  get ,
  has ,
  remove ,
  forEachU ,
  forEach ,
  reduceU ,
  reduce ,
  keepMapInPlaceU ,
  keepMapInPlace ,
  size ,
  toArray ,
  keysToArray ,
  valuesToArray ,
  fromArray ,
  mergeMany ,
  getBucketHistogram ,
  logStats ,
  
}
/* No side effect */
