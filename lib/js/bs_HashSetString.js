'use strict';

var Caml_hash = require("./caml_hash.js");
var Bs_internalSetBuckets = require("./bs_internalSetBuckets.js");
var Bs_internalBucketsType = require("./bs_internalBucketsType.js");

function hash(s) {
  return Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, s));
}

function copyBucket(h_buckets, ndata_tail, _old_bucket) {
  while(true) {
    var old_bucket = _old_bucket;
    if (old_bucket !== undefined) {
      var s = old_bucket.key;
      var nidx = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, s)) & (h_buckets.length - 1 | 0);
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

function tryDoubleResize(h) {
  var odata = h.buckets;
  var osize = odata.length;
  var nsize = (osize << 1);
  if (nsize >= osize) {
    var h_buckets = new Array(nsize);
    var ndata_tail = new Array(nsize);
    h.buckets = h_buckets;
    for(var i = 0 ,i_finish = osize - 1 | 0; i <= i_finish; ++i){
      copyBucket(h_buckets, ndata_tail, odata[i]);
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

function removeBucket(h, _, _$1, key, _prec, _cell) {
  while(true) {
    var cell = _cell;
    var prec = _prec;
    var cell_next = cell.next;
    if (cell.key === key) {
      prec.next = cell_next;
      h.size = h.size - 1 | 0;
      return /* () */0;
    } else if (cell_next !== undefined) {
      _cell = cell_next;
      _prec = cell;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function remove(h, key) {
  var h_buckets = h.buckets;
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (l !== undefined) {
    var next_cell = l.next;
    if (l.key === key) {
      h.size = h.size - 1 | 0;
      h_buckets[i] = next_cell;
      return /* () */0;
    } else if (next_cell !== undefined) {
      return removeBucket(h, h_buckets, i, key, l, next_cell);
    } else {
      return /* () */0;
    }
  } else {
    return /* () */0;
  }
}

function addBucket(h, key, _cell) {
  while(true) {
    var cell = _cell;
    if (cell.key !== key) {
      var n = cell.next;
      if (n !== undefined) {
        _cell = n;
        continue ;
        
      } else {
        h.size = h.size + 1 | 0;
        cell.next = {
          key: key,
          next: Bs_internalBucketsType.emptyOpt
        };
        return /* () */0;
      }
    } else {
      return 0;
    }
  };
}

function add(h, key) {
  var h_buckets = h.buckets;
  var buckets_len = h_buckets.length;
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, key)) & (buckets_len - 1 | 0);
  var l = h_buckets[i];
  if (l !== undefined) {
    addBucket(h, key, l);
  } else {
    h_buckets[i] = {
      key: key,
      next: Bs_internalBucketsType.emptyOpt
    };
    h.size = h.size + 1 | 0;
  }
  if (h.size > (buckets_len << 1)) {
    return tryDoubleResize(h);
  } else {
    return 0;
  }
}

function memInBucket(key, _cell) {
  while(true) {
    var cell = _cell;
    if (cell.key === key) {
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
}

function has(h, key) {
  var h_buckets = h.buckets;
  var nid = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[nid];
  if (bucket !== undefined) {
    return memInBucket(key, bucket);
  } else {
    return /* false */0;
  }
}

function make(size) {
  return Bs_internalBucketsType.make(/* () */0, /* () */0, size);
}

function size(prim) {
  return prim.size;
}

function ofArray(arr) {
  var len = arr.length;
  var v = Bs_internalBucketsType.make(/* () */0, /* () */0, len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    add(v, arr[i]);
  }
  return v;
}

function mergeMany(h, arr) {
  var len = arr.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    add(h, arr[i]);
  }
  return /* () */0;
}

var N = 0;

var C = 0;

var A = 0;

var clear = Bs_internalBucketsType.clear;

var forEach = Bs_internalSetBuckets.forEach;

var reduce = Bs_internalSetBuckets.reduce;

var logStats = Bs_internalSetBuckets.logStats;

var toArray = Bs_internalSetBuckets.toArray;

var copy = Bs_internalSetBuckets.copy;

var getBucketHistogram = Bs_internalSetBuckets.getBucketHistogram;

var isEmpty = Bs_internalBucketsType.isEmpty;

exports.hash = hash;
exports.N = N;
exports.C = C;
exports.A = A;
exports.copyBucket = copyBucket;
exports.tryDoubleResize = tryDoubleResize;
exports.removeBucket = removeBucket;
exports.remove = remove;
exports.addBucket = addBucket;
exports.add = add;
exports.memInBucket = memInBucket;
exports.has = has;
exports.make = make;
exports.clear = clear;
exports.size = size;
exports.forEach = forEach;
exports.reduce = reduce;
exports.logStats = logStats;
exports.toArray = toArray;
exports.copy = copy;
exports.getBucketHistogram = getBucketHistogram;
exports.isEmpty = isEmpty;
exports.ofArray = ofArray;
exports.mergeMany = mergeMany;
/* No side effect */
