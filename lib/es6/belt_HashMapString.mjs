

import * as Caml_option from "./caml_option.mjs";
import * as Caml_hash_primitive from "./caml_hash_primitive.mjs";
import * as Belt_internalBuckets from "./belt_internalBuckets.mjs";
import * as Belt_internalBucketsType from "./belt_internalBucketsType.mjs";

function copyBucketReHash(h_buckets, ndata_tail, _old_bucket) {
  while(true) {
    var old_bucket = _old_bucket;
    if (old_bucket === undefined) {
      return ;
    }
    var nidx = Caml_hash_primitive.caml_hash_final_mix(Caml_hash_primitive.caml_hash_mix_string(0, old_bucket.key)) & (h_buckets.length - 1 | 0);
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

function replaceInBucket(key, info, _cell) {
  while(true) {
    var cell = _cell;
    if (cell.key === key) {
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

function set(h, key, value) {
  var h_buckets = h.buckets;
  var buckets_len = h_buckets.length;
  var i = Caml_hash_primitive.caml_hash_final_mix(Caml_hash_primitive.caml_hash_mix_string(0, key)) & (buckets_len - 1 | 0);
  var l = h_buckets[i];
  if (l !== undefined) {
    if (replaceInBucket(key, value, l)) {
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
      copyBucketReHash(h_buckets$1, ndata_tail, odata[i$1]);
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

function remove(h, key) {
  var h_buckets = h.buckets;
  var i = Caml_hash_primitive.caml_hash_final_mix(Caml_hash_primitive.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[i];
  if (bucket !== undefined) {
    if (bucket.key === key) {
      h_buckets[i] = bucket.next;
      h.size = h.size - 1 | 0;
      return ;
    } else {
      var _prec = bucket;
      var _buckets = bucket.next;
      while(true) {
        var buckets = _buckets;
        var prec = _prec;
        if (buckets === undefined) {
          return ;
        }
        var cell_next = buckets.next;
        if (buckets.key === key) {
          prec.next = cell_next;
          h.size = h.size - 1 | 0;
          return ;
        }
        _buckets = cell_next;
        _prec = buckets;
        continue ;
      };
    }
  }
  
}

function get(h, key) {
  var h_buckets = h.buckets;
  var nid = Caml_hash_primitive.caml_hash_final_mix(Caml_hash_primitive.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  var cell1 = h_buckets[nid];
  if (cell1 === undefined) {
    return ;
  }
  if (key === cell1.key) {
    return Caml_option.some(cell1.value);
  }
  var cell2 = cell1.next;
  if (cell2 === undefined) {
    return ;
  }
  if (key === cell2.key) {
    return Caml_option.some(cell2.value);
  }
  var cell3 = cell2.next;
  if (cell3 !== undefined) {
    if (key === cell3.key) {
      return Caml_option.some(cell3.value);
    } else {
      var _buckets = cell3.next;
      while(true) {
        var buckets = _buckets;
        if (buckets === undefined) {
          return ;
        }
        if (key === buckets.key) {
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
  var nid = Caml_hash_primitive.caml_hash_final_mix(Caml_hash_primitive.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[nid];
  if (bucket !== undefined) {
    var _cell = bucket;
    while(true) {
      var cell = _cell;
      if (cell.key === key) {
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

function make(hintSize) {
  return Belt_internalBucketsType.make(undefined, undefined, hintSize);
}

function size(h) {
  return h.size;
}

function fromArray(arr) {
  var len = arr.length;
  var v = Belt_internalBucketsType.make(undefined, undefined, len);
  for(var i = 0; i < len; ++i){
    var match = arr[i];
    set(v, match[0], match[1]);
  }
  return v;
}

function mergeMany(h, arr) {
  var len = arr.length;
  for(var i = 0; i < len; ++i){
    var match = arr[i];
    set(h, match[0], match[1]);
  }
  
}

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
