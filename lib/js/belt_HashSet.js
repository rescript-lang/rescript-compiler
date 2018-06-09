'use strict';

var Belt_internalSetBuckets = require("./belt_internalSetBuckets.js");
var Belt_internalBucketsType = require("./belt_internalBucketsType.js");

function copyBucket(hash, h_buckets, ndata_tail, _old_bucket) {
  while(true) {
    var old_bucket = _old_bucket;
    var match = old_bucket === undefined ? /* None */0 : [old_bucket];
    if (match !== /* None */0) {
      var nidx = hash(old_bucket.key) & (h_buckets.length - 1 | 0);
      var match$1 = ndata_tail[nidx];
      if ((
          match$1 === undefined ? /* None */0 : [match$1]
        ) !== /* None */0) {
        match$1.next = old_bucket;
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

function remove(h, key) {
  var eq = h.eq;
  var h_buckets = h.buckets;
  var i = h.hash(key) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  var match = l === undefined ? /* None */0 : [l];
  if (match !== /* None */0) {
    var next_cell = l.next;
    if (eq(l.key, key)) {
      h.size = h.size - 1 | 0;
      h_buckets[i] = next_cell;
      return /* () */0;
    } else {
      var match$1 = next_cell === undefined ? /* None */0 : [next_cell];
      if (match$1 !== /* None */0) {
        var eq$1 = eq;
        var h$1 = h;
        var key$1 = key;
        var _prec = l;
        var _cell = next_cell;
        while(true) {
          var cell = _cell;
          var prec = _prec;
          var cell_next = cell.next;
          if (eq$1(cell.key, key$1)) {
            prec.next = cell_next;
            h$1.size = h$1.size - 1 | 0;
            return /* () */0;
          } else {
            var match$2 = cell_next === undefined ? /* None */0 : [cell_next];
            if (match$2 !== /* None */0) {
              _cell = cell_next;
              _prec = cell;
              continue ;
            } else {
              return /* () */0;
            }
          }
        };
      } else {
        return /* () */0;
      }
    }
  } else {
    return /* () */0;
  }
}

function addBucket(h, key, _cell, eq) {
  while(true) {
    var cell = _cell;
    if (eq(cell.key, key)) {
      return 0;
    } else {
      var n = cell.next;
      var match = n === undefined ? /* None */0 : [n];
      if (match !== /* None */0) {
        _cell = n;
        continue ;
      } else {
        h.size = h.size + 1 | 0;
        cell.next = {
          key: key,
          next: Belt_internalBucketsType.emptyOpt
        };
        return /* () */0;
      }
    }
  };
}

function add0(h, key, hash, eq) {
  var h_buckets = h.buckets;
  var buckets_len = h_buckets.length;
  var i = hash(key) & (buckets_len - 1 | 0);
  var l = h_buckets[i];
  var match = l === undefined ? /* None */0 : [l];
  if (match !== /* None */0) {
    addBucket(h, key, l, eq);
  } else {
    h.size = h.size + 1 | 0;
    h_buckets[i] = {
      key: key,
      next: Belt_internalBucketsType.emptyOpt
    };
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
        copyBucket(hash$1, h_buckets$1, ndata_tail, odata[i$1]);
      }
      for(var i$2 = 0 ,i_finish$1 = nsize - 1 | 0; i$2 <= i_finish$1; ++i$2){
        var match$1 = ndata_tail[i$2];
        if ((
            match$1 === undefined ? /* None */0 : [match$1]
          ) !== /* None */0) {
          match$1.next = Belt_internalBucketsType.emptyOpt;
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

function add(h, key) {
  return add0(h, key, h.hash, h.eq);
}

function has(h, key) {
  var eq = h.eq;
  var h_buckets = h.buckets;
  var nid = h.hash(key) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[nid];
  var match = bucket === undefined ? /* None */0 : [bucket];
  if (match !== /* None */0) {
    var eq$1 = eq;
    var key$1 = key;
    var _cell = bucket;
    while(true) {
      var cell = _cell;
      if (eq$1(cell.key, key$1)) {
        return true;
      } else {
        var match$1 = cell.next;
        if ((
            match$1 === undefined ? /* None */0 : [match$1]
          ) !== /* None */0) {
          _cell = match$1;
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

function size(prim) {
  return prim.size;
}

function fromArray(arr, id) {
  var eq = id[/* eq */1];
  var hash = id[/* hash */0];
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
  return /* () */0;
}

var Int = 0;

var $$String = 0;

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
