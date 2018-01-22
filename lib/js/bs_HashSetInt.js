'use strict';

var Caml_hash = require("./caml_hash.js");
var Bs_internalSetBuckets = require("./bs_internalSetBuckets.js");
var Bs_internalBucketsType = require("./bs_internalBucketsType.js");

function insert_bucket(h_buckets, ndata_tail, _, _old_bucket) {
  while(true) {
    var old_bucket = _old_bucket;
    if (old_bucket !== undefined) {
      var s = old_bucket.key;
      var nidx = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, s)) & (h_buckets.length - 1 | 0);
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

function resize(h) {
  var odata = h.buckets;
  var osize = odata.length;
  var nsize = (osize << 1);
  if (nsize >= osize) {
    var h_buckets = new Array(nsize);
    var ndata_tail = new Array(nsize);
    h.buckets = h_buckets;
    for(var i = 0 ,i_finish = osize - 1 | 0; i <= i_finish; ++i){
      insert_bucket(h_buckets, ndata_tail, h, odata[i]);
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

function remove(h, key) {
  var h_buckets = h.buckets;
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, key)) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (l !== undefined) {
    var next_cell = l.next;
    if (l.key === key) {
      h.size = h.size - 1 | 0;
      h_buckets[i] = next_cell;
      return /* () */0;
    } else if (next_cell !== undefined) {
      var h$1 = h;
      var key$1 = key;
      var _prec = l;
      var _cell = next_cell;
      while(true) {
        var cell = _cell;
        var prec = _prec;
        var cell_next = cell.next;
        if (cell.key === key$1) {
          prec.next = cell_next;
          h$1.size = h$1.size - 1 | 0;
          return /* () */0;
        } else if (cell_next !== undefined) {
          _cell = cell_next;
          _prec = cell;
          continue ;
          
        } else {
          return /* () */0;
        }
      };
    } else {
      return /* () */0;
    }
  } else {
    return /* () */0;
  }
}

function add(h, key) {
  var h_buckets = h.buckets;
  var buckets_len = h_buckets.length;
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, key)) & (buckets_len - 1 | 0);
  var l = h_buckets[i];
  if (l !== undefined) {
    var h$1 = h;
    var buckets_len$1 = buckets_len;
    var key$1 = key;
    var _cell = l;
    while(true) {
      var cell = _cell;
      if (cell.key !== key$1) {
        var n = cell.next;
        if (n !== undefined) {
          _cell = n;
          continue ;
          
        } else {
          h$1.size = h$1.size + 1 | 0;
          cell.next = {
            key: key$1,
            next: n
          };
          if (h$1.size > (buckets_len$1 << 1)) {
            return resize(h$1);
          } else {
            return 0;
          }
        }
      } else {
        return 0;
      }
    };
  } else {
    h_buckets[i] = {
      key: key,
      next: Bs_internalBucketsType.emptyOpt
    };
    h.size = h.size + 1 | 0;
    if (h.size > (buckets_len << 1)) {
      return resize(h);
    } else {
      return 0;
    }
  }
}

function has(h, key) {
  var h_buckets = h.buckets;
  var nid = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, key)) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[nid];
  if (bucket !== undefined) {
    var key$1 = key;
    var _cell = bucket;
    while(true) {
      var cell = _cell;
      if (cell.key === key$1) {
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

function ofArray(arr) {
  var len = arr.length;
  var v = Bs_internalBucketsType.create0(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    add(v, arr[i]);
  }
  return v;
}

function mergeArrayDone(h, arr) {
  var len = arr.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    add(h, arr[i]);
  }
  return /* () */0;
}

function mergeArray(h, arr) {
  mergeArrayDone(h, arr);
  return h;
}

var create = Bs_internalBucketsType.create0;

var clear = Bs_internalBucketsType.clear0;

var forEach = Bs_internalSetBuckets.iter0;

var reduce = Bs_internalSetBuckets.fold0;

var size = Bs_internalBucketsType.length0;

var logStats = Bs_internalSetBuckets.logStats0;

var toArray = Bs_internalSetBuckets.toArray0;

exports.create = create;
exports.clear = clear;
exports.add = add;
exports.has = has;
exports.remove = remove;
exports.forEach = forEach;
exports.reduce = reduce;
exports.size = size;
exports.logStats = logStats;
exports.toArray = toArray;
exports.ofArray = ofArray;
exports.mergeArrayDone = mergeArrayDone;
exports.mergeArray = mergeArray;
/* No side effect */
