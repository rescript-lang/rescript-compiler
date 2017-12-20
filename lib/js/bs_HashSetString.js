'use strict';

var Caml_hash              = require("./caml_hash.js");
var Bs_internalSetBuckets  = require("./bs_internalSetBuckets.js");
var Bs_internalBucketsType = require("./bs_internalBucketsType.js");

function insert_bucket(h_buckets, ndata_tail, _, _old_bucket) {
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

function addBucket(h, key, _cell) {
  while(true) {
    var cell = _cell;
    if (cell.key === key) {
      cell.key = key;
      return /* () */0;
    } else {
      var n = cell.next;
      if (n !== undefined) {
        _cell = n;
        continue ;
        
      } else {
        h.size = h.size + 1 | 0;
        cell.next = {
          key: key,
          next: n
        };
        return /* () */0;
      }
    }
  };
}

function add(h, key) {
  var h_buckets = h.buckets;
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (l !== undefined) {
    addBucket(h, key, l);
  } else {
    h.size = h.size + 1 | 0;
    h_buckets[i] = {
      key: key,
      next: Bs_internalBucketsType.emptyOpt
    };
  }
  if (h.size > (h.buckets.length << 1)) {
    var h$1 = h;
    var odata = h$1.buckets;
    var osize = odata.length;
    var nsize = (osize << 1);
    if (nsize >= osize) {
      var h_buckets$1 = new Array(nsize);
      var ndata_tail = new Array(nsize);
      h$1.buckets = h_buckets$1;
      for(var i$1 = 0 ,i_finish = osize - 1 | 0; i$1 <= i_finish; ++i$1){
        insert_bucket(h_buckets$1, ndata_tail, h$1, odata[i$1]);
      }
      for(var i$2 = 0 ,i_finish$1 = nsize - 1 | 0; i$2 <= i_finish$1; ++i$2){
        var match = ndata_tail[i$2];
        if (match !== undefined) {
          match.next = Bs_internalBucketsType.emptyOpt;
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

function mem(h, key) {
  var h_buckets = h.buckets;
  var nid = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
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

var create = Bs_internalBucketsType.create0;

var clear = Bs_internalBucketsType.clear0;

var reset = Bs_internalBucketsType.reset0;

var iter = Bs_internalSetBuckets.iter0;

var fold = Bs_internalSetBuckets.fold0;

var length = Bs_internalBucketsType.length0;

var logStats = Bs_internalSetBuckets.logStats0;

exports.create   = create;
exports.clear    = clear;
exports.reset    = reset;
exports.add      = add;
exports.mem      = mem;
exports.remove   = remove;
exports.iter     = iter;
exports.fold     = fold;
exports.length   = length;
exports.logStats = logStats;
/* Bs_internalBucketsType Not a pure module */
