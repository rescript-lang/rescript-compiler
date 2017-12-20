'use strict';

var Bs_internalSetBuckets  = require("./bs_internalSetBuckets.js");
var Bs_internalBucketsType = require("./bs_internalBucketsType.js");

function insert_bucket(hash, h_buckets, ndata_tail, _, _old_bucket) {
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

function remove0(hash, eq, h, key) {
  var h_buckets = h.buckets;
  var i = hash(key) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (l !== undefined) {
    var next_cell = l.next;
    if (eq(l.key, key)) {
      h.size = h.size - 1 | 0;
      h_buckets[i] = next_cell;
      return /* () */0;
    } else if (next_cell !== undefined) {
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

function addBucket(eq, h, key, _cell) {
  while(true) {
    var cell = _cell;
    if (eq(cell.key, key)) {
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

function add0(hash, eq, h, key) {
  var h_buckets = h.buckets;
  var i = hash(key) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (l !== undefined) {
    addBucket(eq, h, key, l);
  } else {
    h.size = h.size + 1 | 0;
    h_buckets[i] = {
      key: key,
      next: Bs_internalBucketsType.emptyOpt
    };
  }
  if (h.size > (h.buckets.length << 1)) {
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
        insert_bucket(hash$1, h_buckets$1, ndata_tail, h$1, odata[i$1]);
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

function mem0(hash, eq, h, key) {
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

function create(dict, initialize_size) {
  return {
          dict: dict,
          data: Bs_internalBucketsType.create0(initialize_size)
        };
}

function clear(h) {
  return Bs_internalBucketsType.clear0(h.data);
}

function reset(h) {
  return Bs_internalBucketsType.reset0(h.data);
}

function length(h) {
  return h.data.size;
}

function iter(f, h) {
  return Bs_internalSetBuckets.iter0(f, h.data);
}

function fold(f, h, init) {
  return Bs_internalSetBuckets.fold0(f, h.data, init);
}

function logStats(h) {
  return Bs_internalSetBuckets.logStats0(h.data);
}

function add(h, key) {
  var dict = h.dict;
  var data = h.data;
  return add0(dict[/* hash */0], dict[/* eq */1], data, key);
}

function remove(h, key) {
  var dict = h.dict;
  var data = h.data;
  return remove0(dict[/* hash */0], dict[/* eq */1], data, key);
}

function mem(h, key) {
  var dict = h.dict;
  var data = h.data;
  return mem0(dict[/* hash */0], dict[/* eq */1], data, key);
}

var create0 = Bs_internalBucketsType.create0;

var clear0 = Bs_internalBucketsType.clear0;

var reset0 = Bs_internalBucketsType.reset0;

var iter0 = Bs_internalSetBuckets.iter0;

var fold0 = Bs_internalSetBuckets.fold0;

var length0 = Bs_internalBucketsType.length0;

var logStats0 = Bs_internalSetBuckets.logStats0;

exports.create0   = create0;
exports.create    = create;
exports.clear0    = clear0;
exports.clear     = clear;
exports.reset0    = reset0;
exports.reset     = reset;
exports.add0      = add0;
exports.add       = add;
exports.mem0      = mem0;
exports.mem       = mem;
exports.remove0   = remove0;
exports.remove    = remove;
exports.iter0     = iter0;
exports.iter      = iter;
exports.fold0     = fold0;
exports.fold      = fold;
exports.length0   = length0;
exports.length    = length;
exports.logStats0 = logStats0;
exports.logStats  = logStats;
/* Bs_internalBucketsType Not a pure module */
