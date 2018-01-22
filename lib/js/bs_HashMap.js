'use strict';

var Bs_internalBuckets = require("./bs_internalBuckets.js");
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

function resize(hash, h) {
  var odata = h.buckets;
  var osize = odata.length;
  var nsize = (osize << 1);
  if (nsize >= osize) {
    var h_buckets = new Array(nsize);
    var ndata_tail = new Array(nsize);
    h.buckets = h_buckets;
    for(var i = 0 ,i_finish = osize - 1 | 0; i <= i_finish; ++i){
      insert_bucket(hash, h_buckets, ndata_tail, h, odata[i]);
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

function replace_in_bucket(eq, key, info, _cell) {
  while(true) {
    var cell = _cell;
    if (eq(cell.key, key)) {
      cell.key = key;
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

function add0(hash, eq, h, key, value) {
  var h_buckets = h.buckets;
  var i = hash(key) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (l !== undefined) {
    if (replace_in_bucket(eq, key, value, l)) {
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
      var eq$1 = eq;
      var h$1 = h;
      var key$1 = key;
      var _prec = bucket;
      var _bucket = bucket.next;
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

function findOpt0(hash, eq, h, key) {
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

function iter(h, f) {
  return Bs_internalBuckets.iter0(h.data, f);
}

function fold(h, init, f) {
  return Bs_internalBuckets.fold0(h.data, init, f);
}

function logStats(h) {
  return Bs_internalBuckets.logStats0(h.data);
}

function add(h, key, info) {
  var dict = h.dict;
  var data = h.data;
  return add0(dict[/* hash */0], dict[/* eq */1], data, key, info);
}

function remove(h, key) {
  var dict = h.dict;
  var data = h.data;
  return remove0(dict[/* hash */0], dict[/* eq */1], data, key);
}

function findOpt(h, key) {
  var dict = h.dict;
  var data = h.data;
  return findOpt0(dict[/* hash */0], dict[/* eq */1], data, key);
}

function mem(h, key) {
  var dict = h.dict;
  var data = h.data;
  return mem0(dict[/* hash */0], dict[/* eq */1], data, key);
}

function filterMapInplace(h, f) {
  return Bs_internalBuckets.filterMapInplace0(h.data, f);
}

function toArray(h) {
  return Bs_internalBuckets.toArray0(h.data);
}

function ofArray0(hash, eq, arr) {
  var len = arr.length;
  var v = Bs_internalBucketsType.create0(len);
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr[i];
    add0(hash, eq, v, match[0], match[1]);
  }
  return v;
}

function addArray0(hash, eq, h, arr) {
  var len = arr.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    var match = arr[i];
    add0(hash, eq, h, match[0], match[1]);
  }
  return /* () */0;
}

function ofArray(dict, arr) {
  return {
          dict: dict,
          data: ofArray0(dict[/* hash */0], dict[/* eq */1], arr)
        };
}

function addArray(h, arr) {
  var dict = h.dict;
  var data = h.data;
  return addArray0(dict[/* hash */0], dict[/* eq */1], data, arr);
}

function keys(h) {
  return Bs_internalBuckets.keys0(h.data);
}

function values(h) {
  return Bs_internalBuckets.values0(h.data);
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

var reset0 = Bs_internalBucketsType.reset0;

var iter0 = Bs_internalBuckets.iter0;

var fold0 = Bs_internalBuckets.fold0;

var filterMapInplace0 = Bs_internalBuckets.filterMapInplace0;

var length0 = Bs_internalBucketsType.length0;

var logStats0 = Bs_internalBuckets.logStats0;

var toArray0 = Bs_internalBuckets.toArray0;

var keys0 = Bs_internalBuckets.keys0;

var values0 = Bs_internalBuckets.values0;

exports.create0 = create0;
exports.create = create;
exports.clear0 = clear0;
exports.clear = clear;
exports.reset0 = reset0;
exports.reset = reset;
exports.add0 = add0;
exports.add = add;
exports.findOpt0 = findOpt0;
exports.findOpt = findOpt;
exports.mem0 = mem0;
exports.mem = mem;
exports.remove0 = remove0;
exports.remove = remove;
exports.iter0 = iter0;
exports.iter = iter;
exports.fold0 = fold0;
exports.fold = fold;
exports.filterMapInplace0 = filterMapInplace0;
exports.filterMapInplace = filterMapInplace;
exports.length0 = length0;
exports.length = length;
exports.logStats0 = logStats0;
exports.logStats = logStats;
exports.toArray0 = toArray0;
exports.toArray = toArray;
exports.ofArray0 = ofArray0;
exports.ofArray = ofArray;
exports.addArray0 = addArray0;
exports.addArray = addArray;
exports.keys0 = keys0;
exports.keys = keys;
exports.values0 = values0;
exports.values = values;
exports.getData = getData;
exports.getDict = getDict;
exports.packDictData = packDictData;
/* No side effect */
