'use strict';

var Bs_internalBuckets     = require("./bs_internalBuckets.js");
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

function add0(hash, h, key, value) {
  var h_buckets = h.buckets;
  var h_buckets_lenth = h_buckets.length;
  var i = hash(key) & (h_buckets_lenth - 1 | 0);
  var bucket = {
    key: key,
    value: value,
    next: h_buckets[i]
  };
  h_buckets[i] = bucket;
  var h_new_size = h.size + 1 | 0;
  h.size = h_new_size;
  if (h_new_size > (h_buckets_lenth << 1)) {
    return resize(hash, h);
  } else {
    return 0;
  }
}

function remove0(hash, eq, h, key) {
  var h_buckets = h.buckets;
  var i = hash(key) & (h_buckets.length - 1 | 0);
  var eq$1 = eq;
  var h$1 = h;
  var h_buckets$1 = h_buckets;
  var i$1 = i;
  var key$1 = key;
  var _prec = Bs_internalBucketsType.emptyOpt;
  var _buckets = h_buckets[i];
  while(true) {
    var buckets = _buckets;
    var prec = _prec;
    if (buckets !== undefined) {
      var cell_next = buckets.next;
      if (eq$1(buckets.key, key$1)) {
        if (prec !== undefined) {
          prec.next = cell_next;
        } else {
          h_buckets$1[i$1] = cell_next;
        }
        h$1.size = h$1.size - 1 | 0;
        return /* () */0;
      } else {
        _buckets = cell_next;
        _prec = buckets;
        continue ;
        
      }
    } else {
      return /* () */0;
    }
  };
}

function removeAll0(hash, eq, h, key) {
  var h_buckets = h.buckets;
  var i = hash(key) & (h_buckets.length - 1 | 0);
  var eq$1 = eq;
  var h$1 = h;
  var h_buckets$1 = h_buckets;
  var i$1 = i;
  var key$1 = key;
  var _prec = Bs_internalBucketsType.emptyOpt;
  var _buckets = h_buckets[i];
  while(true) {
    var buckets = _buckets;
    var prec = _prec;
    if (buckets !== undefined) {
      var cell_next = buckets.next;
      if (eq$1(buckets.key, key$1)) {
        if (prec !== undefined) {
          prec.next = cell_next;
        } else {
          h_buckets$1[i$1] = cell_next;
        }
        h$1.size = h$1.size - 1 | 0;
      }
      _buckets = cell_next;
      _prec = buckets;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
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

function findAll0(hash, eq, h, key) {
  var find_in_bucket = function (_buckets) {
    while(true) {
      var buckets = _buckets;
      if (buckets !== undefined) {
        if (eq(buckets.key, key)) {
          return /* :: */[
                  buckets.value,
                  find_in_bucket(buckets.next)
                ];
        } else {
          _buckets = buckets.next;
          continue ;
          
        }
      } else {
        return /* [] */0;
      }
    };
  };
  var h_buckets = h.buckets;
  var nid = hash(key) & (h_buckets.length - 1 | 0);
  return find_in_bucket(h_buckets[nid]);
}

function replace_bucket(eq, key, info, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      if (eq(buckets.key, key)) {
        buckets.key = key;
        buckets.value = info;
        return /* false */0;
      } else {
        _buckets = buckets.next;
        continue ;
        
      }
    } else {
      return /* true */1;
    }
  };
}

function replace0(hash, eq, h, key, info) {
  var h_buckets = h.buckets;
  var i = hash(key) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (replace_bucket(eq, key, info, l)) {
    h_buckets[i] = {
      key: key,
      value: info,
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
  return add0(dict[/* hash */0], data, key, info);
}

function remove(h, key) {
  var dict = h.dict;
  var data = h.data;
  return remove0(dict[/* hash */0], dict[/* eq */1], data, key);
}

function removeAll(h, key) {
  var dict = h.dict;
  var data = h.data;
  return removeAll0(dict[/* hash */0], dict[/* eq */1], data, key);
}

function findOpt(h, key) {
  var dict = h.dict;
  var data = h.data;
  return findOpt0(dict[/* hash */0], dict[/* eq */1], data, key);
}

function findAll(h, key) {
  var dict = h.dict;
  var data = h.data;
  return findAll0(dict[/* hash */0], dict[/* eq */1], data, key);
}

function replace(h, key, info) {
  var dict = h.dict;
  var data = h.data;
  return replace0(dict[/* hash */0], dict[/* eq */1], data, key, info);
}

function mem(h, key) {
  var dict = h.dict;
  var data = h.data;
  return mem0(dict[/* hash */0], dict[/* eq */1], data, key);
}

function filterMapInplace(f, h) {
  return Bs_internalBuckets.filterMapInplace0(f, h.data);
}

var create0 = Bs_internalBucketsType.create0;

var clear0 = Bs_internalBucketsType.clear0;

var reset0 = Bs_internalBucketsType.reset0;

var iter0 = Bs_internalBuckets.iter0;

var fold0 = Bs_internalBuckets.fold0;

var filterMapInplace0 = Bs_internalBuckets.filterMapInplace0;

var length0 = Bs_internalBucketsType.length0;

var logStats0 = Bs_internalBuckets.logStats0;

exports.create0           = create0;
exports.create            = create;
exports.clear0            = clear0;
exports.clear             = clear;
exports.reset0            = reset0;
exports.reset             = reset;
exports.add0              = add0;
exports.add               = add;
exports.findOpt0          = findOpt0;
exports.findOpt           = findOpt;
exports.findAll0          = findAll0;
exports.findAll           = findAll;
exports.mem0              = mem0;
exports.mem               = mem;
exports.remove0           = remove0;
exports.remove            = remove;
exports.removeAll0        = removeAll0;
exports.removeAll         = removeAll;
exports.replace0          = replace0;
exports.replace           = replace;
exports.iter0             = iter0;
exports.iter              = iter;
exports.fold0             = fold0;
exports.fold              = fold;
exports.filterMapInplace0 = filterMapInplace0;
exports.filterMapInplace  = filterMapInplace;
exports.length0           = length0;
exports.length            = length;
exports.logStats0         = logStats0;
exports.logStats          = logStats;
/* No side effect */
