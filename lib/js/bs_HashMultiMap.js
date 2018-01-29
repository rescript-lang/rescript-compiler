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

function make(initialize_size, dict) {
  return Bs_internalBucketsType.make(dict[/* hash */0], dict[/* eq */1], initialize_size);
}

function size(prim) {
  return prim.size;
}

function add(h, key, info) {
  var hash = h.hash;
  var h$1 = h;
  var key$1 = key;
  var value = info;
  var h_buckets = h$1.buckets;
  var h_buckets_lenth = h_buckets.length;
  var i = hash(key$1) & (h_buckets_lenth - 1 | 0);
  var bucket = {
    key: key$1,
    value: value,
    next: h_buckets[i]
  };
  h_buckets[i] = bucket;
  var h_new_size = h$1.size + 1 | 0;
  h$1.size = h_new_size;
  if (h_new_size > (h_buckets_lenth << 1)) {
    return resize(hash, h$1);
  } else {
    return 0;
  }
}

function remove(h, key) {
  var hash = h.hash;
  var eq = h.eq;
  var h$1 = h;
  var key$1 = key;
  var h_buckets = h$1.buckets;
  var i = hash(key$1) & (h_buckets.length - 1 | 0);
  var eq$1 = eq;
  var h$2 = h$1;
  var h_buckets$1 = h_buckets;
  var i$1 = i;
  var key$2 = key$1;
  var _prec = Bs_internalBucketsType.emptyOpt;
  var _buckets = h_buckets[i];
  while(true) {
    var buckets = _buckets;
    var prec = _prec;
    if (buckets !== undefined) {
      var cell_next = buckets.next;
      if (eq$1(buckets.key, key$2)) {
        if (prec !== undefined) {
          prec.next = cell_next;
        } else {
          h_buckets$1[i$1] = cell_next;
        }
        h$2.size = h$2.size - 1 | 0;
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

function removeAll(h, key) {
  var hash = h.hash;
  var eq = h.eq;
  var h$1 = h;
  var key$1 = key;
  var h_buckets = h$1.buckets;
  var i = hash(key$1) & (h_buckets.length - 1 | 0);
  var eq$1 = eq;
  var h$2 = h$1;
  var h_buckets$1 = h_buckets;
  var i$1 = i;
  var key$2 = key$1;
  var _prec = Bs_internalBucketsType.emptyOpt;
  var _buckets = h_buckets[i];
  while(true) {
    var buckets = _buckets;
    var prec = _prec;
    if (buckets !== undefined) {
      var cell_next = buckets.next;
      if (eq$1(buckets.key, key$2)) {
        if (prec !== undefined) {
          prec.next = cell_next;
        } else {
          h_buckets$1[i$1] = cell_next;
        }
        h$2.size = h$2.size - 1 | 0;
      }
      _buckets = cell_next;
      _prec = buckets;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function get(h, key) {
  var hash = h.hash;
  var eq = h.eq;
  var h$1 = h;
  var key$1 = key;
  var h_buckets = h$1.buckets;
  var nid = hash(key$1) & (h_buckets.length - 1 | 0);
  var match = h_buckets[nid];
  if (match !== undefined) {
    if (eq(key$1, match.key)) {
      return /* Some */[match.value];
    } else {
      var match$1 = match.next;
      if (match$1 !== undefined) {
        if (eq(key$1, match$1.key)) {
          return /* Some */[match$1.value];
        } else {
          var match$2 = match$1.next;
          if (match$2 !== undefined) {
            if (eq(key$1, match$2.key)) {
              return /* Some */[match$2.value];
            } else {
              var eq$1 = eq;
              var key$2 = key$1;
              var _buckets = match$2.next;
              while(true) {
                var buckets = _buckets;
                if (buckets !== undefined) {
                  if (eq$1(key$2, buckets.key)) {
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

function getAll(h, key) {
  var hash = h.hash;
  var eq = h.eq;
  var h$1 = h;
  var key$1 = key;
  var find_in_bucket = function (_buckets) {
    while(true) {
      var buckets = _buckets;
      if (buckets !== undefined) {
        if (eq(buckets.key, key$1)) {
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
  var h_buckets = h$1.buckets;
  var nid = hash(key$1) & (h_buckets.length - 1 | 0);
  return find_in_bucket(h_buckets[nid]);
}

function replace(h, key, info) {
  var hash = h.hash;
  var eq = h.eq;
  var h$1 = h;
  var key$1 = key;
  var info$1 = info;
  var h_buckets = h$1.buckets;
  var i = hash(key$1) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (replace_bucket(eq, key$1, info$1, l)) {
    h_buckets[i] = {
      key: key$1,
      value: info$1,
      next: l
    };
    h$1.size = h$1.size + 1 | 0;
    if (h$1.size > (h$1.buckets.length << 1)) {
      return resize(hash, h$1);
    } else {
      return 0;
    }
  } else {
    return 0;
  }
}

function has(h, key) {
  var hash = h.hash;
  var eq = h.eq;
  var h$1 = h;
  var key$1 = key;
  var h_buckets = h$1.buckets;
  var nid = hash(key$1) & (h_buckets.length - 1 | 0);
  var bucket = h_buckets[nid];
  if (bucket !== undefined) {
    var eq$1 = eq;
    var key$2 = key$1;
    var _cell = bucket;
    while(true) {
      var cell = _cell;
      if (eq$1(cell.key, key$2)) {
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

var clear = Bs_internalBucketsType.clear;

var forEach = Bs_internalBuckets.forEach;

var reduce = Bs_internalBuckets.reduce;

var keepMapInPlace = Bs_internalBuckets.keepMapInPlace;

var logStats = Bs_internalBuckets.logStats;

exports.make = make;
exports.clear = clear;
exports.add = add;
exports.get = get;
exports.getAll = getAll;
exports.has = has;
exports.remove = remove;
exports.removeAll = removeAll;
exports.replace = replace;
exports.forEach = forEach;
exports.reduce = reduce;
exports.keepMapInPlace = keepMapInPlace;
exports.size = size;
exports.logStats = logStats;
/* No side effect */
