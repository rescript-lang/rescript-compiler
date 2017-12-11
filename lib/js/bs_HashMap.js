'use strict';

var Bs_internalBuckets = require("./bs_internalBuckets.js");

function insert_bucket(hash, h_buckets, ndata_tail, _, _old_bucket) {
  while(true) {
    var old_bucket = _old_bucket;
    if (old_bucket !== undefined) {
      var key = old_bucket[/* key */0];
      var next = old_bucket[/* next */2];
      var nidx = hash(key) & (h_buckets.length - 1 | 0);
      var match = ndata_tail[nidx];
      if (match !== undefined) {
        match[/* next */2] = old_bucket;
      } else {
        h_buckets[nidx] = old_bucket;
      }
      ndata_tail[nidx] = old_bucket;
      _old_bucket = next;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function resize(hash, h) {
  var odata = h[/* buckets */1];
  var osize = odata.length;
  var nsize = (osize << 1);
  if (nsize >= osize) {
    var h_buckets = new Array(nsize);
    var ndata_tail = new Array(nsize);
    h[/* buckets */1] = h_buckets;
    for(var i = 0 ,i_finish = osize - 1 | 0; i <= i_finish; ++i){
      insert_bucket(hash, h_buckets, ndata_tail, h, odata[i]);
    }
    for(var i$1 = 0 ,i_finish$1 = nsize - 1 | 0; i$1 <= i_finish$1; ++i$1){
      var match = ndata_tail[i$1];
      if (match !== undefined) {
        match[/* next */2] = Bs_internalBuckets.emptyOpt;
      }
      
    }
    return /* () */0;
  } else {
    return 0;
  }
}

function add0(hash, h, key, value) {
  var h_buckets = h[/* buckets */1];
  var h_buckets_lenth = h_buckets.length;
  var i = hash(key) & (h_buckets_lenth - 1 | 0);
  var bucket = /* record */[
    /* key */key,
    /* value */value,
    /* next */h_buckets[i]
  ];
  h_buckets[i] = bucket;
  var h_new_size = h[/* size */0] + 1 | 0;
  h[/* size */0] = h_new_size;
  if (h_new_size > (h_buckets_lenth << 1)) {
    return resize(hash, h);
  } else {
    return 0;
  }
}

function remove0(hash, eq, h, key) {
  var h_buckets = h[/* buckets */1];
  var i = hash(key) & (h_buckets.length - 1 | 0);
  var eq$1 = eq;
  var h$1 = h;
  var h_buckets$1 = h_buckets;
  var i$1 = i;
  var key$1 = key;
  var _prec = Bs_internalBuckets.emptyOpt;
  var _buckets = h_buckets[i];
  while(true) {
    var buckets = _buckets;
    var prec = _prec;
    if (buckets !== undefined) {
      var k = buckets[/* key */0];
      var next = buckets[/* next */2];
      if (eq$1(k, key$1)) {
        if (prec !== undefined) {
          prec[/* next */2] = next;
        } else {
          h_buckets$1[i$1] = next;
        }
        h$1[/* size */0] = h$1[/* size */0] - 1 | 0;
        return /* () */0;
      } else {
        _buckets = next;
        _prec = buckets;
        continue ;
        
      }
    } else {
      return /* () */0;
    }
  };
}

function removeAll0(hash, eq, h, key) {
  var h_buckets = h[/* buckets */1];
  var i = hash(key) & (h_buckets.length - 1 | 0);
  var eq$1 = eq;
  var h$1 = h;
  var h_buckets$1 = h_buckets;
  var i$1 = i;
  var key$1 = key;
  var _prec = Bs_internalBuckets.emptyOpt;
  var _buckets = h_buckets[i];
  while(true) {
    var buckets = _buckets;
    var prec = _prec;
    if (buckets !== undefined) {
      var k = buckets[/* key */0];
      var next = buckets[/* next */2];
      if (eq$1(k, key$1)) {
        if (prec !== undefined) {
          prec[/* next */2] = next;
        } else {
          h_buckets$1[i$1] = next;
        }
        h$1[/* size */0] = h$1[/* size */0] - 1 | 0;
      }
      _buckets = next;
      _prec = buckets;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function findOpt0(hash, eq, h, key) {
  var h_buckets = h[/* buckets */1];
  var nid = hash(key) & (h_buckets.length - 1 | 0);
  var match = h_buckets[nid];
  if (match !== undefined) {
    var k1 = match[/* key */0];
    var d1 = match[/* value */1];
    var rest1 = match[/* next */2];
    if (eq(key, k1)) {
      return /* Some */[d1];
    } else if (rest1 !== undefined) {
      var k2 = rest1[/* key */0];
      var d2 = rest1[/* value */1];
      var rest2 = rest1[/* next */2];
      if (eq(key, k2)) {
        return /* Some */[d2];
      } else if (rest2 !== undefined) {
        var k3 = rest2[/* key */0];
        var d3 = rest2[/* value */1];
        var rest3 = rest2[/* next */2];
        if (eq(key, k3)) {
          return /* Some */[d3];
        } else {
          var eq$1 = eq;
          var key$1 = key;
          var _buckets = rest3;
          while(true) {
            var buckets = _buckets;
            if (buckets !== undefined) {
              var k = buckets[/* key */0];
              var d = buckets[/* value */1];
              var rest = buckets[/* next */2];
              if (eq$1(key$1, k)) {
                return /* Some */[d];
              } else {
                _buckets = rest;
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
    } else {
      return /* None */0;
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
        var k = buckets[/* key */0];
        var d = buckets[/* value */1];
        var rest = buckets[/* next */2];
        if (eq(k, key)) {
          return /* :: */[
                  d,
                  find_in_bucket(rest)
                ];
        } else {
          _buckets = rest;
          continue ;
          
        }
      } else {
        return /* [] */0;
      }
    };
  };
  var h_buckets = h[/* buckets */1];
  var nid = hash(key) & (h_buckets.length - 1 | 0);
  return find_in_bucket(h_buckets[nid]);
}

function replace_bucket(eq, key, info, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      var k = buckets[/* key */0];
      var next = buckets[/* next */2];
      if (eq(k, key)) {
        buckets[/* key */0] = key;
        buckets[/* value */1] = info;
        return /* false */0;
      } else {
        _buckets = next;
        continue ;
        
      }
    } else {
      return /* true */1;
    }
  };
}

function replace0(hash, eq, h, key, info) {
  var h_buckets = h[/* buckets */1];
  var i = hash(key) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (replace_bucket(eq, key, info, l)) {
    h_buckets[i] = /* record */[
      /* key */key,
      /* value */info,
      /* next */l
    ];
    h[/* size */0] = h[/* size */0] + 1 | 0;
    if (h[/* size */0] > (h[/* buckets */1].length << 1)) {
      return resize(hash, h);
    } else {
      return 0;
    }
  } else {
    return 0;
  }
}

function mem0(hash, eq, h, key) {
  var h_buckets = h[/* buckets */1];
  var nid = hash(key) & (h_buckets.length - 1 | 0);
  var eq$1 = eq;
  var key$1 = key;
  var _buckets = h_buckets[nid];
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      var k = buckets[/* key */0];
      var rest = buckets[/* next */2];
      if (eq$1(k, key$1)) {
        return /* true */1;
      } else {
        _buckets = rest;
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

function create(dict, initialize_size) {
  return /* record */[
          /* dict */dict,
          /* data */Bs_internalBuckets.create0(initialize_size)
        ];
}

function clear(h) {
  return Bs_internalBuckets.clear0(h[/* data */1]);
}

function reset(h) {
  return Bs_internalBuckets.reset0(h[/* data */1]);
}

function length(h) {
  return h[/* data */1][/* size */0];
}

function iter(f, h) {
  return Bs_internalBuckets.iter0(f, h[/* data */1]);
}

function fold(f, h, init) {
  return Bs_internalBuckets.fold0(f, h[/* data */1], init);
}

function logStats(h) {
  return Bs_internalBuckets.logStats0(h[/* data */1]);
}

function add(h, key, info) {
  var M = h[/* dict */0];
  return add0(M[/* hash */0], h[/* data */1], key, info);
}

function remove(h, key) {
  var M = h[/* dict */0];
  return remove0(M[/* hash */0], M[/* eq */1], h[/* data */1], key);
}

function removeAll(h, key) {
  var M = h[/* dict */0];
  return removeAll0(M[/* hash */0], M[/* eq */1], h[/* data */1], key);
}

function findOpt(h, key) {
  var M = h[/* dict */0];
  return findOpt0(M[/* hash */0], M[/* eq */1], h[/* data */1], key);
}

function findAll(h, key) {
  var M = h[/* dict */0];
  return findAll0(M[/* hash */0], M[/* eq */1], h[/* data */1], key);
}

function replace(h, key, info) {
  var M = h[/* dict */0];
  return replace0(M[/* hash */0], M[/* eq */1], h[/* data */1], key, info);
}

function mem(h, key) {
  var M = h[/* dict */0];
  return mem0(M[/* hash */0], M[/* eq */1], h[/* data */1], key);
}

function filterMapInplace(f, h) {
  return Bs_internalBuckets.filterMapInplace0(f, h[/* data */1]);
}

var create0 = Bs_internalBuckets.create0;

var clear0 = Bs_internalBuckets.clear0;

var reset0 = Bs_internalBuckets.reset0;

var iter0 = Bs_internalBuckets.iter0;

var fold0 = Bs_internalBuckets.fold0;

var filterMapInplace0 = Bs_internalBuckets.filterMapInplace0;

var length0 = Bs_internalBuckets.length0;

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
/* Bs_internalBuckets Not a pure module */
