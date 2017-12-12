'use strict';

var Caml_hash          = require("./caml_hash.js");
var Bs_internalBuckets = require("./bs_internalBuckets.js");

function hash(s) {
  return Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, s));
}

function insert_bucket(h_buckets, ndata_tail, _, _old_bucket) {
  while(true) {
    var old_bucket = _old_bucket;
    if (old_bucket !== undefined) {
      var key = old_bucket[/* key */0];
      var next = old_bucket[/* next */2];
      var nidx = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, key)) & (h_buckets.length - 1 | 0);
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

function resize(h) {
  var odata = h[/* buckets */1];
  var osize = odata.length;
  var nsize = (osize << 1);
  if (nsize >= osize) {
    var h_buckets = new Array(nsize);
    var ndata_tail = new Array(nsize);
    h[/* buckets */1] = h_buckets;
    for(var i = 0 ,i_finish = osize - 1 | 0; i <= i_finish; ++i){
      insert_bucket(h_buckets, ndata_tail, h, odata[i]);
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

function add(h, key, value) {
  var h_buckets = h[/* buckets */1];
  var h_buckets_lenth = h_buckets.length;
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, key)) & (h_buckets_lenth - 1 | 0);
  var bucket = /* record */[
    /* key */key,
    /* value */value,
    /* next */h_buckets[i]
  ];
  h_buckets[i] = bucket;
  var h_new_size = h[/* size */0] + 1 | 0;
  h[/* size */0] = h_new_size;
  if (h_new_size > (h_buckets_lenth << 1)) {
    return resize(h);
  } else {
    return 0;
  }
}

function remove_bucket(h, h_buckets, i, key, _prec, _buckets) {
  while(true) {
    var buckets = _buckets;
    var prec = _prec;
    if (buckets !== undefined) {
      var k = buckets[/* key */0];
      var next = buckets[/* next */2];
      if (k === key) {
        if (prec !== undefined) {
          prec[/* next */2] = next;
        } else {
          h_buckets[i] = next;
        }
        h[/* size */0] = h[/* size */0] - 1 | 0;
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

function remove(h, key) {
  var h_buckets = h[/* buckets */1];
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, key)) & (h_buckets.length - 1 | 0);
  return remove_bucket(h, h_buckets, i, key, Bs_internalBuckets.emptyOpt, h_buckets[i]);
}

function removeAllBuckets(h, h_buckets, i, key, _prec, _buckets) {
  while(true) {
    var buckets = _buckets;
    var prec = _prec;
    if (buckets !== undefined) {
      var k = buckets[/* key */0];
      var next = buckets[/* next */2];
      if (k === key) {
        if (prec !== undefined) {
          prec[/* next */2] = next;
        } else {
          h_buckets[i] = next;
        }
        h[/* size */0] = h[/* size */0] - 1 | 0;
      }
      _buckets = next;
      _prec = buckets;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function removeAll0(h, key) {
  var h_buckets = h[/* buckets */1];
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, key)) & (h_buckets.length - 1 | 0);
  return removeAllBuckets(h, h_buckets, i, key, Bs_internalBuckets.emptyOpt, h_buckets[i]);
}

function find_rec(key, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      var k = buckets[/* key */0];
      var d = buckets[/* value */1];
      var rest = buckets[/* next */2];
      if (key === k) {
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

function findOpt(h, key) {
  var h_buckets = h[/* buckets */1];
  var nid = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, key)) & (h_buckets.length - 1 | 0);
  var match = h_buckets[nid];
  if (match !== undefined) {
    var k1 = match[/* key */0];
    var d1 = match[/* value */1];
    var rest1 = match[/* next */2];
    if (key === k1) {
      return /* Some */[d1];
    } else if (rest1 !== undefined) {
      var k2 = rest1[/* key */0];
      var d2 = rest1[/* value */1];
      var rest2 = rest1[/* next */2];
      if (key === k2) {
        return /* Some */[d2];
      } else if (rest2 !== undefined) {
        var k3 = rest2[/* key */0];
        var d3 = rest2[/* value */1];
        var rest3 = rest2[/* next */2];
        if (key === k3) {
          return /* Some */[d3];
        } else {
          return find_rec(key, rest3);
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

function findAll(h, key) {
  var find_in_bucket = function (_buckets) {
    while(true) {
      var buckets = _buckets;
      if (buckets !== undefined) {
        var k = buckets[/* key */0];
        var d = buckets[/* value */1];
        var rest = buckets[/* next */2];
        if (k === key) {
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
  var nid = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, key)) & (h_buckets.length - 1 | 0);
  return find_in_bucket(h_buckets[nid]);
}

function replace_bucket(key, info, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      var k = buckets[/* key */0];
      var next = buckets[/* next */2];
      if (k === key) {
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

function replace(h, key, info) {
  var h_buckets = h[/* buckets */1];
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, key)) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (replace_bucket(key, info, l)) {
    h_buckets[i] = /* record */[
      /* key */key,
      /* value */info,
      /* next */l
    ];
    h[/* size */0] = h[/* size */0] + 1 | 0;
    if (h[/* size */0] > (h[/* buckets */1].length << 1)) {
      return resize(h);
    } else {
      return 0;
    }
  } else {
    return 0;
  }
}

function mem_in_bucket(key, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      var k = buckets[/* key */0];
      var rest = buckets[/* next */2];
      if (k === key) {
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

function mem(h, key) {
  var h_buckets = h[/* buckets */1];
  var nid = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_int(0, key)) & (h_buckets.length - 1 | 0);
  return mem_in_bucket(key, h_buckets[nid]);
}

var create = Bs_internalBuckets.create0;

var clear = Bs_internalBuckets.clear0;

var reset = Bs_internalBuckets.reset0;

var length = Bs_internalBuckets.length0;

var iter = Bs_internalBuckets.iter0;

var fold = Bs_internalBuckets.fold0;

var logStats = Bs_internalBuckets.logStats0;

var filterMapInplace = Bs_internalBuckets.filterMapInplace0;

exports.hash             = hash;
exports.insert_bucket    = insert_bucket;
exports.resize           = resize;
exports.add              = add;
exports.remove_bucket    = remove_bucket;
exports.remove           = remove;
exports.removeAllBuckets = removeAllBuckets;
exports.removeAll0       = removeAll0;
exports.find_rec         = find_rec;
exports.findOpt          = findOpt;
exports.findAll          = findAll;
exports.replace_bucket   = replace_bucket;
exports.replace          = replace;
exports.mem_in_bucket    = mem_in_bucket;
exports.mem              = mem;
exports.create           = create;
exports.clear            = clear;
exports.reset            = reset;
exports.length           = length;
exports.iter             = iter;
exports.fold             = fold;
exports.logStats         = logStats;
exports.filterMapInplace = filterMapInplace;
/* Bs_internalBuckets Not a pure module */
