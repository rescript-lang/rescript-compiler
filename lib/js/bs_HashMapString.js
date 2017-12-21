'use strict';

var Caml_hash              = require("./caml_hash.js");
var Bs_internalBuckets     = require("./bs_internalBuckets.js");
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

function add(h, key, value) {
  var h_buckets = h.buckets;
  var h_buckets_lenth = h_buckets.length;
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, key)) & (h_buckets_lenth - 1 | 0);
  var bucket = {
    key: key,
    value: value,
    next: h_buckets[i]
  };
  h_buckets[i] = bucket;
  var h_new_size = h.size + 1 | 0;
  h.size = h_new_size;
  if (h_new_size > (h_buckets_lenth << 1)) {
    return resize(h);
  } else {
    return 0;
  }
}

function remove(h, key) {
  var h_buckets = h.buckets;
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
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
      if (buckets.key === key$1) {
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

function removeAll(h, key) {
  var h_buckets = h.buckets;
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
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
      if (buckets.key === key$1) {
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

function findOpt(h, key) {
  var h_buckets = h.buckets;
  var nid = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  var match = h_buckets[nid];
  if (match !== undefined) {
    if (key === match.key) {
      return /* Some */[match.value];
    } else {
      var match$1 = match.next;
      if (match$1 !== undefined) {
        if (key === match$1.key) {
          return /* Some */[match$1.value];
        } else {
          var match$2 = match$1.next;
          if (match$2 !== undefined) {
            if (key === match$2.key) {
              return /* Some */[match$2.value];
            } else {
              var key$1 = key;
              var _buckets = match$2.next;
              while(true) {
                var buckets = _buckets;
                if (buckets !== undefined) {
                  if (key$1 === buckets.key) {
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

function findAll(h, key) {
  var find_in_bucket = function (_buckets) {
    while(true) {
      var buckets = _buckets;
      if (buckets !== undefined) {
        if (buckets.key === key) {
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
  var nid = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  return find_in_bucket(h_buckets[nid]);
}

function replace_bucket(key, info, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      if (buckets.key === key) {
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

function replace(h, key, info) {
  var h_buckets = h.buckets;
  var i = Caml_hash.caml_hash_final_mix(Caml_hash.caml_hash_mix_string(0, key)) & (h_buckets.length - 1 | 0);
  var l = h_buckets[i];
  if (replace_bucket(key, info, l)) {
    h_buckets[i] = {
      key: key,
      value: info,
      next: l
    };
    h.size = h.size + 1 | 0;
    if (h.size > (h.buckets.length << 1)) {
      return resize(h);
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
  var key$1 = key;
  var _buckets = h_buckets[nid];
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      if (buckets.key === key$1) {
        return /* true */1;
      } else {
        _buckets = buckets.next;
        continue ;
        
      }
    } else {
      return /* false */0;
    }
  };
}

var create = Bs_internalBucketsType.create0;

var clear = Bs_internalBucketsType.clear0;

var reset = Bs_internalBucketsType.reset0;

var iter = Bs_internalBuckets.iter0;

var fold = Bs_internalBuckets.fold0;

var filterMapInplace = Bs_internalBuckets.filterMapInplace0;

var length = Bs_internalBucketsType.length0;

var logStats = Bs_internalBuckets.logStats0;

exports.create           = create;
exports.clear            = clear;
exports.reset            = reset;
exports.add              = add;
exports.findOpt          = findOpt;
exports.findAll          = findAll;
exports.mem              = mem;
exports.remove           = remove;
exports.removeAll        = removeAll;
exports.replace          = replace;
exports.iter             = iter;
exports.fold             = fold;
exports.filterMapInplace = filterMapInplace;
exports.length           = length;
exports.logStats         = logStats;
/* Bs_internalBuckets Not a pure module */
