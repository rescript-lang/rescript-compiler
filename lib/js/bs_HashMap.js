'use strict';

var Bs_Array                = require("./bs_Array.js");
var Caml_array              = require("./caml_array.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

var emptyOpt = undefined;

function power_2_above(_x, n) {
  while(true) {
    var x = _x;
    if (x >= n) {
      return x;
    } else if ((x << 1) < x) {
      return x;
    } else {
      _x = (x << 1);
      continue ;
      
    }
  };
}

function create0(initial_size) {
  var s = power_2_above(16, initial_size);
  return /* record */[
          /* size */0,
          /* buckets */new Array(s),
          /* initial_size */s
        ];
}

function clear0(h) {
  h[/* size */0] = 0;
  var h_buckets = h[/* buckets */1];
  var len = h_buckets.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    h_buckets[i] = emptyOpt;
  }
  return /* () */0;
}

function reset0(h) {
  var len = h[/* buckets */1].length;
  var h_initial_size = h[/* initial_size */2];
  if (len === h_initial_size) {
    return clear0(h);
  } else {
    h[/* size */0] = 0;
    h[/* buckets */1] = new Array(h_initial_size);
    return /* () */0;
  }
}

function length0(h) {
  return h[/* size */0];
}

function do_bucket_iter(f, _buckets) {
  while(true) {
    var buckets = _buckets;
    if (buckets !== undefined) {
      var key = buckets[/* key */0];
      var value = buckets[/* value */1];
      var next = buckets[/* next */2];
      f(key, value);
      _buckets = next;
      continue ;
      
    } else {
      return /* () */0;
    }
  };
}

function iter0(f, h) {
  var d = h[/* buckets */1];
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    do_bucket_iter(f, d[i]);
  }
  return /* () */0;
}

function do_bucket_fold(f, _b, _accu) {
  while(true) {
    var accu = _accu;
    var b = _b;
    if (b !== undefined) {
      var key = b[/* key */0];
      var value = b[/* value */1];
      var next = b[/* next */2];
      _accu = f(key, value, accu);
      _b = next;
      continue ;
      
    } else {
      return accu;
    }
  };
}

function fold0(f, h, init) {
  var d = h[/* buckets */1];
  var accu = init;
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    accu = do_bucket_fold(f, d[i], accu);
  }
  return accu;
}

function bucket_length(_accu, _buckets) {
  while(true) {
    var buckets = _buckets;
    var accu = _accu;
    if (buckets !== undefined) {
      var next = buckets[/* next */2];
      _buckets = next;
      _accu = accu + 1 | 0;
      continue ;
      
    } else {
      return accu;
    }
  };
}

function logStats0(h) {
  var mbl = Bs_Array.foldLeft((function (m, b) {
          var m$1 = m;
          var n = bucket_length(0, b);
          if (m$1 > n) {
            return m$1;
          } else {
            return n;
          }
        }), 0, h[/* buckets */1]);
  var histo = Caml_array.caml_make_vect(mbl + 1 | 0, 0);
  Bs_Array.iter((function (b) {
          var l = bucket_length(0, b);
          histo[l] = histo[l] + 1 | 0;
          return /* () */0;
        }), h[/* buckets */1]);
  console.log({
        num_bindings: h[/* size */0],
        num_buckets: h[/* buckets */1].length,
        max_bucket_length: mbl,
        bucket_histogram: histo
      });
  return /* () */0;
}

function key_index(hash, h, key) {
  return hash(key) & (h[/* buckets */1].length - 1 | 0);
}

function resize(hash, h) {
  var odata = h[/* buckets */1];
  var osize = odata.length;
  var nsize = (osize << 1);
  if (nsize >= osize) {
    var ndata = new Array(nsize);
    var ndata_tail = new Array(nsize);
    h[/* buckets */1] = ndata;
    var insert_bucket = function (_buckets) {
      while(true) {
        var buckets = _buckets;
        if (buckets !== undefined) {
          var key = buckets[/* key */0];
          var next = buckets[/* next */2];
          var nidx = key_index(hash, h, key);
          var match = ndata_tail[nidx];
          if (match !== undefined) {
            ndata_tail[nidx] = buckets;
            match[/* next */2] = buckets;
          } else {
            ndata_tail[nidx] = buckets;
            ndata[nidx] = buckets;
          }
          _buckets = next;
          continue ;
          
        } else {
          return /* () */0;
        }
      };
    };
    for(var i = 0 ,i_finish = osize - 1 | 0; i <= i_finish; ++i){
      insert_bucket(odata[i]);
    }
    for(var i$1 = 0 ,i_finish$1 = nsize - 1 | 0; i$1 <= i_finish$1; ++i$1){
      var match = ndata_tail[i$1];
      if (match !== undefined) {
        match[/* next */2] = emptyOpt;
      }
      
    }
    return /* () */0;
  } else {
    return 0;
  }
}

function add0(hash, h, key, info) {
  var i = key_index(hash, h, key);
  var h_buckets = h[/* buckets */1];
  var bucket = /* record */[
    /* key */key,
    /* value */info,
    /* next */h_buckets[i]
  ];
  h_buckets[i] = bucket;
  h[/* size */0] = h[/* size */0] + 1 | 0;
  if (h[/* size */0] > (h_buckets.length << 1)) {
    return resize(hash, h);
  } else {
    return 0;
  }
}

function remove_bucket(eq, key, h, buckets) {
  if (buckets !== undefined) {
    var k = buckets[/* key */0];
    var i = buckets[/* value */1];
    var next = buckets[/* next */2];
    if (eq(k, key)) {
      h[/* size */0] = h[/* size */0] - 1 | 0;
      return next;
    } else {
      return /* record */[
              /* key */k,
              /* value */i,
              /* next */remove_bucket(eq, key, h, next)
            ];
    }
  } else {
    return emptyOpt;
  }
}

function remove0(hash, eq, h, key) {
  var i = key_index(hash, h, key);
  var h_buckets = h[/* buckets */1];
  h_buckets[i] = remove_bucket(eq, key, h, h_buckets[i]);
  return /* () */0;
}

function findOpt0(hash, eq, h, key) {
  var match = h[/* buckets */1][key_index(hash, h, key)];
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
  return find_in_bucket(h[/* buckets */1][key_index(hash, h, key)]);
}

function replace_bucket(eq, key, info, buckets) {
  if (buckets !== undefined) {
    var k = buckets[/* key */0];
    var i = buckets[/* value */1];
    var next = buckets[/* next */2];
    if (eq(k, key)) {
      return /* record */[
              /* key */key,
              /* value */info,
              /* next */next
            ];
    } else {
      return /* record */[
              /* key */k,
              /* value */i,
              /* next */replace_bucket(eq, key, info, next)
            ];
    }
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
}

function replace0(hash, eq, h, key, info) {
  var i = key_index(hash, h, key);
  var h_buckets = h[/* buckets */1];
  var l = h_buckets[i];
  try {
    h_buckets[i] = replace_bucket(eq, key, info, l);
    return /* () */0;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      h_buckets[i] = /* record */[
        /* key */key,
        /* value */info,
        /* next */l
      ];
      h[/* size */0] = h[/* size */0] + 1 | 0;
      if (h[/* size */0] > (h_buckets.length << 1)) {
        return resize(hash, h);
      } else {
        return 0;
      }
    } else {
      throw exn;
    }
  }
}

function mem0(hash, eq, h, key) {
  var eq$1 = eq;
  var key$1 = key;
  var _buckets = h[/* buckets */1][key_index(hash, h, key)];
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
          /* data */create0(initialize_size)
        ];
}

function clear(h) {
  return clear0(h[/* data */1]);
}

function reset(h) {
  return reset0(h[/* data */1]);
}

function length(h) {
  return h[/* data */1][/* size */0];
}

function iter(f, h) {
  return iter0(f, h[/* data */1]);
}

function fold(f, h, init) {
  return fold0(f, h[/* data */1], init);
}

function logStats(h) {
  return logStats0(h[/* data */1]);
}

function add(h, key, info) {
  var M = h[/* dict */0];
  return add0(M[/* hash */0], h[/* data */1], key, info);
}

function remove(h, key) {
  var M = h[/* dict */0];
  return remove0(M[/* hash */0], M[/* eq */1], h[/* data */1], key);
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

exports.create0   = create0;
exports.create    = create;
exports.clear0    = clear0;
exports.clear     = clear;
exports.reset0    = reset0;
exports.reset     = reset;
exports.add0      = add0;
exports.add       = add;
exports.findOpt0  = findOpt0;
exports.findOpt   = findOpt;
exports.findAll0  = findAll0;
exports.findAll   = findAll;
exports.mem0      = mem0;
exports.mem       = mem;
exports.remove    = remove;
exports.remove0   = remove0;
exports.replace0  = replace0;
exports.replace   = replace;
exports.iter0     = iter0;
exports.iter      = iter;
exports.fold0     = fold0;
exports.fold      = fold;
exports.length0   = length0;
exports.length    = length;
exports.logStats0 = logStats0;
exports.logStats  = logStats;
/* emptyOpt Not a pure module */
