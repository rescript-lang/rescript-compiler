'use strict';

var Bs_Array                = require("./bs_Array.js");
var Caml_array              = require("./caml_array.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

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
          /* buckets */Caml_array.caml_make_vect(s, /* Empty */0),
          /* initial_size */s
        ];
}

function clear0(h) {
  h[/* size */0] = 0;
  var h_buckets = h[/* buckets */1];
  var len = h_buckets.length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    h_buckets[i] = /* Empty */0;
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
    h[/* buckets */1] = Caml_array.caml_make_vect(h_initial_size, /* Empty */0);
    return /* () */0;
  }
}

function length0(h) {
  return h[/* size */0];
}

function do_bucket_iter(f, _param) {
  while(true) {
    var param = _param;
    if (param) {
      f(param[0], param[1]);
      _param = param[2];
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
    if (b) {
      _accu = f(b[0], b[1], accu);
      _b = b[2];
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

function bucket_length(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[2];
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

function insert_bucket_list(hash, ndata, h, param) {
  if (param) {
    var key = param[0];
    insert_bucket_list(hash, ndata, h, param[2]);
    var nidx = key_index(hash, h, key);
    ndata[nidx] = /* Cons */[
      key,
      param[1],
      ndata[nidx]
    ];
    return /* () */0;
  } else {
    return /* () */0;
  }
}

function resize(hash, h) {
  var odata = h[/* buckets */1];
  var osize = odata.length;
  var nsize = (osize << 1);
  if (nsize >= osize) {
    var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
    h[/* buckets */1] = ndata;
    for(var i = 0 ,i_finish = osize - 1 | 0; i <= i_finish; ++i){
      insert_bucket_list(hash, ndata, h, odata[i]);
    }
    return /* () */0;
  } else {
    return 0;
  }
}

function add0(hash, h, key, info) {
  var i = key_index(hash, h, key);
  var h_buckets = h[/* buckets */1];
  var bucket_002 = h_buckets[i];
  var bucket = /* Cons */[
    key,
    info,
    bucket_002
  ];
  h_buckets[i] = bucket;
  h[/* size */0] = h[/* size */0] + 1 | 0;
  if (h[/* size */0] > (h_buckets.length << 1)) {
    return resize(hash, h);
  } else {
    return 0;
  }
}

function remove_bucket(eq, key, h, param) {
  if (param) {
    var next = param[2];
    var k = param[0];
    if (eq(k, key)) {
      h[/* size */0] = h[/* size */0] - 1 | 0;
      return next;
    } else {
      return /* Cons */[
              k,
              param[1],
              remove_bucket(eq, key, h, next)
            ];
    }
  } else {
    return /* Empty */0;
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
  if (match) {
    if (eq(key, match[0])) {
      return /* Some */[match[1]];
    } else {
      var rest1 = match[2];
      if (rest1) {
        if (eq(key, rest1[0])) {
          return /* Some */[rest1[1]];
        } else {
          var rest2 = rest1[2];
          if (rest2) {
            if (eq(key, rest2[0])) {
              return /* Some */[rest2[1]];
            } else {
              var eq$1 = eq;
              var key$1 = key;
              var _param = rest2[2];
              while(true) {
                var param = _param;
                if (param) {
                  if (eq$1(key$1, param[0])) {
                    return /* Some */[param[1]];
                  } else {
                    _param = param[2];
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
  var find_in_bucket = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        var rest = param[2];
        if (eq(param[0], key)) {
          return /* :: */[
                  param[1],
                  find_in_bucket(rest)
                ];
        } else {
          _param = rest;
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
  if (buckets) {
    var next = buckets[2];
    var k = buckets[0];
    if (eq(k, key)) {
      return /* Cons */[
              key,
              info,
              next
            ];
    } else {
      return /* Cons */[
              k,
              buckets[1],
              replace_bucket(eq, key, info, next)
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
      h_buckets[i] = /* Cons */[
        key,
        info,
        l
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
  var _param = h[/* buckets */1][key_index(hash, h, key)];
  while(true) {
    var param = _param;
    if (param) {
      if (eq$1(param[0], key$1)) {
        return /* true */1;
      } else {
        _param = param[2];
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
/* No side effect */
