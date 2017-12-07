'use strict';

var $$Array                 = require("./array.js");
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

function create(initial_size) {
  var s = power_2_above(16, initial_size);
  return /* record */[
          /* size */0,
          /* data */Caml_array.caml_make_vect(s, /* Empty */0),
          /* initial_size */s
        ];
}

function clear(h) {
  h[/* size */0] = 0;
  var len = h[/* data */1].length;
  for(var i = 0 ,i_finish = len - 1 | 0; i <= i_finish; ++i){
    Caml_array.caml_array_set(h[/* data */1], i, /* Empty */0);
  }
  return /* () */0;
}

function reset(h) {
  var len = h[/* data */1].length;
  if (h.length < 4 || len === h[/* initial_size */2]) {
    return clear(h);
  } else {
    h[/* size */0] = 0;
    h[/* data */1] = Caml_array.caml_make_vect(h[/* initial_size */2], /* Empty */0);
    return /* () */0;
  }
}

function copy(h) {
  return /* record */[
          /* size */h[/* size */0],
          /* data */$$Array.copy(h[/* data */1]),
          /* initial_size */h[/* initial_size */2]
        ];
}

function length(h) {
  return h[/* size */0];
}

function iter(f, h) {
  var do_bucket = function (_param) {
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
  };
  var d = h[/* data */1];
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    do_bucket(Caml_array.caml_array_get(d, i));
  }
  return /* () */0;
}

function fold(f, h, init) {
  var do_bucket = function (_b, _accu) {
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
  };
  var d = h[/* data */1];
  var accu = init;
  for(var i = 0 ,i_finish = d.length - 1 | 0; i <= i_finish; ++i){
    accu = do_bucket(Caml_array.caml_array_get(d, i), accu);
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

function max(m, n) {
  if (m > n) {
    return m;
  } else {
    return n;
  }
}

function stats(h) {
  var mbl = $$Array.fold_left((function (m, b) {
          return max(m, bucket_length(0, b));
        }), 0, h[/* data */1]);
  var histo = Caml_array.caml_make_vect(mbl + 1 | 0, 0);
  $$Array.iter((function (b) {
          var l = bucket_length(0, b);
          return Caml_array.caml_array_set(histo, l, Caml_array.caml_array_get(histo, l) + 1 | 0);
        }), h[/* data */1]);
  return /* record */[
          /* num_bindings */h[/* size */0],
          /* num_buckets */h[/* data */1].length,
          /* max_bucket_length */mbl,
          /* bucket_histogram */histo
        ];
}

function key_index(hash, h, key) {
  return hash(key) & (h[/* data */1].length - 1 | 0);
}

function resize(hash, h) {
  var odata = h[/* data */1];
  var osize = odata.length;
  var nsize = (osize << 1);
  if (nsize >= osize) {
    var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
    h[/* data */1] = ndata;
    var insert_bucket = function (param) {
      if (param) {
        var key = param[0];
        insert_bucket(param[2]);
        var nidx = key_index(hash, h, key);
        return Caml_array.caml_array_set(ndata, nidx, /* Cons */[
                    key,
                    param[1],
                    Caml_array.caml_array_get(ndata, nidx)
                  ]);
      } else {
        return /* () */0;
      }
    };
    for(var i = 0 ,i_finish = osize - 1 | 0; i <= i_finish; ++i){
      insert_bucket(Caml_array.caml_array_get(odata, i));
    }
    return /* () */0;
  } else {
    return 0;
  }
}

function add(hash, h, key, info) {
  var i = key_index(hash, h, key);
  var bucket_002 = Caml_array.caml_array_get(h[/* data */1], i);
  var bucket = /* Cons */[
    key,
    info,
    bucket_002
  ];
  Caml_array.caml_array_set(h[/* data */1], i, bucket);
  h[/* size */0] = h[/* size */0] + 1 | 0;
  if (h[/* size */0] > (h[/* data */1].length << 1)) {
    return resize(hash, h);
  } else {
    return 0;
  }
}

function remove(hash, eq, h, key) {
  var remove_bucket = function (param) {
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
                remove_bucket(next)
              ];
      }
    } else {
      return /* Empty */0;
    }
  };
  var i = key_index(hash, h, key);
  return Caml_array.caml_array_set(h[/* data */1], i, remove_bucket(Caml_array.caml_array_get(h[/* data */1], i)));
}

function find_rec(eq, key, _param) {
  while(true) {
    var param = _param;
    if (param) {
      if (eq(key, param[0])) {
        return param[1];
      } else {
        _param = param[2];
        continue ;
        
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
}

function find(hash, eq, h, key) {
  var match = Caml_array.caml_array_get(h[/* data */1], key_index(hash, h, key));
  if (match) {
    if (eq(key, match[0])) {
      return match[1];
    } else {
      var rest1 = match[2];
      if (rest1) {
        if (eq(key, rest1[0])) {
          return rest1[1];
        } else {
          var rest2 = rest1[2];
          if (rest2) {
            if (eq(key, rest2[0])) {
              return rest2[1];
            } else {
              return find_rec(eq, key, rest2[2]);
            }
          } else {
            throw Caml_builtin_exceptions.not_found;
          }
        }
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    }
  } else {
    throw Caml_builtin_exceptions.not_found;
  }
}

function find_all(hash, eq, h, key) {
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
  return find_in_bucket(Caml_array.caml_array_get(h[/* data */1], key_index(hash, h, key)));
}

function replace(eq, hash, h, key, info) {
  var replace_bucket = function (param) {
    if (param) {
      var next = param[2];
      var k = param[0];
      if (eq(k, key)) {
        return /* Cons */[
                key,
                info,
                next
              ];
      } else {
        return /* Cons */[
                k,
                param[1],
                replace_bucket(next)
              ];
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
  var i = key_index(hash, h, key);
  var l = Caml_array.caml_array_get(h[/* data */1], i);
  try {
    return Caml_array.caml_array_set(h[/* data */1], i, replace_bucket(l));
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      Caml_array.caml_array_set(h[/* data */1], i, /* Cons */[
            key,
            info,
            l
          ]);
      h[/* size */0] = h[/* size */0] + 1 | 0;
      if (h[/* size */0] > (h[/* data */1].length << 1)) {
        return resize(hash, h);
      } else {
        return 0;
      }
    } else {
      throw exn;
    }
  }
}

function mem(hash, eq, h, key) {
  var _param = Caml_array.caml_array_get(h[/* data */1], key_index(hash, h, key));
  while(true) {
    var param = _param;
    if (param) {
      if (eq(param[0], key)) {
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

exports.power_2_above = power_2_above;
exports.create        = create;
exports.clear         = clear;
exports.reset         = reset;
exports.copy          = copy;
exports.length        = length;
exports.iter          = iter;
exports.fold          = fold;
exports.bucket_length = bucket_length;
exports.max           = max;
exports.stats         = stats;
exports.key_index     = key_index;
exports.resize        = resize;
exports.add           = add;
exports.remove        = remove;
exports.find_rec      = find_rec;
exports.find          = find;
exports.find_all      = find_all;
exports.replace       = replace;
exports.mem           = mem;
/* No side effect */
