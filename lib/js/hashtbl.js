'use strict';

var $$Array = require("./array.js");
var Curry = require("./curry.js");
var Random = require("./random.js");
var Caml_obj = require("./caml_obj.js");
var Caml_hash = require("./caml_hash.js");
var Caml_array = require("./caml_array.js");
var Pervasives = require("./pervasives.js");
var Caml_option = require("./caml_option.js");
var Caml_primitive = require("./caml_primitive.js");
var CamlinternalLazy = require("./camlinternalLazy.js");
var Caml_builtin_exceptions = require("./caml_builtin_exceptions.js");

function hash(x) {
  return Caml_hash.caml_hash(10, 100, 0, x);
}

function hash_param(n1, n2, x) {
  return Caml_hash.caml_hash(n1, n2, 0, x);
}

function seeded_hash(seed, x) {
  return Caml_hash.caml_hash(10, 100, seed, x);
}

function flip_ongoing_traversal(h) {
  h.initial_size = -h.initial_size | 0;
  
}

var randomized = {
  contents: false
};

function randomize(param) {
  randomized.contents = true;
  
}

function is_randomized(param) {
  return randomized.contents;
}

var prng = Caml_obj.caml_lazy_make((function (param) {
        return Random.State.make_self_init(undefined);
      }));

function power_2_above(_x, n) {
  while(true) {
    var x = _x;
    if (x >= n) {
      return x;
    }
    if ((x << 1) < x) {
      return x;
    }
    _x = (x << 1);
    continue ;
  };
}

function create(randomOpt, initial_size) {
  var random = randomOpt !== undefined ? randomOpt : randomized.contents;
  var s = power_2_above(16, initial_size);
  var seed = random ? Random.State.bits(CamlinternalLazy.force(prng)) : 0;
  return {
          size: 0,
          data: Caml_array.caml_make_vect(s, /* Empty */0),
          seed: seed,
          initial_size: s
        };
}

function clear(h) {
  h.size = 0;
  var len = h.data.length;
  for(var i = 0; i < len; ++i){
    Caml_array.caml_array_set(h.data, i, /* Empty */0);
  }
  
}

function reset(h) {
  var len = h.data.length;
  if (len === Pervasives.abs(h.initial_size)) {
    return clear(h);
  } else {
    h.size = 0;
    h.data = Caml_array.caml_make_vect(Pervasives.abs(h.initial_size), /* Empty */0);
    return ;
  }
}

function copy_bucketlist(param) {
  if (!param) {
    return /* Empty */0;
  }
  var key = param[/* key */0];
  var data = param[/* data */1];
  var next = param[/* next */2];
  var loop = function (_prec, _param) {
    while(true) {
      var param = _param;
      var prec = _prec;
      if (!param) {
        return ;
      }
      var key = param[/* key */0];
      var data = param[/* data */1];
      var next = param[/* next */2];
      var r = /* Cons */[
        /* key */key,
        /* data */data,
        /* next */next
      ];
      if (prec) {
        prec[/* next */2] = r;
      } else {
        throw [
              Caml_builtin_exceptions.assert_failure,
              /* tuple */[
                "hashtbl.ml",
                113,
                23
              ]
            ];
      }
      _param = next;
      _prec = r;
      continue ;
    };
  };
  var r = /* Cons */[
    /* key */key,
    /* data */data,
    /* next */next
  ];
  loop(r, next);
  return r;
}

function copy(h) {
  return {
          size: h.size,
          data: $$Array.map(copy_bucketlist, h.data),
          seed: h.seed,
          initial_size: h.initial_size
        };
}

function length(h) {
  return h.size;
}

function resize(indexfun, h) {
  var odata = h.data;
  var osize = odata.length;
  var nsize = (osize << 1);
  if (nsize < osize) {
    return ;
  }
  var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
  var ndata_tail = Caml_array.caml_make_vect(nsize, /* Empty */0);
  var inplace = h.initial_size >= 0;
  h.data = ndata;
  var insert_bucket = function (_cell) {
    while(true) {
      var cell = _cell;
      if (!cell) {
        return ;
      }
      var key = cell[/* key */0];
      var data = cell[/* data */1];
      var next = cell[/* next */2];
      var cell$1 = inplace ? cell : /* Cons */[
          /* key */key,
          /* data */data,
          /* next : Empty */0
        ];
      var nidx = Curry._2(indexfun, h, key);
      var match = Caml_array.caml_array_get(ndata_tail, nidx);
      if (match) {
        match[/* next */2] = cell$1;
      } else {
        Caml_array.caml_array_set(ndata, nidx, cell$1);
      }
      Caml_array.caml_array_set(ndata_tail, nidx, cell$1);
      _cell = next;
      continue ;
    };
  };
  for(var i = 0; i < osize; ++i){
    insert_bucket(Caml_array.caml_array_get(odata, i));
  }
  if (!inplace) {
    return ;
  }
  for(var i$1 = 0; i$1 < nsize; ++i$1){
    var match = Caml_array.caml_array_get(ndata_tail, i$1);
    if (match) {
      match[/* next */2] = /* Empty */0;
    }
    
  }
  
}

function key_index(h, key) {
  return Caml_hash.caml_hash(10, 100, h.seed, key) & (h.data.length - 1 | 0);
}

function add(h, key, data) {
  var i = key_index(h, key);
  var bucket = /* Cons */[
    /* key */key,
    /* data */data,
    /* next */Caml_array.caml_array_get(h.data, i)
  ];
  Caml_array.caml_array_set(h.data, i, bucket);
  h.size = h.size + 1 | 0;
  if (h.size > (h.data.length << 1)) {
    return resize(key_index, h);
  }
  
}

function remove(h, key) {
  var i = key_index(h, key);
  var _prec = /* Empty */0;
  var _c = Caml_array.caml_array_get(h.data, i);
  while(true) {
    var c = _c;
    var prec = _prec;
    if (!c) {
      return ;
    }
    var k = c[/* key */0];
    var next = c[/* next */2];
    if (Caml_obj.caml_equal(k, key)) {
      h.size = h.size - 1 | 0;
      if (prec) {
        prec[/* next */2] = next;
        return ;
      } else {
        return Caml_array.caml_array_set(h.data, i, next);
      }
    }
    _c = next;
    _prec = c;
    continue ;
  };
}

function find(h, key) {
  var match = Caml_array.caml_array_get(h.data, key_index(h, key));
  if (match) {
    var k1 = match[/* key */0];
    var d1 = match[/* data */1];
    var next1 = match[/* next */2];
    if (Caml_obj.caml_equal(key, k1)) {
      return d1;
    }
    if (next1) {
      var k2 = next1[/* key */0];
      var d2 = next1[/* data */1];
      var next2 = next1[/* next */2];
      if (Caml_obj.caml_equal(key, k2)) {
        return d2;
      }
      if (next2) {
        var k3 = next2[/* key */0];
        var d3 = next2[/* data */1];
        var next3 = next2[/* next */2];
        if (Caml_obj.caml_equal(key, k3)) {
          return d3;
        } else {
          var _param = next3;
          while(true) {
            var param = _param;
            if (param) {
              var k = param[/* key */0];
              var data = param[/* data */1];
              var next = param[/* next */2];
              if (Caml_obj.caml_equal(key, k)) {
                return data;
              }
              _param = next;
              continue ;
            }
            throw Caml_builtin_exceptions.not_found;
          };
        }
      }
      throw Caml_builtin_exceptions.not_found;
    }
    throw Caml_builtin_exceptions.not_found;
  }
  throw Caml_builtin_exceptions.not_found;
}

function find_opt(h, key) {
  var match = Caml_array.caml_array_get(h.data, key_index(h, key));
  if (!match) {
    return ;
  }
  var k1 = match[/* key */0];
  var d1 = match[/* data */1];
  var next1 = match[/* next */2];
  if (Caml_obj.caml_equal(key, k1)) {
    return Caml_option.some(d1);
  }
  if (!next1) {
    return ;
  }
  var k2 = next1[/* key */0];
  var d2 = next1[/* data */1];
  var next2 = next1[/* next */2];
  if (Caml_obj.caml_equal(key, k2)) {
    return Caml_option.some(d2);
  }
  if (!next2) {
    return ;
  }
  var k3 = next2[/* key */0];
  var d3 = next2[/* data */1];
  var next3 = next2[/* next */2];
  if (Caml_obj.caml_equal(key, k3)) {
    return Caml_option.some(d3);
  } else {
    var _param = next3;
    while(true) {
      var param = _param;
      if (!param) {
        return ;
      }
      var k = param[/* key */0];
      var data = param[/* data */1];
      var next = param[/* next */2];
      if (Caml_obj.caml_equal(key, k)) {
        return Caml_option.some(data);
      }
      _param = next;
      continue ;
    };
  }
}

function find_all(h, key) {
  var find_in_bucket = function (_param) {
    while(true) {
      var param = _param;
      if (!param) {
        return /* [] */0;
      }
      var k = param[/* key */0];
      var data = param[/* data */1];
      var next = param[/* next */2];
      if (Caml_obj.caml_equal(k, key)) {
        return /* :: */[
                data,
                find_in_bucket(next)
              ];
      }
      _param = next;
      continue ;
    };
  };
  return find_in_bucket(Caml_array.caml_array_get(h.data, key_index(h, key)));
}

function replace_bucket(key, data, _param) {
  while(true) {
    var param = _param;
    if (!param) {
      return true;
    }
    var k = param[/* key */0];
    var next = param[/* next */2];
    if (Caml_obj.caml_equal(k, key)) {
      param[/* key */0] = key;
      param[/* data */1] = data;
      return false;
    }
    _param = next;
    continue ;
  };
}

function replace(h, key, data) {
  var i = key_index(h, key);
  var l = Caml_array.caml_array_get(h.data, i);
  if (replace_bucket(key, data, l)) {
    Caml_array.caml_array_set(h.data, i, /* Cons */[
          /* key */key,
          /* data */data,
          /* next */l
        ]);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(key_index, h);
    } else {
      return ;
    }
  }
  
}

function mem(h, key) {
  var _param = Caml_array.caml_array_get(h.data, key_index(h, key));
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    var k = param[/* key */0];
    var next = param[/* next */2];
    if (Caml_obj.caml_equal(k, key)) {
      return true;
    }
    _param = next;
    continue ;
  };
}

function iter(f, h) {
  var do_bucket = function (_param) {
    while(true) {
      var param = _param;
      if (!param) {
        return ;
      }
      var key = param[/* key */0];
      var data = param[/* data */1];
      var next = param[/* next */2];
      Curry._2(f, key, data);
      _param = next;
      continue ;
    };
  };
  var old_trav = h.initial_size < 0;
  if (!old_trav) {
    flip_ongoing_traversal(h);
  }
  try {
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      do_bucket(Caml_array.caml_array_get(d, i));
    }
    if (!old_trav) {
      return flip_ongoing_traversal(h);
    } else {
      return ;
    }
  }
  catch (exn){
    if (old_trav) {
      throw exn;
    }
    flip_ongoing_traversal(h);
    throw exn;
  }
}

function filter_map_inplace_bucket(f, h, i, _prec, _slot) {
  while(true) {
    var slot = _slot;
    var prec = _prec;
    if (!slot) {
      if (prec) {
        prec[/* next */2] = /* Empty */0;
        return ;
      } else {
        return Caml_array.caml_array_set(h.data, i, /* Empty */0);
      }
    }
    var key = slot[/* key */0];
    var data = slot[/* data */1];
    var next = slot[/* next */2];
    var match = Curry._2(f, key, data);
    if (match !== undefined) {
      if (prec) {
        prec[/* next */2] = slot;
      } else {
        Caml_array.caml_array_set(h.data, i, slot);
      }
      slot[/* data */1] = Caml_option.valFromOption(match);
      _slot = next;
      _prec = slot;
      continue ;
    }
    h.size = h.size - 1 | 0;
    _slot = next;
    continue ;
  };
}

function filter_map_inplace(f, h) {
  var d = h.data;
  var old_trav = h.initial_size < 0;
  if (!old_trav) {
    flip_ongoing_traversal(h);
  }
  try {
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      filter_map_inplace_bucket(f, h, i, /* Empty */0, Caml_array.caml_array_get(h.data, i));
    }
    return ;
  }
  catch (exn){
    if (old_trav) {
      throw exn;
    }
    flip_ongoing_traversal(h);
    throw exn;
  }
}

function fold(f, h, init) {
  var do_bucket = function (_b, _accu) {
    while(true) {
      var accu = _accu;
      var b = _b;
      if (!b) {
        return accu;
      }
      var key = b[/* key */0];
      var data = b[/* data */1];
      var next = b[/* next */2];
      _accu = Curry._3(f, key, data, accu);
      _b = next;
      continue ;
    };
  };
  var old_trav = h.initial_size < 0;
  if (!old_trav) {
    flip_ongoing_traversal(h);
  }
  try {
    var d = h.data;
    var accu = init;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      accu = do_bucket(Caml_array.caml_array_get(d, i), accu);
    }
    if (!old_trav) {
      flip_ongoing_traversal(h);
    }
    return accu;
  }
  catch (exn){
    if (old_trav) {
      throw exn;
    }
    flip_ongoing_traversal(h);
    throw exn;
  }
}

function bucket_length(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (!param) {
      return accu;
    }
    var next = param[/* next */2];
    _param = next;
    _accu = accu + 1 | 0;
    continue ;
  };
}

function stats(h) {
  var mbl = $$Array.fold_left((function (m, b) {
          return Caml_primitive.caml_int_max(m, bucket_length(0, b));
        }), 0, h.data);
  var histo = Caml_array.caml_make_vect(mbl + 1 | 0, 0);
  $$Array.iter((function (b) {
          var l = bucket_length(0, b);
          return Caml_array.caml_array_set(histo, l, Caml_array.caml_array_get(histo, l) + 1 | 0);
        }), h.data);
  return {
          num_bindings: h.size,
          num_buckets: h.data.length,
          max_bucket_length: mbl,
          bucket_histogram: histo
        };
}

function MakeSeeded(H) {
  var key_index = function (h, key) {
    return Curry._2(H.hash, h.seed, key) & (h.data.length - 1 | 0);
  };
  var add = function (h, key, data) {
    var i = key_index(h, key);
    var bucket = /* Cons */[
      /* key */key,
      /* data */data,
      /* next */Caml_array.caml_array_get(h.data, i)
    ];
    Caml_array.caml_array_set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(key_index, h);
    }
    
  };
  var remove = function (h, key) {
    var i = key_index(h, key);
    var _prec = /* Empty */0;
    var _c = Caml_array.caml_array_get(h.data, i);
    while(true) {
      var c = _c;
      var prec = _prec;
      if (!c) {
        return ;
      }
      var k = c[/* key */0];
      var next = c[/* next */2];
      if (Curry._2(H.equal, k, key)) {
        h.size = h.size - 1 | 0;
        if (prec) {
          prec[/* next */2] = next;
          return ;
        } else {
          return Caml_array.caml_array_set(h.data, i, next);
        }
      }
      _c = next;
      _prec = c;
      continue ;
    };
  };
  var find = function (h, key) {
    var match = Caml_array.caml_array_get(h.data, key_index(h, key));
    if (match) {
      var k1 = match[/* key */0];
      var d1 = match[/* data */1];
      var next1 = match[/* next */2];
      if (Curry._2(H.equal, key, k1)) {
        return d1;
      }
      if (next1) {
        var k2 = next1[/* key */0];
        var d2 = next1[/* data */1];
        var next2 = next1[/* next */2];
        if (Curry._2(H.equal, key, k2)) {
          return d2;
        }
        if (next2) {
          var k3 = next2[/* key */0];
          var d3 = next2[/* data */1];
          var next3 = next2[/* next */2];
          if (Curry._2(H.equal, key, k3)) {
            return d3;
          } else {
            var _param = next3;
            while(true) {
              var param = _param;
              if (param) {
                var k = param[/* key */0];
                var data = param[/* data */1];
                var next = param[/* next */2];
                if (Curry._2(H.equal, key, k)) {
                  return data;
                }
                _param = next;
                continue ;
              }
              throw Caml_builtin_exceptions.not_found;
            };
          }
        }
        throw Caml_builtin_exceptions.not_found;
      }
      throw Caml_builtin_exceptions.not_found;
    }
    throw Caml_builtin_exceptions.not_found;
  };
  var find_opt = function (h, key) {
    var match = Caml_array.caml_array_get(h.data, key_index(h, key));
    if (!match) {
      return ;
    }
    var k1 = match[/* key */0];
    var d1 = match[/* data */1];
    var next1 = match[/* next */2];
    if (Curry._2(H.equal, key, k1)) {
      return Caml_option.some(d1);
    }
    if (!next1) {
      return ;
    }
    var k2 = next1[/* key */0];
    var d2 = next1[/* data */1];
    var next2 = next1[/* next */2];
    if (Curry._2(H.equal, key, k2)) {
      return Caml_option.some(d2);
    }
    if (!next2) {
      return ;
    }
    var k3 = next2[/* key */0];
    var d3 = next2[/* data */1];
    var next3 = next2[/* next */2];
    if (Curry._2(H.equal, key, k3)) {
      return Caml_option.some(d3);
    } else {
      var _param = next3;
      while(true) {
        var param = _param;
        if (!param) {
          return ;
        }
        var k = param[/* key */0];
        var data = param[/* data */1];
        var next = param[/* next */2];
        if (Curry._2(H.equal, key, k)) {
          return Caml_option.some(data);
        }
        _param = next;
        continue ;
      };
    }
  };
  var find_all = function (h, key) {
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* [] */0;
        }
        var k = param[/* key */0];
        var d = param[/* data */1];
        var next = param[/* next */2];
        if (Curry._2(H.equal, k, key)) {
          return /* :: */[
                  d,
                  find_in_bucket(next)
                ];
        }
        _param = next;
        continue ;
      };
    };
    return find_in_bucket(Caml_array.caml_array_get(h.data, key_index(h, key)));
  };
  var replace_bucket = function (key, data, _param) {
    while(true) {
      var param = _param;
      if (!param) {
        return true;
      }
      var k = param[/* key */0];
      var next = param[/* next */2];
      if (Curry._2(H.equal, k, key)) {
        param[/* key */0] = key;
        param[/* data */1] = data;
        return false;
      }
      _param = next;
      continue ;
    };
  };
  var replace = function (h, key, data) {
    var i = key_index(h, key);
    var l = Caml_array.caml_array_get(h.data, i);
    if (replace_bucket(key, data, l)) {
      Caml_array.caml_array_set(h.data, i, /* Cons */[
            /* key */key,
            /* data */data,
            /* next */l
          ]);
      h.size = h.size + 1 | 0;
      if (h.size > (h.data.length << 1)) {
        return resize(key_index, h);
      } else {
        return ;
      }
    }
    
  };
  var mem = function (h, key) {
    var _param = Caml_array.caml_array_get(h.data, key_index(h, key));
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var k = param[/* key */0];
      var next = param[/* next */2];
      if (Curry._2(H.equal, k, key)) {
        return true;
      }
      _param = next;
      continue ;
    };
  };
  return {
          create: create,
          clear: clear,
          reset: reset,
          copy: copy,
          add: add,
          remove: remove,
          find: find,
          find_opt: find_opt,
          find_all: find_all,
          replace: replace,
          mem: mem,
          iter: iter,
          filter_map_inplace: filter_map_inplace,
          fold: fold,
          length: length,
          stats: stats
        };
}

function Make(H) {
  var equal = H.equal;
  var key_index = function (h, key) {
    return Curry._1(H.hash, key) & (h.data.length - 1 | 0);
  };
  var add = function (h, key, data) {
    var i = key_index(h, key);
    var bucket = /* Cons */[
      /* key */key,
      /* data */data,
      /* next */Caml_array.caml_array_get(h.data, i)
    ];
    Caml_array.caml_array_set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(key_index, h);
    }
    
  };
  var remove = function (h, key) {
    var i = key_index(h, key);
    var _prec = /* Empty */0;
    var _c = Caml_array.caml_array_get(h.data, i);
    while(true) {
      var c = _c;
      var prec = _prec;
      if (!c) {
        return ;
      }
      var k = c[/* key */0];
      var next = c[/* next */2];
      if (Curry._2(equal, k, key)) {
        h.size = h.size - 1 | 0;
        if (prec) {
          prec[/* next */2] = next;
          return ;
        } else {
          return Caml_array.caml_array_set(h.data, i, next);
        }
      }
      _c = next;
      _prec = c;
      continue ;
    };
  };
  var find = function (h, key) {
    var match = Caml_array.caml_array_get(h.data, key_index(h, key));
    if (match) {
      var k1 = match[/* key */0];
      var d1 = match[/* data */1];
      var next1 = match[/* next */2];
      if (Curry._2(equal, key, k1)) {
        return d1;
      }
      if (next1) {
        var k2 = next1[/* key */0];
        var d2 = next1[/* data */1];
        var next2 = next1[/* next */2];
        if (Curry._2(equal, key, k2)) {
          return d2;
        }
        if (next2) {
          var k3 = next2[/* key */0];
          var d3 = next2[/* data */1];
          var next3 = next2[/* next */2];
          if (Curry._2(equal, key, k3)) {
            return d3;
          } else {
            var _param = next3;
            while(true) {
              var param = _param;
              if (param) {
                var k = param[/* key */0];
                var data = param[/* data */1];
                var next = param[/* next */2];
                if (Curry._2(equal, key, k)) {
                  return data;
                }
                _param = next;
                continue ;
              }
              throw Caml_builtin_exceptions.not_found;
            };
          }
        }
        throw Caml_builtin_exceptions.not_found;
      }
      throw Caml_builtin_exceptions.not_found;
    }
    throw Caml_builtin_exceptions.not_found;
  };
  var find_opt = function (h, key) {
    var match = Caml_array.caml_array_get(h.data, key_index(h, key));
    if (!match) {
      return ;
    }
    var k1 = match[/* key */0];
    var d1 = match[/* data */1];
    var next1 = match[/* next */2];
    if (Curry._2(equal, key, k1)) {
      return Caml_option.some(d1);
    }
    if (!next1) {
      return ;
    }
    var k2 = next1[/* key */0];
    var d2 = next1[/* data */1];
    var next2 = next1[/* next */2];
    if (Curry._2(equal, key, k2)) {
      return Caml_option.some(d2);
    }
    if (!next2) {
      return ;
    }
    var k3 = next2[/* key */0];
    var d3 = next2[/* data */1];
    var next3 = next2[/* next */2];
    if (Curry._2(equal, key, k3)) {
      return Caml_option.some(d3);
    } else {
      var _param = next3;
      while(true) {
        var param = _param;
        if (!param) {
          return ;
        }
        var k = param[/* key */0];
        var data = param[/* data */1];
        var next = param[/* next */2];
        if (Curry._2(equal, key, k)) {
          return Caml_option.some(data);
        }
        _param = next;
        continue ;
      };
    }
  };
  var find_all = function (h, key) {
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* [] */0;
        }
        var k = param[/* key */0];
        var d = param[/* data */1];
        var next = param[/* next */2];
        if (Curry._2(equal, k, key)) {
          return /* :: */[
                  d,
                  find_in_bucket(next)
                ];
        }
        _param = next;
        continue ;
      };
    };
    return find_in_bucket(Caml_array.caml_array_get(h.data, key_index(h, key)));
  };
  var replace_bucket = function (key, data, _param) {
    while(true) {
      var param = _param;
      if (!param) {
        return true;
      }
      var k = param[/* key */0];
      var next = param[/* next */2];
      if (Curry._2(equal, k, key)) {
        param[/* key */0] = key;
        param[/* data */1] = data;
        return false;
      }
      _param = next;
      continue ;
    };
  };
  var replace = function (h, key, data) {
    var i = key_index(h, key);
    var l = Caml_array.caml_array_get(h.data, i);
    if (replace_bucket(key, data, l)) {
      Caml_array.caml_array_set(h.data, i, /* Cons */[
            /* key */key,
            /* data */data,
            /* next */l
          ]);
      h.size = h.size + 1 | 0;
      if (h.size > (h.data.length << 1)) {
        return resize(key_index, h);
      } else {
        return ;
      }
    }
    
  };
  var mem = function (h, key) {
    var _param = Caml_array.caml_array_get(h.data, key_index(h, key));
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var k = param[/* key */0];
      var next = param[/* next */2];
      if (Curry._2(equal, k, key)) {
        return true;
      }
      _param = next;
      continue ;
    };
  };
  var create$1 = function (sz) {
    return create(false, sz);
  };
  return {
          create: create$1,
          clear: clear,
          reset: reset,
          copy: copy,
          add: add,
          remove: remove,
          find: find,
          find_opt: find_opt,
          find_all: find_all,
          replace: replace,
          mem: mem,
          iter: iter,
          filter_map_inplace: filter_map_inplace,
          fold: fold,
          length: length,
          stats: stats
        };
}

var seeded_hash_param = Caml_hash.caml_hash;

exports.create = create;
exports.clear = clear;
exports.reset = reset;
exports.copy = copy;
exports.add = add;
exports.find = find;
exports.find_opt = find_opt;
exports.find_all = find_all;
exports.mem = mem;
exports.remove = remove;
exports.replace = replace;
exports.iter = iter;
exports.filter_map_inplace = filter_map_inplace;
exports.fold = fold;
exports.length = length;
exports.randomize = randomize;
exports.is_randomized = is_randomized;
exports.stats = stats;
exports.Make = Make;
exports.MakeSeeded = MakeSeeded;
exports.hash = hash;
exports.seeded_hash = seeded_hash;
exports.hash_param = hash_param;
exports.seeded_hash_param = seeded_hash_param;
/* No side effect */
