// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_builtin_exceptions = require("../runtime/caml_builtin_exceptions");
var Caml_obj                = require("../runtime/caml_obj");
var CamlinternalLazy        = require("./camlinternalLazy");
var Caml_sys                = require("../runtime/caml_sys");
var Pervasives              = require("./pervasives");
var Sys                     = require("./sys");
var Caml_primitive          = require("../runtime/caml_primitive");
var Caml_array              = require("../runtime/caml_array");
var $$Array                 = require("./array");
var $$String                = require("./string");
var Caml_curry              = require("../runtime/caml_curry");
var Random                  = require("./random");

function hash(x) {
  return Caml_primitive.caml_hash(10, 100, 0, x);
}

function hash_param(n1, n2, x) {
  return Caml_primitive.caml_hash(n1, n2, 0, x);
}

function seeded_hash(seed, x) {
  return Caml_primitive.caml_hash(10, 100, seed, x);
}

var params;

try {
  params = Caml_sys.caml_sys_getenv("OCAMLRUNPARAM");
}
catch (exn){
  if (exn === Caml_builtin_exceptions.Not_found) {
    try {
      params = Caml_sys.caml_sys_getenv("CAMLRUNPARAM");
    }
    catch (exn$1){
      if (exn$1 === Caml_builtin_exceptions.Not_found) {
        params = "";
      }
      else {
        throw exn$1;
      }
    }
  }
  else {
    throw exn;
  }
}

var randomized_default = $$String.contains(params, /* "R" */82);

var randomized = [randomized_default];

function randomize() {
  randomized[0] = /* true */1;
  return /* () */0;
}

var prng = {
  0: function () {
    return Caml_curry.app1(Random.State[1], /* () */0);
  },
  length: 1,
  tag: 246
};

function power_2_above(_x, n) {
  while(true) {
    var x = _x;
    if (x >= n) {
      return x;
    }
    else if (x * 2 > Sys.max_array_length) {
      return x;
    }
    else {
      _x = x * 2;
      continue ;
      
    }
  };
}

function create($staropt$star, initial_size) {
  var random = $staropt$star ? $staropt$star[0] : randomized[0];
  var s = power_2_above(16, initial_size);
  var seed;
  if (random) {
    var tag = prng.tag | 0;
    seed = Caml_curry.app1(Random.State[3], tag === 250 ? prng[0] : (
            tag === 246 ? CamlinternalLazy.force_lazy_block(prng) : prng
          ));
  }
  else {
    seed = 0;
  }
  return /* record */[
          0,
          Caml_array.caml_make_vect(s, /* Empty */0),
          seed,
          s
        ];
}

function clear(h) {
  h[0] = 0;
  var len = h[1].length;
  for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
    h[1][i] = /* Empty */0;
  }
  return /* () */0;
}

function reset(h) {
  var len = h[1].length;
  if (h.length < 4 || len === h[3]) {
    return clear(h);
  }
  else {
    h[0] = 0;
    h[1] = Caml_array.caml_make_vect(h[3], /* Empty */0);
    return /* () */0;
  }
}

function copy(h) {
  return /* record */[
          h[0],
          $$Array.copy(h[1]),
          h[2],
          h[3]
        ];
}

function length(h) {
  return h[0];
}

function resize(indexfun, h) {
  var odata = h[1];
  var osize = odata.length;
  var nsize = osize * 2;
  if (nsize < Sys.max_array_length) {
    var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
    h[1] = ndata;
    var insert_bucket = function (param) {
      if (param) {
        var key = param[0];
        insert_bucket(param[2]);
        var nidx = Caml_curry.app2(indexfun, h, key);
        ndata[nidx] = /* Cons */{
          0: key,
          1: param[1],
          2: ndata[nidx],
          length: 3,
          tag: 0
        };
        return /* () */0;
      }
      else {
        return /* () */0;
      }
    };
    for(var i = 0 ,i_finish = osize - 1; i<= i_finish; ++i){
      insert_bucket(odata[i]);
    }
    return /* () */0;
  }
  else {
    return 0;
  }
}

function key_index(h, key) {
  if (h.length >= 3) {
    return Caml_primitive.caml_hash(10, 100, h[2], key) & h[1].length - 1;
  }
  else {
    return Caml_primitive.caml_hash_univ_param(10, 100, key) % h[1].length;
  }
}

function add(h, key, info) {
  var i = key_index(h, key);
  var bucket_002 = h[1][i];
  var bucket = /* Cons */{
    0: key,
    1: info,
    2: bucket_002,
    length: 3,
    tag: 0
  };
  h[1][i] = bucket;
  ++ h[0];
  if (h[0] > (h[1].length << 1)) {
    return resize(key_index, h);
  }
  else {
    return 0;
  }
}

function remove(h, key) {
  var remove_bucket = function (param) {
    if (param) {
      var next = param[2];
      var k = param[0];
      if (Caml_obj.caml_compare(k, key)) {
        return /* Cons */{
                0: k,
                1: param[1],
                2: remove_bucket(next),
                length: 3,
                tag: 0
              };
      }
      else {
        -- h[0];
        return next;
      }
    }
    else {
      return /* Empty */0;
    }
  };
  var i = key_index(h, key);
  h[1][i] = remove_bucket(h[1][i]);
  return /* () */0;
}

function find(h, key) {
  var match = h[1][key_index(h, key)];
  if (match) {
    var rest1 = match[2];
    if (Caml_obj.caml_compare(key, match[0])) {
      if (rest1) {
        var rest2 = rest1[2];
        if (Caml_obj.caml_compare(key, rest1[0])) {
          if (rest2) {
            if (Caml_obj.caml_compare(key, rest2[0])) {
              var key$1 = key;
              var _param = rest2[2];
              while(true) {
                var param = _param;
                if (param) {
                  if (Caml_obj.caml_compare(key$1, param[0])) {
                    _param = param[2];
                    continue ;
                    
                  }
                  else {
                    return param[1];
                  }
                }
                else {
                  throw Caml_builtin_exceptions.Not_found;
                }
              };
            }
            else {
              return rest2[1];
            }
          }
          else {
            throw Caml_builtin_exceptions.Not_found;
          }
        }
        else {
          return rest1[1];
        }
      }
      else {
        throw Caml_builtin_exceptions.Not_found;
      }
    }
    else {
      return match[1];
    }
  }
  else {
    throw Caml_builtin_exceptions.Not_found;
  }
}

function find_all(h, key) {
  var find_in_bucket = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        var rest = param[2];
        if (Caml_obj.caml_compare(param[0], key)) {
          _param = rest;
          continue ;
          
        }
        else {
          return /* :: */[
                  param[1],
                  find_in_bucket(rest)
                ];
        }
      }
      else {
        return /* [] */0;
      }
    };
  };
  return find_in_bucket(h[1][key_index(h, key)]);
}

function replace(h, key, info) {
  var replace_bucket = function (param) {
    if (param) {
      var next = param[2];
      var k = param[0];
      if (Caml_obj.caml_compare(k, key)) {
        return /* Cons */{
                0: k,
                1: param[1],
                2: replace_bucket(next),
                length: 3,
                tag: 0
              };
      }
      else {
        return /* Cons */{
                0: key,
                1: info,
                2: next,
                length: 3,
                tag: 0
              };
      }
    }
    else {
      throw Caml_builtin_exceptions.Not_found;
    }
  };
  var i = key_index(h, key);
  var l = h[1][i];
  try {
    h[1][i] = replace_bucket(l);
    return /* () */0;
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.Not_found) {
      h[1][i] = /* Cons */{
        0: key,
        1: info,
        2: l,
        length: 3,
        tag: 0
      };
      ++ h[0];
      if (h[0] > (h[1].length << 1)) {
        return resize(key_index, h);
      }
      else {
        return 0;
      }
    }
    else {
      throw exn;
    }
  }
}

function mem(h, key) {
  var _param = h[1][key_index(h, key)];
  while(true) {
    var param = _param;
    if (param) {
      if (Caml_obj.caml_compare(param[0], key)) {
        _param = param[2];
        continue ;
        
      }
      else {
        return /* true */1;
      }
    }
    else {
      return /* false */0;
    }
  };
}

function iter(f, h) {
  var do_bucket = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        Caml_curry.app2(f, param[0], param[1]);
        _param = param[2];
        continue ;
        
      }
      else {
        return /* () */0;
      }
    };
  };
  var d = h[1];
  for(var i = 0 ,i_finish = d.length - 1; i<= i_finish; ++i){
    do_bucket(d[i]);
  }
  return /* () */0;
}

function fold(f, h, init) {
  var do_bucket = function (_b, _accu) {
    while(true) {
      var accu = _accu;
      var b = _b;
      if (b) {
        _accu = Caml_curry.app3(f, b[0], b[1], accu);
        _b = b[2];
        continue ;
        
      }
      else {
        return accu;
      }
    };
  };
  var d = h[1];
  var accu = init;
  for(var i = 0 ,i_finish = d.length - 1; i<= i_finish; ++i){
    accu = do_bucket(d[i], accu);
  }
  return accu;
}

function bucket_length(_accu, _param) {
  while(true) {
    var param = _param;
    var accu = _accu;
    if (param) {
      _param = param[2];
      _accu = accu + 1;
      continue ;
      
    }
    else {
      return accu;
    }
  };
}

function stats(h) {
  var mbl = $$Array.fold_left(function (m, b) {
        return Pervasives.max(m, bucket_length(0, b));
      }, 0, h[1]);
  var histo = Caml_array.caml_make_vect(mbl + 1, 0);
  $$Array.iter(function (b) {
        var l = bucket_length(0, b);
        histo[l] = histo[l] + 1;
        return /* () */0;
      }, h[1]);
  return /* record */[
          h[0],
          h[1].length,
          mbl,
          histo
        ];
}

function MakeSeeded(H) {
  var key_index = function (h, key) {
    return Caml_curry.app2(H[1], h[2], key) & h[1].length - 1;
  };
  var add = function (h, key, info) {
    var i = key_index(h, key);
    var bucket_002 = h[1][i];
    var bucket = /* Cons */{
      0: key,
      1: info,
      2: bucket_002,
      length: 3,
      tag: 0
    };
    h[1][i] = bucket;
    ++ h[0];
    if (h[0] > (h[1].length << 1)) {
      return resize(key_index, h);
    }
    else {
      return 0;
    }
  };
  var remove = function (h, key) {
    var remove_bucket = function (param) {
      if (param) {
        var next = param[2];
        var k = param[0];
        if (Caml_curry.app2(H[0], k, key)) {
          -- h[0];
          return next;
        }
        else {
          return /* Cons */{
                  0: k,
                  1: param[1],
                  2: remove_bucket(next),
                  length: 3,
                  tag: 0
                };
        }
      }
      else {
        return /* Empty */0;
      }
    };
    var i = key_index(h, key);
    h[1][i] = remove_bucket(h[1][i]);
    return /* () */0;
  };
  var find = function (h, key) {
    var match = h[1][key_index(h, key)];
    if (match) {
      var rest1 = match[2];
      if (Caml_curry.app2(H[0], key, match[0])) {
        return match[1];
      }
      else if (rest1) {
        var rest2 = rest1[2];
        if (Caml_curry.app2(H[0], key, rest1[0])) {
          return rest1[1];
        }
        else if (rest2) {
          if (Caml_curry.app2(H[0], key, rest2[0])) {
            return rest2[1];
          }
          else {
            var key$1 = key;
            var _param = rest2[2];
            while(true) {
              var param = _param;
              if (param) {
                if (Caml_curry.app2(H[0], key$1, param[0])) {
                  return param[1];
                }
                else {
                  _param = param[2];
                  continue ;
                  
                }
              }
              else {
                throw Caml_builtin_exceptions.Not_found;
              }
            };
          }
        }
        else {
          throw Caml_builtin_exceptions.Not_found;
        }
      }
      else {
        throw Caml_builtin_exceptions.Not_found;
      }
    }
    else {
      throw Caml_builtin_exceptions.Not_found;
    }
  };
  var find_all = function (h, key) {
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (param) {
          var rest = param[2];
          if (Caml_curry.app2(H[0], param[0], key)) {
            return /* :: */[
                    param[1],
                    find_in_bucket(rest)
                  ];
          }
          else {
            _param = rest;
            continue ;
            
          }
        }
        else {
          return /* [] */0;
        }
      };
    };
    return find_in_bucket(h[1][key_index(h, key)]);
  };
  var replace = function (h, key, info) {
    var replace_bucket = function (param) {
      if (param) {
        var next = param[2];
        var k = param[0];
        if (Caml_curry.app2(H[0], k, key)) {
          return /* Cons */{
                  0: key,
                  1: info,
                  2: next,
                  length: 3,
                  tag: 0
                };
        }
        else {
          return /* Cons */{
                  0: k,
                  1: param[1],
                  2: replace_bucket(next),
                  length: 3,
                  tag: 0
                };
        }
      }
      else {
        throw Caml_builtin_exceptions.Not_found;
      }
    };
    var i = key_index(h, key);
    var l = h[1][i];
    try {
      h[1][i] = replace_bucket(l);
      return /* () */0;
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.Not_found) {
        h[1][i] = /* Cons */{
          0: key,
          1: info,
          2: l,
          length: 3,
          tag: 0
        };
        ++ h[0];
        if (h[0] > (h[1].length << 1)) {
          return resize(key_index, h);
        }
        else {
          return 0;
        }
      }
      else {
        throw exn;
      }
    }
  };
  var mem = function (h, key) {
    var _param = h[1][key_index(h, key)];
    while(true) {
      var param = _param;
      if (param) {
        if (Caml_curry.app2(H[0], param[0], key)) {
          return /* true */1;
        }
        else {
          _param = param[2];
          continue ;
          
        }
      }
      else {
        return /* false */0;
      }
    };
  };
  return [
          create,
          clear,
          reset,
          copy,
          add,
          remove,
          find,
          find_all,
          replace,
          mem,
          iter,
          fold,
          length,
          stats
        ];
}

function Make(H) {
  var equal = H[0];
  var key_index = function (h, key) {
    return Caml_curry.app1(H[1], key) & h[1].length - 1;
  };
  var add = function (h, key, info) {
    var i = key_index(h, key);
    var bucket_002 = h[1][i];
    var bucket = /* Cons */{
      0: key,
      1: info,
      2: bucket_002,
      length: 3,
      tag: 0
    };
    h[1][i] = bucket;
    ++ h[0];
    if (h[0] > (h[1].length << 1)) {
      return resize(key_index, h);
    }
    else {
      return 0;
    }
  };
  var remove = function (h, key) {
    var remove_bucket = function (param) {
      if (param) {
        var next = param[2];
        var k = param[0];
        if (Caml_curry.app2(equal, k, key)) {
          -- h[0];
          return next;
        }
        else {
          return /* Cons */{
                  0: k,
                  1: param[1],
                  2: remove_bucket(next),
                  length: 3,
                  tag: 0
                };
        }
      }
      else {
        return /* Empty */0;
      }
    };
    var i = key_index(h, key);
    h[1][i] = remove_bucket(h[1][i]);
    return /* () */0;
  };
  var find = function (h, key) {
    var match = h[1][key_index(h, key)];
    if (match) {
      var rest1 = match[2];
      if (Caml_curry.app2(equal, key, match[0])) {
        return match[1];
      }
      else if (rest1) {
        var rest2 = rest1[2];
        if (Caml_curry.app2(equal, key, rest1[0])) {
          return rest1[1];
        }
        else if (rest2) {
          if (Caml_curry.app2(equal, key, rest2[0])) {
            return rest2[1];
          }
          else {
            var key$1 = key;
            var _param = rest2[2];
            while(true) {
              var param = _param;
              if (param) {
                if (Caml_curry.app2(equal, key$1, param[0])) {
                  return param[1];
                }
                else {
                  _param = param[2];
                  continue ;
                  
                }
              }
              else {
                throw Caml_builtin_exceptions.Not_found;
              }
            };
          }
        }
        else {
          throw Caml_builtin_exceptions.Not_found;
        }
      }
      else {
        throw Caml_builtin_exceptions.Not_found;
      }
    }
    else {
      throw Caml_builtin_exceptions.Not_found;
    }
  };
  var find_all = function (h, key) {
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (param) {
          var rest = param[2];
          if (Caml_curry.app2(equal, param[0], key)) {
            return /* :: */[
                    param[1],
                    find_in_bucket(rest)
                  ];
          }
          else {
            _param = rest;
            continue ;
            
          }
        }
        else {
          return /* [] */0;
        }
      };
    };
    return find_in_bucket(h[1][key_index(h, key)]);
  };
  var replace = function (h, key, info) {
    var replace_bucket = function (param) {
      if (param) {
        var next = param[2];
        var k = param[0];
        if (Caml_curry.app2(equal, k, key)) {
          return /* Cons */{
                  0: key,
                  1: info,
                  2: next,
                  length: 3,
                  tag: 0
                };
        }
        else {
          return /* Cons */{
                  0: k,
                  1: param[1],
                  2: replace_bucket(next),
                  length: 3,
                  tag: 0
                };
        }
      }
      else {
        throw Caml_builtin_exceptions.Not_found;
      }
    };
    var i = key_index(h, key);
    var l = h[1][i];
    try {
      h[1][i] = replace_bucket(l);
      return /* () */0;
    }
    catch (exn){
      if (exn === Caml_builtin_exceptions.Not_found) {
        h[1][i] = /* Cons */{
          0: key,
          1: info,
          2: l,
          length: 3,
          tag: 0
        };
        ++ h[0];
        if (h[0] > (h[1].length << 1)) {
          return resize(key_index, h);
        }
        else {
          return 0;
        }
      }
      else {
        throw exn;
      }
    }
  };
  var mem = function (h, key) {
    var _param = h[1][key_index(h, key)];
    while(true) {
      var param = _param;
      if (param) {
        if (Caml_curry.app2(equal, param[0], key)) {
          return /* true */1;
        }
        else {
          _param = param[2];
          continue ;
          
        }
      }
      else {
        return /* false */0;
      }
    };
  };
  var create$1 = function (sz) {
    return create(/* Some */[/* false */0], sz);
  };
  return [
          create$1,
          clear,
          reset,
          copy,
          add,
          remove,
          find,
          find_all,
          replace,
          mem,
          iter,
          fold,
          length,
          stats
        ];
}

function seeded_hash_param(prim, prim$1, prim$2, prim$3) {
  return Caml_primitive.caml_hash(prim, prim$1, prim$2, prim$3);
}

exports.create            = create;
exports.clear             = clear;
exports.reset             = reset;
exports.copy              = copy;
exports.add               = add;
exports.find              = find;
exports.find_all          = find_all;
exports.mem               = mem;
exports.remove            = remove;
exports.replace           = replace;
exports.iter              = iter;
exports.fold              = fold;
exports.length            = length;
exports.randomize         = randomize;
exports.stats             = stats;
exports.Make              = Make;
exports.MakeSeeded        = MakeSeeded;
exports.hash              = hash;
exports.seeded_hash       = seeded_hash;
exports.hash_param        = hash_param;
exports.seeded_hash_param = seeded_hash_param;
/* randomized_default Not a pure module */
