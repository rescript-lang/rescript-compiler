// Generated CODE, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj_runtime = require("../runtime/caml_obj_runtime");
var CamlinternalLazy = require("./camlinternalLazy");
var Caml_sys         = require("../runtime/caml_sys");
var Caml_exceptions  = require("../runtime/caml_exceptions");
var Pervasives       = require("./pervasives");
var Sys              = require("./sys");
var Caml_primitive   = require("../runtime/caml_primitive");
var Caml_array       = require("../runtime/caml_array");
var $$Array          = require("./array");
var $$String         = require("./string");
var Random           = require("./random");

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
  if (exn === Caml_exceptions.Not_found) {
    try {
      params = Caml_sys.caml_sys_getenv("CAMLRUNPARAM");
    }
    catch (exn$1){
      if (exn$1 === Caml_exceptions.Not_found) {
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

var randomized = [
  0,
  randomized_default
];

function randomize() {
  randomized[1] = /* true */1;
  return /* () */0;
}

var prng = [
  246,
  function () {
    return Random.State[2](/* () */0);
  }
];

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
    }
  };
}

function create($staropt$star, initial_size) {
  var random = $staropt$star ? $staropt$star[1] : randomized[1];
  var s = power_2_above(16, initial_size);
  var seed;
  if (random) {
    var tag = Caml_obj_runtime.caml_obj_tag(prng);
    seed = Random.State[4](tag === 250 ? prng[1] : (
            tag === 246 ? CamlinternalLazy.force_lazy_block(prng) : prng
          ));
  }
  else {
    seed = 0;
  }
  return [
          /* record */0,
          0,
          Caml_array.caml_make_vect(s, /* Empty */0),
          seed,
          s
        ];
}

function clear(h) {
  h[1] = 0;
  var len = h[2].length;
  for(var i = 0 ,i_finish = len - 1; i<= i_finish; ++i){
    h[2][i] = /* Empty */0;
  }
  return /* () */0;
}

function reset(h) {
  var len = h[2].length;
  if (h.length < 4 || len === h[4]) {
    return clear(h);
  }
  else {
    h[1] = 0;
    h[2] = Caml_array.caml_make_vect(h[4], /* Empty */0);
    return /* () */0;
  }
}

function copy(h) {
  return [
          /* record */0,
          h[1],
          $$Array.copy(h[2]),
          h[3],
          h[4]
        ];
}

function length(h) {
  return h[1];
}

function resize(indexfun, h) {
  var odata = h[2];
  var osize = odata.length;
  var nsize = osize * 2;
  if (nsize < Sys.max_array_length) {
    var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
    h[2] = ndata;
    var insert_bucket = function (param) {
      if (param) {
        var key = param[1];
        insert_bucket(param[3]);
        var nidx = indexfun(h, key);
        ndata[nidx] = [
          /* Cons */0,
          key,
          param[2],
          ndata[nidx]
        ];
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
    return Caml_primitive.caml_hash(10, 100, h[3], key) & h[2].length - 1;
  }
  else {
    return Caml_primitive.caml_hash_univ_param(10, 100, key) % h[2].length;
  }
}

function add(h, key, info) {
  var i = key_index(h, key);
  var bucket_003 = h[2][i];
  var bucket = [
    /* Cons */0,
    key,
    info,
    bucket_003
  ];
  h[2][i] = bucket;
  ++ h[1];
  if (h[1] > (h[2].length << 1)) {
    return resize(key_index, h);
  }
  else {
    return 0;
  }
}

function remove(h, key) {
  var remove_bucket = function (param) {
    if (param) {
      var next = param[3];
      var k = param[1];
      if (Caml_primitive.caml_compare(k, key)) {
        return [
                /* Cons */0,
                k,
                param[2],
                remove_bucket(next)
              ];
      }
      else {
        -- h[1];
        return next;
      }
    }
    else {
      return /* Empty */0;
    }
  };
  var i = key_index(h, key);
  h[2][i] = remove_bucket(h[2][i]);
  return /* () */0;
}

function find(h, key) {
  var match = h[2][key_index(h, key)];
  if (match) {
    var rest1 = match[3];
    if (Caml_primitive.caml_compare(key, match[1])) {
      if (rest1) {
        var rest2 = rest1[3];
        if (Caml_primitive.caml_compare(key, rest1[1])) {
          if (rest2) {
            if (Caml_primitive.caml_compare(key, rest2[1])) {
              var key$1 = key;
              var _param = rest2[3];
              while(true) {
                var param = _param;
                if (param) {
                  if (Caml_primitive.caml_compare(key$1, param[1])) {
                    _param = param[3];
                  }
                  else {
                    return param[2];
                  }
                }
                else {
                  throw Caml_exceptions.Not_found;
                }
              };
            }
            else {
              return rest2[2];
            }
          }
          else {
            throw Caml_exceptions.Not_found;
          }
        }
        else {
          return rest1[2];
        }
      }
      else {
        throw Caml_exceptions.Not_found;
      }
    }
    else {
      return match[2];
    }
  }
  else {
    throw Caml_exceptions.Not_found;
  }
}

function find_all(h, key) {
  var find_in_bucket = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        var rest = param[3];
        if (Caml_primitive.caml_compare(param[1], key)) {
          _param = rest;
        }
        else {
          return [
                  /* :: */0,
                  param[2],
                  find_in_bucket(rest)
                ];
        }
      }
      else {
        return /* [] */0;
      }
    };
  };
  return find_in_bucket(h[2][key_index(h, key)]);
}

function replace(h, key, info) {
  var replace_bucket = function (param) {
    if (param) {
      var next = param[3];
      var k = param[1];
      if (Caml_primitive.caml_compare(k, key)) {
        return [
                /* Cons */0,
                k,
                param[2],
                replace_bucket(next)
              ];
      }
      else {
        return [
                /* Cons */0,
                key,
                info,
                next
              ];
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
  var i = key_index(h, key);
  var l = h[2][i];
  try {
    h[2][i] = replace_bucket(l);
    return /* () */0;
  }
  catch (exn){
    if (exn === Caml_exceptions.Not_found) {
      h[2][i] = [
        /* Cons */0,
        key,
        info,
        l
      ];
      ++ h[1];
      if (h[1] > (h[2].length << 1)) {
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
  var mem_in_bucket = function (param) {
    if (param) {
      return +(Caml_primitive.caml_compare(param[1], key) === 0 || mem_in_bucket(param[3]));
    }
    else {
      return /* false */0;
    }
  };
  return mem_in_bucket(h[2][key_index(h, key)]);
}

function iter(f, h) {
  var do_bucket = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        f(param[1], param[2]);
        _param = param[3];
      }
      else {
        return /* () */0;
      }
    };
  };
  var d = h[2];
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
        _accu = f(b[1], b[2], accu);
        _b = b[3];
      }
      else {
        return accu;
      }
    };
  };
  var d = h[2];
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
      _param = param[3];
      _accu = accu + 1;
    }
    else {
      return accu;
    }
  };
}

function stats(h) {
  var mbl = $$Array.fold_left(function (m, b) {
        return Pervasives.max(m, bucket_length(0, b));
      }, 0, h[2]);
  var histo = Caml_array.caml_make_vect(mbl + 1, 0);
  $$Array.iter(function (b) {
        var l = bucket_length(0, b);
        histo[l] = histo[l] + 1;
        return /* () */0;
      }, h[2]);
  return [
          /* record */0,
          h[1],
          h[2].length,
          mbl,
          histo
        ];
}

function MakeSeeded(H) {
  var key_index = function (h, key) {
    return H[2](h[3], key) & h[2].length - 1;
  };
  var add = function (h, key, info) {
    var i = key_index(h, key);
    var bucket_003 = h[2][i];
    var bucket = [
      /* Cons */0,
      key,
      info,
      bucket_003
    ];
    h[2][i] = bucket;
    ++ h[1];
    if (h[1] > (h[2].length << 1)) {
      return resize(key_index, h);
    }
    else {
      return 0;
    }
  };
  var remove = function (h, key) {
    var remove_bucket = function (param) {
      if (param) {
        var next = param[3];
        var k = param[1];
        if (H[1](k, key)) {
          -- h[1];
          return next;
        }
        else {
          return [
                  /* Cons */0,
                  k,
                  param[2],
                  remove_bucket(next)
                ];
        }
      }
      else {
        return /* Empty */0;
      }
    };
    var i = key_index(h, key);
    h[2][i] = remove_bucket(h[2][i]);
    return /* () */0;
  };
  var find = function (h, key) {
    var match = h[2][key_index(h, key)];
    if (match) {
      var rest1 = match[3];
      if (H[1](key, match[1])) {
        return match[2];
      }
      else if (rest1) {
        var rest2 = rest1[3];
        if (H[1](key, rest1[1])) {
          return rest1[2];
        }
        else if (rest2) {
          if (H[1](key, rest2[1])) {
            return rest2[2];
          }
          else {
            var key$1 = key;
            var _param = rest2[3];
            while(true) {
              var param = _param;
              if (param) {
                if (H[1](key$1, param[1])) {
                  return param[2];
                }
                else {
                  _param = param[3];
                }
              }
              else {
                throw Caml_exceptions.Not_found;
              }
            };
          }
        }
        else {
          throw Caml_exceptions.Not_found;
        }
      }
      else {
        throw Caml_exceptions.Not_found;
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
  var find_all = function (h, key) {
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (param) {
          var rest = param[3];
          if (H[1](param[1], key)) {
            return [
                    /* :: */0,
                    param[2],
                    find_in_bucket(rest)
                  ];
          }
          else {
            _param = rest;
          }
        }
        else {
          return /* [] */0;
        }
      };
    };
    return find_in_bucket(h[2][key_index(h, key)]);
  };
  var replace = function (h, key, info) {
    var replace_bucket = function (param) {
      if (param) {
        var next = param[3];
        var k = param[1];
        if (H[1](k, key)) {
          return [
                  /* Cons */0,
                  key,
                  info,
                  next
                ];
        }
        else {
          return [
                  /* Cons */0,
                  k,
                  param[2],
                  replace_bucket(next)
                ];
        }
      }
      else {
        throw Caml_exceptions.Not_found;
      }
    };
    var i = key_index(h, key);
    var l = h[2][i];
    try {
      h[2][i] = replace_bucket(l);
      return /* () */0;
    }
    catch (exn){
      if (exn === Caml_exceptions.Not_found) {
        h[2][i] = [
          /* Cons */0,
          key,
          info,
          l
        ];
        ++ h[1];
        if (h[1] > (h[2].length << 1)) {
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
    var mem_in_bucket = function (param) {
      if (param) {
        return +(H[1](param[1], key) || mem_in_bucket(param[3]));
      }
      else {
        return /* false */0;
      }
    };
    return mem_in_bucket(h[2][key_index(h, key)]);
  };
  return [
          0,
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
  var equal = H[1];
  var key_index = function (h, key) {
    return H[2](key) & h[2].length - 1;
  };
  var add = function (h, key, info) {
    var i = key_index(h, key);
    var bucket_003 = h[2][i];
    var bucket = [
      /* Cons */0,
      key,
      info,
      bucket_003
    ];
    h[2][i] = bucket;
    ++ h[1];
    if (h[1] > (h[2].length << 1)) {
      return resize(key_index, h);
    }
    else {
      return 0;
    }
  };
  var remove = function (h, key) {
    var remove_bucket = function (param) {
      if (param) {
        var next = param[3];
        var k = param[1];
        if (equal(k, key)) {
          -- h[1];
          return next;
        }
        else {
          return [
                  /* Cons */0,
                  k,
                  param[2],
                  remove_bucket(next)
                ];
        }
      }
      else {
        return /* Empty */0;
      }
    };
    var i = key_index(h, key);
    h[2][i] = remove_bucket(h[2][i]);
    return /* () */0;
  };
  var find = function (h, key) {
    var match = h[2][key_index(h, key)];
    if (match) {
      var rest1 = match[3];
      if (equal(key, match[1])) {
        return match[2];
      }
      else if (rest1) {
        var rest2 = rest1[3];
        if (equal(key, rest1[1])) {
          return rest1[2];
        }
        else if (rest2) {
          if (equal(key, rest2[1])) {
            return rest2[2];
          }
          else {
            var key$1 = key;
            var _param = rest2[3];
            while(true) {
              var param = _param;
              if (param) {
                if (equal(key$1, param[1])) {
                  return param[2];
                }
                else {
                  _param = param[3];
                }
              }
              else {
                throw Caml_exceptions.Not_found;
              }
            };
          }
        }
        else {
          throw Caml_exceptions.Not_found;
        }
      }
      else {
        throw Caml_exceptions.Not_found;
      }
    }
    else {
      throw Caml_exceptions.Not_found;
    }
  };
  var find_all = function (h, key) {
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (param) {
          var rest = param[3];
          if (equal(param[1], key)) {
            return [
                    /* :: */0,
                    param[2],
                    find_in_bucket(rest)
                  ];
          }
          else {
            _param = rest;
          }
        }
        else {
          return /* [] */0;
        }
      };
    };
    return find_in_bucket(h[2][key_index(h, key)]);
  };
  var replace = function (h, key, info) {
    var replace_bucket = function (param) {
      if (param) {
        var next = param[3];
        var k = param[1];
        if (equal(k, key)) {
          return [
                  /* Cons */0,
                  key,
                  info,
                  next
                ];
        }
        else {
          return [
                  /* Cons */0,
                  k,
                  param[2],
                  replace_bucket(next)
                ];
        }
      }
      else {
        throw Caml_exceptions.Not_found;
      }
    };
    var i = key_index(h, key);
    var l = h[2][i];
    try {
      h[2][i] = replace_bucket(l);
      return /* () */0;
    }
    catch (exn){
      if (exn === Caml_exceptions.Not_found) {
        h[2][i] = [
          /* Cons */0,
          key,
          info,
          l
        ];
        ++ h[1];
        if (h[1] > (h[2].length << 1)) {
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
    var mem_in_bucket = function (param) {
      if (param) {
        return +(equal(param[1], key) || mem_in_bucket(param[3]));
      }
      else {
        return /* false */0;
      }
    };
    return mem_in_bucket(h[2][key_index(h, key)]);
  };
  var create$1 = function (sz) {
    return create([
                /* Some */0,
                /* false */0
              ], sz);
  };
  return [
          0,
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
