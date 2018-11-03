'use strict';

var $$Array = require("./array.js");
var Block = require("./block.js");
var Curry = require("./curry.js");
var Random = require("./random.js");
var Caml_obj = require("./caml_obj.js");
var Caml_hash = require("./caml_hash.js");
var Caml_array = require("./caml_array.js");
var Caml_primitive = require("./caml_primitive.js");
var CamlinternalLazy = require("./camlinternalLazy.js");
var Caml_missing_polyfill = require("./caml_missing_polyfill.js");
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

var randomized = /* record */[/* contents */false];

function randomize(param) {
  randomized[0] = true;
  return /* () */0;
}

var prng = Block.__(246, [(function (param) {
        return Random.State[/* make_self_init */1](/* () */0);
      })]);

function power_2_above(_x, n) {
  while(true) {
    var x = _x;
    if (x >= n || (x << 1) < x) {
      return x;
    } else {
      _x = (x << 1);
      continue ;
    }
  };
}

function create($staropt$star, initial_size) {
  var random = $staropt$star !== undefined ? $staropt$star : randomized[0];
  var s = power_2_above(16, initial_size);
  var seed;
  if (random) {
    var tag = prng.tag | 0;
    seed = Random.State[/* bits */3](tag === 250 ? prng[0] : (
            tag === 246 ? CamlinternalLazy.force_lazy_block(prng) : prng
          ));
  } else {
    seed = 0;
  }
  return /* record */[
          /* size */0,
          /* data */Caml_array.caml_make_vect(s, /* Empty */0),
          /* seed */seed,
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
  if (h.length < 4 || len === h[/* initial_size */3]) {
    return clear(h);
  } else {
    h[/* size */0] = 0;
    h[/* data */1] = Caml_array.caml_make_vect(h[/* initial_size */3], /* Empty */0);
    return /* () */0;
  }
}

function copy(h) {
  return /* record */[
          /* size */h[/* size */0],
          /* data */$$Array.copy(h[/* data */1]),
          /* seed */h[/* seed */2],
          /* initial_size */h[/* initial_size */3]
        ];
}

function length(h) {
  return h[/* size */0];
}

function resize(indexfun, h) {
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
        var nidx = Curry._2(indexfun, h, key);
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

function key_index(h, key) {
  if (h.length >= 3) {
    return Caml_hash.caml_hash(10, 100, h[/* seed */2], key) & (h[/* data */1].length - 1 | 0);
  } else {
    return Caml_missing_polyfill.not_implemented("caml_hash_univ_param") % h[/* data */1].length;
  }
}

function add(h, key, info) {
  var i = key_index(h, key);
  var bucket_002 = Caml_array.caml_array_get(h[/* data */1], i);
  var bucket = /* Cons */[
    key,
    info,
    bucket_002
  ];
  Caml_array.caml_array_set(h[/* data */1], i, bucket);
  h[/* size */0] = h[/* size */0] + 1 | 0;
  if (h[/* size */0] > (h[/* data */1].length << 1)) {
    return resize(key_index, h);
  } else {
    return 0;
  }
}

function remove(h, key) {
  var remove_bucket = function (param) {
    if (param) {
      var next = param[2];
      var k = param[0];
      if (Caml_obj.caml_equal(k, key)) {
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
  var i = key_index(h, key);
  return Caml_array.caml_array_set(h[/* data */1], i, remove_bucket(Caml_array.caml_array_get(h[/* data */1], i)));
}

function find(h, key) {
  var match = Caml_array.caml_array_get(h[/* data */1], key_index(h, key));
  if (match) {
    if (Caml_obj.caml_equal(key, match[0])) {
      return match[1];
    } else {
      var rest1 = match[2];
      if (rest1) {
        if (Caml_obj.caml_equal(key, rest1[0])) {
          return rest1[1];
        } else {
          var rest2 = rest1[2];
          if (rest2) {
            if (Caml_obj.caml_equal(key, rest2[0])) {
              return rest2[1];
            } else {
              var key$1 = key;
              var _param = rest2[2];
              while(true) {
                var param = _param;
                if (param) {
                  if (Caml_obj.caml_equal(key$1, param[0])) {
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

function find_all(h, key) {
  var find_in_bucket = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        var rest = param[2];
        if (Caml_obj.caml_equal(param[0], key)) {
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
  return find_in_bucket(Caml_array.caml_array_get(h[/* data */1], key_index(h, key)));
}

function replace(h, key, info) {
  var replace_bucket = function (param) {
    if (param) {
      var next = param[2];
      var k = param[0];
      if (Caml_obj.caml_equal(k, key)) {
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
  var i = key_index(h, key);
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
        return resize(key_index, h);
      } else {
        return 0;
      }
    } else {
      throw exn;
    }
  }
}

function mem(h, key) {
  var _param = Caml_array.caml_array_get(h[/* data */1], key_index(h, key));
  while(true) {
    var param = _param;
    if (param) {
      if (Caml_obj.caml_equal(param[0], key)) {
        return true;
      } else {
        _param = param[2];
        continue ;
      }
    } else {
      return false;
    }
  };
}

function iter(f, h) {
  var do_bucket = function (_param) {
    while(true) {
      var param = _param;
      if (param) {
        Curry._2(f, param[0], param[1]);
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
        _accu = Curry._3(f, b[0], b[1], accu);
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

function stats(h) {
  var mbl = $$Array.fold_left((function (m, b) {
          return Caml_primitive.caml_int_max(m, bucket_length(0, b));
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

function MakeSeeded(H) {
  var key_index = function (h, key) {
    return Curry._2(H[/* hash */1], h[/* seed */2], key) & (h[/* data */1].length - 1 | 0);
  };
  var add = function (h, key, info) {
    var i = key_index(h, key);
    var bucket_002 = Caml_array.caml_array_get(h[/* data */1], i);
    var bucket = /* Cons */[
      key,
      info,
      bucket_002
    ];
    Caml_array.caml_array_set(h[/* data */1], i, bucket);
    h[/* size */0] = h[/* size */0] + 1 | 0;
    if (h[/* size */0] > (h[/* data */1].length << 1)) {
      return resize(key_index, h);
    } else {
      return 0;
    }
  };
  var remove = function (h, key) {
    var remove_bucket = function (param) {
      if (param) {
        var next = param[2];
        var k = param[0];
        if (Curry._2(H[/* equal */0], k, key)) {
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
    var i = key_index(h, key);
    return Caml_array.caml_array_set(h[/* data */1], i, remove_bucket(Caml_array.caml_array_get(h[/* data */1], i)));
  };
  var find = function (h, key) {
    var match = Caml_array.caml_array_get(h[/* data */1], key_index(h, key));
    if (match) {
      var rest1 = match[2];
      if (Curry._2(H[/* equal */0], key, match[0])) {
        return match[1];
      } else if (rest1) {
        var rest2 = rest1[2];
        if (Curry._2(H[/* equal */0], key, rest1[0])) {
          return rest1[1];
        } else if (rest2) {
          if (Curry._2(H[/* equal */0], key, rest2[0])) {
            return rest2[1];
          } else {
            var key$1 = key;
            var _param = rest2[2];
            while(true) {
              var param = _param;
              if (param) {
                if (Curry._2(H[/* equal */0], key$1, param[0])) {
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
        } else {
          throw Caml_builtin_exceptions.not_found;
        }
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
  var find_all = function (h, key) {
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (param) {
          var rest = param[2];
          if (Curry._2(H[/* equal */0], param[0], key)) {
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
    return find_in_bucket(Caml_array.caml_array_get(h[/* data */1], key_index(h, key)));
  };
  var replace = function (h, key, info) {
    var replace_bucket = function (param) {
      if (param) {
        var next = param[2];
        var k = param[0];
        if (Curry._2(H[/* equal */0], k, key)) {
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
    var i = key_index(h, key);
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
          return resize(key_index, h);
        } else {
          return 0;
        }
      } else {
        throw exn;
      }
    }
  };
  var mem = function (h, key) {
    var _param = Caml_array.caml_array_get(h[/* data */1], key_index(h, key));
    while(true) {
      var param = _param;
      if (param) {
        if (Curry._2(H[/* equal */0], param[0], key)) {
          return true;
        } else {
          _param = param[2];
          continue ;
        }
      } else {
        return false;
      }
    };
  };
  return /* module */[
          /* create */create,
          /* clear */clear,
          /* reset */reset,
          /* copy */copy,
          /* add */add,
          /* remove */remove,
          /* find */find,
          /* find_all */find_all,
          /* replace */replace,
          /* mem */mem,
          /* iter */iter,
          /* fold */fold,
          /* length */length,
          /* stats */stats
        ];
}

function Make(H) {
  var equal = H[/* equal */0];
  var key_index = function (h, key) {
    return Curry._1(H[/* hash */1], key) & (h[/* data */1].length - 1 | 0);
  };
  var add = function (h, key, info) {
    var i = key_index(h, key);
    var bucket_002 = Caml_array.caml_array_get(h[/* data */1], i);
    var bucket = /* Cons */[
      key,
      info,
      bucket_002
    ];
    Caml_array.caml_array_set(h[/* data */1], i, bucket);
    h[/* size */0] = h[/* size */0] + 1 | 0;
    if (h[/* size */0] > (h[/* data */1].length << 1)) {
      return resize(key_index, h);
    } else {
      return 0;
    }
  };
  var remove = function (h, key) {
    var remove_bucket = function (param) {
      if (param) {
        var next = param[2];
        var k = param[0];
        if (Curry._2(equal, k, key)) {
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
    var i = key_index(h, key);
    return Caml_array.caml_array_set(h[/* data */1], i, remove_bucket(Caml_array.caml_array_get(h[/* data */1], i)));
  };
  var find = function (h, key) {
    var match = Caml_array.caml_array_get(h[/* data */1], key_index(h, key));
    if (match) {
      var rest1 = match[2];
      if (Curry._2(equal, key, match[0])) {
        return match[1];
      } else if (rest1) {
        var rest2 = rest1[2];
        if (Curry._2(equal, key, rest1[0])) {
          return rest1[1];
        } else if (rest2) {
          if (Curry._2(equal, key, rest2[0])) {
            return rest2[1];
          } else {
            var key$1 = key;
            var _param = rest2[2];
            while(true) {
              var param = _param;
              if (param) {
                if (Curry._2(equal, key$1, param[0])) {
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
        } else {
          throw Caml_builtin_exceptions.not_found;
        }
      } else {
        throw Caml_builtin_exceptions.not_found;
      }
    } else {
      throw Caml_builtin_exceptions.not_found;
    }
  };
  var find_all = function (h, key) {
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (param) {
          var rest = param[2];
          if (Curry._2(equal, param[0], key)) {
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
    return find_in_bucket(Caml_array.caml_array_get(h[/* data */1], key_index(h, key)));
  };
  var replace = function (h, key, info) {
    var replace_bucket = function (param) {
      if (param) {
        var next = param[2];
        var k = param[0];
        if (Curry._2(equal, k, key)) {
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
    var i = key_index(h, key);
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
          return resize(key_index, h);
        } else {
          return 0;
        }
      } else {
        throw exn;
      }
    }
  };
  var mem = function (h, key) {
    var _param = Caml_array.caml_array_get(h[/* data */1], key_index(h, key));
    while(true) {
      var param = _param;
      if (param) {
        if (Curry._2(equal, param[0], key)) {
          return true;
        } else {
          _param = param[2];
          continue ;
        }
      } else {
        return false;
      }
    };
  };
  var create$1 = function (sz) {
    return create(false, sz);
  };
  return /* module */[
          /* create */create$1,
          /* clear */clear,
          /* reset */reset,
          /* copy */copy,
          /* add */add,
          /* remove */remove,
          /* find */find,
          /* find_all */find_all,
          /* replace */replace,
          /* mem */mem,
          /* iter */iter,
          /* fold */fold,
          /* length */length,
          /* stats */stats
        ];
}

var seeded_hash_param = Caml_hash.caml_hash;

exports.create = create;
exports.clear = clear;
exports.reset = reset;
exports.copy = copy;
exports.add = add;
exports.find = find;
exports.find_all = find_all;
exports.mem = mem;
exports.remove = remove;
exports.replace = replace;
exports.iter = iter;
exports.fold = fold;
exports.length = length;
exports.randomize = randomize;
exports.stats = stats;
exports.Make = Make;
exports.MakeSeeded = MakeSeeded;
exports.hash = hash;
exports.seeded_hash = seeded_hash;
exports.hash_param = hash_param;
exports.seeded_hash_param = seeded_hash_param;
/* No side effect */
