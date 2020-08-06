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

var prng = {
  LAZY_DONE: false,
  VAL: (function () {
      return Random.State.make_self_init(undefined);
    })
};

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
    Caml_array.set(h.data, i, /* Empty */0);
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
  var key = param.key;
  var data = param.data;
  var next = param.next;
  var loop = function (_prec, _param) {
    while(true) {
      var param = _param;
      var prec = _prec;
      if (!param) {
        return ;
      }
      var key = param.key;
      var data = param.data;
      var next = param.next;
      var r = /* Cons */{
        key: key,
        data: data,
        next: next
      };
      if (prec) {
        prec.next = r;
      } else {
        throw {
              RE_EXN_ID: "Assert_failure",
              _1: [
                "hashtbl.ml",
                113,
                23
              ],
              Error: new Error()
            };
      }
      _param = next;
      _prec = r;
      continue ;
    };
  };
  var r = /* Cons */{
    key: key,
    data: data,
    next: next
  };
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
      var key = cell.key;
      var data = cell.data;
      var next = cell.next;
      var cell$1 = inplace ? cell : /* Cons */({
            key: key,
            data: data,
            next: /* Empty */0
          });
      var nidx = Curry._2(indexfun, h, key);
      var tail = Caml_array.get(ndata_tail, nidx);
      if (tail) {
        tail.next = cell$1;
      } else {
        Caml_array.set(ndata, nidx, cell$1);
      }
      Caml_array.set(ndata_tail, nidx, cell$1);
      _cell = next;
      continue ;
    };
  };
  for(var i = 0; i < osize; ++i){
    insert_bucket(Caml_array.get(odata, i));
  }
  if (!inplace) {
    return ;
  }
  for(var i$1 = 0; i$1 < nsize; ++i$1){
    var tail = Caml_array.get(ndata_tail, i$1);
    if (tail) {
      tail.next = /* Empty */0;
    }
    
  }
  
}

function key_index(h, key) {
  return Caml_hash.caml_hash(10, 100, h.seed, key) & (h.data.length - 1 | 0);
}

function add(h, key, data) {
  var i = key_index(h, key);
  var bucket = /* Cons */{
    key: key,
    data: data,
    next: Caml_array.get(h.data, i)
  };
  Caml_array.set(h.data, i, bucket);
  h.size = h.size + 1 | 0;
  if (h.size > (h.data.length << 1)) {
    return resize(key_index, h);
  }
  
}

function remove(h, key) {
  var i = key_index(h, key);
  var _prec = /* Empty */0;
  var _c = Caml_array.get(h.data, i);
  while(true) {
    var c = _c;
    var prec = _prec;
    if (!c) {
      return ;
    }
    var k = c.key;
    var next = c.next;
    if (Caml_obj.caml_equal(k, key)) {
      h.size = h.size - 1 | 0;
      if (prec) {
        prec.next = next;
        return ;
      } else {
        return Caml_array.set(h.data, i, next);
      }
    }
    _c = next;
    _prec = c;
    continue ;
  };
}

function find(h, key) {
  var match = Caml_array.get(h.data, key_index(h, key));
  if (match) {
    var k1 = match.key;
    var d1 = match.data;
    var next1 = match.next;
    if (Caml_obj.caml_equal(key, k1)) {
      return d1;
    }
    if (next1) {
      var k2 = next1.key;
      var d2 = next1.data;
      var next2 = next1.next;
      if (Caml_obj.caml_equal(key, k2)) {
        return d2;
      }
      if (next2) {
        var k3 = next2.key;
        var d3 = next2.data;
        var next3 = next2.next;
        if (Caml_obj.caml_equal(key, k3)) {
          return d3;
        } else {
          var _param = next3;
          while(true) {
            var param = _param;
            if (param) {
              var k = param.key;
              var data = param.data;
              var next = param.next;
              if (Caml_obj.caml_equal(key, k)) {
                return data;
              }
              _param = next;
              continue ;
            }
            throw {
                  RE_EXN_ID: "Not_found",
                  Error: new Error()
                };
          };
        }
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

function find_opt(h, key) {
  var match = Caml_array.get(h.data, key_index(h, key));
  if (!match) {
    return ;
  }
  var k1 = match.key;
  var d1 = match.data;
  var next1 = match.next;
  if (Caml_obj.caml_equal(key, k1)) {
    return Caml_option.some(d1);
  }
  if (!next1) {
    return ;
  }
  var k2 = next1.key;
  var d2 = next1.data;
  var next2 = next1.next;
  if (Caml_obj.caml_equal(key, k2)) {
    return Caml_option.some(d2);
  }
  if (!next2) {
    return ;
  }
  var k3 = next2.key;
  var d3 = next2.data;
  var next3 = next2.next;
  if (Caml_obj.caml_equal(key, k3)) {
    return Caml_option.some(d3);
  } else {
    var _param = next3;
    while(true) {
      var param = _param;
      if (!param) {
        return ;
      }
      var k = param.key;
      var data = param.data;
      var next = param.next;
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
      var k = param.key;
      var data = param.data;
      var next = param.next;
      if (Caml_obj.caml_equal(k, key)) {
        return {
                hd: data,
                tl: find_in_bucket(next)
              };
      }
      _param = next;
      continue ;
    };
  };
  return find_in_bucket(Caml_array.get(h.data, key_index(h, key)));
}

function replace_bucket(key, data, _slot) {
  while(true) {
    var slot = _slot;
    if (!slot) {
      return true;
    }
    var k = slot.key;
    var next = slot.next;
    if (Caml_obj.caml_equal(k, key)) {
      slot.key = key;
      slot.data = data;
      return false;
    }
    _slot = next;
    continue ;
  };
}

function replace(h, key, data) {
  var i = key_index(h, key);
  var l = Caml_array.get(h.data, i);
  if (replace_bucket(key, data, l)) {
    Caml_array.set(h.data, i, /* Cons */{
          key: key,
          data: data,
          next: l
        });
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(key_index, h);
    } else {
      return ;
    }
  }
  
}

function mem(h, key) {
  var _param = Caml_array.get(h.data, key_index(h, key));
  while(true) {
    var param = _param;
    if (!param) {
      return false;
    }
    var k = param.key;
    var next = param.next;
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
      var key = param.key;
      var data = param.data;
      var next = param.next;
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
      do_bucket(Caml_array.get(d, i));
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
        prec.next = /* Empty */0;
        return ;
      } else {
        return Caml_array.set(h.data, i, /* Empty */0);
      }
    }
    var key = slot.key;
    var data = slot.data;
    var next = slot.next;
    var data$1 = Curry._2(f, key, data);
    if (data$1 !== undefined) {
      if (prec) {
        prec.next = slot;
      } else {
        Caml_array.set(h.data, i, slot);
      }
      slot.data = Caml_option.valFromOption(data$1);
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
      filter_map_inplace_bucket(f, h, i, /* Empty */0, Caml_array.get(h.data, i));
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
      var key = b.key;
      var data = b.data;
      var next = b.next;
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
      accu = do_bucket(Caml_array.get(d, i), accu);
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
    var next = param.next;
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
          return Caml_array.set(histo, l, Caml_array.get(histo, l) + 1 | 0);
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
    var bucket = /* Cons */{
      key: key,
      data: data,
      next: Caml_array.get(h.data, i)
    };
    Caml_array.set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(key_index, h);
    }
    
  };
  var remove = function (h, key) {
    var i = key_index(h, key);
    var _prec = /* Empty */0;
    var _c = Caml_array.get(h.data, i);
    while(true) {
      var c = _c;
      var prec = _prec;
      if (!c) {
        return ;
      }
      var k = c.key;
      var next = c.next;
      if (Curry._2(H.equal, k, key)) {
        h.size = h.size - 1 | 0;
        if (prec) {
          prec.next = next;
          return ;
        } else {
          return Caml_array.set(h.data, i, next);
        }
      }
      _c = next;
      _prec = c;
      continue ;
    };
  };
  var find = function (h, key) {
    var match = Caml_array.get(h.data, key_index(h, key));
    if (match) {
      var k1 = match.key;
      var d1 = match.data;
      var next1 = match.next;
      if (Curry._2(H.equal, key, k1)) {
        return d1;
      }
      if (next1) {
        var k2 = next1.key;
        var d2 = next1.data;
        var next2 = next1.next;
        if (Curry._2(H.equal, key, k2)) {
          return d2;
        }
        if (next2) {
          var k3 = next2.key;
          var d3 = next2.data;
          var next3 = next2.next;
          if (Curry._2(H.equal, key, k3)) {
            return d3;
          } else {
            var _param = next3;
            while(true) {
              var param = _param;
              if (param) {
                var k = param.key;
                var data = param.data;
                var next = param.next;
                if (Curry._2(H.equal, key, k)) {
                  return data;
                }
                _param = next;
                continue ;
              }
              throw {
                    RE_EXN_ID: "Not_found",
                    Error: new Error()
                  };
            };
          }
        }
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
  var find_opt = function (h, key) {
    var match = Caml_array.get(h.data, key_index(h, key));
    if (!match) {
      return ;
    }
    var k1 = match.key;
    var d1 = match.data;
    var next1 = match.next;
    if (Curry._2(H.equal, key, k1)) {
      return Caml_option.some(d1);
    }
    if (!next1) {
      return ;
    }
    var k2 = next1.key;
    var d2 = next1.data;
    var next2 = next1.next;
    if (Curry._2(H.equal, key, k2)) {
      return Caml_option.some(d2);
    }
    if (!next2) {
      return ;
    }
    var k3 = next2.key;
    var d3 = next2.data;
    var next3 = next2.next;
    if (Curry._2(H.equal, key, k3)) {
      return Caml_option.some(d3);
    } else {
      var _param = next3;
      while(true) {
        var param = _param;
        if (!param) {
          return ;
        }
        var k = param.key;
        var data = param.data;
        var next = param.next;
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
        var k = param.key;
        var d = param.data;
        var next = param.next;
        if (Curry._2(H.equal, k, key)) {
          return {
                  hd: d,
                  tl: find_in_bucket(next)
                };
        }
        _param = next;
        continue ;
      };
    };
    return find_in_bucket(Caml_array.get(h.data, key_index(h, key)));
  };
  var replace_bucket = function (key, data, _slot) {
    while(true) {
      var slot = _slot;
      if (!slot) {
        return true;
      }
      var k = slot.key;
      var next = slot.next;
      if (Curry._2(H.equal, k, key)) {
        slot.key = key;
        slot.data = data;
        return false;
      }
      _slot = next;
      continue ;
    };
  };
  var replace = function (h, key, data) {
    var i = key_index(h, key);
    var l = Caml_array.get(h.data, i);
    if (replace_bucket(key, data, l)) {
      Caml_array.set(h.data, i, /* Cons */{
            key: key,
            data: data,
            next: l
          });
      h.size = h.size + 1 | 0;
      if (h.size > (h.data.length << 1)) {
        return resize(key_index, h);
      } else {
        return ;
      }
    }
    
  };
  var mem = function (h, key) {
    var _param = Caml_array.get(h.data, key_index(h, key));
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var k = param.key;
      var next = param.next;
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
    var bucket = /* Cons */{
      key: key,
      data: data,
      next: Caml_array.get(h.data, i)
    };
    Caml_array.set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(key_index, h);
    }
    
  };
  var remove = function (h, key) {
    var i = key_index(h, key);
    var _prec = /* Empty */0;
    var _c = Caml_array.get(h.data, i);
    while(true) {
      var c = _c;
      var prec = _prec;
      if (!c) {
        return ;
      }
      var k = c.key;
      var next = c.next;
      if (Curry._2(equal, k, key)) {
        h.size = h.size - 1 | 0;
        if (prec) {
          prec.next = next;
          return ;
        } else {
          return Caml_array.set(h.data, i, next);
        }
      }
      _c = next;
      _prec = c;
      continue ;
    };
  };
  var find = function (h, key) {
    var match = Caml_array.get(h.data, key_index(h, key));
    if (match) {
      var k1 = match.key;
      var d1 = match.data;
      var next1 = match.next;
      if (Curry._2(equal, key, k1)) {
        return d1;
      }
      if (next1) {
        var k2 = next1.key;
        var d2 = next1.data;
        var next2 = next1.next;
        if (Curry._2(equal, key, k2)) {
          return d2;
        }
        if (next2) {
          var k3 = next2.key;
          var d3 = next2.data;
          var next3 = next2.next;
          if (Curry._2(equal, key, k3)) {
            return d3;
          } else {
            var _param = next3;
            while(true) {
              var param = _param;
              if (param) {
                var k = param.key;
                var data = param.data;
                var next = param.next;
                if (Curry._2(equal, key, k)) {
                  return data;
                }
                _param = next;
                continue ;
              }
              throw {
                    RE_EXN_ID: "Not_found",
                    Error: new Error()
                  };
            };
          }
        }
        throw {
              RE_EXN_ID: "Not_found",
              Error: new Error()
            };
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Not_found",
          Error: new Error()
        };
  };
  var find_opt = function (h, key) {
    var match = Caml_array.get(h.data, key_index(h, key));
    if (!match) {
      return ;
    }
    var k1 = match.key;
    var d1 = match.data;
    var next1 = match.next;
    if (Curry._2(equal, key, k1)) {
      return Caml_option.some(d1);
    }
    if (!next1) {
      return ;
    }
    var k2 = next1.key;
    var d2 = next1.data;
    var next2 = next1.next;
    if (Curry._2(equal, key, k2)) {
      return Caml_option.some(d2);
    }
    if (!next2) {
      return ;
    }
    var k3 = next2.key;
    var d3 = next2.data;
    var next3 = next2.next;
    if (Curry._2(equal, key, k3)) {
      return Caml_option.some(d3);
    } else {
      var _param = next3;
      while(true) {
        var param = _param;
        if (!param) {
          return ;
        }
        var k = param.key;
        var data = param.data;
        var next = param.next;
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
        var k = param.key;
        var d = param.data;
        var next = param.next;
        if (Curry._2(equal, k, key)) {
          return {
                  hd: d,
                  tl: find_in_bucket(next)
                };
        }
        _param = next;
        continue ;
      };
    };
    return find_in_bucket(Caml_array.get(h.data, key_index(h, key)));
  };
  var replace_bucket = function (key, data, _slot) {
    while(true) {
      var slot = _slot;
      if (!slot) {
        return true;
      }
      var k = slot.key;
      var next = slot.next;
      if (Curry._2(equal, k, key)) {
        slot.key = key;
        slot.data = data;
        return false;
      }
      _slot = next;
      continue ;
    };
  };
  var replace = function (h, key, data) {
    var i = key_index(h, key);
    var l = Caml_array.get(h.data, i);
    if (replace_bucket(key, data, l)) {
      Caml_array.set(h.data, i, /* Cons */{
            key: key,
            data: data,
            next: l
          });
      h.size = h.size + 1 | 0;
      if (h.size > (h.data.length << 1)) {
        return resize(key_index, h);
      } else {
        return ;
      }
    }
    
  };
  var mem = function (h, key) {
    var _param = Caml_array.get(h.data, key_index(h, key));
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var k = param.key;
      var next = param.next;
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
