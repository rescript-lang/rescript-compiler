

import * as Obj from "./obj.js";
import * as Sys from "./sys.js";
import * as $$Array from "./array.js";
import * as Curry from "./curry.js";
import * as Random from "./random.js";
import * as Hashtbl from "./hashtbl.js";
import * as Caml_array from "./caml_array.js";
import * as Caml_int32 from "./caml_int32.js";
import * as Caml_option from "./caml_option.js";
import * as Caml_primitive from "./caml_primitive.js";
import * as CamlinternalLazy from "./camlinternalLazy.js";
import * as Caml_js_exceptions from "./caml_js_exceptions.js";

function create(param) {
  return Obj.Ephemeron.create(1);
}

function get_key(t) {
  return Obj.Ephemeron.get_key(t, 0);
}

function get_key_copy(t) {
  return Obj.Ephemeron.get_key_copy(t, 0);
}

function set_key(t, k) {
  return Obj.Ephemeron.set_key(t, 0, k);
}

function unset_key(t) {
  return Obj.Ephemeron.unset_key(t, 0);
}

function check_key(t) {
  return Obj.Ephemeron.check_key(t, 0);
}

function blit_key(t1, t2) {
  return Obj.Ephemeron.blit_key(t1, 0, t2, 0, 1);
}

function get_data(t) {
  return Obj.Ephemeron.get_data(t);
}

function get_data_copy(t) {
  return Obj.Ephemeron.get_data_copy(t);
}

function set_data(t, d) {
  return Obj.Ephemeron.set_data(t, d);
}

function unset_data(t) {
  return Obj.Ephemeron.unset_data(t);
}

function check_data(t) {
  return Obj.Ephemeron.check_data(t);
}

function blit_data(t1, t2) {
  return Obj.Ephemeron.blit_data(t1, t2);
}

function MakeSeeded(H) {
  var create = function (k, d) {
    var c = Obj.Ephemeron.create(1);
    Obj.Ephemeron.set_data(c, d);
    set_key(c, k);
    return c;
  };
  var hash = H.hash;
  var equal = function (c, k) {
    var k$prime = Obj.Ephemeron.get_key(c, 0);
    if (k$prime !== undefined) {
      if (Curry._2(H.equal, k, Caml_option.valFromOption(k$prime))) {
        return /* ETrue */0;
      } else {
        return /* EFalse */1;
      }
    } else {
      return /* EDead */2;
    }
  };
  var set_key_data = function (c, k, d) {
    Obj.Ephemeron.unset_data(c);
    set_key(c, k);
    return Obj.Ephemeron.set_data(c, d);
  };
  var power_2_above = function (_x, n) {
    while(true) {
      var x = _x;
      if (x >= n) {
        return x;
      }
      if ((x << 1) > Sys.max_array_length) {
        return x;
      }
      _x = (x << 1);
      continue ;
    };
  };
  var prng = {
    RE_LAZY: "todo",
    value: (function () {
        return Random.State.make_self_init(undefined);
      })
  };
  var create$1 = function (randomOpt, initial_size) {
    var random = randomOpt !== undefined ? randomOpt : Hashtbl.is_randomized(undefined);
    var s = power_2_above(16, initial_size);
    var seed = random ? Random.State.bits(CamlinternalLazy.force(prng)) : 0;
    return {
            size: 0,
            data: Caml_array.caml_make_vect(s, /* Empty */0),
            seed: seed,
            initial_size: s
          };
  };
  var clear = function (h) {
    h.size = 0;
    var len = h.data.length;
    for(var i = 0; i < len; ++i){
      Caml_array.caml_array_set(h.data, i, /* Empty */0);
    }
    
  };
  var reset = function (h) {
    var len = h.data.length;
    if (len === h.initial_size) {
      return clear(h);
    } else {
      h.size = 0;
      h.data = Caml_array.caml_make_vect(h.initial_size, /* Empty */0);
      return ;
    }
  };
  var copy = function (h) {
    return {
            size: h.size,
            data: $$Array.copy(h.data),
            seed: h.seed,
            initial_size: h.initial_size
          };
  };
  var key_index = function (h, hkey) {
    return hkey & (h.data.length - 1 | 0);
  };
  var clean = function (h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        if (check_key(c)) {
          return /* Cons */[
                  param[0],
                  c,
                  do_bucket(rest)
                ];
        }
        h.size = h.size - 1 | 0;
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var resize = function (h) {
    var odata = h.data;
    var osize = odata.length;
    var nsize = (osize << 1);
    clean(h);
    if (!(nsize < Sys.max_array_length && h.size >= (osize >>> 1))) {
      return ;
    }
    var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
    h.data = ndata;
    var insert_bucket = function (param) {
      if (!param) {
        return ;
      }
      var hkey = param[0];
      insert_bucket(param[2]);
      var nidx = key_index(h, hkey);
      return Caml_array.caml_array_set(ndata, nidx, /* Cons */[
                  hkey,
                  param[1],
                  Caml_array.caml_array_get(ndata, nidx)
                ]);
    };
    for(var i = 0; i < osize; ++i){
      insert_bucket(Caml_array.caml_array_get(odata, i));
    }
    
  };
  var add = function (h, key, info) {
    var hkey = Curry._2(hash, h.seed, key);
    var i = key_index(h, hkey);
    var container = create(key, info);
    var bucket_002 = Caml_array.caml_array_get(h.data, i);
    var bucket = /* Cons */[
      hkey,
      container,
      bucket_002
    ];
    Caml_array.caml_array_set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(h);
    }
    
  };
  var remove = function (h, key) {
    var hkey = Curry._2(hash, h.seed, key);
    var remove_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var next = param[2];
        var c = param[1];
        var hk = param[0];
        if (hkey !== hk) {
          return /* Cons */[
                  hk,
                  c,
                  remove_bucket(next)
                ];
        }
        var match = equal(c, key);
        switch (match) {
          case /* ETrue */0 :
              h.size = h.size - 1 | 0;
              return next;
          case /* EFalse */1 :
              return /* Cons */[
                      hk,
                      c,
                      remove_bucket(next)
                    ];
          case /* EDead */2 :
              h.size = h.size - 1 | 0;
              _param = next;
              continue ;
          
        }
      };
    };
    var i = key_index(h, hkey);
    return Caml_array.caml_array_set(h.data, i, remove_bucket(Caml_array.caml_array_get(h.data, i)));
  };
  var find = function (h, key) {
    var hkey = Curry._2(hash, h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (param) {
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = equal(c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = get_data(c);
          if (d !== undefined) {
            return Caml_option.valFromOption(d);
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  };
  var find_opt = function (h, key) {
    var hkey = Curry._2(hash, h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return ;
      }
      var rest = param[2];
      var c = param[1];
      if (hkey === param[0]) {
        var match = equal(c, key);
        if (match !== 0) {
          _param = rest;
          continue ;
        }
        var d = get_data(c);
        if (d !== undefined) {
          return d;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var find_all = function (h, key) {
    var hkey = Curry._2(hash, h.seed, key);
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* [] */0;
        }
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = equal(c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = get_data(c);
          if (d !== undefined) {
            return /* :: */[
                    Caml_option.valFromOption(d),
                    find_in_bucket(rest)
                  ];
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    return find_in_bucket(Caml_array.caml_array_get(h.data, key_index(h, hkey)));
  };
  var replace = function (h, key, info) {
    var hkey = Curry._2(hash, h.seed, key);
    var i = key_index(h, hkey);
    var l = Caml_array.caml_array_get(h.data, i);
    try {
      var _param = l;
      while(true) {
        var param = _param;
        if (param) {
          var next = param[2];
          var c = param[1];
          if (hkey === param[0]) {
            var match = equal(c, key);
            if (match === 0) {
              return set_key_data(c, key, info);
            }
            _param = next;
            continue ;
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
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        var container = create(key, info);
        Caml_array.caml_array_set(h.data, i, /* Cons */[
              hkey,
              container,
              l
            ]);
        h.size = h.size + 1 | 0;
        if (h.size > (h.data.length << 1)) {
          return resize(h);
        } else {
          return ;
        }
      }
      throw exn;
    }
  };
  var mem = function (h, key) {
    var hkey = Curry._2(hash, h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var rest = param[2];
      if (param[0] === hkey) {
        var match = equal(param[1], key);
        if (match === 0) {
          return true;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var iter = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return ;
        }
        var c = param[1];
        var match = get_key(c);
        var match$1 = get_data(c);
        if (match !== undefined && match$1 !== undefined) {
          Curry._2(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1));
        }
        _param = param[2];
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      do_bucket(Caml_array.caml_array_get(d, i));
    }
    
  };
  var fold = function (f, h, init) {
    var do_bucket = function (_b, _accu) {
      while(true) {
        var accu = _accu;
        var b = _b;
        if (!b) {
          return accu;
        }
        var c = b[1];
        var match = get_key(c);
        var match$1 = get_data(c);
        var accu$1 = match !== undefined && match$1 !== undefined ? Curry._3(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1), accu) : accu;
        _accu = accu$1;
        _b = b[2];
        continue ;
      };
    };
    var d = h.data;
    var accu = init;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      accu = do_bucket(Caml_array.caml_array_get(d, i), accu);
    }
    return accu;
  };
  var filter_map_inplace = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        var match = get_key(c);
        var match$1 = get_data(c);
        if (match !== undefined) {
          if (match$1 !== undefined) {
            var k = Caml_option.valFromOption(match);
            var new_d = Curry._2(f, k, Caml_option.valFromOption(match$1));
            if (new_d !== undefined) {
              set_key_data(c, k, Caml_option.valFromOption(new_d));
              return /* Cons */[
                      param[0],
                      c,
                      do_bucket(rest)
                    ];
            }
            _param = rest;
            continue ;
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var length = function (h) {
    return h.size;
  };
  var bucket_length = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      _param = param[2];
      _accu = accu + 1 | 0;
      continue ;
    };
  };
  var stats = function (h) {
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
  };
  var bucket_length_alive = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      var rest = param[2];
      if (check_key(param[1])) {
        _param = rest;
        _accu = accu + 1 | 0;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var stats_alive = function (h) {
    var size = {
      contents: 0
    };
    var mbl = $$Array.fold_left((function (m, b) {
            return Caml_primitive.caml_int_max(m, bucket_length_alive(0, b));
          }), 0, h.data);
    var histo = Caml_array.caml_make_vect(mbl + 1 | 0, 0);
    $$Array.iter((function (b) {
            var l = bucket_length_alive(0, b);
            size.contents = size.contents + l | 0;
            return Caml_array.caml_array_set(histo, l, Caml_array.caml_array_get(histo, l) + 1 | 0);
          }), h.data);
    return {
            num_bindings: size.contents,
            num_buckets: h.data.length,
            max_bucket_length: mbl,
            bucket_histogram: histo
          };
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
          stats: stats,
          clean: clean,
          stats_alive: stats_alive
        };
}

function Make(H) {
  var equal = H.equal;
  var hash = function (_seed, x) {
    return Curry._1(H.hash, x);
  };
  var create = function (k, d) {
    var c = Obj.Ephemeron.create(1);
    Obj.Ephemeron.set_data(c, d);
    set_key(c, k);
    return c;
  };
  var equal$1 = function (c, k) {
    var k$prime = Obj.Ephemeron.get_key(c, 0);
    if (k$prime !== undefined) {
      if (Curry._2(equal, k, Caml_option.valFromOption(k$prime))) {
        return /* ETrue */0;
      } else {
        return /* EFalse */1;
      }
    } else {
      return /* EDead */2;
    }
  };
  var set_key_data = function (c, k, d) {
    Obj.Ephemeron.unset_data(c);
    set_key(c, k);
    return Obj.Ephemeron.set_data(c, d);
  };
  var power_2_above = function (_x, n) {
    while(true) {
      var x = _x;
      if (x >= n) {
        return x;
      }
      if ((x << 1) > Sys.max_array_length) {
        return x;
      }
      _x = (x << 1);
      continue ;
    };
  };
  var prng = {
    RE_LAZY: "todo",
    value: (function () {
        return Random.State.make_self_init(undefined);
      })
  };
  var clear = function (h) {
    h.size = 0;
    var len = h.data.length;
    for(var i = 0; i < len; ++i){
      Caml_array.caml_array_set(h.data, i, /* Empty */0);
    }
    
  };
  var reset = function (h) {
    var len = h.data.length;
    if (len === h.initial_size) {
      return clear(h);
    } else {
      h.size = 0;
      h.data = Caml_array.caml_make_vect(h.initial_size, /* Empty */0);
      return ;
    }
  };
  var copy = function (h) {
    return {
            size: h.size,
            data: $$Array.copy(h.data),
            seed: h.seed,
            initial_size: h.initial_size
          };
  };
  var key_index = function (h, hkey) {
    return hkey & (h.data.length - 1 | 0);
  };
  var clean = function (h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        if (check_key(c)) {
          return /* Cons */[
                  param[0],
                  c,
                  do_bucket(rest)
                ];
        }
        h.size = h.size - 1 | 0;
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var resize = function (h) {
    var odata = h.data;
    var osize = odata.length;
    var nsize = (osize << 1);
    clean(h);
    if (!(nsize < Sys.max_array_length && h.size >= (osize >>> 1))) {
      return ;
    }
    var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
    h.data = ndata;
    var insert_bucket = function (param) {
      if (!param) {
        return ;
      }
      var hkey = param[0];
      insert_bucket(param[2]);
      var nidx = key_index(h, hkey);
      return Caml_array.caml_array_set(ndata, nidx, /* Cons */[
                  hkey,
                  param[1],
                  Caml_array.caml_array_get(ndata, nidx)
                ]);
    };
    for(var i = 0; i < osize; ++i){
      insert_bucket(Caml_array.caml_array_get(odata, i));
    }
    
  };
  var add = function (h, key, info) {
    var hkey = hash(h.seed, key);
    var i = key_index(h, hkey);
    var container = create(key, info);
    var bucket_002 = Caml_array.caml_array_get(h.data, i);
    var bucket = /* Cons */[
      hkey,
      container,
      bucket_002
    ];
    Caml_array.caml_array_set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(h);
    }
    
  };
  var remove = function (h, key) {
    var hkey = hash(h.seed, key);
    var remove_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var next = param[2];
        var c = param[1];
        var hk = param[0];
        if (hkey !== hk) {
          return /* Cons */[
                  hk,
                  c,
                  remove_bucket(next)
                ];
        }
        var match = equal$1(c, key);
        switch (match) {
          case /* ETrue */0 :
              h.size = h.size - 1 | 0;
              return next;
          case /* EFalse */1 :
              return /* Cons */[
                      hk,
                      c,
                      remove_bucket(next)
                    ];
          case /* EDead */2 :
              h.size = h.size - 1 | 0;
              _param = next;
              continue ;
          
        }
      };
    };
    var i = key_index(h, hkey);
    return Caml_array.caml_array_set(h.data, i, remove_bucket(Caml_array.caml_array_get(h.data, i)));
  };
  var find = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (param) {
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = equal$1(c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = get_data(c);
          if (d !== undefined) {
            return Caml_option.valFromOption(d);
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  };
  var find_opt = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return ;
      }
      var rest = param[2];
      var c = param[1];
      if (hkey === param[0]) {
        var match = equal$1(c, key);
        if (match !== 0) {
          _param = rest;
          continue ;
        }
        var d = get_data(c);
        if (d !== undefined) {
          return d;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var find_all = function (h, key) {
    var hkey = hash(h.seed, key);
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* [] */0;
        }
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = equal$1(c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = get_data(c);
          if (d !== undefined) {
            return /* :: */[
                    Caml_option.valFromOption(d),
                    find_in_bucket(rest)
                  ];
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    return find_in_bucket(Caml_array.caml_array_get(h.data, key_index(h, hkey)));
  };
  var replace = function (h, key, info) {
    var hkey = hash(h.seed, key);
    var i = key_index(h, hkey);
    var l = Caml_array.caml_array_get(h.data, i);
    try {
      var _param = l;
      while(true) {
        var param = _param;
        if (param) {
          var next = param[2];
          var c = param[1];
          if (hkey === param[0]) {
            var match = equal$1(c, key);
            if (match === 0) {
              return set_key_data(c, key, info);
            }
            _param = next;
            continue ;
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
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        var container = create(key, info);
        Caml_array.caml_array_set(h.data, i, /* Cons */[
              hkey,
              container,
              l
            ]);
        h.size = h.size + 1 | 0;
        if (h.size > (h.data.length << 1)) {
          return resize(h);
        } else {
          return ;
        }
      }
      throw exn;
    }
  };
  var mem = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var rest = param[2];
      if (param[0] === hkey) {
        var match = equal$1(param[1], key);
        if (match === 0) {
          return true;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var iter = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return ;
        }
        var c = param[1];
        var match = get_key(c);
        var match$1 = get_data(c);
        if (match !== undefined && match$1 !== undefined) {
          Curry._2(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1));
        }
        _param = param[2];
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      do_bucket(Caml_array.caml_array_get(d, i));
    }
    
  };
  var fold = function (f, h, init) {
    var do_bucket = function (_b, _accu) {
      while(true) {
        var accu = _accu;
        var b = _b;
        if (!b) {
          return accu;
        }
        var c = b[1];
        var match = get_key(c);
        var match$1 = get_data(c);
        var accu$1 = match !== undefined && match$1 !== undefined ? Curry._3(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1), accu) : accu;
        _accu = accu$1;
        _b = b[2];
        continue ;
      };
    };
    var d = h.data;
    var accu = init;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      accu = do_bucket(Caml_array.caml_array_get(d, i), accu);
    }
    return accu;
  };
  var filter_map_inplace = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        var match = get_key(c);
        var match$1 = get_data(c);
        if (match !== undefined) {
          if (match$1 !== undefined) {
            var k = Caml_option.valFromOption(match);
            var new_d = Curry._2(f, k, Caml_option.valFromOption(match$1));
            if (new_d !== undefined) {
              set_key_data(c, k, Caml_option.valFromOption(new_d));
              return /* Cons */[
                      param[0],
                      c,
                      do_bucket(rest)
                    ];
            }
            _param = rest;
            continue ;
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var length = function (h) {
    return h.size;
  };
  var bucket_length = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      _param = param[2];
      _accu = accu + 1 | 0;
      continue ;
    };
  };
  var stats = function (h) {
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
  };
  var bucket_length_alive = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      var rest = param[2];
      if (check_key(param[1])) {
        _param = rest;
        _accu = accu + 1 | 0;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var stats_alive = function (h) {
    var size = {
      contents: 0
    };
    var mbl = $$Array.fold_left((function (m, b) {
            return Caml_primitive.caml_int_max(m, bucket_length_alive(0, b));
          }), 0, h.data);
    var histo = Caml_array.caml_make_vect(mbl + 1 | 0, 0);
    $$Array.iter((function (b) {
            var l = bucket_length_alive(0, b);
            size.contents = size.contents + l | 0;
            return Caml_array.caml_array_set(histo, l, Caml_array.caml_array_get(histo, l) + 1 | 0);
          }), h.data);
    return {
            num_bindings: size.contents,
            num_buckets: h.data.length,
            max_bucket_length: mbl,
            bucket_histogram: histo
          };
  };
  var create$1 = function (sz) {
    var randomOpt = false;
    var random = randomOpt !== undefined ? randomOpt : Hashtbl.is_randomized(undefined);
    var s = power_2_above(16, sz);
    var seed = random ? Random.State.bits(CamlinternalLazy.force(prng)) : 0;
    return {
            size: 0,
            data: Caml_array.caml_make_vect(s, /* Empty */0),
            seed: seed,
            initial_size: s
          };
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
          stats: stats,
          clean: clean,
          stats_alive: stats_alive
        };
}

function create$1(param) {
  return Obj.Ephemeron.create(2);
}

function get_key1(t) {
  return Obj.Ephemeron.get_key(t, 0);
}

function get_key1_copy(t) {
  return Obj.Ephemeron.get_key_copy(t, 0);
}

function set_key1(t, k) {
  return Obj.Ephemeron.set_key(t, 0, k);
}

function unset_key1(t) {
  return Obj.Ephemeron.unset_key(t, 0);
}

function check_key1(t) {
  return Obj.Ephemeron.check_key(t, 0);
}

function get_key2(t) {
  return Obj.Ephemeron.get_key(t, 1);
}

function get_key2_copy(t) {
  return Obj.Ephemeron.get_key_copy(t, 1);
}

function set_key2(t, k) {
  return Obj.Ephemeron.set_key(t, 1, k);
}

function unset_key2(t) {
  return Obj.Ephemeron.unset_key(t, 1);
}

function check_key2(t) {
  return Obj.Ephemeron.check_key(t, 1);
}

function blit_key1(t1, t2) {
  return Obj.Ephemeron.blit_key(t1, 0, t2, 0, 1);
}

function blit_key2(t1, t2) {
  return Obj.Ephemeron.blit_key(t1, 1, t2, 1, 1);
}

function blit_key12(t1, t2) {
  return Obj.Ephemeron.blit_key(t1, 0, t2, 0, 2);
}

function get_data$1(t) {
  return Obj.Ephemeron.get_data(t);
}

function get_data_copy$1(t) {
  return Obj.Ephemeron.get_data_copy(t);
}

function set_data$1(t, d) {
  return Obj.Ephemeron.set_data(t, d);
}

function unset_data$1(t) {
  return Obj.Ephemeron.unset_data(t);
}

function check_data$1(t) {
  return Obj.Ephemeron.check_data(t);
}

function blit_data$1(t1, t2) {
  return Obj.Ephemeron.blit_data(t1, t2);
}

function MakeSeeded$1(H1, H2) {
  var create = function (param, d) {
    var c = Obj.Ephemeron.create(2);
    Obj.Ephemeron.set_data(c, d);
    set_key1(c, param[0]);
    set_key2(c, param[1]);
    return c;
  };
  var hash = function (seed, param) {
    return Curry._2(H1.hash, seed, param[0]) + Caml_int32.imul(Curry._2(H2.hash, seed, param[1]), 65599) | 0;
  };
  var equal = function (c, param) {
    var match = Obj.Ephemeron.get_key(c, 0);
    var match$1 = Obj.Ephemeron.get_key(c, 1);
    if (match !== undefined && match$1 !== undefined) {
      if (Curry._2(H1.equal, param[0], Caml_option.valFromOption(match)) && Curry._2(H2.equal, param[1], Caml_option.valFromOption(match$1))) {
        return /* ETrue */0;
      } else {
        return /* EFalse */1;
      }
    } else {
      return /* EDead */2;
    }
  };
  var get_key = function (c) {
    var match = Obj.Ephemeron.get_key(c, 0);
    var match$1 = Obj.Ephemeron.get_key(c, 1);
    if (match !== undefined && match$1 !== undefined) {
      return /* tuple */[
              Caml_option.valFromOption(match),
              Caml_option.valFromOption(match$1)
            ];
    }
    
  };
  var set_key_data = function (c, param, d) {
    Obj.Ephemeron.unset_data(c);
    set_key1(c, param[0]);
    set_key2(c, param[1]);
    return Obj.Ephemeron.set_data(c, d);
  };
  var check_key = function (c) {
    if (Obj.Ephemeron.check_key(c, 0)) {
      return Obj.Ephemeron.check_key(c, 1);
    } else {
      return false;
    }
  };
  var power_2_above = function (_x, n) {
    while(true) {
      var x = _x;
      if (x >= n) {
        return x;
      }
      if ((x << 1) > Sys.max_array_length) {
        return x;
      }
      _x = (x << 1);
      continue ;
    };
  };
  var prng = {
    RE_LAZY: "todo",
    value: (function () {
        return Random.State.make_self_init(undefined);
      })
  };
  var create$1 = function (randomOpt, initial_size) {
    var random = randomOpt !== undefined ? randomOpt : Hashtbl.is_randomized(undefined);
    var s = power_2_above(16, initial_size);
    var seed = random ? Random.State.bits(CamlinternalLazy.force(prng)) : 0;
    return {
            size: 0,
            data: Caml_array.caml_make_vect(s, /* Empty */0),
            seed: seed,
            initial_size: s
          };
  };
  var clear = function (h) {
    h.size = 0;
    var len = h.data.length;
    for(var i = 0; i < len; ++i){
      Caml_array.caml_array_set(h.data, i, /* Empty */0);
    }
    
  };
  var reset = function (h) {
    var len = h.data.length;
    if (len === h.initial_size) {
      return clear(h);
    } else {
      h.size = 0;
      h.data = Caml_array.caml_make_vect(h.initial_size, /* Empty */0);
      return ;
    }
  };
  var copy = function (h) {
    return {
            size: h.size,
            data: $$Array.copy(h.data),
            seed: h.seed,
            initial_size: h.initial_size
          };
  };
  var key_index = function (h, hkey) {
    return hkey & (h.data.length - 1 | 0);
  };
  var clean = function (h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        if (check_key(c)) {
          return /* Cons */[
                  param[0],
                  c,
                  do_bucket(rest)
                ];
        }
        h.size = h.size - 1 | 0;
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var resize = function (h) {
    var odata = h.data;
    var osize = odata.length;
    var nsize = (osize << 1);
    clean(h);
    if (!(nsize < Sys.max_array_length && h.size >= (osize >>> 1))) {
      return ;
    }
    var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
    h.data = ndata;
    var insert_bucket = function (param) {
      if (!param) {
        return ;
      }
      var hkey = param[0];
      insert_bucket(param[2]);
      var nidx = key_index(h, hkey);
      return Caml_array.caml_array_set(ndata, nidx, /* Cons */[
                  hkey,
                  param[1],
                  Caml_array.caml_array_get(ndata, nidx)
                ]);
    };
    for(var i = 0; i < osize; ++i){
      insert_bucket(Caml_array.caml_array_get(odata, i));
    }
    
  };
  var add = function (h, key, info) {
    var hkey = hash(h.seed, key);
    var i = key_index(h, hkey);
    var container = create(key, info);
    var bucket_002 = Caml_array.caml_array_get(h.data, i);
    var bucket = /* Cons */[
      hkey,
      container,
      bucket_002
    ];
    Caml_array.caml_array_set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(h);
    }
    
  };
  var remove = function (h, key) {
    var hkey = hash(h.seed, key);
    var remove_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var next = param[2];
        var c = param[1];
        var hk = param[0];
        if (hkey !== hk) {
          return /* Cons */[
                  hk,
                  c,
                  remove_bucket(next)
                ];
        }
        var match = equal(c, key);
        switch (match) {
          case /* ETrue */0 :
              h.size = h.size - 1 | 0;
              return next;
          case /* EFalse */1 :
              return /* Cons */[
                      hk,
                      c,
                      remove_bucket(next)
                    ];
          case /* EDead */2 :
              h.size = h.size - 1 | 0;
              _param = next;
              continue ;
          
        }
      };
    };
    var i = key_index(h, hkey);
    return Caml_array.caml_array_set(h.data, i, remove_bucket(Caml_array.caml_array_get(h.data, i)));
  };
  var find = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (param) {
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = equal(c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = get_data$1(c);
          if (d !== undefined) {
            return Caml_option.valFromOption(d);
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  };
  var find_opt = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return ;
      }
      var rest = param[2];
      var c = param[1];
      if (hkey === param[0]) {
        var match = equal(c, key);
        if (match !== 0) {
          _param = rest;
          continue ;
        }
        var d = get_data$1(c);
        if (d !== undefined) {
          return d;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var find_all = function (h, key) {
    var hkey = hash(h.seed, key);
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* [] */0;
        }
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = equal(c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = get_data$1(c);
          if (d !== undefined) {
            return /* :: */[
                    Caml_option.valFromOption(d),
                    find_in_bucket(rest)
                  ];
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    return find_in_bucket(Caml_array.caml_array_get(h.data, key_index(h, hkey)));
  };
  var replace = function (h, key, info) {
    var hkey = hash(h.seed, key);
    var i = key_index(h, hkey);
    var l = Caml_array.caml_array_get(h.data, i);
    try {
      var _param = l;
      while(true) {
        var param = _param;
        if (param) {
          var next = param[2];
          var c = param[1];
          if (hkey === param[0]) {
            var match = equal(c, key);
            if (match === 0) {
              return set_key_data(c, key, info);
            }
            _param = next;
            continue ;
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
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        var container = create(key, info);
        Caml_array.caml_array_set(h.data, i, /* Cons */[
              hkey,
              container,
              l
            ]);
        h.size = h.size + 1 | 0;
        if (h.size > (h.data.length << 1)) {
          return resize(h);
        } else {
          return ;
        }
      }
      throw exn;
    }
  };
  var mem = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var rest = param[2];
      if (param[0] === hkey) {
        var match = equal(param[1], key);
        if (match === 0) {
          return true;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var iter = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return ;
        }
        var c = param[1];
        var match = get_key(c);
        var match$1 = get_data$1(c);
        if (match !== undefined && match$1 !== undefined) {
          Curry._2(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1));
        }
        _param = param[2];
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      do_bucket(Caml_array.caml_array_get(d, i));
    }
    
  };
  var fold = function (f, h, init) {
    var do_bucket = function (_b, _accu) {
      while(true) {
        var accu = _accu;
        var b = _b;
        if (!b) {
          return accu;
        }
        var c = b[1];
        var match = get_key(c);
        var match$1 = get_data$1(c);
        var accu$1 = match !== undefined && match$1 !== undefined ? Curry._3(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1), accu) : accu;
        _accu = accu$1;
        _b = b[2];
        continue ;
      };
    };
    var d = h.data;
    var accu = init;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      accu = do_bucket(Caml_array.caml_array_get(d, i), accu);
    }
    return accu;
  };
  var filter_map_inplace = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        var match = get_key(c);
        var match$1 = get_data$1(c);
        if (match !== undefined) {
          if (match$1 !== undefined) {
            var k = Caml_option.valFromOption(match);
            var new_d = Curry._2(f, k, Caml_option.valFromOption(match$1));
            if (new_d !== undefined) {
              set_key_data(c, k, Caml_option.valFromOption(new_d));
              return /* Cons */[
                      param[0],
                      c,
                      do_bucket(rest)
                    ];
            }
            _param = rest;
            continue ;
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var length = function (h) {
    return h.size;
  };
  var bucket_length = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      _param = param[2];
      _accu = accu + 1 | 0;
      continue ;
    };
  };
  var stats = function (h) {
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
  };
  var bucket_length_alive = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      var rest = param[2];
      if (check_key(param[1])) {
        _param = rest;
        _accu = accu + 1 | 0;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var stats_alive = function (h) {
    var size = {
      contents: 0
    };
    var mbl = $$Array.fold_left((function (m, b) {
            return Caml_primitive.caml_int_max(m, bucket_length_alive(0, b));
          }), 0, h.data);
    var histo = Caml_array.caml_make_vect(mbl + 1 | 0, 0);
    $$Array.iter((function (b) {
            var l = bucket_length_alive(0, b);
            size.contents = size.contents + l | 0;
            return Caml_array.caml_array_set(histo, l, Caml_array.caml_array_get(histo, l) + 1 | 0);
          }), h.data);
    return {
            num_bindings: size.contents,
            num_buckets: h.data.length,
            max_bucket_length: mbl,
            bucket_histogram: histo
          };
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
          stats: stats,
          clean: clean,
          stats_alive: stats_alive
        };
}

function Make$1(H1, H2) {
  var hash = function (_seed, x) {
    return Curry._1(H1.hash, x);
  };
  var partial_arg_equal = H1.equal;
  var hash$1 = function (_seed, x) {
    return Curry._1(H2.hash, x);
  };
  var H2_equal = H2.equal;
  var create = function (param, d) {
    var c = Obj.Ephemeron.create(2);
    Obj.Ephemeron.set_data(c, d);
    set_key1(c, param[0]);
    set_key2(c, param[1]);
    return c;
  };
  var hash$2 = function (seed, param) {
    return Curry._2(hash, seed, param[0]) + Caml_int32.imul(Curry._2(hash$1, seed, param[1]), 65599) | 0;
  };
  var equal = function (c, param) {
    var match = Obj.Ephemeron.get_key(c, 0);
    var match$1 = Obj.Ephemeron.get_key(c, 1);
    if (match !== undefined && match$1 !== undefined) {
      if (Curry._2(partial_arg_equal, param[0], Caml_option.valFromOption(match)) && Curry._2(H2_equal, param[1], Caml_option.valFromOption(match$1))) {
        return /* ETrue */0;
      } else {
        return /* EFalse */1;
      }
    } else {
      return /* EDead */2;
    }
  };
  var get_key = function (c) {
    var match = Obj.Ephemeron.get_key(c, 0);
    var match$1 = Obj.Ephemeron.get_key(c, 1);
    if (match !== undefined && match$1 !== undefined) {
      return /* tuple */[
              Caml_option.valFromOption(match),
              Caml_option.valFromOption(match$1)
            ];
    }
    
  };
  var set_key_data = function (c, param, d) {
    Obj.Ephemeron.unset_data(c);
    set_key1(c, param[0]);
    set_key2(c, param[1]);
    return Obj.Ephemeron.set_data(c, d);
  };
  var check_key = function (c) {
    if (Obj.Ephemeron.check_key(c, 0)) {
      return Obj.Ephemeron.check_key(c, 1);
    } else {
      return false;
    }
  };
  var power_2_above = function (_x, n) {
    while(true) {
      var x = _x;
      if (x >= n) {
        return x;
      }
      if ((x << 1) > Sys.max_array_length) {
        return x;
      }
      _x = (x << 1);
      continue ;
    };
  };
  var prng = {
    RE_LAZY: "todo",
    value: (function () {
        return Random.State.make_self_init(undefined);
      })
  };
  var create$1 = function (randomOpt, initial_size) {
    var random = randomOpt !== undefined ? randomOpt : Hashtbl.is_randomized(undefined);
    var s = power_2_above(16, initial_size);
    var seed = random ? Random.State.bits(CamlinternalLazy.force(prng)) : 0;
    return {
            size: 0,
            data: Caml_array.caml_make_vect(s, /* Empty */0),
            seed: seed,
            initial_size: s
          };
  };
  var clear = function (h) {
    h.size = 0;
    var len = h.data.length;
    for(var i = 0; i < len; ++i){
      Caml_array.caml_array_set(h.data, i, /* Empty */0);
    }
    
  };
  var reset = function (h) {
    var len = h.data.length;
    if (len === h.initial_size) {
      return clear(h);
    } else {
      h.size = 0;
      h.data = Caml_array.caml_make_vect(h.initial_size, /* Empty */0);
      return ;
    }
  };
  var copy = function (h) {
    return {
            size: h.size,
            data: $$Array.copy(h.data),
            seed: h.seed,
            initial_size: h.initial_size
          };
  };
  var key_index = function (h, hkey) {
    return hkey & (h.data.length - 1 | 0);
  };
  var clean = function (h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        if (Curry._1(check_key, c)) {
          return /* Cons */[
                  param[0],
                  c,
                  do_bucket(rest)
                ];
        }
        h.size = h.size - 1 | 0;
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var resize = function (h) {
    var odata = h.data;
    var osize = odata.length;
    var nsize = (osize << 1);
    clean(h);
    if (!(nsize < Sys.max_array_length && h.size >= (osize >>> 1))) {
      return ;
    }
    var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
    h.data = ndata;
    var insert_bucket = function (param) {
      if (!param) {
        return ;
      }
      var hkey = param[0];
      insert_bucket(param[2]);
      var nidx = key_index(h, hkey);
      return Caml_array.caml_array_set(ndata, nidx, /* Cons */[
                  hkey,
                  param[1],
                  Caml_array.caml_array_get(ndata, nidx)
                ]);
    };
    for(var i = 0; i < osize; ++i){
      insert_bucket(Caml_array.caml_array_get(odata, i));
    }
    
  };
  var add = function (h, key, info) {
    var hkey = Curry._2(hash$2, h.seed, key);
    var i = key_index(h, hkey);
    var container = Curry._2(create, key, info);
    var bucket_002 = Caml_array.caml_array_get(h.data, i);
    var bucket = /* Cons */[
      hkey,
      container,
      bucket_002
    ];
    Caml_array.caml_array_set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(h);
    }
    
  };
  var remove = function (h, key) {
    var hkey = Curry._2(hash$2, h.seed, key);
    var remove_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var next = param[2];
        var c = param[1];
        var hk = param[0];
        if (hkey !== hk) {
          return /* Cons */[
                  hk,
                  c,
                  remove_bucket(next)
                ];
        }
        var match = Curry._2(equal, c, key);
        switch (match) {
          case /* ETrue */0 :
              h.size = h.size - 1 | 0;
              return next;
          case /* EFalse */1 :
              return /* Cons */[
                      hk,
                      c,
                      remove_bucket(next)
                    ];
          case /* EDead */2 :
              h.size = h.size - 1 | 0;
              _param = next;
              continue ;
          
        }
      };
    };
    var i = key_index(h, hkey);
    return Caml_array.caml_array_set(h.data, i, remove_bucket(Caml_array.caml_array_get(h.data, i)));
  };
  var find = function (h, key) {
    var hkey = Curry._2(hash$2, h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (param) {
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = Curry._2(equal, c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = Curry._1(get_data$1, c);
          if (d !== undefined) {
            return Caml_option.valFromOption(d);
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  };
  var find_opt = function (h, key) {
    var hkey = Curry._2(hash$2, h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return ;
      }
      var rest = param[2];
      var c = param[1];
      if (hkey === param[0]) {
        var match = Curry._2(equal, c, key);
        if (match !== 0) {
          _param = rest;
          continue ;
        }
        var d = Curry._1(get_data$1, c);
        if (d !== undefined) {
          return d;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var find_all = function (h, key) {
    var hkey = Curry._2(hash$2, h.seed, key);
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* [] */0;
        }
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = Curry._2(equal, c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = Curry._1(get_data$1, c);
          if (d !== undefined) {
            return /* :: */[
                    Caml_option.valFromOption(d),
                    find_in_bucket(rest)
                  ];
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    return find_in_bucket(Caml_array.caml_array_get(h.data, key_index(h, hkey)));
  };
  var replace = function (h, key, info) {
    var hkey = Curry._2(hash$2, h.seed, key);
    var i = key_index(h, hkey);
    var l = Caml_array.caml_array_get(h.data, i);
    try {
      var _param = l;
      while(true) {
        var param = _param;
        if (param) {
          var next = param[2];
          var c = param[1];
          if (hkey === param[0]) {
            var match = Curry._2(equal, c, key);
            if (match === 0) {
              return Curry._3(set_key_data, c, key, info);
            }
            _param = next;
            continue ;
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
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        var container = Curry._2(create, key, info);
        Caml_array.caml_array_set(h.data, i, /* Cons */[
              hkey,
              container,
              l
            ]);
        h.size = h.size + 1 | 0;
        if (h.size > (h.data.length << 1)) {
          return resize(h);
        } else {
          return ;
        }
      }
      throw exn;
    }
  };
  var mem = function (h, key) {
    var hkey = Curry._2(hash$2, h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var rest = param[2];
      if (param[0] === hkey) {
        var match = Curry._2(equal, param[1], key);
        if (match === 0) {
          return true;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var iter = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return ;
        }
        var c = param[1];
        var match = Curry._1(get_key, c);
        var match$1 = Curry._1(get_data$1, c);
        if (match !== undefined && match$1 !== undefined) {
          Curry._2(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1));
        }
        _param = param[2];
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      do_bucket(Caml_array.caml_array_get(d, i));
    }
    
  };
  var fold = function (f, h, init) {
    var do_bucket = function (_b, _accu) {
      while(true) {
        var accu = _accu;
        var b = _b;
        if (!b) {
          return accu;
        }
        var c = b[1];
        var match = Curry._1(get_key, c);
        var match$1 = Curry._1(get_data$1, c);
        var accu$1 = match !== undefined && match$1 !== undefined ? Curry._3(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1), accu) : accu;
        _accu = accu$1;
        _b = b[2];
        continue ;
      };
    };
    var d = h.data;
    var accu = init;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      accu = do_bucket(Caml_array.caml_array_get(d, i), accu);
    }
    return accu;
  };
  var filter_map_inplace = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        var match = Curry._1(get_key, c);
        var match$1 = Curry._1(get_data$1, c);
        if (match !== undefined) {
          if (match$1 !== undefined) {
            var k = Caml_option.valFromOption(match);
            var new_d = Curry._2(f, k, Caml_option.valFromOption(match$1));
            if (new_d !== undefined) {
              Curry._3(set_key_data, c, k, Caml_option.valFromOption(new_d));
              return /* Cons */[
                      param[0],
                      c,
                      do_bucket(rest)
                    ];
            }
            _param = rest;
            continue ;
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var length = function (h) {
    return h.size;
  };
  var bucket_length = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      _param = param[2];
      _accu = accu + 1 | 0;
      continue ;
    };
  };
  var stats = function (h) {
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
  };
  var bucket_length_alive = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      var rest = param[2];
      if (Curry._1(check_key, param[1])) {
        _param = rest;
        _accu = accu + 1 | 0;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var stats_alive = function (h) {
    var size = {
      contents: 0
    };
    var mbl = $$Array.fold_left((function (m, b) {
            return Caml_primitive.caml_int_max(m, bucket_length_alive(0, b));
          }), 0, h.data);
    var histo = Caml_array.caml_make_vect(mbl + 1 | 0, 0);
    $$Array.iter((function (b) {
            var l = bucket_length_alive(0, b);
            size.contents = size.contents + l | 0;
            return Caml_array.caml_array_set(histo, l, Caml_array.caml_array_get(histo, l) + 1 | 0);
          }), h.data);
    return {
            num_bindings: size.contents,
            num_buckets: h.data.length,
            max_bucket_length: mbl,
            bucket_histogram: histo
          };
  };
  var create$2 = create$1;
  var create$3 = function (sz) {
    return Curry._2(create$2, false, sz);
  };
  return {
          create: create$3,
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
          stats: stats,
          clean: clean,
          stats_alive: stats_alive
        };
}

function create$2(n) {
  return Obj.Ephemeron.create(n);
}

function get_key$1(t, n) {
  return Obj.Ephemeron.get_key(t, n);
}

function get_key_copy$1(t, n) {
  return Obj.Ephemeron.get_key_copy(t, n);
}

function set_key$1(t, n, k) {
  return Obj.Ephemeron.set_key(t, n, k);
}

function unset_key$1(t, n) {
  return Obj.Ephemeron.unset_key(t, n);
}

function check_key$1(t, n) {
  return Obj.Ephemeron.check_key(t, n);
}

function blit_key$1(t1, o1, t2, o2, l) {
  return Obj.Ephemeron.blit_key(t1, o1, t2, o2, l);
}

function get_data$2(t) {
  return Obj.Ephemeron.get_data(t);
}

function get_data_copy$2(t) {
  return Obj.Ephemeron.get_data_copy(t);
}

function set_data$2(t, d) {
  return Obj.Ephemeron.set_data(t, d);
}

function unset_data$2(t) {
  return Obj.Ephemeron.unset_data(t);
}

function check_data$2(t) {
  return Obj.Ephemeron.check_data(t);
}

function blit_data$2(t1, t2) {
  return Obj.Ephemeron.blit_data(t1, t2);
}

function MakeSeeded$2(H) {
  var create = function (k, d) {
    var c = Obj.Ephemeron.create(k.length);
    Obj.Ephemeron.set_data(c, d);
    for(var i = 0 ,i_finish = k.length; i < i_finish; ++i){
      set_key$1(c, i, Caml_array.caml_array_get(k, i));
    }
    return c;
  };
  var hash = function (seed, k) {
    var h = 0;
    for(var i = 0 ,i_finish = k.length; i < i_finish; ++i){
      h = Caml_int32.imul(Curry._2(H.hash, seed, Caml_array.caml_array_get(k, i)), 65599) + h | 0;
    }
    return h;
  };
  var equal = function (c, k) {
    var len = k.length;
    var len$prime = Obj.Ephemeron.length(c);
    if (len !== len$prime) {
      return /* EFalse */1;
    }
    var _i = len - 1 | 0;
    while(true) {
      var i = _i;
      if (i < 0) {
        return /* ETrue */0;
      }
      var ki = Obj.Ephemeron.get_key(c, i);
      if (ki === undefined) {
        return /* EDead */2;
      }
      if (!Curry._2(H.equal, Caml_array.caml_array_get(k, i), Caml_option.valFromOption(ki))) {
        return /* EFalse */1;
      }
      _i = i - 1 | 0;
      continue ;
    };
  };
  var get_key = function (c) {
    var len = Obj.Ephemeron.length(c);
    if (len === 0) {
      return [];
    }
    var k0 = Obj.Ephemeron.get_key(c, 0);
    if (k0 === undefined) {
      return ;
    }
    var a = Caml_array.caml_make_vect(len, Caml_option.valFromOption(k0));
    var _i = len - 1 | 0;
    while(true) {
      var i = _i;
      if (i < 1) {
        return a;
      }
      var ki = Obj.Ephemeron.get_key(c, i);
      if (ki === undefined) {
        return ;
      }
      Caml_array.caml_array_set(a, i, Caml_option.valFromOption(ki));
      _i = i - 1 | 0;
      continue ;
    };
  };
  var set_key_data = function (c, k, d) {
    Obj.Ephemeron.unset_data(c);
    for(var i = 0 ,i_finish = k.length; i < i_finish; ++i){
      set_key$1(c, i, Caml_array.caml_array_get(k, i));
    }
    return Obj.Ephemeron.set_data(c, d);
  };
  var check_key = function (c) {
    var _i = Obj.Ephemeron.length(c) - 1 | 0;
    while(true) {
      var i = _i;
      if (i < 0) {
        return true;
      }
      if (!Obj.Ephemeron.check_key(c, i)) {
        return false;
      }
      _i = i - 1 | 0;
      continue ;
    };
  };
  var power_2_above = function (_x, n) {
    while(true) {
      var x = _x;
      if (x >= n) {
        return x;
      }
      if ((x << 1) > Sys.max_array_length) {
        return x;
      }
      _x = (x << 1);
      continue ;
    };
  };
  var prng = {
    RE_LAZY: "todo",
    value: (function () {
        return Random.State.make_self_init(undefined);
      })
  };
  var create$1 = function (randomOpt, initial_size) {
    var random = randomOpt !== undefined ? randomOpt : Hashtbl.is_randomized(undefined);
    var s = power_2_above(16, initial_size);
    var seed = random ? Random.State.bits(CamlinternalLazy.force(prng)) : 0;
    return {
            size: 0,
            data: Caml_array.caml_make_vect(s, /* Empty */0),
            seed: seed,
            initial_size: s
          };
  };
  var clear = function (h) {
    h.size = 0;
    var len = h.data.length;
    for(var i = 0; i < len; ++i){
      Caml_array.caml_array_set(h.data, i, /* Empty */0);
    }
    
  };
  var reset = function (h) {
    var len = h.data.length;
    if (len === h.initial_size) {
      return clear(h);
    } else {
      h.size = 0;
      h.data = Caml_array.caml_make_vect(h.initial_size, /* Empty */0);
      return ;
    }
  };
  var copy = function (h) {
    return {
            size: h.size,
            data: $$Array.copy(h.data),
            seed: h.seed,
            initial_size: h.initial_size
          };
  };
  var key_index = function (h, hkey) {
    return hkey & (h.data.length - 1 | 0);
  };
  var clean = function (h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        if (check_key(c)) {
          return /* Cons */[
                  param[0],
                  c,
                  do_bucket(rest)
                ];
        }
        h.size = h.size - 1 | 0;
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var resize = function (h) {
    var odata = h.data;
    var osize = odata.length;
    var nsize = (osize << 1);
    clean(h);
    if (!(nsize < Sys.max_array_length && h.size >= (osize >>> 1))) {
      return ;
    }
    var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
    h.data = ndata;
    var insert_bucket = function (param) {
      if (!param) {
        return ;
      }
      var hkey = param[0];
      insert_bucket(param[2]);
      var nidx = key_index(h, hkey);
      return Caml_array.caml_array_set(ndata, nidx, /* Cons */[
                  hkey,
                  param[1],
                  Caml_array.caml_array_get(ndata, nidx)
                ]);
    };
    for(var i = 0; i < osize; ++i){
      insert_bucket(Caml_array.caml_array_get(odata, i));
    }
    
  };
  var add = function (h, key, info) {
    var hkey = hash(h.seed, key);
    var i = key_index(h, hkey);
    var container = create(key, info);
    var bucket_002 = Caml_array.caml_array_get(h.data, i);
    var bucket = /* Cons */[
      hkey,
      container,
      bucket_002
    ];
    Caml_array.caml_array_set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(h);
    }
    
  };
  var remove = function (h, key) {
    var hkey = hash(h.seed, key);
    var remove_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var next = param[2];
        var c = param[1];
        var hk = param[0];
        if (hkey !== hk) {
          return /* Cons */[
                  hk,
                  c,
                  remove_bucket(next)
                ];
        }
        var match = equal(c, key);
        switch (match) {
          case /* ETrue */0 :
              h.size = h.size - 1 | 0;
              return next;
          case /* EFalse */1 :
              return /* Cons */[
                      hk,
                      c,
                      remove_bucket(next)
                    ];
          case /* EDead */2 :
              h.size = h.size - 1 | 0;
              _param = next;
              continue ;
          
        }
      };
    };
    var i = key_index(h, hkey);
    return Caml_array.caml_array_set(h.data, i, remove_bucket(Caml_array.caml_array_get(h.data, i)));
  };
  var find = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (param) {
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = equal(c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = get_data$2(c);
          if (d !== undefined) {
            return Caml_option.valFromOption(d);
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  };
  var find_opt = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return ;
      }
      var rest = param[2];
      var c = param[1];
      if (hkey === param[0]) {
        var match = equal(c, key);
        if (match !== 0) {
          _param = rest;
          continue ;
        }
        var d = get_data$2(c);
        if (d !== undefined) {
          return d;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var find_all = function (h, key) {
    var hkey = hash(h.seed, key);
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* [] */0;
        }
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = equal(c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = get_data$2(c);
          if (d !== undefined) {
            return /* :: */[
                    Caml_option.valFromOption(d),
                    find_in_bucket(rest)
                  ];
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    return find_in_bucket(Caml_array.caml_array_get(h.data, key_index(h, hkey)));
  };
  var replace = function (h, key, info) {
    var hkey = hash(h.seed, key);
    var i = key_index(h, hkey);
    var l = Caml_array.caml_array_get(h.data, i);
    try {
      var _param = l;
      while(true) {
        var param = _param;
        if (param) {
          var next = param[2];
          var c = param[1];
          if (hkey === param[0]) {
            var match = equal(c, key);
            if (match === 0) {
              return set_key_data(c, key, info);
            }
            _param = next;
            continue ;
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
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        var container = create(key, info);
        Caml_array.caml_array_set(h.data, i, /* Cons */[
              hkey,
              container,
              l
            ]);
        h.size = h.size + 1 | 0;
        if (h.size > (h.data.length << 1)) {
          return resize(h);
        } else {
          return ;
        }
      }
      throw exn;
    }
  };
  var mem = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var rest = param[2];
      if (param[0] === hkey) {
        var match = equal(param[1], key);
        if (match === 0) {
          return true;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var iter = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return ;
        }
        var c = param[1];
        var match = get_key(c);
        var match$1 = get_data$2(c);
        if (match !== undefined && match$1 !== undefined) {
          Curry._2(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1));
        }
        _param = param[2];
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      do_bucket(Caml_array.caml_array_get(d, i));
    }
    
  };
  var fold = function (f, h, init) {
    var do_bucket = function (_b, _accu) {
      while(true) {
        var accu = _accu;
        var b = _b;
        if (!b) {
          return accu;
        }
        var c = b[1];
        var match = get_key(c);
        var match$1 = get_data$2(c);
        var accu$1 = match !== undefined && match$1 !== undefined ? Curry._3(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1), accu) : accu;
        _accu = accu$1;
        _b = b[2];
        continue ;
      };
    };
    var d = h.data;
    var accu = init;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      accu = do_bucket(Caml_array.caml_array_get(d, i), accu);
    }
    return accu;
  };
  var filter_map_inplace = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        var match = get_key(c);
        var match$1 = get_data$2(c);
        if (match !== undefined) {
          if (match$1 !== undefined) {
            var k = Caml_option.valFromOption(match);
            var new_d = Curry._2(f, k, Caml_option.valFromOption(match$1));
            if (new_d !== undefined) {
              set_key_data(c, k, Caml_option.valFromOption(new_d));
              return /* Cons */[
                      param[0],
                      c,
                      do_bucket(rest)
                    ];
            }
            _param = rest;
            continue ;
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var length = function (h) {
    return h.size;
  };
  var bucket_length = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      _param = param[2];
      _accu = accu + 1 | 0;
      continue ;
    };
  };
  var stats = function (h) {
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
  };
  var bucket_length_alive = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      var rest = param[2];
      if (check_key(param[1])) {
        _param = rest;
        _accu = accu + 1 | 0;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var stats_alive = function (h) {
    var size = {
      contents: 0
    };
    var mbl = $$Array.fold_left((function (m, b) {
            return Caml_primitive.caml_int_max(m, bucket_length_alive(0, b));
          }), 0, h.data);
    var histo = Caml_array.caml_make_vect(mbl + 1 | 0, 0);
    $$Array.iter((function (b) {
            var l = bucket_length_alive(0, b);
            size.contents = size.contents + l | 0;
            return Caml_array.caml_array_set(histo, l, Caml_array.caml_array_get(histo, l) + 1 | 0);
          }), h.data);
    return {
            num_bindings: size.contents,
            num_buckets: h.data.length,
            max_bucket_length: mbl,
            bucket_histogram: histo
          };
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
          stats: stats,
          clean: clean,
          stats_alive: stats_alive
        };
}

function Make$2(H) {
  var equal = H.equal;
  var create = function (k, d) {
    var c = Obj.Ephemeron.create(k.length);
    Obj.Ephemeron.set_data(c, d);
    for(var i = 0 ,i_finish = k.length; i < i_finish; ++i){
      set_key$1(c, i, Caml_array.caml_array_get(k, i));
    }
    return c;
  };
  var hash = function (seed, k) {
    var h = 0;
    for(var i = 0 ,i_finish = k.length; i < i_finish; ++i){
      h = Caml_int32.imul(Curry._1(H.hash, Caml_array.caml_array_get(k, i)), 65599) + h | 0;
    }
    return h;
  };
  var equal$1 = function (c, k) {
    var len = k.length;
    var len$prime = Obj.Ephemeron.length(c);
    if (len !== len$prime) {
      return /* EFalse */1;
    }
    var _i = len - 1 | 0;
    while(true) {
      var i = _i;
      if (i < 0) {
        return /* ETrue */0;
      }
      var ki = Obj.Ephemeron.get_key(c, i);
      if (ki === undefined) {
        return /* EDead */2;
      }
      if (!Curry._2(equal, Caml_array.caml_array_get(k, i), Caml_option.valFromOption(ki))) {
        return /* EFalse */1;
      }
      _i = i - 1 | 0;
      continue ;
    };
  };
  var get_key = function (c) {
    var len = Obj.Ephemeron.length(c);
    if (len === 0) {
      return [];
    }
    var k0 = Obj.Ephemeron.get_key(c, 0);
    if (k0 === undefined) {
      return ;
    }
    var a = Caml_array.caml_make_vect(len, Caml_option.valFromOption(k0));
    var _i = len - 1 | 0;
    while(true) {
      var i = _i;
      if (i < 1) {
        return a;
      }
      var ki = Obj.Ephemeron.get_key(c, i);
      if (ki === undefined) {
        return ;
      }
      Caml_array.caml_array_set(a, i, Caml_option.valFromOption(ki));
      _i = i - 1 | 0;
      continue ;
    };
  };
  var set_key_data = function (c, k, d) {
    Obj.Ephemeron.unset_data(c);
    for(var i = 0 ,i_finish = k.length; i < i_finish; ++i){
      set_key$1(c, i, Caml_array.caml_array_get(k, i));
    }
    return Obj.Ephemeron.set_data(c, d);
  };
  var check_key = function (c) {
    var _i = Obj.Ephemeron.length(c) - 1 | 0;
    while(true) {
      var i = _i;
      if (i < 0) {
        return true;
      }
      if (!Obj.Ephemeron.check_key(c, i)) {
        return false;
      }
      _i = i - 1 | 0;
      continue ;
    };
  };
  var power_2_above = function (_x, n) {
    while(true) {
      var x = _x;
      if (x >= n) {
        return x;
      }
      if ((x << 1) > Sys.max_array_length) {
        return x;
      }
      _x = (x << 1);
      continue ;
    };
  };
  var prng = {
    RE_LAZY: "todo",
    value: (function () {
        return Random.State.make_self_init(undefined);
      })
  };
  var clear = function (h) {
    h.size = 0;
    var len = h.data.length;
    for(var i = 0; i < len; ++i){
      Caml_array.caml_array_set(h.data, i, /* Empty */0);
    }
    
  };
  var reset = function (h) {
    var len = h.data.length;
    if (len === h.initial_size) {
      return clear(h);
    } else {
      h.size = 0;
      h.data = Caml_array.caml_make_vect(h.initial_size, /* Empty */0);
      return ;
    }
  };
  var copy = function (h) {
    return {
            size: h.size,
            data: $$Array.copy(h.data),
            seed: h.seed,
            initial_size: h.initial_size
          };
  };
  var key_index = function (h, hkey) {
    return hkey & (h.data.length - 1 | 0);
  };
  var clean = function (h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        if (check_key(c)) {
          return /* Cons */[
                  param[0],
                  c,
                  do_bucket(rest)
                ];
        }
        h.size = h.size - 1 | 0;
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var resize = function (h) {
    var odata = h.data;
    var osize = odata.length;
    var nsize = (osize << 1);
    clean(h);
    if (!(nsize < Sys.max_array_length && h.size >= (osize >>> 1))) {
      return ;
    }
    var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
    h.data = ndata;
    var insert_bucket = function (param) {
      if (!param) {
        return ;
      }
      var hkey = param[0];
      insert_bucket(param[2]);
      var nidx = key_index(h, hkey);
      return Caml_array.caml_array_set(ndata, nidx, /* Cons */[
                  hkey,
                  param[1],
                  Caml_array.caml_array_get(ndata, nidx)
                ]);
    };
    for(var i = 0; i < osize; ++i){
      insert_bucket(Caml_array.caml_array_get(odata, i));
    }
    
  };
  var add = function (h, key, info) {
    var hkey = hash(h.seed, key);
    var i = key_index(h, hkey);
    var container = create(key, info);
    var bucket_002 = Caml_array.caml_array_get(h.data, i);
    var bucket = /* Cons */[
      hkey,
      container,
      bucket_002
    ];
    Caml_array.caml_array_set(h.data, i, bucket);
    h.size = h.size + 1 | 0;
    if (h.size > (h.data.length << 1)) {
      return resize(h);
    }
    
  };
  var remove = function (h, key) {
    var hkey = hash(h.seed, key);
    var remove_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var next = param[2];
        var c = param[1];
        var hk = param[0];
        if (hkey !== hk) {
          return /* Cons */[
                  hk,
                  c,
                  remove_bucket(next)
                ];
        }
        var match = equal$1(c, key);
        switch (match) {
          case /* ETrue */0 :
              h.size = h.size - 1 | 0;
              return next;
          case /* EFalse */1 :
              return /* Cons */[
                      hk,
                      c,
                      remove_bucket(next)
                    ];
          case /* EDead */2 :
              h.size = h.size - 1 | 0;
              _param = next;
              continue ;
          
        }
      };
    };
    var i = key_index(h, hkey);
    return Caml_array.caml_array_set(h.data, i, remove_bucket(Caml_array.caml_array_get(h.data, i)));
  };
  var find = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (param) {
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = equal$1(c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = get_data$2(c);
          if (d !== undefined) {
            return Caml_option.valFromOption(d);
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  };
  var find_opt = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return ;
      }
      var rest = param[2];
      var c = param[1];
      if (hkey === param[0]) {
        var match = equal$1(c, key);
        if (match !== 0) {
          _param = rest;
          continue ;
        }
        var d = get_data$2(c);
        if (d !== undefined) {
          return d;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var find_all = function (h, key) {
    var hkey = hash(h.seed, key);
    var find_in_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* [] */0;
        }
        var rest = param[2];
        var c = param[1];
        if (hkey === param[0]) {
          var match = equal$1(c, key);
          if (match !== 0) {
            _param = rest;
            continue ;
          }
          var d = get_data$2(c);
          if (d !== undefined) {
            return /* :: */[
                    Caml_option.valFromOption(d),
                    find_in_bucket(rest)
                  ];
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    return find_in_bucket(Caml_array.caml_array_get(h.data, key_index(h, hkey)));
  };
  var replace = function (h, key, info) {
    var hkey = hash(h.seed, key);
    var i = key_index(h, hkey);
    var l = Caml_array.caml_array_get(h.data, i);
    try {
      var _param = l;
      while(true) {
        var param = _param;
        if (param) {
          var next = param[2];
          var c = param[1];
          if (hkey === param[0]) {
            var match = equal$1(c, key);
            if (match === 0) {
              return set_key_data(c, key, info);
            }
            _param = next;
            continue ;
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
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === "Not_found") {
        var container = create(key, info);
        Caml_array.caml_array_set(h.data, i, /* Cons */[
              hkey,
              container,
              l
            ]);
        h.size = h.size + 1 | 0;
        if (h.size > (h.data.length << 1)) {
          return resize(h);
        } else {
          return ;
        }
      }
      throw exn;
    }
  };
  var mem = function (h, key) {
    var hkey = hash(h.seed, key);
    var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
    while(true) {
      var param = _param;
      if (!param) {
        return false;
      }
      var rest = param[2];
      if (param[0] === hkey) {
        var match = equal$1(param[1], key);
        if (match === 0) {
          return true;
        }
        _param = rest;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var iter = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return ;
        }
        var c = param[1];
        var match = get_key(c);
        var match$1 = get_data$2(c);
        if (match !== undefined && match$1 !== undefined) {
          Curry._2(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1));
        }
        _param = param[2];
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      do_bucket(Caml_array.caml_array_get(d, i));
    }
    
  };
  var fold = function (f, h, init) {
    var do_bucket = function (_b, _accu) {
      while(true) {
        var accu = _accu;
        var b = _b;
        if (!b) {
          return accu;
        }
        var c = b[1];
        var match = get_key(c);
        var match$1 = get_data$2(c);
        var accu$1 = match !== undefined && match$1 !== undefined ? Curry._3(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1), accu) : accu;
        _accu = accu$1;
        _b = b[2];
        continue ;
      };
    };
    var d = h.data;
    var accu = init;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      accu = do_bucket(Caml_array.caml_array_get(d, i), accu);
    }
    return accu;
  };
  var filter_map_inplace = function (f, h) {
    var do_bucket = function (_param) {
      while(true) {
        var param = _param;
        if (!param) {
          return /* Empty */0;
        }
        var rest = param[2];
        var c = param[1];
        var match = get_key(c);
        var match$1 = get_data$2(c);
        if (match !== undefined) {
          if (match$1 !== undefined) {
            var k = Caml_option.valFromOption(match);
            var new_d = Curry._2(f, k, Caml_option.valFromOption(match$1));
            if (new_d !== undefined) {
              set_key_data(c, k, Caml_option.valFromOption(new_d));
              return /* Cons */[
                      param[0],
                      c,
                      do_bucket(rest)
                    ];
            }
            _param = rest;
            continue ;
          }
          _param = rest;
          continue ;
        }
        _param = rest;
        continue ;
      };
    };
    var d = h.data;
    for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
      Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
    }
    
  };
  var length = function (h) {
    return h.size;
  };
  var bucket_length = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      _param = param[2];
      _accu = accu + 1 | 0;
      continue ;
    };
  };
  var stats = function (h) {
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
  };
  var bucket_length_alive = function (_accu, _param) {
    while(true) {
      var param = _param;
      var accu = _accu;
      if (!param) {
        return accu;
      }
      var rest = param[2];
      if (check_key(param[1])) {
        _param = rest;
        _accu = accu + 1 | 0;
        continue ;
      }
      _param = rest;
      continue ;
    };
  };
  var stats_alive = function (h) {
    var size = {
      contents: 0
    };
    var mbl = $$Array.fold_left((function (m, b) {
            return Caml_primitive.caml_int_max(m, bucket_length_alive(0, b));
          }), 0, h.data);
    var histo = Caml_array.caml_make_vect(mbl + 1 | 0, 0);
    $$Array.iter((function (b) {
            var l = bucket_length_alive(0, b);
            size.contents = size.contents + l | 0;
            return Caml_array.caml_array_set(histo, l, Caml_array.caml_array_get(histo, l) + 1 | 0);
          }), h.data);
    return {
            num_bindings: size.contents,
            num_buckets: h.data.length,
            max_bucket_length: mbl,
            bucket_histogram: histo
          };
  };
  var create$1 = function (sz) {
    var randomOpt = false;
    var random = randomOpt !== undefined ? randomOpt : Hashtbl.is_randomized(undefined);
    var s = power_2_above(16, sz);
    var seed = random ? Random.State.bits(CamlinternalLazy.force(prng)) : 0;
    return {
            size: 0,
            data: Caml_array.caml_make_vect(s, /* Empty */0),
            seed: seed,
            initial_size: s
          };
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
          stats: stats,
          clean: clean,
          stats_alive: stats_alive
        };
}

var K1 = {
  create: create,
  get_key: get_key,
  get_key_copy: get_key_copy,
  set_key: set_key,
  unset_key: unset_key,
  check_key: check_key,
  blit_key: blit_key,
  get_data: get_data,
  get_data_copy: get_data_copy,
  set_data: set_data,
  unset_data: unset_data,
  check_data: check_data,
  blit_data: blit_data,
  Make: Make,
  MakeSeeded: MakeSeeded
};

var K2 = {
  create: create$1,
  get_key1: get_key1,
  get_key1_copy: get_key1_copy,
  set_key1: set_key1,
  unset_key1: unset_key1,
  check_key1: check_key1,
  get_key2: get_key2,
  get_key2_copy: get_key2_copy,
  set_key2: set_key2,
  unset_key2: unset_key2,
  check_key2: check_key2,
  blit_key1: blit_key1,
  blit_key2: blit_key2,
  blit_key12: blit_key12,
  get_data: get_data$1,
  get_data_copy: get_data_copy$1,
  set_data: set_data$1,
  unset_data: unset_data$1,
  check_data: check_data$1,
  blit_data: blit_data$1,
  Make: Make$1,
  MakeSeeded: MakeSeeded$1
};

var Kn = {
  create: create$2,
  get_key: get_key$1,
  get_key_copy: get_key_copy$1,
  set_key: set_key$1,
  unset_key: unset_key$1,
  check_key: check_key$1,
  blit_key: blit_key$1,
  get_data: get_data$2,
  get_data_copy: get_data_copy$2,
  set_data: set_data$2,
  unset_data: unset_data$2,
  check_data: check_data$2,
  blit_data: blit_data$2,
  Make: Make$2,
  MakeSeeded: MakeSeeded$2
};

var GenHashTable = {
  MakeSeeded: (function (funarg) {
      var H = {
        create: funarg.create,
        hash: funarg.hash,
        equal: funarg.equal,
        get_data: funarg.get_data,
        get_key: funarg.get_key,
        set_key_data: funarg.set_key_data,
        check_key: funarg.check_key
      };
      var power_2_above = function (_x, n) {
        while(true) {
          var x = _x;
          if (x >= n) {
            return x;
          }
          if ((x << 1) > Sys.max_array_length) {
            return x;
          }
          _x = (x << 1);
          continue ;
        };
      };
      var prng = {
        RE_LAZY: "todo",
        value: (function () {
            return Random.State.make_self_init(undefined);
          })
      };
      var create = function (randomOpt, initial_size) {
        var random = randomOpt !== undefined ? randomOpt : Hashtbl.is_randomized(undefined);
        var s = power_2_above(16, initial_size);
        var seed = random ? Random.State.bits(CamlinternalLazy.force(prng)) : 0;
        return {
                size: 0,
                data: Caml_array.caml_make_vect(s, /* Empty */0),
                seed: seed,
                initial_size: s
              };
      };
      var clear = function (h) {
        h.size = 0;
        var len = h.data.length;
        for(var i = 0; i < len; ++i){
          Caml_array.caml_array_set(h.data, i, /* Empty */0);
        }
        
      };
      var reset = function (h) {
        var len = h.data.length;
        if (len === h.initial_size) {
          return clear(h);
        } else {
          h.size = 0;
          h.data = Caml_array.caml_make_vect(h.initial_size, /* Empty */0);
          return ;
        }
      };
      var copy = function (h) {
        return {
                size: h.size,
                data: $$Array.copy(h.data),
                seed: h.seed,
                initial_size: h.initial_size
              };
      };
      var key_index = function (h, hkey) {
        return hkey & (h.data.length - 1 | 0);
      };
      var clean = function (h) {
        var do_bucket = function (_param) {
          while(true) {
            var param = _param;
            if (!param) {
              return /* Empty */0;
            }
            var rest = param[2];
            var c = param[1];
            if (Curry._1(H.check_key, c)) {
              return /* Cons */[
                      param[0],
                      c,
                      do_bucket(rest)
                    ];
            }
            h.size = h.size - 1 | 0;
            _param = rest;
            continue ;
          };
        };
        var d = h.data;
        for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
          Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
        }
        
      };
      var resize = function (h) {
        var odata = h.data;
        var osize = odata.length;
        var nsize = (osize << 1);
        clean(h);
        if (!(nsize < Sys.max_array_length && h.size >= (osize >>> 1))) {
          return ;
        }
        var ndata = Caml_array.caml_make_vect(nsize, /* Empty */0);
        h.data = ndata;
        var insert_bucket = function (param) {
          if (!param) {
            return ;
          }
          var hkey = param[0];
          insert_bucket(param[2]);
          var nidx = key_index(h, hkey);
          return Caml_array.caml_array_set(ndata, nidx, /* Cons */[
                      hkey,
                      param[1],
                      Caml_array.caml_array_get(ndata, nidx)
                    ]);
        };
        for(var i = 0; i < osize; ++i){
          insert_bucket(Caml_array.caml_array_get(odata, i));
        }
        
      };
      var add = function (h, key, info) {
        var hkey = Curry._2(H.hash, h.seed, key);
        var i = key_index(h, hkey);
        var container = Curry._2(H.create, key, info);
        var bucket_002 = Caml_array.caml_array_get(h.data, i);
        var bucket = /* Cons */[
          hkey,
          container,
          bucket_002
        ];
        Caml_array.caml_array_set(h.data, i, bucket);
        h.size = h.size + 1 | 0;
        if (h.size > (h.data.length << 1)) {
          return resize(h);
        }
        
      };
      var remove = function (h, key) {
        var hkey = Curry._2(H.hash, h.seed, key);
        var remove_bucket = function (_param) {
          while(true) {
            var param = _param;
            if (!param) {
              return /* Empty */0;
            }
            var next = param[2];
            var c = param[1];
            var hk = param[0];
            if (hkey !== hk) {
              return /* Cons */[
                      hk,
                      c,
                      remove_bucket(next)
                    ];
            }
            var match = Curry._2(H.equal, c, key);
            switch (match) {
              case /* ETrue */0 :
                  h.size = h.size - 1 | 0;
                  return next;
              case /* EFalse */1 :
                  return /* Cons */[
                          hk,
                          c,
                          remove_bucket(next)
                        ];
              case /* EDead */2 :
                  h.size = h.size - 1 | 0;
                  _param = next;
                  continue ;
              
            }
          };
        };
        var i = key_index(h, hkey);
        return Caml_array.caml_array_set(h.data, i, remove_bucket(Caml_array.caml_array_get(h.data, i)));
      };
      var find = function (h, key) {
        var hkey = Curry._2(H.hash, h.seed, key);
        var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
        while(true) {
          var param = _param;
          if (param) {
            var rest = param[2];
            var c = param[1];
            if (hkey === param[0]) {
              var match = Curry._2(H.equal, c, key);
              if (match !== 0) {
                _param = rest;
                continue ;
              }
              var d = Curry._1(H.get_data, c);
              if (d !== undefined) {
                return Caml_option.valFromOption(d);
              }
              _param = rest;
              continue ;
            }
            _param = rest;
            continue ;
          }
          throw {
                RE_EXN_ID: "Not_found",
                Error: new Error()
              };
        };
      };
      var find_opt = function (h, key) {
        var hkey = Curry._2(H.hash, h.seed, key);
        var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
        while(true) {
          var param = _param;
          if (!param) {
            return ;
          }
          var rest = param[2];
          var c = param[1];
          if (hkey === param[0]) {
            var match = Curry._2(H.equal, c, key);
            if (match !== 0) {
              _param = rest;
              continue ;
            }
            var d = Curry._1(H.get_data, c);
            if (d !== undefined) {
              return d;
            }
            _param = rest;
            continue ;
          }
          _param = rest;
          continue ;
        };
      };
      var find_all = function (h, key) {
        var hkey = Curry._2(H.hash, h.seed, key);
        var find_in_bucket = function (_param) {
          while(true) {
            var param = _param;
            if (!param) {
              return /* [] */0;
            }
            var rest = param[2];
            var c = param[1];
            if (hkey === param[0]) {
              var match = Curry._2(H.equal, c, key);
              if (match !== 0) {
                _param = rest;
                continue ;
              }
              var d = Curry._1(H.get_data, c);
              if (d !== undefined) {
                return /* :: */[
                        Caml_option.valFromOption(d),
                        find_in_bucket(rest)
                      ];
              }
              _param = rest;
              continue ;
            }
            _param = rest;
            continue ;
          };
        };
        return find_in_bucket(Caml_array.caml_array_get(h.data, key_index(h, hkey)));
      };
      var replace = function (h, key, info) {
        var hkey = Curry._2(H.hash, h.seed, key);
        var i = key_index(h, hkey);
        var l = Caml_array.caml_array_get(h.data, i);
        try {
          var _param = l;
          while(true) {
            var param = _param;
            if (param) {
              var next = param[2];
              var c = param[1];
              if (hkey === param[0]) {
                var match = Curry._2(H.equal, c, key);
                if (match === 0) {
                  return Curry._3(H.set_key_data, c, key, info);
                }
                _param = next;
                continue ;
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
        catch (raw_exn){
          var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
          if (exn.RE_EXN_ID === "Not_found") {
            var container = Curry._2(H.create, key, info);
            Caml_array.caml_array_set(h.data, i, /* Cons */[
                  hkey,
                  container,
                  l
                ]);
            h.size = h.size + 1 | 0;
            if (h.size > (h.data.length << 1)) {
              return resize(h);
            } else {
              return ;
            }
          }
          throw exn;
        }
      };
      var mem = function (h, key) {
        var hkey = Curry._2(H.hash, h.seed, key);
        var _param = Caml_array.caml_array_get(h.data, key_index(h, hkey));
        while(true) {
          var param = _param;
          if (!param) {
            return false;
          }
          var rest = param[2];
          if (param[0] === hkey) {
            var match = Curry._2(H.equal, param[1], key);
            if (match === 0) {
              return true;
            }
            _param = rest;
            continue ;
          }
          _param = rest;
          continue ;
        };
      };
      var iter = function (f, h) {
        var do_bucket = function (_param) {
          while(true) {
            var param = _param;
            if (!param) {
              return ;
            }
            var c = param[1];
            var match = Curry._1(H.get_key, c);
            var match$1 = Curry._1(H.get_data, c);
            if (match !== undefined && match$1 !== undefined) {
              Curry._2(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1));
            }
            _param = param[2];
            continue ;
          };
        };
        var d = h.data;
        for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
          do_bucket(Caml_array.caml_array_get(d, i));
        }
        
      };
      var fold = function (f, h, init) {
        var do_bucket = function (_b, _accu) {
          while(true) {
            var accu = _accu;
            var b = _b;
            if (!b) {
              return accu;
            }
            var c = b[1];
            var match = Curry._1(H.get_key, c);
            var match$1 = Curry._1(H.get_data, c);
            var accu$1 = match !== undefined && match$1 !== undefined ? Curry._3(f, Caml_option.valFromOption(match), Caml_option.valFromOption(match$1), accu) : accu;
            _accu = accu$1;
            _b = b[2];
            continue ;
          };
        };
        var d = h.data;
        var accu = init;
        for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
          accu = do_bucket(Caml_array.caml_array_get(d, i), accu);
        }
        return accu;
      };
      var filter_map_inplace = function (f, h) {
        var do_bucket = function (_param) {
          while(true) {
            var param = _param;
            if (!param) {
              return /* Empty */0;
            }
            var rest = param[2];
            var c = param[1];
            var match = Curry._1(H.get_key, c);
            var match$1 = Curry._1(H.get_data, c);
            if (match !== undefined) {
              if (match$1 !== undefined) {
                var k = Caml_option.valFromOption(match);
                var new_d = Curry._2(f, k, Caml_option.valFromOption(match$1));
                if (new_d !== undefined) {
                  Curry._3(H.set_key_data, c, k, Caml_option.valFromOption(new_d));
                  return /* Cons */[
                          param[0],
                          c,
                          do_bucket(rest)
                        ];
                }
                _param = rest;
                continue ;
              }
              _param = rest;
              continue ;
            }
            _param = rest;
            continue ;
          };
        };
        var d = h.data;
        for(var i = 0 ,i_finish = d.length; i < i_finish; ++i){
          Caml_array.caml_array_set(d, i, do_bucket(Caml_array.caml_array_get(d, i)));
        }
        
      };
      var length = function (h) {
        return h.size;
      };
      var bucket_length = function (_accu, _param) {
        while(true) {
          var param = _param;
          var accu = _accu;
          if (!param) {
            return accu;
          }
          _param = param[2];
          _accu = accu + 1 | 0;
          continue ;
        };
      };
      var stats = function (h) {
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
      };
      var bucket_length_alive = function (_accu, _param) {
        while(true) {
          var param = _param;
          var accu = _accu;
          if (!param) {
            return accu;
          }
          var rest = param[2];
          if (Curry._1(H.check_key, param[1])) {
            _param = rest;
            _accu = accu + 1 | 0;
            continue ;
          }
          _param = rest;
          continue ;
        };
      };
      var stats_alive = function (h) {
        var size = {
          contents: 0
        };
        var mbl = $$Array.fold_left((function (m, b) {
                return Caml_primitive.caml_int_max(m, bucket_length_alive(0, b));
              }), 0, h.data);
        var histo = Caml_array.caml_make_vect(mbl + 1 | 0, 0);
        $$Array.iter((function (b) {
                var l = bucket_length_alive(0, b);
                size.contents = size.contents + l | 0;
                return Caml_array.caml_array_set(histo, l, Caml_array.caml_array_get(histo, l) + 1 | 0);
              }), h.data);
        return {
                num_bindings: size.contents,
                num_buckets: h.data.length,
                max_bucket_length: mbl,
                bucket_histogram: histo
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
              stats: stats,
              clean: clean,
              stats_alive: stats_alive
            };
    })
};

export {
  K1 ,
  K2 ,
  Kn ,
  GenHashTable ,
  
}
/* No side effect */
